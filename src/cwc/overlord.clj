(ns cwc.overlord
  (:gen-class)
  (:import [java.net URL URLEncoder]
           java.util.Date)
  (:require [clj-http.client :as http]
            [flatland.useful.exception :refer [rescue]]
            [cheshire.core :as json]
            [clj-time.core :as t]
            [clj-time.coerce :as tc]
            [clojure.java.io :as io]
            [overtone.at-at :as at-]
            [cwc.game :as data]
            [clojurewerkz.urly.core :as urly]
            [clojure.tools.cli :refer [cli]]))

(def pool (at-/mk-pool))
(def game (atom nil))
(def cfg (atom {:seq 0}))
(def votes (atom {}))
(def usercounters (atom {:teams [0 0]}))

;;; lifted from newer urly
(defn ^String encode-path
  "Escapes input as URI path"
  [^String s]
  (.replace (URLEncoder/encode s) "+" "%20"))
;;; ^^ lifted

(defn db-get [k]
  (rescue
    (let [docuri (str (urly/resolve (:db @cfg) (encode-path k)))]
      (println "Grab:" docuri)
      (some-> docuri str
              (http/get {:as :json})
              :body))
    nil))

(defn db-put [doc k]
  (let [docuri (str (urly/resolve (:db @cfg) (encode-path k)))
        pres (:body (http/put docuri {:as :json
                                      :body (json/generate-string doc)
                                      :headers {"Content-Type" "application/json"}}))]
    (assoc doc :_rev (:rev pres))))

(defn ref->db [theref id]
  (add-watch theref ::store-db
             (fn [_key theref oldv newv]
               ;; Update did not change rev
               (when (= (:_rev oldv) (:_rev newv))
                 (println "Sending new game state...")
                 (swap! theref db-put id)))))

(defn tally-vote [vote]
  (println "Considering vote:" (pr-str vote))
  (let [votepart (select-keys vote [:game :locations :piece :team :turn])
        {:keys [locations piece team turn]} vote]
    (if (and (= (:number @game) (:game vote))
             (= (:turn @game) turn))
      (do (println "Vote is OK, counting")
          (swap! votes (fn [m] (merge-with + m {votepart 1}))))
      (println "Vote was for wrong turn or game."))))

(defn schedule-move [f]
  (at-/at (.getTime (:moveDeadline @game (Date.)))
          f pool :desc "Count votes and apply move"))

(defn add-usercounters [game]
  (-> game data/vectify
      (assoc-in [:teams 0 :participantCount] (get-in @usercounters [:teams 0]))
      (assoc-in [:teams 1 :participantCount] (get-in @usercounters [:teams 1]))))

(defn apply-votes []
  (println "Time's up, applying votes!")
  (if-let [most-pop (some->> @votes (sort-by val) last key)]
    (do (println "Most popular vote was:" (pr-str most-pop))
        (reset! votes {})
        (swap! game (fn [g] (-> g
                                (data/apply-move most-pop)
                                (data/affix-moves)))))
    ;else
    (do (println "No votes were cast! Resetting timer.")
        (swap! game assoc :moveDeadline
               (tc/to-date (t/plus (t/now) (t/seconds (:moveInterval @game)))))))
  (schedule-move apply-votes))


(defn count-user [user]
  (println "Saw user doc: " (pr-str user))
  (when (some-> user :team number?)
    (swap! usercounters update-in [:teams (:team user)] (fnil inc 0))
    (println "Counted user!" (:team user) @usercounters)))

(defn stream-watch []
  (let [changes (str (urly/resolve (:db @cfg) "_changes"))
        last-seq (:seq @cfg 0)
        stream (:body (http/get (str changes)
                                {:query-params {"feed" "continuous"
                                                "since" last-seq}
                                 :as :stream}))]
    (doseq [line (line-seq (io/reader stream))]
      (let [change (json/parse-string line true)
            id (:id change)]
        (println "Got change:" (pr-str change))
        (swap! cfg assoc :seq (:seq change))
        (when (and id (.startsWith id "user:"))
          (count-user (db-get id)))
        (when (and id (.startsWith id "vote:"))
          (tally-vote (db-get id))))))
  (println "Changes feed dropped! Restarting... (at seq.." (:seq @cfg) ")")
  (recur))

(defn counter-update []
  (println "Updating user counts" @usercounters)
  (swap! game add-usercounters))

(defn -main [& args]
  (let [[opts args usage]
        (cli args
             ["-d" "--db-url" "Database URL"]
             ["-i" "--interval" "Seconds per turn"
              :parse-fn read-string :default 30]
             ["-h" "--help" "Show this message"
              :flag true])
        {:keys [help db-url interval]} opts]
    (when (or help (not db-url))
      (println usage)
      (System/exit 1))
    (swap! cfg assoc :db db-url)
    (reset! game (-> (data/initial-game 1 interval)
                     (assoc :number (rand-int 999999))
                     data/affix-moves))
    ;; Start syncing game to DB
    (ref->db game "game-1")
    ;; Grab the ref of the doc currently in DB
    (swap! game assoc :_rev (or (:_rev (db-get "game-1")) ""))
    (swap! game db-put "game-1")
    ;; Start watching the changes stream
    (.start (Thread. stream-watch))
    ;; Set up job to apply ovtes
    (schedule-move apply-votes)
    ;; Periodically update the team counts
    (at-/every 30000 counter-update :desc "Counter-update" :fixed-delay true)))
