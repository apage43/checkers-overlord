(ns cwc.overlord
  (:gen-class)
  (:import [java.net URL URLEncoder]
           [java.util Date UUID])
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
(def votes-doc (atom {}))
(def usercounters (atom {:teams [0 0]}))

(defn make-gameid [] (str (UUID/randomUUID)))

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
  (loop [getrev false]
    (or (try
          (let [docuri (str (urly/resolve (:db @cfg) (encode-path k)))
                doc (if-not getrev doc (assoc doc :_rev (:_rev (db-get k) "")))
                pres (:body (http/put docuri {:as :json
                                              :body (json/generate-string doc)
                                              :headers {"Content-Type" "application/json"}}))]
            (assoc doc :_rev (:rev pres)))
          (catch Exception _e nil))
        (recur true))))

(defn db-kill [change]
  (rescue
    (let [{:keys [id changes deleted]} change
          rev (some-> changes first :rev)
          docuri (str (urly/resolve (:db @cfg) (encode-path id)))]
      ;; Don't delete already deleted docs
      (when-not deleted
        (http/delete docuri {:query-params {:rev rev}}))) nil))

(defn ref->db [theref id]
  (add-watch theref ::store-db
             (fn [_key theref oldv newv]
               ;; Update did not change rev
               (when (= (:_rev oldv) (:_rev newv))
                 (println "Sending new state for" id)
                 (swap! theref db-put id)))))

(declare votes-update)

(defn tally-vote [vote]
  (println "Considering vote:" (pr-str vote))
  (let [votepart (select-keys vote [:game :locations :piece :team :turn])
        {:keys [locations piece team turn]} vote]
    (if (and (= (:number @game) (:game vote))
             (= (:turn @game) turn))
      (do (println "Vote is OK, counting")
          (swap! votes (fn [m] (merge-with + m {votepart 1})))
          (votes-update))
      (println "Vote was for wrong turn or game."))))

(defn schedule-move [f]
  (at-/at (.getTime (:moveDeadline @game (Date.)))
          f pool :desc "Count votes and apply move"))

(defn add-usercounters [game]
  (-> game data/vectify
      (assoc-in [:teams 0 :participantCount] (get-in @usercounters [:teams 0]))
      (assoc-in [:teams 1 :participantCount] (get-in @usercounters [:teams 1]))))

(declare apply-votes)

(defn start-new-game []
  (let [gameid (make-gameid)]
    (swap! cfg assoc :game-docid (str "game:" gameid))
    (swap! cfg assoc :votes-docid (str "votes:" gameid)))
  (remove-watch game ::store-db)
  (remove-watch votes-doc ::store-db)
  (ref->db game (:game-docid @cfg))
  (ref->db votes-doc (:votes-docid @cfg))
  (reset! usercounters {:teams [0 0]})
  (reset! game (-> (data/initial-game 1 (:interval @cfg))
                   (assoc :votesDoc (:votes-docid @cfg))
                   (assoc :channels ["game"])
                   (assoc :number (rand-int 999999))
                   data/affix-moves))
  (schedule-move apply-votes))

(defn apply-votes []
  (println "Time's up, applying votes!")
  (if-let [most-pop (some->> @votes (sort-by val) last key)]
    (do (println "Most popular vote was:" (pr-str most-pop))
        (reset! votes {})
        (swap! game (fn [g] (-> g
                                add-usercounters
                                (data/apply-move most-pop)
                                (data/affix-moves)))))
    ;else
    (do (println "No votes were cast! Resetting timer.")
        (swap! game assoc :moveDeadline
               (tc/to-date (t/plus (t/now) (t/seconds (:moveInterval @game)))))))
  (if-not (:winningTeam @game)
    (schedule-move apply-votes)
    ;; Wait one round-length, and restart
    (schedule-move start-new-game)))


(defn count-user [user]
  (println "Saw user doc: " (pr-str user))
  (when (and (some-> user :team number?)
             (= (:number @game) (some-> user :game)))
    (swap! usercounters update-in [:teams (:team user)] (fnil inc 0))
    (println "Counted user!" (:team user) @usercounters)))

(defn stream-watch []
  (try
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
        ;; Delete old game docs
        (when (= id "game-1") (db-kill change))
        (when (and id (.startsWith id "game:")
                   (not (= id (:game-docid @cfg))))
          (db-kill change))

        (when (and id (.startsWith id "user:"))
          (count-user (db-get id)))
        (when (and id (.startsWith id "vote:"))
          (tally-vote (db-get id))))))
    (catch Exception e
      (println "Error on changes feed connection:" (str e))
      (Thread/sleep 10000)))
  (println "Changes feed dropped! Restarting... (at seq.." (:seq @cfg) ")")
  (recur))

(defn votes-update []
  (println "Updating vote totals")
  (swap! votes-doc merge
         {:game (:number @game)
          :channels ["game"]
          :turn (:turn @game)
          :team (:activeTeam @game)
          :count (reduce + (vals @votes))
          :moves (->> @votes (sort-by val) reverse (take 3)
                      (map (fn [[vote votecount]]
                              (assoc
                                (-> @game (data/apply-move vote) :moves last)
                                :count votecount))))}))

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
    (swap! cfg assoc :db (if (.endsWith db-url "/") db-url (str db-url "/")))
    (swap! cfg assoc :interval interval)
    (start-new-game)
    (reset! votes-doc  {:game 0 :turn 0 :team 0 :count 0 :moves []})
    (swap! game db-put (:game-docid @cfg))
    ;; Start watching the changes stream
    (.start (Thread. stream-watch))))
