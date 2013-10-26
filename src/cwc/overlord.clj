(ns cwc.overlord
  (:gen-class)
  (:import [java.net URL URLEncoder]
           [java.util Date UUID])
  (:require [clj-http.client :as http]
            [flatland.useful.exception :refer [rescue]]
            [cheshire.core :as json]
            [compojure.core :refer :all]
            [ring.util.response :as response]
            [clj-time.core :as t]
            [clj-time.coerce :as tc]
            [clojure.java.io :as io]
            [overtone.at-at :as at-]
            [cwc.game :as data]
            [clojurewerkz.urly.core :as urly]
            [clojure.tools.cli :refer [cli]]
            [aleph.http :refer [start-http-server
                                wrap-aleph-handler
                                wrap-ring-handler]]))

(def userview-mapfunction "
    function (doc, meta) {
      if(meta.id.match(/^user:/)) {
        emit([doc.game, doc.team]);
      }
    }")

(defn json-response
  "Transform `obj` to JSON and create a ring response object of it."
  [obj]
  (-> (response/response (json/generate-string obj))
      (response/content-type "application/json; charset=utf-8")))

(def pool (at-/mk-pool))
(def game (atom nil))
(def cfg (atom {:seq "*:0"
                :autoCommit false
                :auth ["overlord" "theoverlord"]
                :game-docid "game:checkers"
                :votes-docid "votes:checkers"}))
(def votes (atom {}))
(def votes-doc (atom {}))
(def usercounters (atom {:teams [0 0]}))
(def next-move (atom nil))

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
              (http/get {:basic-auth (:auth @cfg) :as :json})
              :body))
    nil))

(defn db-put [doc k]
  (loop [getrev false]
    (or (try
          (let [docuri (str (urly/resolve (:db @cfg) (encode-path k)))
                doc (if-not getrev doc (assoc doc :_rev (:_rev (db-get k) "")))
                pres (:body (http/put docuri {:as :json
                                              :basic-auth (:auth @cfg)
                                              :body (json/generate-string doc)
                                              :headers {"Content-Type" "application/json"}}))]
            (assoc doc :_rev (:rev pres)))
          (catch Exception e
            (Thread/sleep 1000)
            (println "Failed to update document" (str e))))
        (recur true))))

(defn db-kill [change]
  (rescue
    (let [{:keys [id changes deleted]} change
          rev (some-> changes first :rev)
          docuri (str (urly/resolve (:db @cfg) (encode-path id)))]
      ;; Don't delete already deleted docs
      (when-not deleted
        (http/delete docuri {:basic-auth (:auth @cfg) :query-params {:rev rev}}))) nil))

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
  (at-/at (+ 4000 (.getTime (:moveDeadline @game (Date.))))
          f pool :desc "Count votes and apply move"))

(defn add-usercounters [game]
  (-> game data/vectify
      (assoc-in [:teams 0 :participantCount] (get-in @usercounters [:teams 0]))
      (assoc-in [:teams 1 :participantCount] (get-in @usercounters [:teams 1]))))

(declare apply-votes)

(defn start-new-game []
  (println "Starting new game.")
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
  ; silly hack - swap with itself to trigger watch
  (swap! game identity)
  (reset! next-move (schedule-move apply-votes)))

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
    (reset! next-move (schedule-move apply-votes))
    ;; Wait one round-length, and restart
    (start-new-game)))


(defn count-user [user]
  (println "Saw user doc: " (pr-str user))
  (when (and (not (@cfg :userview))
             (some-> user :team number?)
             (= (:number @game) (some-> user :game)))
    (swap! usercounters update-in [:teams (:team user)] (fnil inc 0))
    (println "Counted user!" (:team user) @usercounters)))

(defn stream-watch []
  (try
    (let [changes (str (urly/resolve (:db @cfg) "_changes"))
        last-seq (:seq @cfg "*:0")
        changeset (:body (http/get (str changes)
                                {:query-params {"feed" "longpoll"
                                                "timeout" "20000"
                                                "since" last-seq}
                                 :basic-auth (:auth @cfg)
                                 :as :json}))]
    (doseq [change (:results changeset)]
      (let [id (:id change)]
        (println "Got change:" (pr-str change))
        (swap! cfg assoc :seq (:seq change))
        ;; Delete old game docs
        (when (= id "game-1") (db-kill change))
        (when (and id (.startsWith id "game:")
                   (not (= id (:game-docid @cfg))))
          (db-kill change))
        (when (and id (.startsWith id "votes:")
                   (not (= id (:votes-docid @cfg))))
          (db-kill change))
        (when (and id (.startsWith id "user:"))
          (count-user (db-get id)))
        (when (and id (.startsWith id "vote:"))
          (tally-vote (db-get id))))))
    (catch Exception e
      (println "Error on changes feed connection:" (str e))
      (Thread/sleep 10000)))
  (println "Got change set! Restarting long poll... (at seq.." (pr-str (:seq @cfg)) ")")
  (recur))

(defn votes-update []
  (println "Updating vote totals")
  (let [total (reduce + (vals @votes))
        curteam (:activeTeam @game)]
   (swap! votes-doc merge
         {:game (:number @game)
          :channels ["game"]
          :turn (:turn @game)
          :team curteam
          :count total
          :moves (->> @votes (sort-by val) reverse (take 3)
                      (map (fn [[vote votecount]]
                              (assoc
                                (-> @game (data/apply-move vote) :moves last)
                                :count votecount))))})
    ;; If all votes are in, end the turn (and cancel the outstanding task)
    (when (and (:autoCommit @cfg)
               (pos? total))
               ; (= total (-> @usercounters :teams (nth curteam 0))))
        (swap! next-move (fn [m] (when m (at-/stop m)) nil))
      (apply-votes))))


(defn grab-user-counts []
  (rescue
    (let [gamenum (:number @game)
          {:keys [userview]} @cfg
          start [gamenum]
          end [gamenum {}]
          result (:body (http/get userview {:as :json
                                            :query-params {:startkey (json/generate-string start)
                                                           :endkey (json/generate-string end)
                                                           :group true}}))
          counts (into {} (-> result :rows (->> (map (juxt (comp second :key) :value)))))]
      (swap! usercounters (fn [current]
                            (-> current
                                (assoc-in [:teams 0] (counts 0 0))
                                (assoc-in [:teams 1] (counts 1 0))))))
    nil))

(defroutes api
  (GET "/" {} (json-response {:overlord "checkers"}))
  (POST "/new-game" {}
        ;; kill any outstanding scheduled task for the current game
        (swap! next-move (fn [m] (when m (at-/stop m)) nil))
        (start-new-game)
        (json-response {:status "ok" :game-id (:game-docid @cfg)})))

(defn -main [& args]
  (let [[opts args usage]
        (cli args
             ["-d" "--db-url" "Database URL"]
             ["-q" "--user-view-url" "User Count View URL"
              :default "http://mango.hq.couchbase.com:8092/checkers/_design/checkers/_view/users"]
             ["-i" "--interval" "Seconds per turn"
              :parse-fn read-string :default 30]
             ["-a" "--auto-commit" "Auto commit turn when votes are in?"
              :flag true :default false]
             ["-c" "--control-port" "Port to run control API on"
              :default 16888]
             ["-h" "--help" "Show this message"
              :flag true])
        {:keys [help db-url interval user-view-url auto-commit control-port]} opts]
    (when (or help (not db-url))
      (println usage)
      (System/exit 1))
    (swap! cfg assoc :db (if (.endsWith db-url "/") db-url (str db-url "/")))
    (swap! cfg assoc :interval interval)
    (swap! cfg assoc :autoCommit auto-commit)
    (start-new-game)
    (reset! votes-doc  {:game 0 :turn 0 :team 0 :count 0 :moves []})
    (swap! game db-put (:game-docid @cfg))
    (swap! cfg assoc :api-server (-> #'api wrap-ring-handler
                                     (start-http-server {:port control-port})))
    ;; Start watching the changes stream
    (.start (Thread. stream-watch))))
