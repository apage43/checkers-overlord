(ns cwc.pretend
  (:gen-class)
  (:require [clj-http.client :as http]
            [cwc.checkers :as rules]
            [cwc.game :as data]
            clojure.string
            [cheshire.core :as json]))


(defn force-put [docurl data]
  (http/put docurl {:body (json/generate-string
                            (merge data
                                   (try {:_rev (-> docurl (http/get {:as :json}) :body :_rev)}
                                        (catch Exception e nil))))
                    :headers {"Content-Type" "application/json"}}))

(defn pdn->vote [game pdn]
  (let [{:keys [activeTeam]} game
        idx-path (-> pdn rules/pdn->path rules/path->compact-idx)
        mine-by-loc (data/piece-location-index game activeTeam)
        vote {:team activeTeam
              :piece (mine-by-loc (first idx-path))
              :locations (vec (rest idx-path))}]
    (println (pr-str pdn) "->" (pr-str vote)) vote))

(def samplegame
  (-> "9-14 23-18 14x23 27x18 5-9 26-23 12-16 30-26 16-19 24x15 10x19 23x16
       11x20 22-17 7-11 18-15 11x18 28-24 20x27 32x5 8-11 26-23 4-8 25-22 11-15
       17-13 8-11 21-17 11-16 23-18 15-19 17-14 19-24 14-10 6x15 18x11 24-28
       22-17 28-32 17-14 32-28 31-27 16-19 27-24 19-23 24-20 23-26 29-25 26-30
       25-21 30-26 14-9 26-23 20-16 23-18 16-12 18-14 11-8 28-24 8-4 24-19 4-8
       19-16 9-6 1x10 5-1 10-15 1-6 2x9 13x6 16-11 8-4 15-18 6-1 18-22 1-6 22-26
       6-1 26-30 1-6 30-26 6-1 26-22 1-6 22-18 6-1 14-9 1-5 9-6 21-17 18-22
       17-13 6-1 5-9 22-17 9-5 17-14"
      (clojure.string/split #"\s+")
      vec))

(defn -main [& args]
  (let [docurl (or (first args) "http://localhost:4984/checkers/game-1")
        duration (or (some->> (second args) read-string) 10)]
    (force-put docurl (data/initial-game 1 duration))
    (let [finalgame
          (loop [game (data/initial-game 1 duration)
                 movenum 1
                 [move & more] samplegame]
            (let [updated (-> game
                              (data/apply-move (pdn->vote game move))
                              data/affix-moves)]
              (force-put docurl updated)
              (Thread/sleep (* 1000 duration))
              (if more (recur updated (inc movenum) more)
                updated)))]
      (force-put docurl (assoc finalgame :winningTeam 0)))
    (println "Done.")))

