(ns cwc.game
  (:import java.util.Date)
  (:require [cwc.checkers :as rules]
            [clj-time.core :as t]
            [clj-time.coerce :as tc]
            [clojure.set :as cs]
            [cheshire.generate :as jgen]
            [cheshire.core :as json]))

(defn make-team [begin end]
  {:participantCount 0
   :pieces (for [loc (range begin (inc end))]
             {:location loc})})

(defn initial-game [game-number move-interval]
  {:number game-number
   :startTime (tc/to-date (t/now))
   :moveDeadline (tc/to-date (t/plus (t/now) (t/seconds move-interval)))
   :moveInterval move-interval
   :turn 1
   :activeTeam 0
   :moves []
   :teams
   [(make-team 1 12)
    (make-team 21 32)]})

(def team-colors {0 :r 1 :b})
(def kingify {:r :R, :R :R, :b :B, :B :B})

(defn place-pieces [board game team]
  (reduce (fn [board piece]
            (if-let [loc (:location piece)]
              (if-not (:captured piece)
                (assoc-in board (rules/pdn->yx loc) ((comp (if (:king piece)
                                                           kingify
                                                           identity)
                                                         team-colors) team))
                board)
              board))
          board
          (:pieces (nth (:teams game) team))))

(defn game-board [game]
  (-> rules/empty-board
      (place-pieces game 0)
      (place-pieces game 1)))

(def opponent {0 1, 1 0})

(defn kinged? [team move]
  (let [endloc (last move)]
    (if (({0 #{29 30 31 32}
           1 #{1 2 3 4}} team) endloc)
      true false)))

(defn piece-location-index [game team]
  (reduce (fn [index [idx piece]]
            (assoc index (:location piece) idx)) {}
          (-> game :teams (nth team)
              :pieces (->> (map vector (range)) ; Important to add indices *before* filtering
                           (filter (comp not :captured second)))))) ; captures

(defn vectify
  [game]
  (-> game
      (update-in [:teams] vec)
      (update-in [:teams 0 :pieces] vec)
      (update-in [:teams 1 :pieces] vec)))

(defn affix-moves [game]
  (let [board (game-board game)
        curteam (:activeTeam game)
        moves (->> (rules/evaluate-moves board (team-colors curteam))
                   (map rules/path->compact-idx))
        by-mover (group-by first moves)
        opponent-by-loc (piece-location-index game (opponent curteam))
        new-pieces (for [piece (-> game :teams (nth curteam) :pieces)]
                     (reduce
                       (fn [piece move]
                         (merge-with (comp vec concat)
                                     piece {:validMoves [{:locations (rest move)
                                                          :captures (for [c (rules/captures move)]
                                                                      {:piece (opponent-by-loc c)
                                                                       :team (opponent curteam)})
                                                          :king (kinged? curteam move)}]}))
                       piece (if-not (:captured piece)
                               (by-mover (:location piece)) [])))]
    (-> game vectify
        (assoc-in [:teams curteam :pieces] (vec new-pieces)))))

(defn apply-move [game vote]
  (let [{:keys [team piece locations]} vote
        moverloc (-> game :teams (nth team) :pieces (nth piece) :location)
        path (vec (concat [moverloc] locations))
        opponent-by-loc (piece-location-index game (opponent team))]
    (-> game vectify
        (assoc :moves (conj (:moves game) vote))
        (assoc :activeTeam (opponent team))
        (assoc :moveDeadline (tc/to-date (t/plus (t/now) (t/seconds (:moveInterval game)))))
        (update-in [:turn] inc)
        (as-> game
          (reduce (fn [game captureloc]
                    (if-let [opidx (opponent-by-loc captureloc)]
                      (do
                        (println team "captured" (opponent team) "piece" opidx "at" captureloc)
                        (assoc-in game [:teams (opponent team)
                                        :pieces opidx
                                        :captured] true))
                      (do (println "BAD JUMP" captureloc (opponent-by-loc captureloc))
                          game)))
                  game
                  (rules/captures path)))
        (assoc-in [:teams team :pieces piece :location] (last locations))
        (as-> game
          (if (kinged? team path)
            (assoc-in game [:teams team :pieces piece :king] true)
            game))
        (update-in [:teams team :pieces] (partial mapv #(dissoc % :validMoves))))))

(comment
  (use 'clojure.pprint)
  (use 'clojure.repl)
  (pprint (initial-game 1 180))

  (-> (initial-game 1 180)
      pprint)

  ;(def docurl "http://sync.couchbasecloud.com:4984/checkers/game-1")
  (def docurl "http://localhost:4984/checkers/game-1")
  (-> (initial-game 1 180)
      ;(apply-move {:team 0 :piece 8 :locations [14]})
      ;(apply-move {:team 1 :piece 2 :locations [18]})
      ;(apply-move {:team 0 :piece 8 :locations [23]})
      ;(apply-move {:team 1 :piece 6 :locations [18]})
      affix-moves
      (as-> doc
        (http/put docurl
                  {:body (json/generate-string
                           (merge doc
                                  (try {:_rev (-> docurl (http/get {:as :json}) :body :_rev)}
                                       (catch Exception e nil))))
                   :headers {"Content-Type" "application/json"}})))
  (require '[clj-http.client :as http])
  (println (slurp "game-sample.json"))
  )
