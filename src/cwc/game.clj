(ns cwc.game
  (:import java.util.Date)
  (:require [cwc.checkers :as rules]
            [clj-time.core :as t]
            [clj-time.coerce :as tc]
            [clojure.set :as cs]
            [cheshire.generate :as jgen]
            [lonocloud.synthread :as ->]
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
        (as-> game
          (if (= 0 (count moves))
            (assoc game :winningTeam (opponent curteam))
            game))
        (assoc-in [:teams curteam :pieces] (vec new-pieces)))))

(defn apply-move [game vote]
  (let [{:keys [team piece] path :locations} vote
        moverloc (-> game :teams (nth team) :pieces (nth piece) :location)
        opponent-by-loc (piece-location-index game (opponent team))]
    (-> game vectify
        (->/in [:teams team :pieces]
               (->/each (dissoc :validMoves))) ;; Remove old valid moves
        (->/assoc :moves (conj vote) ;; Add the vote to the moves list
                  :turn inc ;; Bump the turn number
                  :moveDeadline (->/reset (-> (t/now) ;; Reset the deadline timer
                                              (t/plus (t/seconds (:moveInterval game)))
                                              (tc/to-date)))
                  :activeTeam opponent) ;; The new activeTeam will be the old one's opponent
        (->/in [:teams team :pieces piece]
               (->/when (kinged? team path) (assoc :king true)) ;; Set the king flag if we landed in an end row
               (assoc :location (last path))) ;; And actually move the piece
        (->/for [cap (rules/captures path)] ;; Find each captured piece
          (->/when-let [opidx (opponent-by-loc cap)] ;; and flag it as such
            (->/aside _ (println "Captured piece" opidx "by path" path))
            (assoc-in [:teams (opponent team) :pieces opidx :captured] true))))))

