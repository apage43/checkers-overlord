(ns cwc.checkers
  "Checkers board and rules functions"
  (:require clojure.string))

(def empty-board
  (->> (repeat 8 nil) vec
       (repeat 8) vec))

(defn print-board [board]
  (doseq [row board]
    (println (apply str (interpose " " (map #({:r "r" :b "b"} % "_") row))))))

;; The numbers 1-32, all PDN positions.
(def pdn-all (range 1 33))

;; Mapping of Portable Draughts Notation positions to row, col positions
(defn pdn->yx [pdn-index]
  (let [row (long (/ (dec pdn-index) 4))
        col (+ (if (even? row) 1 0)
               (* 2 (mod (dec pdn-index) 4)))]
    [row col]))

;; The same, in reverse
(def yx->pdn
  (reduce (fn [mapping i]
            (assoc mapping (pdn->yx i) i))
          {} pdn-all))

(defn fill [board fillwith start end]
  (reduce (fn [board pos]
            (assoc-in board (pdn->yx pos) fillwith))
          board (range start (inc end))))

(def initial-board
  (-> empty-board
      (fill :r 1 12)
      (fill :b 21 32)))

(def king? #{:R :B})
(def color {:r :r, :b :b, :R :r, :B :b})

(defn dist [a b] (let [diff (- a b)] (if (neg? diff) (- diff) diff)))

(defn forward? [from-color fy ty]
  (if (= from-color :r)
    (< fy ty)
    (> fy ty)))

(defn jump? [move]
  (let [[[fy fx] [ty tx]] move]
    (and (= (dist fy ty) 2)
         (= (dist fx tx) 2))))

(defn move-allowed?
  "Takes a board, from position and to position as [y x] pairs,
   (row before column since it corresponds with 2D array indexing, board[y][x])
   and checks if move is allowed.

   Does *not* reject non-jump moves when jumps are possible."
  [board from to]
  (let [[fy fx] from
        [ty tx] to
        from-cell (get-in board from)
        to-cell (get-in board to)]
    (and
      ;; Must always be moving a piece
      from-cell
      ;; destination cell must be empty
      (not to-cell)
      ;; moving piece must be a king, or moving forward
      (or (king? from-cell)
          (forward? (color from-cell) fy ty))
      ;; And the move is either
      (or
        ;; One space, diagonal
        (and (= (dist fy ty) 1)
             (= (dist fx tx) 1))
        ;; Or two spaces diagonal, a jump
        (and
          (jump? [from to])
          (let [mid-pos [(/ (+ ty fy) 2) (/ (+ tx fx) 2)]
                mid-cell (get-in board mid-pos)]
            (and
              ;; A piece must occupy the midpoint
              mid-cell
              ;; and be of the opponent color.
              (not= (color mid-cell)
                    (color from-cell)))))))))

(defn path->compact-idx [path]
  (let [[begin & more] (map #(map yx->pdn %) path)]
    (vec (concat begin
                 (map second more)))))

(defn apply-move [board [from to]]
  (-> board
      (assoc-in from nil)
      (assoc-in to (get-in board from))))

(defn apply-path [board path & {:keys [jumps] :or {jumps true}}]
  (let [moved (reduce apply-move board path)
        jumps (filter jump? path)]
    (if-not jumps moved
      ;; Remove jumped pieces
      (reduce (fn [board jump]
                (let [[[fy fx] [ty tx]] jump
                      mid-y (/ (+ fy ty) 2)
                      mid-x (/ (+ fx tx) 2)
                      mid [mid-y mid-x]]
                  (assoc-in board mid nil)))
              moved jumps))))

(defn search-moves
  "Search for possible moves"
  [board moving-color & [eligible-cells]]
  (apply concat
         (for [pos (or eligible-cells pdn-all)
               :let [from-yxpos (pdn->yx pos)
                     from-cell (get-in board from-yxpos)]
               :when (= moving-color (color from-cell))]
           (for [pos pdn-all
                 :let [to-yxpos (pdn->yx pos)]
                 :when (move-allowed? board from-yxpos to-yxpos)]
             [from-yxpos to-yxpos]))))

(defn extend-jumpchains [board moving-color paths]
  (let [jumpends (filter (comp jump? last) paths)
        ;; Extend any chains that can be extended
        jumpends (apply concat
                        (for [path jumpends
                              :let [final-cell (last (path->compact-idx path))
                                    ;; Don't removed jumped pieces here
                                    partial-board (apply-path board path :jumps false)
                                    next-moves (search-moves board moving-color [final-cell])]]
                          (if (seq next-moves)
                            (mapv (fn [next-move]
                                    (vec (concat path next-move)))
                                  next-moves)
                            [path])))]
    (if (and (seq jumpends) (not= jumpends paths))
      (extend-jumpchains board moving-color jumpends)
      paths)))

(defn evaluate-moves
  [board moving-color]
  (let [first-pass (map vector (search-moves board moving-color))]
    (extend-jumpchains board moving-color first-pass)))

(defn move->pdn [[from to]]
  "Formats move in Portable Draughts Notation (assumes move is valid.)"
  (let [pdn-f (yx->pdn from)
        pdn-t (yx->pdn to)]
   (case (dist (first from) (first to))
    1 (str pdn-f "-" pdn-t)
    2 (str pdn-f "x" pdn-t)
    nil)))

(defn path->pdn [path]
  (let [idxed (path->compact-idx path)]
    (with-out-str
      (loop [[head & more] idxed]
        (if (first more)
          (do
            (print head)
            (if (< 1 (dist (first (pdn->yx head))
                           (first (pdn->yx (first more)))))
              (print "x")
              (print "-"))
            (recur more))
          (print head))))))

(defn- dir [f t] (if (neg? (- t f)) -1 1))

(defn pdn->path [pdn]
  (let [parts (clojure.string/split pdn #"[x\-]")
        pdns (map read-string parts)
        pdn-path (partition 2 1 pdns)
        path (mapv (partial mapv pdn->yx) pdn-path)]
    (apply concat
           (for [[[fy fx] [ty tx] :as hop] path]
             (if (< 2 (dist fy ty))
               (let [xv (* 2 (dir fx tx))
                     yv (* 2 (dir fy ty))]
                 (for [n (range (/ (dist fy ty) 2))]
                   [[(+ fy (* n yv)) (+ fx (* n xv))]
                    [(+ fy (* (inc n) yv)) (+ fx (* (inc n) xv))]]))
               [hop])))))

(defn two-board-print [ba bb]
  (let [pa (with-out-str (print-board ba))
        pb (with-out-str (print-board bb))]
    (doseq [toprint (map #(str %1 "     " %2)
                           (clojure.string/split-lines pa)
                           (clojure.string/split-lines pb))]
        (println toprint))))

(defn apply-pdns [board pdns]
  (reduce (fn [board pdn]
            (apply-path board (pdn->path pdn)))
          board pdns))

(defn captures [pdnpath]
  (let [locs (map pdn->yx pdnpath)
        hops (partition 2 1 locs)]
    (for [hop hops
          :let [[[fy fx] [ty tx]] hop
                mid [(/ (+ fy ty) 2) (/ (+ fx tx) 2)]]
          :when (jump? hop)]
      (yx->pdn mid))))

(comment

  (def cell-prettymap
    {:b {:color :b
         :king false}
     :B {:color :r
         :king true}
     :r {:color :r
         :king false}
     :R {:color :R
         :king true}})
  (defn game-state [board currentplayer]
    {:current-player currentplayer
     :board (map (comp cell-prettymap (partial get-in board) pdn->yx) pdn-all)
     :allowed-moves
     (mapv path->compact-idx (evaluate-moves board currentplayer))})

  (require '[cheshire.core :as json])
  (let [pjson (json/generate-string (game-state initial-board :r) {:pretty true})]
    (spit "sample.json" pjson)
    (println pjson))

  (use 'clojure.pprint)
  (pprint (evaluate-moves initial-board :r))

  (let [board (apply-pdns initial-board ["9-14" "23-18" "14x23"])
        player :b]
    (print-board board)
    (println)
    (doseq [m (evaluate-moves board player)]
      (println (path->pdn m))
      (two-board-print board (apply-path board m))
      (println)))

  (print-board (apply-path initial-board (pdn->path "9-14")))

  (let [board initial-board]
    (-> board
        (apply-pdns ["9-14"
                     "23-18"])
        print-board))

  (print-board initial-board))
