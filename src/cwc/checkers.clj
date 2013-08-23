(ns cwc.checkers
  "Checkers board and rules functions")

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
      (fill :b 1 12)
      (fill :r 21 32)))

(def king? #{:R :B})
(def color {:r :r, :b :b, :R :r, :B :b})

(defn dist [a b] (let [diff (- a b)] (if (neg? diff) (- diff) diff)))

(defn forward? [from-color fy ty]
  (if (= from-color :b)
    (< fy ty)
    (> fy ty)))

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
          (= (dist fy ty) 2)
          (= (dist fx tx) 2)
          (let [mid-pos [(/ (- tx fx) 2) (/ (- ty fy) 2)]
                mid-cell (get-in board mid-pos)]
            (and 
              ;; A piece must occupy the midpoint
              mid-cell
              ;; and be of the opponent color.
              (not= (color mid-cell)
                    (color from-cell)))))))))

(defn search-moves 
  "Brute force search for possible moves"
  [board moving-color]
  (apply concat
         (for [pos pdn-all
               :let [from-yxpos (pdn->yx pos)
                     from-cell (get-in board from-yxpos)]
               :when (= moving-color (color from-cell))]
           (for [pos pdn-all
                 :let [to-yxpos (pdn->yx pos)]
                 :when (move-allowed? board from-yxpos to-yxpos)]
             [from-yxpos to-yxpos]))))

(defn move->pdn [from to]
  "Formats move in Portable Draughts Notation (assumes move is valid.)"
  (let [pdn-f (yx->pdn from)
        pdn-t (yx->pdn to)]
   (case (dist (first from) (first to))
    1 (str pdn-f "-" pdn-t)
    2 (str pdn-f "x" pdn-t)
    nil)))

(comment

  ;; boolean failure tracer
  (defmacro f [expr]
    `(let [res# ~expr]
       (when-not res#
         (println "failed:" (pr-str '~expr)))
       res#))

  (map
    (partial apply move->pdn)
    (search-moves initial-board :b))

  (print-board initial-board))
