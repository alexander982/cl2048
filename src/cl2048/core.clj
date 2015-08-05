(ns cl2048.core
  (:gen-class))

(def board-width 4)
(def board-height 4)

(defn new-empty-board []
  (apply hash-map (interleave
                   (for [x (range board-width)
                         y (range board-height)] [x y])
                   (for [v (range (* board-width board-height))] 0))))

(defn empty-cells [board]
  (let [ks (keys board)]
    (filter #(zero? (get board %)) ks)))

(defn new-cell-val []
  (if (> (rand) 0.8) 4 2))

(defn spawn-cell [board]
  (assoc board
         (rand-nth (empty-cells board))
         (new-cell-val)))

(defn new-board []
  (spawn-cell (spawn-cell (new-empty-board))))

(defn merge-cells [board ks]
  (reduce (fn [b [c1 c2]]
              (if (= (b c1) (b c2))
                (assoc (assoc b c1 (* (b c1) 2)) c2 0)
                b))
            board ks))

(defn non-empty-row-cells [board n]
  (partition 2 1
             (filter (fn [k] (not (zero? (board k))))
                     (for [x (range board-width)] [n x]))))

(defn merge-row [board n]
  (merge-cells board (non-empty-row-cells board n)))

(defn merge-rows [board]
  (reduce #(merge-row % %2) board (range board-height)))

(defn non-empty-column-cells [board n]
  (partition 2 1
             (filter (fn [k] (not (zero? (board k))))
                     (for [y (range board-width)] [y n]))))

(defn merge-column [board n]
  (merge-cells board (non-empty-column-cells board n)))

(defn merge-columns [board]
  (reduce #(merge-column % %2) board (range board-width)))

(defn move-cells [board n side]
  (let [ks (if (or (= side :left) (= side :right))
             (for [x (range board-width)] [n x])
             (for [y (range board-height)] [y n]))
        vs (for [k ks] (board k))
        nz (vec (filter (complement zero?) vs))
        n (- board-width (count nz))]
    (partition 2 2 (interleave ks
                               (if (or (= side :left) (= side :up))
                                 (into nz (repeat n 0))
                                 (into (vec (repeat n 0)) nz))))))

(defn assoc-kv [map [k v]]
  (assoc map k v))

(defn move-row-left
  "Move all not empty cells to left side"
  [board n]
  (reduce assoc-kv board (move-cells board n :left)))

(defn move-row-right
  "Move all not empth cell to right side"
  [board n]
  (reduce assoc-kv board (move-cells board n :right)))

(defn move-column-up [board n]
  (reduce assoc-kv board (move-cells board n :up)))

(defn move-column-down [board n]
  (reduce assoc-kv board (move-cells board n :down)))

(defn move-rows-left [board]
  (reduce move-row-left board (range board-width)))

(defn move-rows-right [board]
  (reduce move-row-right board (range board-width)))

(defn move-columns-up [board]
  (reduce move-column-up board (range board-height)))

(defn move-columns-down [board]
  (reduce move-column-down board (range board-height)))
;;;fixme

(defn move-left [board]
  (-> board
      (merge-rows)
      (move-rows-left)
      (spawn-cell)))

(defn move-right [board]
  (-> board
      (merge-rows)
      (move-rows-right)
      (spawn-cell)))

(defn move-up [board]
  (-> board
      (merge-columns)
      (move-columns-up)
      (spawn-cell)))

(defn move-down [board]
  (-> board
      (merge-columns)
      (move-columns-down)
      (spawn-cell)))

(defn merge-cells?
  "Проверяет можно ли объединить ячейки"
  [board ks]
  (reduce (fn [t? [k1 k2]]
            (or t? (= (board k1) (board k2))))
          false ks))

(defn merges? [board]
  (or (reduce (fn [t? n] (or t? (merge-cells?
                                 board (non-empty-row-cells board n))))
              false (range board-width))
      (reduce (fn [t? n] (or t? (merge-cells?
                                 board (non-empty-column-cells board n))))
              false (range board-height))))

(defn lose? [board]
  (if (and (= (count (empty-cells board)) 0)
           (not (merges? board)))
    true
    false))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
