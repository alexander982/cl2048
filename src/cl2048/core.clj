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

(defn merge-row [board n]
  (let [ks (map (fn [[a b]] (list [n a] [n b]))
                (partition 2 1 (range board-width)))]
    (merge-cells board ks)))

(defn merge-rows [board]
  (reduce #(merge-row % %2) board (range board-height)))

(defn merge-column [board n]
  (let [ks (map (fn [[a b]] (list [a n] [b n]))
                (partition 2 1 (range board-height)))]
    (merge-cells board ks)))

(defn merge-columns [board]
  (reduce #(merge-column % %2) board (range board-width)))

(defn move-row-left
  "Move all not empty cells to left side"
  [board n]
  (let [ks (for [x (range board-width)] [n x])
        vs (for [k ks] (board k))
        nz (vec (filter (complement zero?) vs))]
    (reduce (fn [b [k v]]
              (assoc b k v))
            board
            (partition 2 2(interleave
                           ks
                           (into nz
                                 (repeat (- board-width (count nz))
                                         0)))))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
