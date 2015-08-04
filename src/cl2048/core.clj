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
  (let [ks (partition 2 1
                      (filter (fn [k] (not (zero? (board k))))
                              (for [x (range board-width)] [n x])))] 
    (merge-cells board ks)))

(defn merge-rows [board]
  (reduce #(merge-row % %2) board (range board-height)))

(defn merge-column [board n]
  (let [ks (partition 2 1
                      (filter (fn [k] (not (zero? (board k))))
                              (for [y (range board-width)] [y n])))]
    (merge-cells board ks)))

(defn merge-columns [board]
  (reduce #(merge-column % %2) board (range board-width)))

(defn move-row
  ([board n]
   (move-row board n :left))
  ([board n side]
   (let [ks (for [x (range board-width)] [n x])
         vs (for [k ks] (board k))
         nz (vec (filter (complement zero?) vs))
         n (- board-width (count nz))]
     (partition 2 2 (interleave ks
                                (if (= side :left)
                                  (into nz (repeat n 0))
                                  (into (vec (repeat n 0)) nz)))))))

(defn move-row-left
  "Move all not empty cells to left side"
  [board n]
  (reduce (fn [b [k v]] (assoc b k v))
          board
          (move-row board n :left)))

(defn move-row-right [board n]
  (reduce (fn [b [k v]] (assoc b k v))
          board
          (move-row board n :right)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
