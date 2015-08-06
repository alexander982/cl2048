(ns cl2048.core
  (:import (java.awt Color Dimension)
           (javax.swing JPanel JFrame JOptionPane)
           (java.awt.event ActionListener KeyListener))
  (:gen-class))

(def board-width 4)
(def board-height 4)
(def cell-width 60)
(def cell-height 60)

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
                     (for [x (range board-width)] [x n]))))

(defn merge-row [board n direction]
  (merge-cells board (if (= direction :left-to-right)
                       (non-empty-row-cells board n)
                       (reverse (map reverse
                                     (non-empty-row-cells board n))))))

(defn merge-rows [board direction]
  (reduce #(merge-row % %2 direction) board (range board-height)))

(defn non-empty-column-cells [board n]
  (partition 2 1
             (filter (fn [k] (not (zero? (board k))))
                     (for [y (range board-width)] [n y]))))

(defn merge-column [board n direction]
  (merge-cells board (if (= direction :up-down)
                       (non-empty-column-cells board n)
                       (reverse (map reverse
                                     (non-empty-column-cells board n))))))

(defn merge-columns [board direction]
  (reduce #(merge-column % %2 direction) board (range board-width)))

(defn move-cells [board n side]
  (let [ks (if (or (= side :left) (= side :right))
             (for [x (range board-width)] [x n])
             (for [y (range board-height)] [n y]))
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
      (merge-rows :left-to-right)
      (move-rows-left)
      (spawn-cell)))

(defn move-right [board]
  (-> board
      (merge-rows :right-to-left)
      (move-rows-right)
      (spawn-cell)))

(defn move-up [board]
  (-> board
      (merge-columns :up-down)
      (move-columns-up)
      (spawn-cell)))

(defn move-down [board]
  (-> board
      (merge-columns :down-up)
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

(defn loose? [board]
  (if (and (= (count (empty-cells board)) 0)
           (not (merges? board)))
    true
    false))

(defn reset-board [board]
  (swap! board new-board))

(defn loose [board frame]
  (JOptionPane/showMessageDialog frame "You Lose!")
  (reset-board board))

(defn update-board [frame board f]
  (if (loose? @board)
    (loose board frame)
    (swap! board f))
  (println @board))

(defn draw-board [g board]
  (.setColor g (Color. 200 200 200))
  (.fillRect g 0 0
             (inc (* board-width (inc cell-width)))
             (inc (* board-height (inc cell-height))))
  (doseq [x (range board-width)
          y (range board-height)]
    (.setColor g (Color. 230 230 230))
    (.fillRect g
               (+ (inc (* 1 x)) (* cell-width x))
               (+ (inc (* 1 y)) (* cell-height y))
               cell-width
               cell-height)
    (when (not (zero? (board [x y])))
      (.setColor g (Color. 0 0 0))
      (.drawString g (str (board [x y]))
                   (+ 10 (* 1 x) (* cell-width x))
                   (+ 30 (* 1 y) (* cell-height y))))))

(defn game-panel [frame board]
  (proxy [JPanel KeyListener] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (draw-board g @board))
    (keyPressed [e]
      (println "key pressed" (.getKeyCode e))
      (let [key (.getKeyCode e)]
        (cond
          (= key java.awt.event.KeyEvent/VK_LEFT)
          (update-board frame board move-left)
          (= key java.awt.event.KeyEvent/VK_RIGHT)
          (update-board frame board move-right)
          (= key java.awt.event.KeyEvent/VK_UP)
          (update-board frame board move-up)
          (= key java.awt.event.KeyEvent/VK_DOWN)
          (update-board frame board move-down))) 
      (.repaint this))
    (getPreferredSize []
      (Dimension. (inc (* board-width (inc cell-width)))
                  (inc (* board-height (inc cell-height)))))
    (keyReleased [e])
    (keyTyped [e])))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [board (atom (new-board))
        frame (JFrame. "2048 by Alexander")
        panel (game-panel frame board)]
    (doto panel
      (.setFocusable true)
      (.addKeyListener panel))
    (doto frame
      (.add panel)
      (.pack)
      #_(.setResizable false)
      (.setVisible true))))
