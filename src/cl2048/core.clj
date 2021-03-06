(ns cl2048.core
  (:import (java.awt Color Dimension)
           (javax.swing JPanel JFrame JOptionPane)
           (java.awt.event ActionListener KeyListener WindowListener)
           (java.io PushbackReader))
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn])
  (:gen-class))

(def board-width 4)
(def board-height 4)
(def cell-width 60)
(def cell-height 60)
(def left-margin 2)
(def up-margin 24)

(defn new-empty-board []
  "Generate totaly empty board as map with coordinate vectors as keys"
  (apply hash-map (interleave
                   (for [x (range board-width)
                         y (range board-height)] [x y])
                   (for [v (range (* board-width board-height))] 0))))

(defn empty-cells
  "Return sequence of empty cells keys"
  [board]
  (let [ks (keys board)]
    (filter #(zero? (get board %)) ks)))

(defn new-cell-val
  "Generate new cell value to spawn"
  []
  (if (> (rand) 0.9) 4 2))

(defn spawn-cell
  "Add 2 or 4 on random empty cell"
  [board]
  (assoc board
         (rand-nth (empty-cells board))
         (new-cell-val)))

(defn new-board
  "Return map of new game board"
  [& args]
  (spawn-cell (spawn-cell (new-empty-board))))

(defn new-game
  "Return new game map"
  [& args]
  (if (> (count args) 0)
    (let [{:keys [score hiscore]} (first args)]
      {:score 0
       :hiscore (if (> score hiscore) score hiscore)
       :board (new-board)})
    {:score 0
     :hiscore 0
     :board (new-board)}))

(defn merge-cells
  "Merge cells if its values are equal

  Check of cells pairs in ks sequence for equality and merge them if so.
  ks - ((с1 с2) .. ). Return map with cells merged."
  [board ks]
  (reduce (fn [b [c1 c2]]
              (if (= (b c1) (b c2))
                (assoc (assoc b c1 (* (b c1) 2)) c2 0)
                b))
          board ks))

(defn score-cells
  "Count a score value of given cells pairs"
  [board ks]
  (let [game (assoc {} :board board :score 0)]
    ((reduce (fn [g [c1 c2]]
               (let [a ((g :board) c1)
                     b ((g :board) c2)
                     s (g :score)]
                 (if (= a b )
                   (-> (assoc g :score (+ s a b))
                       (assoc-in [:board c1] (* 2 a))
                       (assoc-in [:board c2] 0))
                   g)))
             game ks)
     :score)))

(defn non-empty-row-cells
  "Return non empty cels pairs coordinates of a board in row n"
  [board n]
  (partition 2 1
             (filter (fn [k] (not (zero? (board k))))
                     (for [x (range board-width)] [x n]))))

(defn merge-row
  "Merge row n of a board in given direction"
  [board n direction]
  (merge-cells board (if (= direction :left-to-right)
                       (non-empty-row-cells board n)
                       (reverse (map reverse
                                     (non-empty-row-cells board n))))))

(defn merge-rows
  "Merge all rows of a board"
  [board direction]
  (reduce #(merge-row % %2 direction) board (range board-height)))

(defn score-rows
  "Return score for merging all rows"
  [board]
  (reduce (fn [s n] (+ s (score-cells board
                                      (non-empty-row-cells board n))))
          0 (range board-height)))

(defn non-empty-column-cells
  "Return non empty cells pairs coordinates of collumn n of a board"
  [board n]
  (partition 2 1
             (filter (fn [k] (not (zero? (board k))))
                     (for [y (range board-width)] [n y]))))

(defn merge-column
  "Merge column n of a board in given direction"
  [board n direction]
  (merge-cells board (if (= direction :up-down)
                       (non-empty-column-cells board n)
                       (reverse (map reverse
                                     (non-empty-column-cells board n))))))

(defn merge-columns
  "Merge all columns of a board in given direction"
  [board direction]
  (reduce #(merge-column % %2 direction) board (range board-width)))

(defn score-columns
  "Return score for merging all columns"
  [board]
  (reduce (fn [s n] (+ s (score-cells board
                                      (non-empty-column-cells board n))))
          0 (range board-width)))

(defn cells-coord
  "Return cells coords of n row or column. what is :row or :column"
  [n what]
  (if (= what :row)
    (for [x (range board-width)] [x n])
    (for [y (range board-height)] [n y])))

(defn move-cells [board n side]
  "Return sequence of row or colum n of a board coordinates and values
   moved to side. Where side is :left, :right, :up or :down"
  (let [ks (if (or (= side :left) (= side :right))
             (cells-coord n :row)
             (cells-coord n :column))
        vs (for [k ks] (board k))
        nz (vec (filter (complement zero?) vs))
        n (- board-width (count nz))]
    (partition 2 2 (interleave ks
                               (if (or (= side :left) (= side :up))
                                 (into nz (repeat n 0))
                                 (into (vec (repeat n 0)) nz))))))

(defn move?
  "Check if what(:row or :column) n can be moved to a side"
  [board n side what]
  (let [rc (partition 2 1 (for [k (cells-coord n what)] (board k)))]
    (reduce (fn [t? [a b]]
              (or t? (if (and (zero? a) (not (zero? b))) true false)))
            false
            (if (or (= side :left) (= side :up))
              rc
              (reverse (map reverse rc))))))

(defn move-rows?
  "Check if rows can be moved to side :left or :right"
  [board side]
  (reduce #(or % (move? board %2 side :row))
          false (range board-height)))

(defn move-columns?
  "Check if columns can be moved to side :up or :down"
  [board side]
  (reduce #(or % (move? board %2 side :column))
          false (range board-width)))

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

(defn move-left
  "Update board and score on move to the left"
  [{:keys [board score] :as game}]
  (-> (assoc-in game [:board] (-> board
                                  (merge-rows :left-to-right)
                                  (move-rows-left)
                                  (spawn-cell)))
      (assoc :score (+ score (score-rows board)))))

(defn move-right
  "Update board and score on move to the right"
  [{:keys [board score] :as game}]
  (-> (assoc-in game [:board] (-> board
                                  (merge-rows :right-to-left)
                                  (move-rows-right)
                                  (spawn-cell)))
      (assoc :score (+ score (score-rows board)))))

(defn move-up
  "Update board and score on move up"
  [{:keys [board score] :as game}]
  (-> (assoc-in game [:board] (-> board
                                  (merge-columns :up-down)
                                  (move-columns-up)
                                  (spawn-cell)))
      (assoc :score (+ score (score-columns board)))))

(defn move-down
  "Update board and score on move down"
  [{:keys [board score] :as game}]
  (-> (assoc-in game [:board] (-> board
                               (merge-columns :down-up)
                               (move-columns-down)
                               (spawn-cell)))
      (assoc :score (+ score (score-columns board)))))

(defn merge-cells?
  "Check if there is a merges in given coordinates of cells pairs"
  [board ks]
  (reduce (fn [t? [k1 k2]]
            (or t? (= (board k1) (board k2))))
          false ks))

(defn merge-rows?
  "Check if there is merges in rows of board"
  [board]
  (reduce (fn [t? n]
            (or t? (merge-cells? board (non-empty-row-cells board n))))
          false (range board-width)))

(defn merge-columns?
  "Check if there is merges in the cloumns of board"
  [board]
  (reduce (fn [t? n]
            (or t? (merge-cells? board (non-empty-column-cells board n))))
          false (range board-height)))

(defn merges?
  "Check if board has any merges"
  [board]
  (or (merge-rows? board)
      (merge-columns? board)))

(defn loose?
  "Check if board does not have any moves or merges"
  [board]
  (if (and (= (count (empty-cells board)) 0)
           (not (merges? board)))
    true
    false))

(defn reset-game
  "Set new game"
  [game]
  (swap! game new-game))

(defn update-game [game f]
  (swap! game f))

;=== gui

(defn draw-board [g {:keys [board score hiscore]}]
  (.setColor g (Color. 200 200 200))
  (.fillRect g 0 0
             (+ left-margin 2 (inc (* board-width (inc cell-width))))
             (+ up-margin 2 (inc (* board-height (inc cell-height)))))
  (doseq [x (range board-width)
          y (range board-height)]
    (.setColor g (Color. 230 230 230))
    (.fillRect g
               (+ left-margin (inc (* 1 x)) (* cell-width x))
               (+ up-margin (inc (* 1 y)) (* cell-height y))
               cell-width
               cell-height)
    (when (not (zero? (board [x y])))
      (.setColor g (Color. 0 0 0))
      (.drawString g (str (board [x y]))
                   (+ 10 left-margin (* 1 x) (* cell-width x))
                   (+ 30 up-margin (* 1 y) (* cell-height y)))))
  (.setColor g (Color. 0 0 0))
  ;;draw score
  (.drawString g (apply str ["Score: " score]) 3 20)
  ;;draw hiscore
  (if (> score hiscore)
    (.drawString g (apply str ["Hiscore: " score]) 125 20)
    (.drawString g (apply str ["Hiscore: " hiscore]) 125 20)))

(defn game-panel [frame game]
  (proxy [JPanel KeyListener WindowListener] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (draw-board g @game))
    (keyPressed [e]
      (let [key (.getKeyCode e)
            board (@game :board)]
        (cond
          (= key java.awt.event.KeyEvent/VK_LEFT)
          (when (or (merge-rows? board)
                    (move-rows? board :left))
            (update-game game move-left))
          (= key java.awt.event.KeyEvent/VK_RIGHT)
          (when (or (merge-rows? board)
                    (move-rows? board :right))
            (update-game game move-right))
          (= key java.awt.event.KeyEvent/VK_UP)
          (when (or (merge-columns? board)
                    (move-columns? board :up))
            (update-game game move-up))
          (= key java.awt.event.KeyEvent/VK_DOWN)
          (when (or (merge-columns? board)
                    (move-columns? board :down))
            (update-game game move-down)))
        (when (loose? board)
          (JOptionPane/showMessageDialog frame "You Lose!")
          (reset-game game)))
      (.repaint this))
    (getPreferredSize []
      (Dimension. (+ left-margin (inc (* board-width (inc cell-width))))
                  (+ up-margin (inc (* board-height (inc cell-height))))))
    (keyReleased [e])
    (keyTyped [e])
    (windowClosing [e]
      (let [{:keys [score hiscore] :as gm} @game]
        (if (> score hiscore)
          (spit "save~" (assoc gm :hiscore score))
          (spit "save~" gm))))
    (windowActivated [e])
    (windowClosed [e])
    (windowDeactivated [e])
    (windowDeiconified [e])
    (windowIconified [e])
    (windowOpened [e])))

(defn -main
  [& args]
  (let [game (if (.exists (io/file "save~"))
               (atom (edn/read (PushbackReader.
                                (io/reader "save~"))))
               (atom (new-game)))
        frame (JFrame. "2048")
        panel (game-panel frame game)]
    (doto panel
      (.setFocusable true)
      (.addKeyListener panel))
    (doto frame
      (.add panel)
      (.pack)
      (.addWindowListener panel)
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setResizable false)
      (.setVisible true))))
