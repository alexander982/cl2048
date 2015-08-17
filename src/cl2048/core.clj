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

(defn empty-cells
  "Возвращает список координат пустых ячеек"
  [board]
  (let [ks (keys board)]
    (filter #(zero? (get board %)) ks)))

(defn new-cell-val
  "Возвращает новое значение ячейки, которое может быть 4 или 2.
  Вероятность появления 2 выше чем 4."
  []
  (if (> (rand) 0.8) 4 2))

(defn spawn-cell
  "Возвращает массив игровой зоны с добавленным в случайную ячейку
  новым значением"
  [board]
  (assoc board
         (rand-nth (empty-cells board))
         (new-cell-val)))

(defn new-board
  "Возвращает массив с двумя непустыми ячейками"
  [& args]
  (spawn-cell (spawn-cell (new-empty-board))))

(defn new-game
  "Return new game map"
  [& args]
  {:score 0
   :hiscore 0
   :board (new-board)})

(defn merge-cells
  "Проверяет соседние клетки списка координат ks на равенство и
  увеличивает значение первой кнопки в два раза а вторую обнуляет.
  ks - список пар коодинат ((с1 с2) .. )"
  [board ks]
  (reduce (fn [b [c1 c2]]
              (if (= (b c1) (b c2))
                (assoc (assoc b c1 (* (b c1) 2)) c2 0)
                b))
            board ks))

(defn non-empty-row-cells
  "Возвращает список пар координат строки непустых ячеек игровой зоны"
  [board n]
  (partition 2 1
             (filter (fn [k] (not (zero? (board k))))
                     (for [x (range board-width)] [x n]))))

(defn merge-row
  "Возвращает массив в котором ячейки в строке n объединены (если это
  возможно)"
  [board n direction]
  (merge-cells board (if (= direction :left-to-right)
                       (non-empty-row-cells board n)
                       (reverse (map reverse
                                     (non-empty-row-cells board n))))))

(defn merge-rows
  "Возвращает массив в котором все ячейки в строке, которые могут быть
   объединены, слиты вместе"
  [board direction]
  (reduce #(merge-row % %2 direction) board (range board-height)))

(defn non-empty-column-cells
  "Возвращает список пар координат столбца непустых ячеек игровой зоны"
  [board n]
  (partition 2 1
             (filter (fn [k] (not (zero? (board k))))
                     (for [y (range board-width)] [n y]))))

(defn merge-column
  "Возвращает массив в котором ячейки в столбце n объединены (если это
  возможно)"
  [board n direction]
  (merge-cells board (if (= direction :up-down)
                       (non-empty-column-cells board n)
                       (reverse (map reverse
                                     (non-empty-column-cells board n))))))

(defn merge-columns
  "Возвращает массив в котором все ячейки в столбце, которые могут быть
  объединены, слиты вместе"
  [board direction]
  (reduce #(merge-column % %2 direction) board (range board-width)))

(defn cells-coord
  "Return cells coords of n row or column. what is :row or :column"
  [n what]
  (if (= what :row)
    (for [x (range board-width)] [x n])
    (for [y (range board-height)] [n y])))

(defn move-cells [board n side]
  "Возвращает список пар ключ значение строки или столбца n (зависит от 
   стороны side) сдвинутых в сторону side. side может быть :left, :right
   :up, :down"
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
  "Check if what(:row or :column) n can be moved to side"
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

(defn move-left [{board :board :as game}]
  (assoc-in game [:board] (-> board
                               (merge-rows :left-to-right)
                               (move-rows-left)
                               (spawn-cell))))

(defn move-right [{board :board :as game}]
  (assoc-in game [:board] (-> board
                               (merge-rows :right-to-left)
                               (move-rows-right)
                               (spawn-cell))))

(defn move-up [{board :board :as game}]
  (assoc-in game [:board] (-> board
                               (merge-columns :up-down)
                               (move-columns-up)
                               (spawn-cell))))

(defn move-down [{board :board :as game}]
  (assoc-in game [:board] (-> board
                               (merge-columns :down-up)
                               (move-columns-down)
                               (spawn-cell))))

(defn merge-cells?
  "Проверяет можно ли объединить ячейки"
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

(defn game-panel [frame game]
  (proxy [JPanel KeyListener] []
    (paintComponent [g]
      (proxy-super paintComponent g)
      (draw-board g (@game :board)))
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
      (Dimension. (inc (* board-width (inc cell-width)))
                  (inc (* board-height (inc cell-height)))))
    (keyReleased [e])
    (keyTyped [e])))

(defn -main
  [& args]
  (let [game (atom (new-game))
        frame (JFrame. "2048")
        panel (game-panel frame game)]
    (doto panel
      (.setFocusable true)
      (.addKeyListener panel))
    (doto frame
      (.add panel)
      (.pack)
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      #_(.setResizable false)
      (.setVisible true))))
