(ns tictactoe.core
  (:require [quil.core :as q]
            [clojure.core.async :refer :all]))

                                        ;0 not set
                                        ;-1 is an X
                                        ;1 is a O
(def curr-status (atom (vec (repeat 3 (vec (repeat 3 0))))))
(def curr-player (atom 0))
(def computer (atom 0))
(def player (atom 0))

(defn choose-players
  "Randomly assign a number to a player. One of them will be considered the computer."[]
  )
(defn new-rotation []
  (go
     (<! (timeout 200))
     (swap!  rot (partial + 0.2))
     @rot)
  )

(defn draw-vertical-lines []
  (let [x1 (int (/ (q/width) 3))
        x2 (* x1 2)]
    (q/line x1 0 x1 (q/height))
    (q/line x2 0 x2 (q/height))))


(defn draw-horizontal-lines []
  (let [y1 (int (/ (q/height) 3))
        y2 (* y1 2)]
    (q/line 0 y1 (q/width) y1)
    (q/line 0 y2 (q/width) y2)))


(defn draw-wires
  "Draw the tictactoe structure "
  []
  (draw-vertical-lines)
  (draw-horizontal-lines)
  )

(defn identify-cell
  "Given [x y] coordinate identify which cell has been clicked on"
  [x y]
  (let [row (quot x (/ (q/height) 3))
        col (quot y (/ (q/width) 3))]
    [col row]))

(defn get-cell-size
  "Return a pair [width height] corresponding to the width and height of a cell which will host the X or O"
  []
  [(int (/ (q/width) 3)) (int (/ (q/height) 3))])


(defn get-cell-upper-left-corner [row col]
  [(* col (int (/ (q/width) 3)))
   (* row (int (/ (q/height) 3)))])

(defn get-cell-center [row col]
  (let [[x y] (get-cell-upper-left-corner row col)
        [cell-width cell-height] (get-cell-size)
        center [(+ x (int (/ cell-width 2))) (+ y (int (/ cell-height 2)))]]
    center))

(defn printX [row col]
  (let [[x y] (get-cell-upper-left-corner row col)
        [cell-width cell-height] (get-cell-size)]
    (q/stroke-float 255 255 255)
    (q/line x y (+ x cell-width) (+ y cell-height))
    (q/line (+ x cell-width) y x (+ y cell-height))))

(defn printO [row col]
  (let [[x y] (get-cell-center row col)
        [width height] (get-cell-size)]
    (q/ellipse x y width width )))

(defn handle-mouse-click []
  (let [button (q/mouse-button)
        x (q/mouse-x)
        y (q/mouse-y)
        [row col] (identify-cell x y)]))

(defn draw []
  (q/background-float 100)
  (q/stroke-float 10)
  (q/fill-float (rand-int 125) (rand-int 125) (rand-int 125))
                                        ;change the translation to make it turn arouind..
  (draw-wires))

(defn setup []
  (q/smooth)
  (q/no-stroke)
  (q/fill 400)
  (q/frame-rate 10)
  (choose-players)
  )

(q/defsketch nico
  :title "TicTacToe"
  :setup setup
  :draw draw
  :size [400 400]
  :mouse-clicked handle-mouse-click)
