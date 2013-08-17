(ns tictactoe.core
  (:require [quil.core :as q]
            [clojure.core.async :refer :all]))

                                        ;0 not set
                                        ;-1 is an X
                                        ;1 is a O
(def curr-status (atom (vec (repeat 3 (vec (repeat 3 0))))))
(def curr-player (atom 0))
(declare computer )
(declare player)
(def winner (atom 0))

(defn choose-players
  "Randomly assign a number to a player. One of them will be considered the computer."
  []
  (if (> (rand 100) 50)
    (do
      (def computer  :O )
      (def player    :X))
    (do
      (def computer  :X )
      (def player    :O))))

(defn choose-starter
  "Determines who starts the game between the computer and the player"
  []
  (let [players {0 computer 1 player}]
    (swap! curr-player (constantly (get players (int (rand 2)))))))

(defn set-next-player []
  (let [curr @curr-player]
    (do
      (check-winner)
      (when (= @winner 0)
        (if (= curr computer)
          (swap! curr-player (constantly player))
          (swap! curr-player (constantly  computer)))))))


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
    (q/ellipse x y (- width 5) (- width 5) )))

(defn draw-curr-status
  "Given the current status of the game, draw the Xs and Os on the grid"
  []
  (let [grid @curr-status
        curr 0]
    (doseq [row (map-indexed (fn [idx elem] [elem idx])  grid)
            col (map-indexed (fn [idx elem] [elem idx]) (first row))
            :let [
                  x (second row)
                  y (second col)
                  val (first col)]]
      (condp = val
        :X (printX x y)
        :O (printO x y)
        (do)))))

(defn draw-curr-player []
  (q/fill-float 0 0 0)
  ;Only print the current player turn when there's no winner.
  (when (= @winner 0)
    (condp = @curr-player
      computer (q/text "It's computer turn..." 0 (- (q/height) 10))
      player (q/text "It's your turn " 0 (- (q/height) 10))
      (q/text "Starting...")))
  )


(defn is-move-available []
  (some #{true} (for [row @curr-status col row]
                  (= col 0))))

(defn computer-moves
  "This is the computer 'AI' :) function. Make computer's next move"
  []
  (loop []
    (let [[row col] [(rand-int 3) (rand-int 3)]]
      (if (= (get-in @curr-status [row col]) 0)
        (swap! curr-status update-in [row col] (constantly @curr-player))
        (recur)))))

(defn is-winning
  "Determines wheter the collection has all equal elements which is the condition for a winning combination"
  [coll]
  (or (= #{:X} (apply hash-set (flatten coll)))
      (= #{:O} (apply hash-set (flatten coll)))))

(defn get-status-row
  "Returns the x-th row of the current status matrix"
  [x]
  (get @curr-status x))

(defn get-status-col
  "Retrieve an array with the statuses of the given col"
  [x]
  (vec (for [row @curr-status]
         (get row x))))

(defn get-diagonals
  "Retrieves the two diagonals of the Tictactoe status matrix"
  []
  (let [dim (count (first @curr-status))
        diag1 (vec (for [x (range dim)]
                     (get-in @curr-status [x x])))
        diag2 (vec (for [x (reverse (range dim)) ]
                     (get-in @curr-status [x (q/abs (- (- dim 1) x))])))]
    [diag1 diag2]))

(defn check-winner
  "Check for any winning combination. We need to check all rows and cols and the two diagonals for a winning tris"
  []
  (let [combinations (-> (map is-winning (get-diagonals))
                         (conj (map is-winning (for [x (range (count (first @curr-status)))]
                                                (get-status-row x))))
                         (conj (map is-winning (for [x (range (count (first @curr-status)))]
                                                (get-status-col x))))
                         flatten
                         )]
    (when (some #{true} combinations)
      (swap! winner (constantly @curr-player)))))

(defn announce-winner []
  (if (= @winner computer)
    "Sorry.. You Lost!"
    "YOU WON!"))

(defn handle-mouse-click
  "User clicked. Render the move"
  []
  (let [button (q/mouse-button)
        x (q/mouse-x)
        y (q/mouse-y)
        [row col] (identify-cell x y)
        curr-cell-status (get-in @curr-status [row col])]
    (when (and (= @winner 0) (= curr-cell-status 0))
      (swap! curr-status update-in [row col] (constantly @curr-player))
      (set-next-player))))

(defn draw []
  (q/background-float 100)
  (q/stroke-float 10)
  (q/fill-float (rand-int 125) (rand-int 125) (rand-int 125))
  (draw-wires)
  (draw-curr-status)
  (draw-curr-player)
  (cond  (not= @winner 0) (let [winner-str (announce-winner)]
                            (do
                              (q/text-size 30)
                              (q/fill 255 255 255)
                              (q/text winner-str
                                      (- (int (/ (q/width) 2)) (int (/ (q/text-width winner-str) 2)))
                                      (int (/ (q/height) 2)))))
         (not (is-move-available)) (q/text "GAME OVER" (- (int (/ (q/width) 2)) (q/text-width "GAME OVER")) (int (/ (q/height) 2)))
         :else (do
                 (when (= @curr-player computer)
                   (q/text "Thinking... " 0 10)
                                        ;Would be nice to have a Thread/sleep to simulate the thinking process :)
                   (computer-moves)
                   (set-next-player)))))

(defn setup []
  (q/smooth)
  (q/no-stroke)
  (q/fill 400)
  (q/frame-rate 10)
  (choose-players)
  (choose-starter)
  )

(q/defsketch nico
  :title "TicTacToe"
  :setup setup
  :draw draw
  :size [400 400]
  :mouse-clicked handle-mouse-click)
