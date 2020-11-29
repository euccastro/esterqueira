(ns esterqueira.logic
  (:require [esterqueira.numbers :refer [number-centers]]))

;;; O sistema de coordenadas em state está centrado no centro da tela mais com
;;; coordenadas em pixels.


(def initial-ball-speed 0.4)
(def racket-bounce-speed-multiplier 1.05)


(defn- round [x]
  (Math/round (double x)))


(defn ball0 [radius]
  {:x 0 :y 0 :speed (* radius initial-ball-speed) :direction (/ Math/PI 4)})


(defn on-resize [width height]
  (let [radius (round (/ height 60))
        half-width (round (/ width 2))
        half-height (round (/ height 2))
        racket-half-height (* radius 4)
        racket {:y 0
                :half-height racket-half-height}]
    {:half-width (float (/ width 2))
     :half-height (float (/ height 2))
     :racket-travel (- half-height racket-half-height)
     :radius radius
     :score {:left 0 :right 0}
     :ball (ball0 radius)
     :rackets
     {:left
      (assoc racket :x (- radius half-width))
      :right
      (assoc racket :x (- half-width radius))}}))


(def tau (* Math/PI 2))
(def half-pi (/ Math/PI 2))
(def three-halfs-pi (* Math/PI 3/2))


(defn reflect-x [angle]
  (mod (- angle) tau))


(defn reflect-y [angle]
  (mod
   (+ angle (* 2 (- half-pi angle)))
   tau))


(defn bounce-direction [racket-x racket-y ball-x ball-y]
  (let [dy (- ball-y racket-y)
        dx (- ball-x racket-x)]
    (Math/atan2 dy dx)))


(defn tick [{:keys [rackets racket-travel radius half-width half-height]
             {:keys [x y direction speed]} :ball
             {left-score :left right-score :right} :score
             :as state}
            [_ _ _ _ joystick-y _]]
  (let [left-racket-y (- (* joystick-y racket-travel))
        right-racket-y left-racket-y
        x (+ x (* (Math/cos direction) speed))
        y (+ y (* (Math/sin direction) speed))
        ball-bounce-x (- half-width (* radius 3))
        ball-travel-x (+ half-width radius)
        ball-travel-y (- half-height radius)
        ball0 (ball0 radius)

        ;; point scored?
        [new-left-score new-right-score]
        (cond
          (< x (- ball-travel-x))
          [left-score (inc right-score)]
          (> x ball-travel-x)
          [(inc left-score) right-score]
          :else [left-score right-score])

        [y direction]
        (cond
          ;; reset if point scored
          (not= [left-score right-score] [new-left-score new-right-score])
          ((juxt :y :direction) ball0)
          ;; ball bounce on wall
          (< y (- ball-travel-y))
          [(- ball-travel-y) (reflect-x direction)]
          (> y ball-travel-y)
          [ball-travel-y (reflect-x direction)]
          ;; default
          :else [y direction])

        [x direction speed]
        (cond
          ;; reset if point scored
          (< left-score new-left-score)
          ((juxt :x (comp reflect-y :direction) :speed) ball0)
          (< right-score new-right-score)
          ((juxt :x :direction :speed) ball0)
          ;; ball bounce on racket
          (and
           (<= x (- ball-bounce-x))
           (< (Math/abs (- y left-racket-y))
              (+ radius (-> rackets :left :half-height))))
          [(- ball-bounce-x)
           (bounce-direction (- half-width) left-racket-y x y)
           (* speed racket-bounce-speed-multiplier)]
          (and
           (>= x ball-bounce-x)
           (< (Math/abs (- y right-racket-y))
              (+ radius (-> rackets :right :half-height))))
          [ball-bounce-x
           (bounce-direction half-width right-racket-y x y)
           (* speed racket-bounce-speed-multiplier)]
          :else [x direction speed])

        ;; game end/restart
        [new-left-score new-right-score]
        (if (> (max new-left-score new-right-score) 9)
          [0 0]
          [new-left-score new-right-score])]

    (-> state
        (assoc-in [:rackets :left :y] left-racket-y)
        (assoc-in [:rackets :right :y] right-racket-y)
        (assoc :ball {:x x :y y :speed speed :direction direction})
        (assoc :score {:left new-left-score :right new-right-score}))))


(defn game->gl [half-width half-height [x y w h]]
  [(/ x half-width) (/ y half-height) (/ w half-width) (/ h half-height)])


(defn draw [{:keys [half-width half-height radius ball rackets]
             {left-score :left right-score :right} :score}]
  ;; devolve umha sequência de rectângulos.
  ;; cada rectângulo é um vector [x y w h]
  ;; onde x y w h som o centro, ancho e alto em pixels
  (map (partial game->gl half-width half-height)
       (concat
        (list [(:x ball) (:y ball) radius radius])
        (for [racket (vals rackets)]
          [(:x racket) (:y racket) radius (:half-height racket)])
        (for [[number offset] [[left-score -1] [right-score 1]]
              [x y] (number-centers number)]
          [(round (+ (* offset (/ half-width 3)) (* x radius 2)))
           (round (+ (* half-height 2/3) (* y radius 2)))
           radius
           radius]))))


(comment
  (def height 401)
  (def width 801)
  (def state (on-resize width height))
  (draw state))
