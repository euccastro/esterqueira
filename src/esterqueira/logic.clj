(ns esterqueira.logic)

;;; O sistema de coordenadas em state está centrado no centro da tela mais com
;;; coordenadas em pixels.

(defn- round [x]
  (Math/round (double x)))


(defn ball0 [radius]
  {:x 0 :y 0 :vx (/ radius 4) :vy (/ radius 4)})

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


(defn tick [{:keys [rackets racket-travel radius half-width half-height]
             {:keys [x y vx vy]} :ball
             {score-left :left score-right :right} :score
             :as state}
            [_ _ _ _ joystick-y _]]
  (let [left-racket-y (- (* joystick-y racket-travel))
        right-racket-y left-racket-y
        x (+ x vx)
        y (+ y vy)
        ball-bounce-x (- half-width (* radius 3))
        ball-travel-x (+ half-width radius)
        ball-travel-y (- half-height radius)
        ball0 (ball0 radius)

        ;; point scored?
        [new-score-left new-score-right]
        (cond
          (< x (- ball-travel-x))
          [score-left (inc score-right)]
          (> x ball-travel-x)
          [(inc score-left) score-right]
          :else [score-left score-right])

        [y vy]
        (cond
          ;; reset if point scored
          (not= [score-left score-right] [new-score-left new-score-right])
          ((juxt :y :vy) ball0)
          ;; ball bounce on wall
          (< y (- ball-travel-y)) [(- ball-travel-y) (- vy)]
          (> y ball-travel-y) [ball-travel-y (- vy)]
          ;; default
          :else [y vy])

        [x vx]
        (cond
          ;; reset if point scored
          (< score-left new-score-left)
          ((juxt :x :vx) ball0)
          (< score-right new-score-right)
          [(:x ball0) (- (:vx ball0))]
          ;; ball bounce on racket
          (and
           (<= x (- ball-bounce-x))
           (< (Math/abs (- y left-racket-y))
              (+ radius (-> rackets :left :half-height))))
          [(- ball-bounce-x) (- vx)]
          (and
           (>= x ball-bounce-x)
           (< (Math/abs (- y right-racket-y))
                (+ radius (-> rackets :right :half-height))))
          [ball-bounce-x (- vx)]
          :else [x vx])]
    (-> state
        (assoc-in [:rackets :left :y] left-racket-y)
        (assoc-in [:rackets :right :y] right-racket-y)
        (assoc :ball {:x x :y y :vx vx :vy vy})
        (assoc :score {:left new-score-left :right new-score-right}))))


(defn game->gl [half-width half-height [x y w h]]
  [(/ x half-width) (/ y half-height) (/ w half-width) (/ h half-height)])


(defn draw [{:keys [half-width half-height radius ball rackets] :as state}]
  ;; devolve umha sequência de rectângulos.
  ;; cada rectângulo é um vector [x y w h]
  ;; onde x y w h som o centro, ancho e alto em pixels
  (map (partial game->gl half-width half-height)
       (concat
        (list [(:x ball) (:y ball) radius radius])
        (for [racket (vals rackets)]
          [(:x racket) (:y racket) radius (:half-height racket)]))))


(comment
  (def height 401)
  (def width 801)
  (def state (on-resize width height))
  (draw state))
