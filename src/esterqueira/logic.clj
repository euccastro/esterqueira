(ns esterqueira.logic)

;;; O sistema de coordenadas em state está centrado no centro da tela mais com
;;; coordenadas em pixels.

(defn- round [x]
  (Math/round (double x)))


(defn on-resize [width height]
  (let [radius (round (/ height 40))
        half-width (round (/ width 2))
        half-height (round (/ height 2))
        racket {:y 0
                :half-height (* radius 4)}]
    {:half-width (float (/ width 2))
     :half-height (float (/ height 2))
     :radius radius
     :score {:left 0 :right 0}
     :ball {:x 0 :y 0 :vx (* radius 4) :vy (* radius 4)}
     :rackets
     {:left
      (assoc racket :x (- radius half-width))
      :right
      (assoc racket :x (- half-width radius))}}))


#_(defn tick [state]
  ;; XXX take input
  )


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
