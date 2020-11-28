(ns esterqueira.main
  (:require [esterqueira.logic :as logic]
            [esterqueira.gl :as gl]))


(defn main [{:keys [width height title]
             :or {width 1024 height 768 title "Pong"}}]
  (gl/run {:width width
           :height height
           :title title
           :resize-handler logic/on-resize
           :draw-handler logic/draw
           :tick-handler logic/tick}))
