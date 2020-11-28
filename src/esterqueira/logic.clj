(ns esterqueira.logic)


(defn initial-state []
  ;; velocidade da bola em pixels/s, posiçom da bola e das raquetas em pixels, e
  ;; pontuaçom dos jogadores
  )


(defn on-resize [state width height]
  ;; recalcula ancho da raqueta, que é a referência base para todo o resto das
  ;; dimensons do jogo
  )


(defn tick [state pressed-keys])


(defn draw [state]
  ;; devolve umha sequência de rectângulos.
  ;; cada rectângulo é um vector [x y w h]
  ;; onde x y w h som o centro, ancho e alto em pixels
  )
