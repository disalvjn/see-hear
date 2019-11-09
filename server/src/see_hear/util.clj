
(ns see-hear.util)

(defn rgb->hex
  [[r g b]]
  (str
   "#"
   (Integer/toString r 16)
   (Integer/toString g 16)
   (Integer/toString b 16)))

(defn rand-between
  [low high]
  (+ low (rand-int (- high low))))