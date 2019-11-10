
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

;; https://martin.ankerl.com/2009/12/09/how-to-create-random-colors-programmatically/
(defn hsv->rgb
  [h s v]
  (let [hi (int (Math/floor (* h 6)))
        f (- (* h 6) hi)
        p (* v (- 1 s))
        q (* v (- 1 (* f s)))
        t (* v (- 1 (* (- 1 f) s)))
        [r g b] (case hi
                  0 [v t p]
                  1 [q v p]
                  2 [p v t]
                  3 [p q v]
                  4 [t p v]
                  5 [v p q])]
    [(Math/round (* 256 r))
     (Math/round (* 256 g))
     (Math/round (* 256 b))]))

(def golden-ratio-conjugate 0.618033988749895)
(defn gen-random-color
  []
  (let [last-h (atom (rand))]
    (fn [s v]
      (let [next-h (mod (+ @last-h golden-ratio-conjugate) 1)
            color (rgb->hex (hsv->rgb next-h s v))]
        (reset! last-h next-h)
        color))))

(def ^:private random-color' (gen-random-color))

(defn random-color
  [s v]
  (random-color' s v))

(defn map-value
  [f map]
  (reduce (fn [acc [k v]] (assoc acc k (f v))) {} map))

(defn index-by
  [f coll]
  (reduce (fn [acc item] (assoc acc (f item) item)) {} coll))
