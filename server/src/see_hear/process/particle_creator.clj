(ns see-hear.process.particle-creator
  (:require [see-hear.item.particle :as particle]
            [see-hear.util :as util]))

(defonce particle-creator-message-handler
  (atom {}))

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
            color (hsv->rgb next-h s v)]
        (reset! last-h next-h)
        color))))

(def random-color (gen-random-color))


(defn particle-creator 
  [{:keys [max-x max-y]
    :or {max-x 800, max-y 800}}]

  {:process/type 
   :particle-creator
   :process/item-type :particle

   :process/step (fn [this-state particles] particles)

   :process/messages particle-creator-message-handler
   
   :process/state (atom {:particle-creator/max-x max-x 
                         :particle-creator/max-y max-y
                         :last-hue (rand)})})

(swap! particle-creator-message-handler
       assoc :particle-creator/create
       (fn [this-state particles-state number-of-particles]
         (let [{:keys [particle-creator/max-x particle-creator/max-y]} @this-state]
           (swap! particles-state
                  into
                  (map (fn [_]
                         (assoc particle/particle
                                :item/id (rand-int 2100000000)
                                :particle/x (rand-int max-x)
                                :particle/y (rand-int max-y)
                                :particle/vx (* 3 (rand))
                                :particle/vy (* 3 (rand))
                                :particle/max-x max-x
                                :particle/max-y max-y
                                :particle/radius (* 8 (rand))
                                :particle/color 
                                (util/rgb->hex 
                                 [(random-color 0.5 0.95)
                                  (random-color 0.5 0.95)
                                  (random-color 0.5 0.95)])))
                       (range 0 number-of-particles))))))

(swap! particle-creator-message-handler
       assoc :particle-creator/destroy
       (fn [this-state particles-state & [number-of-particles]]
         (if number-of-particles
           (swap! particles-state #(drop number-of-particles %))
           (reset! particles-state []))))