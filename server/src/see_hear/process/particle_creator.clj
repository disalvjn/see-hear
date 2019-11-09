(ns see-hear.process.particle-creator
  (:require [see-hear.item.particle :as particle]
            [see-hear.util :as util]))

(defn particle-creator 
  [{:keys [number-particles max-x max-y]
    :or {number-particles 500, max-x 500, max-y 500}}]
  {:process/type :particle-creator
   :process/item-type :particle
   
   ;; this is a pure function that the process handler will update the state with
   :process/step (fn [particles] particles)
   
   :process/messages
   {:particle-creator/create 
    (fn [this-state particles-state number-of-particles]
      (swap! particles-state
             into
             (map (fn [_]
                    (assoc particle/particle
                           :particle/x (rand-int max-x)
                           :particle/y (rand-int max-y)
                           :particle/vx (* 3 (rand))
                           :particle/vy (* 3 (rand))
                           :particle/max-x max-x
                           :particle/max-y max-y
                           :particle/radius (util/rand-between 3 30)
                           :particle/color (util/rgb->hex [(util/rand-between 0 255) 
                                                           (util/rand-between 0 255) 
                                                           (util/rand-between 0 255)])))
                  (range 0 number-of-particles))))}
   
   :process/state (atom {})})