(ns see-hear.process.particle-creator
  (:require [see-hear.item.particle :as particle]
            [see-hear.util :as util]))

(defonce particle-creator-message-handler
  (atom {}))

(defn particle-creator 
  [{:keys [max-x max-y]
    :or {max-x 800, max-y 800}}]

  {:process/type :particle-creator

   :component/item-type :particle

   :process/step (fn [this-state views particles] particles)

   :process/messages particle-creator-message-handler
   
   :component/state (atom {:particle-creator/max-x max-x 
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
                                :particle/vx (+ 5 (* 3 (rand)))
                                :particle/vy (+ 5 (* 3 (rand)))
                                :particle/max-x max-x
                                :particle/max-y max-y
                                :particle/radius (+ 10 (* 8 (rand)))
                                :particle/color (util/random-color 0.5 0.95)))
                       (range 0 number-of-particles))))))

(swap! particle-creator-message-handler
       assoc :particle-creator/destroy
       (fn [this-state particles-state & [number-of-particles]]
         (if number-of-particles
           (swap! particles-state #(drop number-of-particles %))
           (reset! particles-state []))))