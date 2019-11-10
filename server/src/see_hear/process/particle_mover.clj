(ns see-hear.process.particle-mover
  (:require [see-hear.item.particle :as particle]
            [see-hear.util :as util]))

(defonce particle-mover-message-handler
  (atom {}))

(def strategy-map
  {:bounce particle/move-bounce
   :wrap particle/move-wrap})

(defn particle-mover
  []
  {:process/type :particle-mover
   :component/item-type :particle
   :process/step (fn [this-state views particles] 
                   (map (:particle-mover/move-strategy @this-state) particles))
   :component/state (atom {:particle-mover/move-strategy particle/move-bounce})
   :process/messages particle-mover-message-handler})

(defn swap-particles!
  [particles-state f]
  (swap! particles-state #(mapv f %)))

(swap! particle-mover-message-handler
       assoc :particle-mover/move-strategy
       (fn [this-state particles-state strategy]
         (swap! this-state assoc :particle-mover/move-strategy (strategy-map strategy))))

(swap! particle-mover-message-handler
       assoc :particle-mover/accelerate
       (fn [this-state particles-state dv]
         (swap-particles! 
          particles-state
          (fn [p] 
            (-> p
                (update :particle/vx * dv)
                (update :particle/vy * dv))))))

(swap! particle-mover-message-handler
       assoc :particle-mover/freeze
       (fn [this-state particles-state]
         (swap-particles!
          particles-state
          (fn [p] (assoc p
                         :particle/vx 0
                         :particle/vy 0)))))


(swap! particle-mover-message-handler
       assoc :particle-mover/scatter
       (fn [this-state particles-state]
         (swap-particles! 
          particles-state 
          particle/scatter-preserving-magnitude)))