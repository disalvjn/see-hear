(ns see-hear.process.particle-mover
  (:require [see-hear.item.particle :as particle]
            [see-hear.util :as util]
            [see-hear.component :as component]))

(component/def-process particle-mover
  :domain :particle
  :initialize (fn [] (atom {:particle-mover/move-strategy particle/move-bounce})))

(component/def-work particle-mover
  [this-state views particles]
  (map (:particle-mover/move-strategy @this-state) particles))

(defn swap-particles!
  [particles-state f]
  (swap! particles-state #(mapv f %)))

(component/def-message particle-mover move-strategy
  [this-state particles-state strategy]
  (let [strategy-map {:bounce particle/move-bounce
                      :wrap particle/move-wrap}]
    (swap! this-state assoc :particle-mover/move-strategy (strategy-map strategy))))

(component/def-message particle-mover accelerate
  [this-state particles-state dv]
  (swap-particles!
   particles-state
   (fn [p]
     (-> p
         (update :particle/vx * dv)
         (update :particle/vy * dv)))))

(component/def-message particle-mover freeze
  [this-state particles-state]
  (swap-particles!
   particles-state
   (fn [p] (assoc p
                  :particle/vx 0
                  :particle/vy 0))))


(component/def-message particle-mover scatter
  [this-state particles-state]
  (swap-particles!
   particles-state
   particle/scatter-preserving-magnitude))