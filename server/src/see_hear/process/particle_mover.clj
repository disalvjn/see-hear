(ns see-hear.process.particle-mover
  (:require [see-hear.item.particle :as particle]
            [see-hear.util :as util]))

(defonce particle-mover-message-handler
  (atom {}))

(defn particle-mover
  []
  {:process/type :particle-mover
   :process/item-type :particle
   :process/step (fn [this-state particles] 
                   (map (:particle-mover/move-strategy @this-state) particles))
   :process/state (atom {:particle-mover/move-strategy particle/move})
   :process/messages particle-mover-message-handler})