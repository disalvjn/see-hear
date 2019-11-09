(ns see-hear.process.particle-mover
  (:require [see-hear.item.particle :as particle]
            [see-hear.util :as util]))

(defn particle-mover
  []
  {:process/type :particle-mover
   :process/item-type :particle
   :process/step (fn [particles] (map particle/move particles))
   :process/state (atom {})})