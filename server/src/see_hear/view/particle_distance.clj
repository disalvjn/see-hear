(ns see-hear.view.particle-distance
  (:require [see-hear.item.particle :as particle]))

(defn particle-distance
  []
  {:view/type :particle-distance
   :view/item-type :particle
   :view/view 
   (fn [particles]
     (for [p1 particles
           p2 particles]
       {:particle-distance/from (:item/id p1)
        :particle-distance/to (:item/id p2)
        :particle-distance/distance 
        (:particle-distance/distance p1 p2)}))})