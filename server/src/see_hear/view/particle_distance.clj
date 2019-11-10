(ns see-hear.view.particle-distance
  (:require [see-hear.item.particle :as particle]
            [see-hear.view :as view]
            [see-hear.util :as util]))

(view/def-view particle-distance
  []
  {:view/type :particle-distance

   :component/item-type :particle
   
   :component/state (atom {})

   :view/view 
   (fn [this-state particles]
     (->>
      (for [p1 particles
            p2 particles
            :when (not= (:item/id p1) (:item/id p2))]
        {:particle-distance/from (:item/id p1)
         :particle-distance/to p2
         :particle-distance/distance 
         (particle/distance p1 p2)})
      (group-by :particle-distance/from)))})