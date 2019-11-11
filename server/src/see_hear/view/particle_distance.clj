(ns see-hear.view.particle-distance
  (:require [see-hear.item.particle :as particle]
            [see-hear.component :as component]
            [see-hear.util :as util]))

(component/def-view particle-distance
  :domain :particle
  :initialize (fn [] (atom {})))

(component/def-work particle-distance
  [this-state particles]
  (->>
   (for [p1 particles
         p2 particles
         :when (not= (:item/id p1) (:item/id p2))]
     {:particle-distance/from (:item/id p1)
      :particle-distance/to p2
      :particle-distance/distance
      (particle/distance p1 p2)})
   (group-by :particle-distance/from)
   (util/map-value #(sort-by :particle-distance/distance %))))