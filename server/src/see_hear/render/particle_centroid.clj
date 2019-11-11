
(ns see-hear.render.particle-centroid
  (:require [see-hear.shape :as shape]
            [see-hear.item.particle :as particle]
            [see-hear.util :as util]
            [see-hear.component :as component]
            [clojure.core.match :as match]))

(component/def-render particle-centroid
  :domain :particle
  :return :shapes
  :views [{:view/use? (constantly true)
           :view/type :particle-distance}]
  :initialize (fn [] (atom {:line-width 0.5, :number-points 1})))

(component/def-work particle-centroid
  [this-state views particles]
  (let [{:keys [line-width number-points]} this-state
        id->particle (util/index-by :item/id particles)
        particle-distance (->> views
                               :particle-distance
                               (util/map-value #(sort-by :particle-distance/distance %)))]
    (for [p particles
          :let [closest (->> (get particle-distance (:item/id p))
                             (take number-points)
                             (map :particle-distance/to))
                centroid (particle/centroid (conj closest p))]]
      (assoc shape/circle
             :shape/color (:particle/color p)
             :shape/line-width line-width
             :circle/x (:particle/x centroid)
             :circle/y (:particle/y centroid)
             :circle/radius (* 2 (particle/distance centroid p))
             :circle/fill? false))))

(component/def-message particle-centroid line-width 
  [this-state width] 
  (swap! this-state assoc :line-width width))


(component/def-message particle-centroid number-points [this-state n] (swap! this-state assoc :number-points n))
