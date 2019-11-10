
(ns see-hear.render.particle-centroid
  (:require [see-hear.shape :as shape]
            [see-hear.item.particle :as particle]
            [see-hear.util :as util]
            [clojure.core.match :as match]))

(defonce particle-centroid-message-handler
  (atom {}))

(defn particle-centroid
  []
  {:render/type :particle-centroid
   :component/item-type :particle
   :render/return-type :shapes
   :render/render 
   (fn [this-state views particles]
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
   
   :render/messages particle-centroid-message-handler
   
   :component/views
   [{:view/use? (constantly true)
     :view/type :particle-distance}]
   
   
   :component/state (atom {:line-width 0.5, :number-points 1})})

(swap! particle-centroid-message-handler
       assoc :particle-centroid/line-width
       (fn [this-state width] (swap! this-state assoc :line-width width)))


(swap! particle-centroid-message-handler
       assoc :particle-centroid/number-points
       (fn [this-state n] (swap! this-state assoc :number-points n)))
