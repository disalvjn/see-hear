(ns see-hear.view.particle-blob
  (:require [see-hear.shape :as shape]))

(def particle-blob
  {:view/type :particle-blob
   :view/item-type :particle
   :view/return-type :shapes
   :view/view (fn [particles]
                (map (fn [{:keys [particle/color particle/x particle/y particle/radius]}]
                       (-> shape/circle
                           (assoc :shape/color color
                                  :shape/line-width 0.5
                                  :circle/x x
                                  :circle/y y
                                  :circle/radius radius
                                  :circle/fill? true)))
                     particles))})