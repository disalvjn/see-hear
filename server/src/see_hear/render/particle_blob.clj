(ns see-hear.render.particle-blob
  (:require [see-hear.shape :as shape]
            [see-hear.component :as component]))

(component/def-render particle-blob
  :domain :particle
  :return :shapes
  :initialize (fn [] (atom {})))
                         

(component/def-work particle-blob
  [this-state views particles]
  (map (fn [{:keys [particle/color particle/x particle/y particle/radius]}]
         (-> shape/circle
             (assoc :shape/color color
                    :shape/line-width 0.5
                    :circle/x x
                    :circle/y y
                    :circle/radius radius
                    :circle/fill? true)))
       particles))