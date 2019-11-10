(ns see-hear.render.particle-blob
  (:require [see-hear.shape :as shape]))

(defonce particle-blob-message-handler
  (atom {}))

(defn particle-blob
  []
  {:render/type :particle-blob
   :component/item-type :particle
   :render/return-type :shapes
   :render/render 
   (fn [this-state views particles]
     (map (fn [{:keys [particle/color particle/x particle/y particle/radius]}]
            (-> shape/circle
                (assoc :shape/color color
                       :shape/line-width 0.5
                       :circle/x x
                       :circle/y y
                       :circle/radius radius
                       :circle/fill? true)))
          particles))
   :render/messages particle-blob-message-handler
   :component/state (atom {})})