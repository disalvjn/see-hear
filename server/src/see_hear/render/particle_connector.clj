
(ns see-hear.render.particle-connector
  (:require [see-hear.shape :as shape]))

(defonce particle-connector-message-handler
  (atom {}))

(defn particle-connector
  []
  {:render/type :particle-connector
   :render/item-type :particle
   :render/return-type :shapes
   :render/render 
   (fn [this-state particles]
     (let [{:keys [line-width]} this-state]
       (for [p1 particles
             p2 particles
             :when (not= (:item/id p1) (:item/id p2))]
         (-> shape/line 
             (assoc :shape/color (:particle/color p1)
                    :shape/line-width line-width
                    :line/from-x (:particle/x p1)
                    :line/from-y (:particle/y p1)
                    :line/to-x (:particle/x p2)
                    :line/to-y (:particle/y p2))))))
   
   :render/messages particle-connector-message-handler
   
   
   :render/state (atom {:line-width 0.5})})

(swap! particle-connector-message-handler
       assoc :particle-connector/line-width
       (fn [this-state width] (swap! this-state assoc :line-width width)))