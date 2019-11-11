
(ns see-hear.render.particle-connector
  (:require [see-hear.shape :as shape]
            [see-hear.util :as util]
            [see-hear.component :as component]
            [clojure.core.match :as match]))

(defn line-between
  [line-width p1 p2]
  (-> shape/line
      (assoc :shape/color (:particle/color p1)
             :shape/line-width line-width
             :line/from-x (:particle/x p1)
             :line/from-y (:particle/y p1)
             :line/to-x (:particle/x p2)
             :line/to-y (:particle/y p2))))

(component/def-render particle-connector
  :domain :particle
  :return :shapes

  :views [{:view/use? (fn [this-state]
                        (match/match (:strategy this-state)
                          [:all] false
                          :else true))
           :view/type :particle-distance}]

  :initialize (fn [strategy] (atom {:line-width 0.5, :strategy strategy})))

(component/def-work particle-connector
  [this-state views particles]
  (let [{:keys [line-width]} this-state
        id->particle (util/index-by :item/id particles)
        particle-distance (:particle-distance views)
        particle-pairs
        (match/match (:strategy this-state)
          [:all] (for [p1 particles, p2 particles
                       :when (not= (:item/id p1) (:item/id p2))]
                   [p1 p2])
          [:closest n]
          (mapcat (fn [p1]
                    (let [ps (->> (get particle-distance (:item/id p1))
                                  (take n)
                                  (map :particle-distance/to))]
                      (for [p2 ps]
                        [p1 p2])))
                  particles)

          [:absolute dist]
          (mapcat (fn [p1]
                    (let [ps (->> (get particle-distance (:item/id p1))
                                  (take-while #(< (:particle-distance/distance %) dist))
                                  (map :particle-distance/to))]
                      (for [p2 ps]
                        [p1 p2])))
                  particles))]
    (map (fn [[p1 p2]] (line-between line-width p1 p2)) particle-pairs)))

(component/def-message particle-connector line-width
  [this-state width] 
  (swap! this-state assoc :line-width width))

(component/def-message particle-connector strategy
  [this-state strategy]
  (swap! this-state assoc :strategy strategy))