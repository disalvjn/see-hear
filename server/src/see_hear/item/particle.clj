(ns see-hear.item.particle)

(def particle
  {:item/type :particle
   :particle/x 0
   :particle/y 0
   :particle/vx 0
   :particle/vy 0
   :particle/max-x 0
   :particle/max-y 0
   :particle/radius 0
   :particle/color "#ffffff"})


(defn distance
  [particle1 particle2]
  (Math/sqrt
   (+
    (Math/pow (- (:particle/x particle1) (:particle/x particle2)) 2)
    (Math/pow (- (:particle/y particle1) (:particle/y particle2)) 2))))

(defn move
  [particle]
  (let [{:keys [particle/x particle/vx
                particle/y particle/vy
                particle/max-x particle/max-y]} particle
        new-x (+ x vx)
        new-y (+ y vy)]
    (assoc particle
           :particle/x (if (and max-x (> max-x 0) (> new-x max-x)) 0 new-x)
           :particle/y (if (and max-y (> max-y 0) (> new-y max-y)) 0 new-y))))
