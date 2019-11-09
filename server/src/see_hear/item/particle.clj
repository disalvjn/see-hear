(ns see-hear.item.particle)

(def particle
  {:item/type :particle
   :item/id 0
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
        new-y (+ y vy)
        over-x? (and max-x (> max-x 0) (or (< new-x 0) (> new-x max-x)))
        over-y? (and max-y (> max-y 0) (or (< new-y 0) (> new-y max-y)))]
    (merge particle
           {:particle/x (if over-x? x new-x)
            :particle/y (if over-y? y new-y)}
           (if over-x? {:particle/vx (* -1 vx)})
           (if over-y? {:particle/vy (* -1 vy)}))))
