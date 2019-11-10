(ns see-hear.item.particle
  (:require [see-hear.util :as util]))

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

(defn square [x] (* x x))

(defn scatter-preserving-magnitude
  [{:keys [particle/vx particle/vy] :as particle}]
  (let [old-magnitude (Math/sqrt (+ (* vx vx) (* vy vy)))
        new-vx (* old-magnitude (rand))
        new-vy (Math/sqrt (- (square old-magnitude)
                             (square new-vx)))]
    (assoc particle
           :particle/vx new-vx
           :particle/vy new-vy)))

(defn move-bounce
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
           (if over-x? {:particle/vx (* -1 vx)} nil)
           (if over-y? {:particle/vy (* -1 vy)} nil))))

(defn move-wrap
  [particle]
  (let [{:keys [particle/x particle/vx
                particle/y particle/vy
                particle/max-x particle/max-y]} particle
        new-x (+ x vx)
        new-y (+ y vy) ]
    (merge particle
           {:particle/x (cond (neg? new-x) max-x
                              (> new-x max-x) 0
                              :else new-x)
            :particle/y (cond (neg? new-y) max-y
                              (> new-y max-y) 0
                              :else new-y)})))

(defn centroid
  [particles]
  (let [n (count particles)
        average (fn [key] (/ (apply + (map key particles)) n))]
    (merge (first particles)
           {:particle/x (average :particle/x)
            :particle/y (average :particle/y)
            :particle/vx (average :particle/vx)
            :particle/vy (average :particle/vy)
            :particle/radius (average :particle/radius)
            :particle/id (rand-int 2100000000)})))