(ns see-hear.core
  (:require [cheshire.core :as json]
            [org.httpkit.server :as httpkit]
            [clojure.string :as str]))

(defonce channel (atom nil))
(defonce stops (atom []))

(defn websocket-handler [ring-request]
  (httpkit/with-channel ring-request req-channel
    (when (httpkit/websocket? req-channel)
      (reset! channel req-channel))))

(defn go
  []
  (swap! stops conj (httpkit/run-server websocket-handler {:port 8080})))

(defn send!
  [obj]
  (httpkit/send! @channel (json/generate-string obj)))

(defn rgb->hex
  [[r g b]]
  (str
   "#"
   (Integer/toString r 16)
   (Integer/toString g 16)
   (Integer/toString b 16)))


(def empty-state
  {:state/shapes []

   :state/processes []

   :state/meta
   {:meta/width 500
    :meta/height 500
    :meta/global-composite-operation "lighter"}})

(def circle
  {:shape/type :circle
   :shape/color "#ffffff"
   :shape/line-width 0.5
   :circle/x 0
   :circle/y 0
   :circle/radius 1
   :circle/fill? true})

(def line
  {:shape/type :line
   :shape/color "#ffffff"
   :shape/line-width 0.5
   :line/from-x 0
   :line/to-x 0
   :line/from-y 0
   :line/to -y 0})

(def particle
  {:particle/x 0
   :particle/y 0
   :particle/vx 0
   :particle/vy 0
   :particle/radius 0
   :particle/color "#ffffff"})

(def colors ["#f35d4f","#f36849","#c0d988","#6ddaf1","#f1e85b"])

(defmulti distance :)

(defn distance
  [s1 s2]
  (Math/sqrt
   (+
    (Math/pow (- (:particle/x s1) (:particle/x s2)) 2)
    (Math/pow (- (:particle/y s1) (:particle/y s2)) 2))))

(defn move-particle
  [particle]
  (let [{:keys [particle/x particle/vx
                particle/y particle/vy
                particle/max-x particle/max-y]} particle
        new-x (+ x vx)
        new-y (+ y vy)]
    (assoc particle
           :particle/x (if (> new-x max-x) 0 new-x)
           :particle/y (if (> new-y max-y) 0 new-y))))

;; A process updates a state
;; the shapes are a view of the state
;; I think we want to decouple the two

;; [state, process] -> [state, process]
(defn particle-process
  [{:keys [number-particles max-x max-y]
    :or {number-particles 500, max-x 500, max-y 500}}]
  {:process/type :particle-process
   :process/init
   (fn [self]
     (assoc self
            :particle-process/particles
            (mapv
             (fn [_]
               {:particle/x (Math/round (* max-x (rand)))
                :particle/y (Math/round (* max-y) (rand))
                :particle/radius (Math/round (+ (rand) 1))}
               )
             (range 0 number-particles)))

     )
   :process/update
   (fn [self]
     (update self :particle-process/particles #(mapv move-particle %)))

   :particle-process/particles []})

;; processes
;; computed (e.g. factor map)
;; views
