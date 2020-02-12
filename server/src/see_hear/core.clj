(ns see-hear.core
  (:require [cheshire.core :as json]
            [org.httpkit.server :as httpkit]
            [clojure.string :as str]
            [see-hear.process.particle-creator :as particle-creator]
            [see-hear.process.particle-mover :as particle-mover]
            [see-hear.state :as state]
            [see-hear.render.particle-blob :as particle-blob]
            [see-hear.render.particle-connector :as particle-connector]
            [see-hear.view.particle-distance :as particle-distance]
            [see-hear.render.particle-centroid :as particle-centroid]
            [see-hear.util :as util]
            [clojure.core.match :refer [match]]))

(defonce channel (atom nil))
(defonce stops (atom []))
(defonce loop? (atom true))
(defonce delay (atom 30))
(def state (state/state))

(defn websocket-handler [ring-request]
  (httpkit/with-channel ring-request req-channel
    (when (httpkit/websocket? req-channel)
      (reset! channel req-channel))))

(defn go
  []
  (swap! stops conj (httpkit/run-server websocket-handler {:port 8080})))

(defn websocket-send!
  [obj]
  (httpkit/send! @channel (json/generate-string obj)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; todo: handle vertical lines by attaching below? above? to the line?

(defn point [[x y]]
  {:point/x x, :point/y y})

(defn translate-point
  [[x y] point]
  (-> point (update :point/x + x) (update :point/y + y)))

(defn reflect-x
  [x-axis {:keys [point/x point/y]}]
  (point [(+ x (* 2 (- x-axis x))) y]))

(defn reflect-y
  [y-axis {:keys [point/x point/y]}]
  (point [x (+ y (* 2 (- y-axis y)))]))

(defn polygon
  [p1 p2 p3 & ps]
  {:shape/type :polygon
   :polygon/points (mapv point (into [p1 p2 p3] ps))})

(defn circle
  [center radius & [{:keys [start-degree end-degree]}]]
  {:shape/type :circle
   :circle/center (point center)
   :circle/radius radius
   :circle/start (Math/toRadians (or start-degree 0))
   :circle/end (Math/toRadians (or end-degree 360))})

(defn render
  [shape color]
  {:render/color color
   :render/shape shape})

(defn cell
  [[grid-x grid-y] renders]
  {:cell/grid-x grid-x
   :cell/grid-y grid-y
   :cell/renders renders})

(defmulti translate (fn [[x y] shape] (:shape/type shape)))

(defmulti reflect (fn [axis-type axis-point shape] (:shape/type shape)))

(defmulti apply-grid (fn [grid-factor shape] (:shape/type shape)))

(defmulti tile-cell (fn [[strategy-name options] cell] strategy-name))

(defn apply-grid-to-point 
  [grid-factor point]
    (-> point (update :point/x * grid-factor) (update :point/y * grid-factor)))

(defmethod apply-grid :polygon
  [grid-factor polygon]
  (update polygon :polygon/points (partial map (partial apply-grid-to-point grid-factor))))

(defmethod translate :polygon
  [coords polygon]
  (update polygon :polygon/points (partial map (partial translate-point coords))))

(defmethod reflect :polygon
  [axis-type axis-point polygon]
  (let [reflect-fn (case axis-type
                     :x (partial reflect-x axis-point)
                     :y (partial reflect-y axis-point))]
  (update polygon :polygon/points #(map reflect-fn %))))

(defmethod apply-grid :circle
  [grid-factor circle]
  (-> circle 
      (update :circle/center (partial apply-grid-to-point grid-factor))
      (update :circle/radius * grid-factor)))

(defmethod translate :circle
  [coords circle]
  (update circle :circle/center (partial translate-point coords)))

(defmethod reflect :circle
  [axis-type axis-point circle]
  (let [reflect-fn (case axis-type
                     :x (partial reflect-x axis-point)
                     :y (partial reflect-y axis-point))
        rad-offset (case axis-type
                     :x (/ Math/PI 4)
                     :y (* 3 (/ Math/PI 4)))]
    (-> circle
        (update :circle/center reflect-fn)
        (update :circle/start + rad-offset)
        (update :circle/end + rad-offset))))

(defn map-shapes
  [f renders]
  (map (fn [render] (update render :render/shape f)) renders))

(defmethod tile-cell :mirror
  [[_ {:keys [axis stagger?]}] {:keys [cell/grid-x cell/grid-y cell/renders]}]
  (let [new-y (if (or (= axis :y) stagger?) (* 2 grid-y) grid-y)
        new-x (if (or (= axis :x) stagger?) (* 2 grid-x) grid-x)
        reflect-fn (case axis
                     :x (partial reflect :x grid-x)
                     :y (partial reflect :y grid-y))
        reflected-renders (map-shapes reflect-fn renders)
        translate-reflected
        (case axis
          :x [(* -1 grid-x) grid-y]
          :y [grid-x (* -1 grid-y)])
        staggered-renders (if stagger? 
                            (concat (map-shapes (partial translate [grid-x grid-y]) renders)
                                    (map-shapes (partial translate translate-reflected) reflected-renders))) ]
    (cell [new-x new-y] (concat renders reflected-renders staggered-renders))))

(defmethod tile-cell :glide
  [[_ {:keys [axis double?]}] {:keys [cell/grid-x cell/grid-y cell/renders]}]
  (let [new-y (if (or (= axis :y) double?) (* 2 grid-y) grid-y)
        new-x (if (or (= axis :x) double?) (* 2 grid-x) grid-x)
        reflect-over-x (comp (partial translate [grid-x (* -1 grid-y)]) (partial reflect :y grid-y))
        reflect-over-y (comp (partial translate [(* -1 grid-x) grid-y]) (partial reflect :x grid-x))
        double-reflect (comp (partial reflect :x grid-x) (partial reflect :y grid-y))]
    (cell [new-x new-y]
          (concat renders
                  (if (or double? (= axis :x)) (map-shapes reflect-over-x renders))
                  (if (or double? (= axis :y)) (map-shapes reflect-over-y renders))
                  (if double? (map-shapes double-reflect renders))))))

(defn tiled-cell->image
  [tiled-cell grid-factor [repetitions-x repetitions-y]]
  (let [{:keys [cell/grid-x cell/grid-y cell/renders]} tiled-cell
        cell-height (* grid-y grid-factor)
        cell-width (* grid-x grid-factor)
        height (* cell-height repetitions-y )
        width (* cell-width repetitions-x )
        gridded-renders (map-shapes (partial apply-grid grid-factor) renders)
        repeated-renders (->> (for [i (range 0 repetitions-x)
                                    j (range 0 repetitions-y)]
                                (map-shapes (partial translate [(* cell-width i) (* cell-height j)]) gridded-renders))
                              (reduce into []))]
    {:image/height height
     :image/width width
     :image/renders repeated-renders}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def color-1 (util/random-color 0.5 0.96))
(def color-2 (util/random-color 0.5 0.96))

(def cell-1
  (cell [10 10]
        [(render (polygon [0 0] [5 5] [5 0]) color-1)
         (render (polygon [10 10] [5 5] [10 0]) color-2)]))


(def tile-1 (tile-cell [:mirror {:axis :x :stagger? true}] cell-1))

(def image 
  (tiled-cell->image tile-1 20 [3 3]))

(websocket-send! image)

(let [fur-background "#fffacd"
      ear-color "#f0e68c"
      tongue-red "#f08080"
      snout-charcoal "#4a4a4a"
      tooth-color "#fffafa"
      eye-color "#f4a460"

      bone-2-color "#e6e6fa"

      background-1 "#6495ed"
      background-2 "#8fbc8f"]

  (websocket-send!
   (as-> (cell [12 12]
               [;; empty space background
    ;          (render (polygon [0 0] [12 12] [12 0]) background-2)
                
                (render (polygon [0 0] [0 12] [6 6]) background-1)
                (render (polygon [0 0] [12 0] [6 6]) background-2)
                (render (polygon [12 12] [12 0] [6 6]) background-1)
                (render (polygon [12 12] [0 12] [6 6]) background-2)

              ; (render (polygon [0 10] [0 12] [12 12]) background-2)
              ; (render (polygon [10 0] [12 0] [12 12]) background-1)
                
              ;; fur background
                (render (polygon [0 1] [0 5] [4 7] [7 9] [9 7] [7 4] [5 0] [1 0]) fur-background)

              ;; left eye
                (render (polygon [2 4] [2 5] [3 5] [3 4]) eye-color)
              ;; right eye
                (render (polygon [4 2] [4 3] [5 3] [5 2]) eye-color)
              ;; left ear
                (render (polygon [0 2] [0 5] [3 7] [1 2]) ear-color)
              ;; ear completion by left tongue
                (render (polygon [7 12] [10 12] [10 11]) ear-color)
              ;; right ear
                (render (polygon [2 0] [2 1] [7 3] [5 0]) ear-color)
              ;; ear completion by right tongue
                (render (polygon [12 7] [12 10] [11 10]) ear-color)

              ;; tongue
                (render (polygon [6 6] [6 8] [9 12] [12 12] [12 9] [8 6]) tongue-red)
              ;; tongue completion above head
              ; (render (polygon [0 0] [1 0] [0 1]) tongue-red)
                
              ;; bone 2 core
                (render (polygon [9 10] [11 12] [12 12] [12 11] [10 9]) bone-2-color)
              ;; bone 2 core completion
                (render (polygon [0 0] [1 0] [0 1]) bone-2-color)
                (render (circle [9 10] 0.75) bone-2-color)
                (render (circle [10 9] 0.75) bone-2-color)


              ;; snout
                (render (polygon [4 5] [4.5 6.5] [6.5 4.5] [5 4]) snout-charcoal)

              ;; left tooth
                (render (translate [-0.5 -0.5] (polygon [5 8] [6 9] [8 10] [6 7])) tooth-color)
              ;; right tooth
                (render (translate [-0.5 -0.5] (polygon [8 5] [7 6] [10 8] [9 6])) tooth-color)


              ;; left bone core
                (render (polygon [1 12] [3 10] [2 9] [0 11] [0 12]) tooth-color)
              ;; left bone core completion
                (render (polygon [11 0] [12 0] [12 1]) tooth-color)
              ;; left bone left circle
                (render (circle [2 9] 0.75) tooth-color)
              ;; left bone right circle
                (render (circle [3 10] 0.75) tooth-color)

              ;; paw print core
                (render (circle [10 2] 1.25) snout-charcoal)
                (render (circle [8 2] 0.70) snout-charcoal)
                (render (circle [8.5 3.5] 0.70) snout-charcoal)
                (render (circle [10 4] 0.70) snout-charcoal)

              ;; small paw print 1
                (render (circle [6 11] 0.8) snout-charcoal)
                (render (circle [5 10] 0.5) snout-charcoal)
                (render (circle [6 9.5] 0.5) snout-charcoal)
                (render (circle [7 10] 0.5) snout-charcoal)]) $
     (tile-cell [:glide {:double? true}] $)
     (tile-cell [:mirror {:axis :x :stagger? true}] $)
     (tiled-cell->image $ 7 [10 10]))))

(go)