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
            [see-hear.util :as util]))

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
(defn line [from-x from-y to-x to-y]
  (let [m (/ (- to-y from-y) (- to-x from-x))
        f (fn [x] (+ from-y (* m (- x from-x))))]
    {:line/from-x 0, :line/from-y 0, :line/to-x 400, :line/to-y 800, :line/f f}))

(defn below?
  [line]
  (fn [x y]
    (> ((:line/f line) x) y)))

(defn above?
  [line]
  (fn [x y]
    (< ((:line/f line) x) y)))

(defn rules->color
  [rules x y]
  (->> rules 
       (filter (fn [rule] 
                 (every? (fn [pred] (pred x y))
                         (:rule/and rule))))
       first
       :rule/color))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def line-A (line 0 0 500 800))
(def line-B (line 600 700 200 300))

(def c-below-A-B (util/random-color 0.5 0.95))
(def c-above-A-B (util/random-color 0.5 0.95))
(def c-below-A-above-B (util/random-color 0.5 0.95))
(def c-above-A-below-B (util/random-color 0.5 0.95))

(def rules 
  [{:rule/and [(below? line-A) (below? line-B)],
    :rule/color c-below-A-B},
   {:rule/and [(below? line-A) (above? line-B)]
    :rule/color c-below-A-above-B}
   {:rule/and [(above? line-A) (below? line-B)],
    :rule/color c-above-A-below-B}
   {:rule/and [(above? line-A) (above? line-B)]
    :rule/color c-above-A-B } ])

(def data 
  {:height 800,
   :width 800,
   :pixels (for [x (range 0 750), y (range 0 750)]
             {:pixel/x x, :pixel/y y, :pixel/color (rules->color rules x y)}
             )
   })

(websocket-send! data)

(go)

; (defn send-render! [] (websocket-send! (state/render state)))

; (defn init
;   []
;   (state/obliterate! state)
;   (state/add-process! state (particle-creator/particle-creator {}))
;   (state/add-process! state (particle-mover/particle-mover))
;   (state/add-render! state (particle-blob/particle-blob))
;   (state/add-render! state (particle-connector/particle-connector [:closest 3]))
;   (state/send! state [:particle-creator/create 2]))

; (defn spawn-render!
;   []
;   (future (while @loop?
;             (try
;               (state/step! state)
;               (send-render!)
;               (catch Exception _))
;             (Thread/sleep @delay))))

; (defn send!
;   [message]
;   (state/send! state message))

; (defn add-render!
;   [renderer]
;   (state/add-render! state renderer))

; (defn drop-render!
;   [render-type]
;   (state/drop-render! state render-type))

; (defn add-process!
;   [process]
;   (state/add-process! state process))

; (defn drop-process
;   [process-type]
;   (state/drop-process! process-type))

;; (go)
