(ns see-hear.core
  (:require [cheshire.core :as json]
            [org.httpkit.server :as httpkit]
            [clojure.string :as str]
            [see-hear.process.particle-creator :as particle-creator]
            [see-hear.process.particle-mover :as particle-mover]
            [see-hear.state :as state]
            [see-hear.render.particle-blob :as particle-blob]
            [see-hear.render.particle-connector :as particle-connector]
            [see-hear.view.particle-distance :as particle-distance]))

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

(defn send-render! [] (websocket-send! (state/render state)))

(defn init
  []
  (state/obliterate! state)
  (state/add-process! state (particle-creator/particle-creator {}))
  (state/add-process! state (particle-mover/particle-mover))
  (state/add-render! state (particle-blob/particle-blob))
  (state/add-render! state (particle-connector/particle-connector [:closest 3]))
  (state/send! state [:particle-creator/create 2]))

(defn spawn-render!
  []
  (future (while @loop?
            (try
              (state/step! state)
              (catch Exception _))
            (send-render!)
            (Thread/sleep @delay))))

(defn send!
  [message]
  (state/send! state message))

(defn add-render!
  [renderer]
  (state/add-render! state renderer))

(defn drop-render!
  [render-type]
  (state/drop-render! state render-type))

(defn add-process!
  [process]
  (state/add-process! state process))

(defn drop-process
  [process-type]
  (state/drop-process! process-type))

;; (go)
(init)
;; (spawn-render!)
;; => #<Future@352a0bd4 pending>
