(ns see-hear.core
  (:require [cheshire.core :as json]
            [org.httpkit.server :as httpkit]
            [clojure.string :as str]
            [see-hear.process.particle-creator :as particle-creator]
            [see-hear.process.particle-mover :as particle-mover]
            [see-hear.state :as state]
            [see-hear.view.particle-blob :as particle-blob]))

(defonce channel (atom nil))
(defonce stops (atom []))
(defonce loop? (atom true))
(def state (state/state))

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

(defn send-view! [] (send! (state/view state)))

(defn init
  []
  (state/obliterate! state)
  (state/register-process! state (particle-creator/particle-creator {}))
  (state/register-process! state (particle-mover/particle-mover))
  (state/register-view! state particle-blob/particle-blob)
  (state/send! state [:particle-creator/create 2]))

(defn spawn-view!
  []
  (future (while @loop?
            (state/step! state)
            (send-view!)
            (Thread/sleep 30))))

(go)
(init)