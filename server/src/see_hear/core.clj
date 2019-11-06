(ns see-hear.core
  (:require [cheshire.core :as json]
            [org.httpkit.server :as httpkit]))

(defonce channel (atom nil))
(defonce stops (atom []))

(defn websocket-handler [ring-request]
  (httpkit/with-channel ring-request req-channel
    (if (httpkit/websocket? req-channel)
      (reset! channel req-channel)
      (throw (Exception. "aaaalllll")))))

(defn go
  []
  (swap! stops conj (httpkit/run-server websocket-handler {:port 8080})))

(defn send!
  [obj]
  (httpkit/send! @channel (json/generate-string obj)))
