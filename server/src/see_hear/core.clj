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
