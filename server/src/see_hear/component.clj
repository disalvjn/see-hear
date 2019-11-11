(ns see-hear.component
  (:require [clojure.set :as set]))

(defonce component-name->constructor (atom {}))

(defmacro def-view
  [view-name args view-map]
  (let [view-type (keyword (name view-name))]
  `(swap! view-type->constructor
          assoc ~view-type
          (fn ~args (assoc ~view-map :view/type ~view-type)))))

(defn message-handler-name
  [component-name]
  (symbol (str (name component-name) "-message-handler")))

(defn work-atom-name
  [component-name]
  (symbol (str (name component-name) "-work-handler")))

(defmacro def-component
  [component-name & kvps]
  (let [comp-name-keyword (keyword (name component-name))
        message-handler  (message-handler-name component-name)
        work-handler (work-atom-name component-name)
        {:keys [domain initialize views return component-type]} (apply hash-map kvps)
        [_ constructor-args constructor-body] initialize]
    `(do
       (defonce ~message-handler (atom {}))
       
       (defonce ~work-handler (atom (fn [& _#] _#)))
       
       (defn ~component-name
         ~constructor-args
         {:component/type ~component-type
          :component/name ~comp-name-keyword
          :component/return ~return
          :component/item-type ~domain
          :component/messages ~message-handler
          :component/state ~constructor-body
          :component/views ~views
          :component/work ~work-handler})

       (swap! component-name->constructor
              assoc ~comp-name-keyword
              ~component-name))))

(macroexpand-1
'(def-component particle-mover 
   :domain     :particle
   :initialize (fn [strategy] (atom {:particle-mover/strategy strategy}))
   :work       (fn [stuff stuff2] (+ stuff stuff2))
   ))

(defmacro def-process
  [process-name & kvps]
  (let [kvps' (as-> kvps $
                (apply hash-map $)
                (assoc $ :component-type :process)
                (set/rename-keys $ {:step :work})
                (reduce into [] $))]
    `(def-component ~process-name ~@kvps')))

(defmacro def-render
  [render-name & kvps]
  (let [kvps' (as-> kvps $
                (apply hash-map $)
                (assoc $ :component-type :render)
                (set/rename-keys $ {:render :work})
                (reduce into [] $))]
    `(def-component ~render-name ~@kvps')))

(defmacro def-view
  [view-name & kvps]
  (let [kvps' (as-> kvps $
                (apply hash-map $)
                (assoc $ :component-type :view)
                (set/rename-keys $ {:view :work})
                (reduce into [] $))]
    `(def-component ~view-name ~@kvps')))

(defmacro def-message
  [component-name message-name args & body]
  (let [message-handler (message-handler-name component-name)
        full-message-name (keyword (str (name component-name) "/" (name message-name)))]
  `(swap! ~message-handler
          assoc ~full-message-name
          (fn ~args ~@body))))

(defmacro def-work
  [component-name args & body]
  (let [work-handler (work-atom-name component-name)]
  `(reset! ~work-handler (fn ~args ~@body))))

(defn create
  [component-name & args]
  (apply (get @component-name->constructor component-name) args))
