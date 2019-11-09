(ns see-hear.state)

(defn state
  []
  {:state/items (atom {})
   :state/renders (atom [])
   :state/processes (atom [])})

(defn step!
  [{:keys [state/items state/processes]}]
  (doseq [process @processes]
    (if-let [items-state (get @items (:process/item-type process))]
      (swap! items-state #((:process/step process) (:process/state process) %))
      (swap! items assoc (:process/item-type process)
             (atom ((:process/step process) (:process/state process) nil))))))

(defn send!
  [{:keys [state/items state/processes state/renders]} [message-type & args]]
  (let [applicable-processes (filter #(get @(:process/messages %) message-type) 
                                     @processes)
        applicable-renders (filter #(get @(:render/messages %) message-type)
                                   @renders)]
    (doseq [render applicable-renders]
      (apply (get @(:render/messages render) 
                  message-type)
             (:render/state render)
             args))
    (doseq [process applicable-processes]
      (let [message-handler (get @(:process/messages process) 
                                 message-type)
            item-type (:process/item-type process)]
        (do 
          (when (not (get @items item-type))
            (swap! items assoc item-type (atom nil)))
          (apply message-handler (:process/state process) (get @items item-type) args))))))

(defn add-process!
  [state process]
  (swap! (:state/processes state) conj process))

(defn drop-process!
  [state process-type]
  (swap! (:state/processes state) 
         (partial filter #(not= process-type (:process/type %)))))

(defn add-render!
  [state render]
  (swap! (:state/renders state) conj render))

(defn drop-render!
  [state render-type]
  (swap! (:state/renders state)
         (partial filter #(not= render-type (:render/type %)))))

(defn obliterate!
  [state]
  (reset! (:state/items state) {})
  (reset! (:state/processes state) [])
  (reset! (:state/renders state) []))

(defn render
  [state]
  (apply merge-with into
         (map (fn [{:keys [render/return-type render/item-type render/render] :as renderer}]
                {return-type (render (some-> (:render/state renderer) deref) 
                                     @(get @(:state/items state) item-type))}) 
              @(:state/renders state))))