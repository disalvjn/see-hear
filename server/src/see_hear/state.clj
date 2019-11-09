(ns see-hear.state)

(defn state
  []
  {:state/items (atom {})
   :state/views (atom [])
   :state/processes (atom [])})

(defn step!
  [{:keys [state/items state/processes]}]
  (doseq [process @processes]
    (if-let [items-state (get @items (:process/item-type process))]
      (swap! items-state (:process/step process))
      (swap! items assoc (atom ((:process/step nil)))))))

(defn send!
  [{:keys [state/items state/processes]} [message-type & args]]
  (let [applicable-processes (filter #(get (:process/messages %) message-type) @processes)]
    (doseq [process applicable-processes]
      (let [message-handler (get-in process [:process/messages message-type])
            item-type (:process/item-type process)]
        (do 
          (when (not (get @items item-type))
            (swap! items assoc item-type (atom nil)))
          (apply message-handler (:process/state process) (get @items item-type) args))))))

(defn register-process!
  [state process]
  (swap! (:state/processes state) conj process))

(defn register-view!
  [state view]
  (swap! (:state/views state) conj view))

(defn obliterate!
  [state]
  (reset! (:state/items state) {})
  (reset! (:state/processes state) [])
  (reset! (:state/views state) []))

(defn view
  [state]
  (apply merge-with into
         (map (fn [{:keys [view/return-type view/item-type view/view]}]
                {return-type (view @(get @(:state/items state) item-type))}) 
              @(:state/views state))))