(ns see-hear.state
  (:require [see-hear.util :as util]
            [see-hear.component :as component]))

(defn state
  []
  {:state/items (atom {})
   :state/renders (atom [])
   ;; view-type -> {:view view-agent :value last-computation}
   :state/views (atom {})
   :state/processes (atom [])})

(defn views-for
  [state component]
  (let [all-views (util/map-value :value @(:state/views state))
        component-view-types (map :view/type (:component/views component))]
    (select-keys all-views component-view-types)))

(defn required-views
  [components]
  (->> components
       (mapcat (fn [process]
                 (if-let [views (:component/views process)]
                   (filter (fn [{:keys [view/use?]}]
                             (use? @(:component/state process)))
                           views)
                   (list))))
       (map :view/type)
       set))

(defn build-unbuilt-views!
  [state components]
  (let [{:keys [state/views state/items]} state

        requested-process-views
        (required-views components)
        
        unbuilt-views
        (remove (fn [view-type] (get @views view-type)) 
                requested-process-views)]

    (doseq [unbuilt-view-type unbuilt-views]
      (let [view-agent (component/create unbuilt-view-type)
            items (get @items (:component/item-type view-agent))]
        (swap! views assoc 
               unbuilt-view-type
               {:view view-agent
                :value (if items 
                         (@(:component/work view-agent) (:component/state view-agent) @items)
                         nil)})))))

(defn update-views!
  [state]
  (doseq [[view-type {:keys [view]}] @(:state/views state)]
    (swap! (:state/views state) 
           assoc view-type 
           {:view view 
            :value (@(:component/work view) 
                    (:component/state view) 
                    @(get @(:state/items state) (:component/item-type view)))})))

(defn step!
  [{:keys [state/items state/processes] :as state}]
  (build-unbuilt-views! state @processes)
  (doseq [process @processes]
    (if-let [items-state (get @items (:component/item-type process))]
      (swap! items-state #(@(:component/work process) 
                           (:component/state process) 
                           (views-for state process)
                           %))
      (swap! items assoc (:component/item-type process)
             (atom (@(:component/work process) 
                    (:component/state process) 
                    (views-for state process)
                    nil)))))
  (update-views! state))

(defn send!
  [{:keys [state/items state/processes state/renders]} [message-type & args]]
  (let [applicable-processes (filter #(get @(:component/messages %) message-type) 
                                     @processes)
        applicable-renders (filter #(get @(:component/messages %) message-type)
                                   @renders)]
    (doseq [render applicable-renders]
      (apply (get @(:component/messages render) 
                  message-type)
             (:component/state render)
             args))
    (doseq [process applicable-processes]
      (let [message-handler (get @(:component/messages process) 
                                 message-type)
            item-type (:component/item-type process)]
        (do 
          (when (not (get @items item-type))
            (swap! items assoc item-type (atom nil)))
          (apply message-handler (:component/state process) (get @items item-type) args))))))

(defn add-process!
  [state process]
  (swap! (:state/processes state) conj process))

(defn drop-process!
  [state process-name]
  (swap! (:state/processes state) 
         (partial filter #(not= process-name (:component/name %)))))

(defn add-render!
  [state render]
  (swap! (:state/renders state) conj render))

(defn drop-render!
  [state render-name]
  (swap! (:state/renders state)
         (partial filter #(not= render-name (:component/name %)))))

(defn obliterate!
  [state]
  (reset! (:state/items state) {})
  (reset! (:state/views state) {})
  (reset! (:state/processes state) [])
  (reset! (:state/renders state) []))

(defn render
  [state]
  (build-unbuilt-views! state @(:state/renders state))
  (apply merge-with into
         (map (fn [{:keys [component/return component/item-type component/work] :as renderer}]
                {return (@work
                         (some-> (:component/state renderer) deref)
                         (views-for state renderer)
                         @(get @(:state/items state) item-type))}) 
              @(:state/renders state))))