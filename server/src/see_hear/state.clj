(ns see-hear.state
  (:require [see-hear.view :as view]
            [see-hear.util :as util]))

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
      (let [view-agent (view/create unbuilt-view-type)
            items (get @items (:component/item-type view-agent))]
        (swap! views assoc 
               unbuilt-view-type
               {:view view-agent
                :value (if items 
                         ((:view/view view-agent) (:component/state view-agent) @items)
                         nil)})))))

(defn update-views!
  [state]
  (doseq [[view-type {:keys [view]}] @(:state/views state)]
    (swap! (:state/views state) 
           assoc view-type 
           {:view view 
            :value ((:view/view view) 
                    (:component/state view) 
                    @(get @(:state/items state) (:component/item-type view)))})))

(defn step!
  [{:keys [state/items state/processes] :as state}]
  (build-unbuilt-views! state @processes)
  (doseq [process @processes]
    (if-let [items-state (get @items (:component/item-type process))]
      (swap! items-state #((:process/step process) 
                           (:component/state process) 
                           (views-for state process)
                           %))
      (swap! items assoc (:component/item-type process)
             (atom ((:process/step process) 
                    (:component/state process) 
                    (views-for state process)
                    nil)))))
  (update-views! state))

(defn send!
  [{:keys [state/items state/processes state/renders]} [message-type & args]]
  (let [applicable-processes (filter #(get @(:process/messages %) message-type) 
                                     @processes)
        applicable-renders (filter #(get @(:render/messages %) message-type)
                                   @renders)]
    (doseq [render applicable-renders]
      (apply (get @(:render/messages render) 
                  message-type)
             (:component/state render)
             args))
    (doseq [process applicable-processes]
      (let [message-handler (get @(:process/messages process) 
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
  (reset! (:state/views state) {})
  (reset! (:state/processes state) [])
  (reset! (:state/renders state) []))

(defn render
  [state]
  (build-unbuilt-views! state @(:state/renders state))
  (apply merge-with into
         (map (fn [{:keys [render/return-type component/item-type render/render] :as renderer}]
                {return-type (render 
                              (some-> (:component/state renderer) deref) 
                              (views-for state renderer)
                              @(get @(:state/items state) item-type))}) 
              @(:state/renders state))))