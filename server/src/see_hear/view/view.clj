(ns see-hear.view)

(defonce view-type->constructor (atom {}))

(defmacro def-view
  [view-name args view-map]
  (let [view-type (keyword (name view-name))]
  `(swap! view-type->constructor
          assoc ~view-type
          (fn ~args (assoc ~view-map :view/type ~view-type)))))

(defn create
  [view-type]
  ((get @view-type->constructor view-type)))