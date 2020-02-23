(ns see-hear.shape
  [:refer clojure.set :as set])

(defn spy [x] (clojure.pprint/pprint x) x)

(defn cpoint
  [x y]
  {:point/type :cartesian :point/x x :point/y y})

(defn ppoint
  [theta mag]
  {:point/type :polar :point/theta theta :point/mag mag})

(defmulti point-equal? (fn [p1 p2 & [epsilon]] (:point/type p1)))

(defmulti intersection-point (fn [line x-or-theta & [y-or-magnitude]] (:point/type (:line/from-point line))))

(defmulti split-at-intersection (fn [line1 line2] (:point/type (:line/from-point line1))))

(defn abs
  [x]
  (max x (* -1 x)))

(defmethod point-equal? :cartesian
  [p1 p2 & [epsilon]]
  (let [e (or epsilon 0.001)]
    (and (< (abs (- (:point/x p1) (:point/x p2))) e)
         (< (abs (- (:point/y p1) (:point/y p2))) e))))

(defn do-points
  [f {:keys [line/from-point line/to-point] :as cartesian-line}]
  (let [{fx :point/x fy :point/y} from-point
        {tx :point/x ty :point/y} to-point]
    (f fx fy tx ty)))

(defn cartesian-coefficients
  [{:keys [line/from-point line/to-point]}]
  (let [{fx :point/x fy :point/y} from-point
        {tx :point/x ty :point/y} to-point
        m (/ (- ty fy) (- tx fx))
        b (- fy (* m fx))]
    [m b]))

(defn cartesian-equation 
  [line] 
  (let [[m b] (cartesian-coefficients line)]
    (fn [x] (+ (* m x) b))))

;; given a line segment, what is the (x, y) that intersects with it
;; at a specified x? Y can be optionally specified if the line segment
;; makes a vertical line
(defmethod intersection-point :cartesian
  [{:keys [line/from-point line/to-point] :as line} x & [y]]
  (let [{fx :point/x fy :point/y} from-point
        {tx :point/x ty :point/y} to-point ]
    (if (= fx tx)
      (if (= x fx) (cpoint x y) nil)
      (cpoint x ((cartesian-equation line) x)))))

(defn within?
  [[a1 a2] b]
  (if (< a1 a2) (<= a1 b a2) (<= a2 b a1)))

(defn cline
  [from-point to-point]
  {:line/from-point (if (map? from-point) from-point (apply cpoint from-point)) 
   :line/to-point (if (map? to-point) to-point (apply cpoint to-point))})

(defn left-to-right
  [line]
  (do-points (fn [fx fy tx ty]
               (cond (= fx tx)
                     (if (< fy ty)
                       (cline (cpoint fx fy) (cpoint tx ty))
                       (cline (cpoint tx ty) (cpoint fx fy)))

                     (< fx tx)
                     (cline (cpoint fx fy) (cpoint tx ty))

                     :else
                     (cline (cpoint tx ty) (cpoint fx fy))))
             line))

(defn split-line
  [{:keys [line/from-point line/to-point] :as line} point]
  (if (or (point-equal? from-point point) (point-equal? to-point point))
    [line]
    [(cline from-point point) (cline to-point point)]))

(defn vertical?
  [{:keys [line/from-point line/to-point]}]
  (= (:point/x from-point) (:point/x to-point)))

(defn lies-on?
  [line {:keys [point/x point/y]}]
  (do-points (fn [fx fy tx ty]
               (and
                (within? [fx tx] x)
                (within? [fy ty] y)))
             line))

(defn lies-on-both?
  [line-1 line-2 point]
  (and (lies-on? line-1 point)
       (lies-on? line-2 point)))
        
(defn vertical-intersection
  [vertical-line proper-line]
  (let [vertical-x (:point/x (:line/from-point vertical-line))
        line-fn (cartesian-equation proper-line)
        intersect-y (line-fn vertical-x)
        intersect-point (cpoint vertical-x intersect-y) ]
    (if (lies-on-both? vertical-line proper-line intersect-point) intersect-point nil)))

(defn split-vertical
  [vertical-line proper-line]
  (let [intersect (vertical-intersection vertical-line proper-line)]
    (if intersect
      [(split-line vertical-line intersect) (split-line proper-line intersect)]
      [[vertical-line] [proper-line]])))

(defn split-proper
  [line1 line2]
  (let [;; y = ax + c
        [a c] (cartesian-coefficients line1)
        ;; y = bx + d
        [b d] (cartesian-coefficients line2)]
    (if (= a b) ;parallel
      [line1 line2]
      (let [intersect-x (/ (- d c) (- a b))
            intersect-y (+ (* a intersect-x) c)
            intersect-point (cpoint intersect-x intersect-y)]
        (if (lies-on-both? line1 line2 intersect-point)
          (split-line line1 intersect-point) 
          [line1])))))

(defmethod split-at-intersection :cartesian
  [line1' line2']
  (let [line1 (left-to-right line1')
        line2 (left-to-right line2') ]
    (cond (and (vertical? line1) (vertical? line2))
          [line1 line2]
          
          (vertical? line1)
          (first (split-vertical line1 line2))
          
          (vertical? line2)
          (second (split-vertical line2 line1))
          
          :else
          (split-proper line1 line2))))
  
;; TODO: line type to accomodate curves.
;; multimethod on lines. intersection should dispatch on line then on point

(defn all-pairs
  [f coll]
  (for [x coll, y coll :when (not= x y)]
    (f x y)))

(defn all-pairs-cat
  [f coll]
  (reduce concat (all-pairs f coll)))

;; => (#:line{:from-point #:point{:type :cartesian, :x 0, :y 10}, :to-point #:point{:type :cartesian, :x 0, :y 3N}} 
;; #:line{:from-point #:point{:type :cartesian, :x 0, :y 0}, :to-point #:point{:type :cartesian, :x 0, :y 3N}} 
;; #:line{:from-point #:point{:type :cartesian, :x 0, :y 3}, :to-point #:point{:type :cartesian, :x 10, :y 7}})

(defn line-equal?
  [line1 line2]
  (let [{fp1 :line/from-point tp1 :line/to-point} line1
        {fp2 :line/from-point tp2 :line/to-point} line2
        equal? (fn [[xp1 xp2] [yp1 yp2]] 
                 (and (point-equal? xp1 xp2) (point-equal? yp1 yp2)))]
    (or
     (equal? [fp1 fp2] [tp1 tp2])
     (equal? [fp1 tp2] [tp1 fp2]))))

(defn split-new
  [line1 line2]
  (remove (fn [x] (or (line-equal? line1 x) (line-equal? line2 x)))
          (split-at-intersection line1 line2)))

(defn split-all
  [line lines]
  (let [splits (set (mapcat (partial split-new line) lines))]
    (if (empty? splits)
      [(left-to-right line)]
      splits)))

(defn split-fixed-point
  [line-to-split against-lines']
  (let [against-lines (remove #(line-equal? line-to-split %) against-lines')]
  (loop [splitting [line-to-split]]
    (let [next-split (set (map left-to-right (mapcat #(split-all % against-lines) splitting)))]
      (if (= splitting next-split)
        next-split
        (recur next-split))))))


(defn split-segments
  [lines]
  (->> (for [line lines]
         (split-fixed-point line (remove (partial line-equal? line) lines)))
       (apply concat)))
  
(def tetra
  (let [left (cline [0 0] [0 10])
        bottom (cline [0 10] [10 10])
        right (cline [10 10] [10 0])
        top (cline [0 0] [10 0])
        top-diag (cline [8 0] [10 2])
        bottom-diag (cline [0 8] [2 10])
        big-slash (cline [0 3] [10 7])
        small-slash (cline (intersection-point big-slash 4) [7 10])]
    [left  bottom  right  top top-diag bottom-diag big-slash small-slash]))

(def square-with-cross
  [(cline [0 0] [0 5])
   (cline [0 5] [5 5])
   (cline [5 5] [5 0])
   (cline [0 2] [5 2])
   (cline [5 0] [0 0])])

(def square 
  [(cline [0 0] [0 5])
   (cline [0 5] [5 5])
   (cline [5 5] [5 0])
   (cline [5 0] [0 0])])


(count (split-segments square))
(count (split-segments square-with-cross))
(count (split-segments tetra))