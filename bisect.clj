(ns
    bisect
  "Algorithms for bisecting a set of points"
  (:use [clojure.math]))


(defn-
  to-polar
  "Take an [X Y] pair and return its polar coordinates"
  ([x
    y]
   (to-polar [x y]))
  ([[x
     y]]
   {:radius (sqrt (+ (pow x
                          2)
                     (pow y
                          2)))
    :angle  (mod (atan2 y ;;/docs/api/java/lang/Math.html#atan2-double-double-
                        x)
                 (* 2.0
                    PI))})) ;; Yes the order is weird `y` then `x`
#_
(to-polar 1 -1)
#_
(to-polar [2.2,  1.5])
#_
(-> [1
     1]
    to-polar)
;; => {:radius-sqrd 2.0, :angle 0.7853981633974483}

(defn-
  rad2deg
  [angle-rad]
  (* (/ angle-rad
        (* 2.0
           PI))
     360))
#_
(rad2deg PI)
;; => 180.0

(defn-
  to-cartesian
  "Take an [X Y] pair and return its polar coordinates"
  [{:keys [radius
           angle]
    :as   polar-coord}]
  [(* (cos angle)
      radius)
   (* (sin angle)
      radius)])
#_
(to-cartesian (to-polar 1 2))
;; => [1.0 1.0000000000000002]
#_
(to-cartesian {:radius 1.0
               :angle  (/ PI
                          4.0)})
#_
(-> [1
     1]
    to-polar
    to-cartesian)
;; => [1.0 1.0000000000000002]

(defn-
  to-halfplane
  "Remap the points to the 0-180 range
  But now the `radius` can be negative"
  [{:keys [radius
           angle]
    :as   polar-coord}]
  (if (< (mod angle
              (* 2.0
                 PI))
         PI)
    polar-coord
    {:radius (- radius)
     :angle  (- angle
                PI)}))
#_
(-> [-1
     -1]
    to-polar
    to-halfplane)
;; => {:radius-sqrd -2.0, :angle 0.7853981633974483}


(defn-
  abs-polar
  "Do `abs` on the radius"
  [{:keys [radius
           angle]
    :as   polar-coord}]
  (update polar-coord
          :radius
          abs))
#_
(->> [1 -1]
     to-polar
     to-halfplane
     abs-polar
     to-cartesian)

(defn-
  angular-distance-to-x-axis
  [point]
  (let [angle-mod (-> point
                      to-polar
                      :angle
                      (mod PI))]
    angle-mod
    #_
    (if (> angle-mod
           (/ PI
              2.0))
      (- PI
         angle-mod)
      angle-mod)))
#_
(->> [[2.2,  1.5]
      [1.1, -2.4]
      [-1.2, 1.6]
      [-0.5, -2.7]]
     (sort #(< (angular-distance-to-x-axis %1)
               (angular-distance-to-x-axis %2))))
;; => (0.598418893478537
;;     2.0005586058915847
;;     2.214297435588181
;;     1.3876855095324123)

(defn
  to-angle
  "Simple trig"
  [point]
  (-> point
      to-polar
      :angle
      (mod (* 2.0
              PI))))
#_
(->> [[2.2,  1.5]
      [1.1, -2.4]
      [-1.2, 1.6]
      [-0.5, -2.7]]
     (mapv to-angle))
;; => [0.5984188934785372
;;     5.142151259481378
;;     2.214297435588181
;;     4.529278163122205]

(defn-
  points-along-angle
  [points
   angle]
  (let [TWOPI         (* 2.0
                         PI)
        a-mod-pi (mod angle
                      PI)
        divisor-angle  (if (> a-mod-pi
                              (* 0.5
                                 PI))
                         a-mod-pi
                         (+ a-mod-pi
                            PI))]
    (->> points
         (mapv (fn update-point
                 [point]
                 (let [point-angle (-> point
                                       to-angle
                                       (mod TWOPI))
                       delta-angle (- point-angle
                                      divisor-angle)]
                     (update point
                             2     ;; typically will be `nil`
                             #(merge %
                                       ;; 0 to -PI
                                     (if (and (> point-angle
                                                 (- divisor-angle
                                                    PI))
                                              (< point-angle
                                                 divisor-angle))
                                       {:above? true
                                        :delta-angle (- divisor-angle
                                                        point-angle)}
                                       {:above? false
                                        :delta-angle (- divisor-angle
                                                        point-angle)})))))))))
#_
(-> [[2.2,  1.5]
     [1.1, -2.4]
     [-1.2, 1.6]
     [-0.5, -2.7]
     [-0.5, 2.7]]
    (points-along-angle  (* 0.75
                            PI) ))
;; => [[2.2 1.5 {:above? true, :delta-angle -1.7577755967138078}]
;;     [1.1 -2.4 {:above? false, :delta-angle 2.785956769289033}]
;;     [-1.2 1.6 {:above? true, :delta-angle -0.14189705460416402}]
;;     [-0.5 -2.7 {:above? false, :delta-angle 2.1730836729298604}]
;;     [-0.5 2.7 {:above? true, :delta-angle -0.602287346134964}]]

(defn-
  angle-dichotomies
  "Takes a list of POINTS
  Which is a vector of 2D coordinates
  [[x0 y0
  x1 y1
  ..
  xn yn]]
  And returns a vector of angles that split the group"
  [points]
  (let [main-dichotomies (->> points
                              (map to-polar)
                              (map to-halfplane)
                              (sort-by :angle)
                              (map :angle)
                              (partition 2
                                         1)
                              (map #(/ (apply +
                                              %)
                                       2.0)))
        angles-to-x-axis (->> points
                              (map angular-distance-to-x-axis)
                              sort
                              #_#_#_
                              (sort #(< (angular-distance-to-x-axis %1)
                                        (angular-distance-to-x-axis %2)))
                              (map to-polar)
                              (map :angle))
        top-line         (first angles-to-x-axis)
        bottom-line      (last angles-to-x-axis)
        extra-dichotomy (/ (+ top-line ;; angle btwn first/last line
                              PI
                              bottom-line)
                           2.0)
        all-dichotomies (into []
                              (conj main-dichotomies
                                    ;;#_
                                    extra-dichotomy))]
    (->> all-dichotomies
         (filterv (fn degenerate-dichotomy? ;; sometimes an extra dichotomy is generated
                    [dichotomy-angle]
                    (let [grouped (->> (points-along-angle points
                                                                dichotomy-angle)
                                            (group-by (fn above?
                                                        [point]
                                                        (-> point
                                                            (get 2)
                                                            :above?)))
                                            vals)]
                      (if (== 1
                              (count grouped))
                        false
                        true)))))))
#_
(-> [[-1,  +1]
     [-1, -2]]
    angle-dichotomies)
#_
(-> [[1,  1]
     [1, -2]
     [-1, 1]
     [-1, -2]]
    angle-dichotomies)
;; => (3.141592653589793
;;     0.9462734405957693
;;     1.5707963267948966
;;     2.1953192129940238)
#_
(->> [[1,  1]
      [1, -2]
      [-1, 1]
      [-1, -2]]
     (map to-polar)
     (map to-halfplane)
     (map abs-polar)
     (map to-cartesian)
;; => ([1.0000000000000002 1.0]
;;     [-1.0 2.0]
;;     [-1.0 1.0000000000000002]
;;     [1.0000000000000002 2.0])
     angle-dichotomies)
;; => [3.141592653589793 ;; THIS ONE IS MISSING NOW.. IS IT A PROBLEM??
;;     0.9462734405957693
;;     1.5707963267948966
;;     2.1953192129940238]

(defn
  angle-to-unitvector
  "Given an angle in radians
  Return a 2D vector of length 1"
  [angle]
  [(cos angle)
   (sin angle)])
#_
(-> [[1,  1]
     [1, -2]
     [-1, 1]
     [-1, -2.5]]
    angle-dichotomies
    first                   ; => 0.9462734405957693
    angle-to-unitvector)    ; => [0.584710284663765 0.8112421851755608]

(defn-
  centroid
  "Calculate the *angular* centroid of a bunch of points
  In this case I'll be wanting the centroid of the top and bottom halves
  NOTE: This is not the cartesian centroid..
  the resulting vectors have been normalized!
  So they function directly as basis vectors"
  [points]
  (let [[a b] (->> points
                   (reduce (fn [[total-x, total-y]
                                [point-x, point-y]]
                             [(+ total-x
                                 point-x)
                              (+ total-y
                                 point-y)]))
                   (take 2)
                   (mapv #(/ %
                             (count points))))]
    (let [length (pow (+ (pow a
                              2)
                         (pow b
                              2))
                      0.5)]
      [(/ a
          length)
       (/ b
          length)])))
#_
(->> [[2.2,  1.5, {:above true}]
      [1.1, -2.4]
      [-1.2, 1.6]
      [-0.5, -2.7]]
     centroid)
;; => [0.6246950475544243 -0.7808688094430302]
#_
(->> [[-0.10779173540213875 -0.15475804884523486]]
     centroid)
;; => [-0.5715430565610071 -0.8205720775757005]
#_
(->> [[-0.10779173540213875 -0.15475804884523486 {:above? false}]]
     centroid)
;; => [-0.5715430565610071 -0.8205720775757005]

(defn
  vec-length
  [[x y]]
  (sqrt (+ (pow x
                2.0)
           (pow y
                2.0))))
#_
(vec-length [2 2])

(defn-
  normalize-vector
  [point]
  (let  [length (vec-length point)]
    (mapv #(/ %
              length)
          point)))
#_
(normalize-vector [2 5])

(defn-
  distance-to-axis
  "Get the distance from a point to a given axis"
  [point
   axis]
  (let [axis-norm (normalize-vector axis)
        inner-p (reduce +
                        (map *
                             point ;; MAYBE ALSO NORMALIZE???
                             axis-norm))
        projection (->> axis-norm
                        (mapv #(* %
                                  inner-p)))
        orthogonal-comp (map -
                             point
                             projection)]
    (vec-length orthogonal-comp)))
#_
(distance-to-axis [2.2,  1.5]
                  [1.1, -2.4])

(defn-
  variance
  [points
   axis]
  (/ (->> points
          (map (fn [point]
                 (distance-to-axis point
                                   axis)))
          (reduce +))
     (count points)))
#_
(-> [[2.2,  1.5]
     [1.1, -2.4]
     [-1.2, 1.6]
     [-0.5, -2.7]]
    (variance [1 0]))

(defn-
  two-plane-variance
  [points
   dichotomy-angle]
  (let [[top-points
         bot-points] (vals (group-by #(-> %
                                          (get 2)
                                          :above?)
                                     (points-along-angle points
                                                         dichotomy-angle)))]
    (let [num-top      (count top-points)
          num-bot      (count bot-points)
          top-centroid (centroid top-points)
          bot-centroid (centroid bot-points)
          top-variance (variance top-points
                                 top-centroid)
          bot-variance (variance bot-points
                                 bot-centroid)]
      (/ (+ (* top-variance
               num-top)
            (* bot-variance
               num-bot))
         (+ num-top
            num-bot)))))
#_
(let [data  [[2.2,  1.5]
            [1.1, -0.4]
            [-1.2, 1.6]
            [-0.7, -2.7]]
      angle 3.3]
  (two-plane-variance data
                      angle))

(defn
  min-var-angle
  "return the angle that best splits the points
  in a way that minimizes the variance"
  [points]
  (let [dichotomy-angles (->> points
                              angle-dichotomies)]
    (->> dichotomy-angles
         (map #(two-plane-variance points
                                   %))
         (map-indexed vector)
         (apply min-key
                second)
         first
         (get dichotomy-angles))))
#_
(min-var-angle [[2.2,  1.5]
                 [1.1, -0.4]
                 [-1.2, 1.6]
                 [-0.7, -2.7]])

(defn
  min-var
  "return a map with
  {:angle      that-splits-the-plane-to-minimize-variance
   :points   vector-of-point with a `:classified` key
   :centroid-a centroid-of-one-half
   :centroid-b centroid-of-the-other-half}"
  [points]
  (let [angle (min-var-angle points)
        classified (points-along-angle points
                                       angle)
        grouped (group-by #(-> %
                                     (get 2)
                                     :above?)
                                classified)
        points-above (get grouped ;;above
                          true)
        points-below (get grouped ;;below
                          false)]
    (let [centroid-above (centroid points-above)
          centroid-below (centroid points-below)]
      {:angle angle
       :points classified
       :centroid-a centroid-above
       :centroid-b centroid-below})))
#_
(let [data                 [[-2.45, 1.55] ;;first group
                            [-2.33, 1.25]
                            [-2.05, 1.63]
                            [-2.88, 1.32]
                            [-2.22, -1.52] ;;second geroup
                            [-2.14, -1.24]
                            [-2.06, -1.66]
                            [-2.79, -1.39]]
      {:keys [angle
              points-a
              points-b
              centroid-a
              centroid-b]} (min-var data)]
  (let [data-lines     (->> data
                            (map #(quickthing/line-through-point data
                                                                 %))
                            (reduce into))
        angle-line     (quickthing/line-through-point data
                                                      (->> angle
                                                           angle-to-unitvector)
                                                      {:attribs {:stroke           "blue"
                                                                 :stroke-dasharray (str 2.0
                                                                                        " "
                                                                                        2.0)}})
        red-points     (quickthing/adjustable-circles (->> points-a
                                                           (mapv #(conj (conj %
                                                                              7.0)
                                                                        {:fill "red"}))))
        green-points   (quickthing/adjustable-circles (->> points-b
                                                           (mapv #(conj (conj %
                                                                              7.0)
                                                                        {:fill "green"}))))
        red-centroid   (quickthing/vector2d centroid-a
                                            {:attribs {:stroke "red"}})
        green-centroid (quickthing/vector2d centroid-b
                                            {:attribs {:stroke "green"}})]
    (->> (-> (quickthing/zero-axis data
                                   {:width  500
                                    :height 500})
             (assoc :data
                    (into []
                          cat
                          [red-points
                           green-points
                           red-centroid
                           green-centroid
                           angle-line]))
             thi.ng.geom.viz.core/svg-plot2d-cartesian
             quickthing/svg-wrap
             quickthing/svg2xml)
         (spit "out/test-dots.svg"))
    (min-var data)))
;; => {:angle 3.157817958919284,
;;     :points
;;     [[-2.45 1.55 {:above? true}]
;;      [-2.33 1.25 {:above? true}]
;;      [-2.05 1.63 {:above? true}]
;;      [-2.88 1.32 {:above? true}]
;;      [-2.22 -1.52 {:above? false}]
;;      [-2.14 -1.24 {:above? false}]
;;      [-2.06 -1.66 {:above? false}]
;;      [-2.79 -1.39 {:above? false}]],
;;     :centroid-a [-0.8604498292678083 0.509535171811524],
;;     :centroid-b [-0.8457724626884987 -0.5335437576786294]}




(let [angle 2.8157669920508726
      data  [[-0.024578662253719318, 0.05558278260624202]
             [-0.014193761143782346, 0.005703712447101661]
             [-0.16131258565448184,  0.2633602438965861]
             [-0.031774789528481416, -0.005571887118227592]
             [-0.07212716828092831, -0.004997627405708711]]]
      (min-var data))
;; => {:angle 2.985133363250391,
;;     :points
;;     [[-0.024578662253719318 0.05558278260624202 {:above? false}]
;;      [-0.014193761143782346 0.005703712447101661 {:above? false}]
;;      [-0.16131258565448184 0.2633602438965861 {:above? false}]
;;      [-0.031774789528481416 -0.005571887118227592 {:above? true}]
;;      [-0.07212716828092831 -0.004997627405708711 {:above? true}]],
;;     :centroid-a [-0.9948657395406916 -0.10120355867336382],
;;     :centroid-b [-0.5246724638803938 0.8513041792718258]}
