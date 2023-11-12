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

(defn-
  above-angle?
  [point
   angle]
  (let [line-angle (mod angle
                        (* 1.0
                           PI))
        point-angle (-> point
                        to-polar
                        :angle)]
    (and (> point-angle
            (mod line-angle
                 (* 2.0
                    PI)))
         (< point-angle
            (mod (+ line-angle
                    PI)
                 (* 2.0
                    PI))))))
#_
(above-angle? [1.2, 1.6]
              3.2)

(defn-
  points-along-angle
  [points
   angle]
  (->> points
       (mapv (fn update-point
               [point]
                (update point
                        2     ;; typically will be `nil`
                        #(merge %
                                {:above? (above-angle? point
                                                       angle)}))))))
#_
(-> [[2.2,  1.5]
     [1.1, -2.4]
     [-1.2, 1.6]
     [-0.5, -2.7]]
    (points-along-angle 0.0))

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
     angle-dichotomies)
;; => (3.141592653589793
;;     0.9462734405957693
;;     1.5707963267948966
;;     2.1953192129940238)

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
  "Calculate the centroid of a bunch of points
  In this case I'll be wanting the centroid of the top and bottom halves"
  [points]
  (->> points
       (reduce (fn [[total-x, total-y]
                    [point-x, point-y]]
                 [(+ total-x
                     point-x)
                  (+ total-y
                     point-y)]))
       (take 2)
       (mapv #(/ %
                 (count points)))))
#_
(->> [[2.2,  1.5]
      [1.1, -2.4]
      [-1.2, 1.6]
      [-0.5, -2.7]]
     centroid)


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
   :points-a   vector-of-points-in-one-half
   :points-b   vector-of-points-in-other-half
   :centroid-a centroid-of-one-half
   :centroid-b centroid-of-the-other-half}"
  [points]
  (let [angle (min-var-angle points)
        classified (points-along-angle points
                                       angle)
        [points-a ;; we're recomputing this.. :/
         points-b] (vals (group-by #(-> %
                                          (get 2)
                                          :above?)
                                   classified))]
    (let [centroid-a (centroid points-a)
          centroid-b (centroid points-b)]
      {:angle angle
       :points classified
       :centroid-a centroid-a
       :centroid-b centroid-b})))
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
             quickthing/serialize)
         (spit "out/test-dots.svg"))))
#_
(let [simple-data         [[2.2,  1.5]
                           [1.1, -0.4]
                           [-1.2, 1.6]
                           [-0.7, -2.7]]
      big-data            [[-2.45, 1.55] ;;first group
                           [-2.33, 1.25]
                           [-2.05, 1.63]
                           [-2.88, 1.32]
                           [-2.22, -1.52] ;;second group
                           [-2.14, -1.24]
                           [-2.06, -1.66]
                           [-2.79, -1.39]]
      trivial-data        [[1.45, 1.55]
                           [-2.22, -1.52]]
      data                big-data
      dichotomy-angles    (->> data
                               angle-dichotomies)
      data-lines          (->> data
                               (map #(quickthing/line-through-point data
                                                                    %))
                               (reduce into))
      dichotomy-lines     (->> dichotomy-angles
                               (map angle-to-unitvector)
                               (map #(quickthing/line-through-point data
                                                                    %
                                                                    {:attribs {:stroke-dasharray (str 10.0
                                                                                                      " "
                                                                                                      10.0)}}))
                               (reduce into))
      best-angle          (->> data
                               best-dichotomy-angle)
      best-dichotomy-line (quickthing/line-through-point data
                                                         (->> data
                                                              best-dichotomy-angle
                                                              angle-to-unitvector)
                                                         {:attribs {:stroke           "blue"
                                                                    :stroke-dasharray (str 2.0
                                                                                           " "
                                                                                           2.0)}})
      dividing-line       (quickthing/line-through-point data
                                                         [(cos best-angle)
                                                          (sin best-angle)]
                                                         {:attribs {:stroke "purple"}})
      [top-points
       bot-points]        (-> data
                              (points-along-angle best-angle))
      top-centroid        (->> top-points
                               centroid)
      bot-centroid        (->> bot-points
                               centroid)]
  (->> (-> (quickthing/zero-axis data
                                 {:width  500
                                  :height 500})
           (assoc :data
                  (into [(quickthing/adjustable-circles data)
                         (quickthing/adjustable-circles (->> top-points
                                                             (mapv #(conj (conj %
                                                                                7.0)
                                                                          {:fill "red"}))))
                         (quickthing/adjustable-circles  (->> bot-points
                                                              (mapv #(conj (conj %
                                                                                 7.0)
                                                                           {:fill "green"}))))]
                        cat
                        [#_data-lines
                         (quickthing/vector2d top-centroid
                                              {:attribs {:stroke "red"}})
                         (quickthing/vector2d bot-centroid
                                              {:attribs {:stroke "green"}})
                         dividing-line
                         dichotomy-lines
                         best-dichotomy-line]))
           thi.ng.geom.viz.core/svg-plot2d-cartesian
           quickthing/svg-wrap
           quickthing/serialize)
       (spit "out/test-dots.svg")))
