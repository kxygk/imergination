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

(defn
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

(defn
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


(defn
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

(defn
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
                              (partition 2 1)
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
        bottom-line      (last angles-to-x-axis)]
    (conj main-dichotomies
          (/ (+ top-line
                PI
                bottom-line)
             2.0))))
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

(defn-
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

(defn above-angle?
  [point
   angle]
  (let [line-angle (mod angle
                        (* 1.0
                           PI))
        point-angle (-> point
                        to-polar
                        :angle)]
    ;;    point-angle #_
    (and (> point-angle
            (mod line-angle
                 (* 2.0
                    PI)))
         (< point-angle
            (mod (+ line-angle
                    PI)
                 (* 2.0
                    PI))))))
(above-angle? [-1.2,  1.6]
              0)

(defn
  points-along-angle
  [points
   angle]
  (let [{above true
         below false } (->> points
                            (group-by #(above-angle? %
                                                     angle)))]
    [above
     below]))
#_
(-> [[2.2,  1.5]
     [1.1, -2.4]
     [-1.2, 1.6]
     [-0.5, -2.7]]
    (points-along-angle 0.0))

(defn
  centroid
  "Calculate the centroid of a bunch of points
  In this case I'll be wanting the centroid of the top and bottom halves"
  [points]
  (->> points
       (reduce (fn [[total-x
                     total-y]
                    [point-x
                     point-y]]
                 [(+ total-x
                     point-x)
                  (+ total-y
                     point-y)]))
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

(defn
  normalize-vector
  [point]
  (let  [length (vec-length point)]
    (mapv #(/ %
              length)
          point)))
#_
(normalize-vector [2 5])

(defn
  distance-to-axis
  "Get the average distance from the points to the given axis"
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

(defn
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


#_
(let [angle 0.0
      data             (->> [[2.2,  1.5]
                             [1.1, -2.4]
                             [-1.2, 1.6]
                             [-0.5, -2.7]]  #_#_#_#_
                            (map to-polar)
                            (map to-halfplane)
                            (map abs-polar)
                            (map to-cartesian))
      dichotomy-points (->> data
                            angle-dichotomies
                            (map angle-to-unitvector))
      data-lines       (->> data
                            (map #(quickthing/line-through-point data
                                                                 %))
                            (reduce into))
      dichotomy-lines  (->> dichotomy-points
                            (map #(quickthing/line-through-point data
                                                                 %
                                                                 {:attribs {:stroke-dasharray (str 10.0
                                                                                                   " "
                                                                                                   10.0)}}))
                            (reduce into))
      dividing-line (quickthing/line-through-point data
                                                   [1.0
                                                    angle]
                                                   {:attribs {:stroke "purple"}})
      [top-points
       bottom-points] (-> data
                          (points-along-angle angle))]
  (println top-points
           "\n"
           bottom-points)
  (->> (-> (quickthing/zero-axis data
                                 {:width  500
                                  :height 500})
           (assoc :data
                  (into [(quickthing/adjustable-circles data)
                         (quickthing/adjustable-circles (->> top-points
                                                             (mapv #(conj (conj % 7.0)
                                                                          {:fill "red"}))))
                         (quickthing/adjustable-circles  (->> bottom-points
                                                             (mapv #(conj (conj % 7.0)
                                                                          {:fill "green"}))))]
                        cat
                        [data-lines
                         dividing-line
                         dichotomy-lines]))
           thi.ng.geom.viz.core/svg-plot2d-cartesian
           quickthing/svg-wrap
           quickthing/serialize)
       (spit "out/test-dots.svg")))
