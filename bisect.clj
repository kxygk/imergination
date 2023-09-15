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
    :angle      (+ PI
                   (atan2 y ;;/docs/api/java/lang/Math.html#atan2-double-double-
                          x))})) ;; Yes the order is weird `y` then `x`
#_
(to-polar 1 -1)
;; => {:radius 1.4142135623730951, :angle -0.7853981633974483}
#_
(-> [1
     1]
    to-polar)
;; => {:radius-sqrd 2.0, :angle 0.7853981633974483}

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
               :angle (/ PI
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
    to-polar)
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
  angle-dichotomies
  "Takes a list of POINTS
  Which is a vector of 2D coordinates
  [[x0 y0
  x1 y1
  ..
  xn yn]]
  And returns a vector of vectors that split the group"
  [points]
  (->> points
       (map to-polar)
       (map to-halfplane)
       (sort-by :angle)
       (map :angle)
       (partition 2 1)
       (map #(/ (apply +
                       %)
                2.0))))
#_
(-> [[1,  1]
     [1, -2]
     [-1, 1]
     [-1, -2]]
    angle-dichotomies)
;; => (0.9462734405957693 1.5707963267948966 2.1953192129940238)
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

#_
(let [data             (->> [[2,  1.5]
                             [1, -2]
                             [-1.2, 1]
                             [-0.5, -2.5]] #_#_#_#_
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
                            (reduce into))]
  (->> (-> (quickthing/zero-axis data
                                 {:width 500
                                  :height 500})
           (assoc :data
                  (into [(quickthing/adjustable-circles data)]
                        cat
                        [data-lines
                         dichotomy-lines]))
           thi.ng.geom.viz.core/svg-plot2d-cartesian
           quickthing/svg-wrap
           quickthing/serialize)
       (spit "out/test-dots.svg")))


(->> [[2,  1.5]]
     (map to-polar)
     (map to-halfplane)
     (map to-cartesian))
