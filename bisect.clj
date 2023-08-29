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
   {:radius-sqrd (+ (pow x
                         2)
                    (pow y
                         2))
    :angle       (atan2 y ;;/docs/api/java/lang/Math.html#atan2-double-double-
                        x)})) ;; Yes the order is weird `y` then `x`
#_
(to-polar 1 1)
#_
(-> [1
     1]
    to-polar)
;; => {:radius-sqrd 2.0, :angle 0.7853981633974483}

(defn
  to-halfplane
  "Remap the points to the 0-180 range
  But now the `radius-sqrd` can be negative"
  [{:keys [radius-sqrd
           angle]
    :as   polar-coord}]
  (let [PI-over-2 (/ PI
                     2.0)]
    (if (pos? angle)
      polar-coord
      {:radius-sqrd (- radius-sqrd)
       :angle       (- angle)})))
#_
(-> [1
     -1]
    to-polar
    to-halfplane)
;; => {:radius-sqrd -2.0, :angle 0.7853981633974483}

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
