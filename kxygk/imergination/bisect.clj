(ns
    kxygk.imergination.bisect
  "Algorithms for bisecting a set of points"
  (:use [clojure.math]))

(def TWOPI
  (* 2.0
     PI))

(defn
  angle-from-bottom-to-unitvector
  "Given an angle in radians
  Return a 2D vector of length 1"
  [angle-from-bottom]
  [(sin angle-from-bottom)
   (- (cos angle-from-bottom))])
#_
(angle-from-bottom-to-unitvector (* 3.0
                                    (/ PI
                                       4.0)))


(defn
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
    ;;/docs/api/java/lang/Math.html#atan2-double-double-
    :angle-from-down  (mod (+ (atan2 y 
                           x)
                    (/ PI
                       2.0))
                 (* 2.0
                    PI))})) ;; Yes the order is weird `y` then `x`
#_
(to-polar [1 1])
;; => {:radius 1.4142135623730951, :angle-from-down 2.356194490192345}

(defn
  add-angle-from-down
  "Take 2D points and add their `:angle-from-down`"
  [point]
  (update point
          2
          #(assoc %
                  :angle-from-down
                  (-> point
                      to-polar
                      :angle-from-down))))
#_
(add-angle-from-down [1 1])
;; => [1 1 {:angle-from-down 2.356194490192345}]

(defn
  add-above?
  "Check if points are above of below an `angle-from-down`.
  `bisection-angle` is relative to downwards
  Adds a `:above?` key"
  [bisection-angle
   points]
  (->> points
       (mapv (fn [point]
             (update point
                     2
                     #(assoc %
                             :above?
                             (> (:angle-from-down (get point
                                                       2))
                                bisection-angle)))))))
(->> [[1 1]
      [1 -1]]
     (mapv add-angle-from-down) 
     (add-above? (/ PI
                    2.0)))
;; => [[1 1 {:angle-from-down 2.356194490192345, :above? true}]
;;     [1 -1 {:angle-from-down 0.7853981633974483, :above? false}]]



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
  (->> points
       (map last)
       (sort-by :angle-from-down)
       (map :angle-from-down)
       (partition 2
                  1)
       (map #(/ (apply +
                       %)
                2.0))))
#_
(->> [[1,  1]
     [1, -2]
     [1, 1]
     [1, 2]]
     (mapv add-angle-from-down)
     angle-dichotomies)

(vals (group-by #(-> %
                     (get 2)
                     :above?)
                (->> [[1,  1]
                      [1, -2]
                      [1, 1]
                      [1, 2]]
                     (mapv add-angle-from-down)
                     (add-above? (/ PI
                                    2.0) ))))
;; => ([[1 1 {:angle-from-down 2.356194490192345, :above? true}]
;;      [1 1 {:angle-from-down 2.356194490192345, :above? true}]
;;      [1 2 {:angle-from-down 2.677945044588987, :above? true}]]
;;     [[1 -2 {:angle-from-down 0.46364760900080615, :above? false}]])

(defn
  inter-class-variance
  "POints are of the form
  ;; => [24.164284948952567
  ;;     -44.09616040647046
  ;;     {:index 0,
  ;;      :cycle-frac 0,
  ;;      :angle 5.213688218814581,
  ;;      :length 50.283039185070635,
  ;;      :err-x 12.984442896066495,
  ;;      :err-y 33.19259196387512,
  ;;      :err-angle 0.2803344675436626}]
  "
  [unclassified-points
   dichotomy-angle]
  (let [points-all (->> unclassified-points
                        (add-above? dichotomy-angle))]
        (let [{points-top true
               points-bot false} (->> points-all
                                      (add-above? dichotomy-angle)
                                      (group-by #(-> %
                                                     (get 2)
                                                     :above?)))]
          (let [num-top (count points-top)
                num-bot (count points-bot)]
            (let [num-tot (+ num-top
                             num-bot)]
              (let [mean-tot (/ (->> points-all
                                     (mapv #(get %
                                                 2))
                                     (mapv :angle-from-down)
                                     (apply +))
                                num-tot)
                    mean-top (/ (->> points-top
                                     (mapv #(get %
                                                 2))
                                     (mapv :angle-from-down)
                                     (apply +))
                                num-top)
                    mean-bot (/ (->> points-bot
                                     (mapv #(get %
                                                 2))
                                     (mapv :angle-from-down)
                                     (apply +))
                                num-bot)
                    frac-top (/ num-top
                                num-tot)
                    frac-bot (/ num-bot
                                num-tot)]
                {:angle-from-bottom dichotomy-angle #_ (mod (- dichotomy-angle
                                                                   (/ PI
                                                                      2.0))
                                                                TWOPI)
                 :points            points-all
                 :centroid-a        (-> mean-top
                                 angle-from-bottom-to-unitvector)
                 :centroid-b        (-> mean-bot
                                 angle-from-bottom-to-unitvector)
                 :interclass-var    (+ (* frac-top
                                       (pow (- mean-top
                                               mean-tot)
                                            2.0))
                                    (* frac-bot
                                       (pow (- mean-bot
                                               mean-tot)
                                            2.0)))} ))))))
  #_
(inter-class-variance (->> [[1,  1]
                            [1, -2]
                            [1, 1]
                            [1, 2]]
                           (mapv add-angle-from-down))
                      (/ PI
                         2.0))
;; => {:angle-from-bottom 1.5707963267948966,
;;     :points
;;     [[1 1 {:angle-from-down 2.356194490192345, :above? true}]
;;      [1 1 {:angle-from-down 2.356194490192345, :above? true}]
;;      [1 2 {:angle-from-down 2.677945044588987, :above? true}]
;;      [1 -2 {:angle-from-down 0.46364760900080615, :above? false}]],
;;     :centroid-a [0.6273518651509457 0.7787359226924295],
;;     :centroid-b [0.447213595499958 -0.8944271909999159],
;;     :interclass-var 0.7498478072144794}

(defn
  otsu
  "return the angle that best splits the points
  in a way that minimizes the variance"
    [points]
    (let [dichotomy-angles (->> points
                                angle-dichotomies)]
      (->> dichotomy-angles
           (map (fn [dichotomy]
                  (inter-class-variance points
                                        dichotomy)))
           (sort-by :interclass-var)
           last)))








(defn
  angular-weighted-mean
  [points]
  (let [[weighted-sum
         sum-of-weights] (->> points
                              (reduce (fn [[partial-weighted-sum
                                            partial-sum-of-weights]
                                           [_
                                            _
                                            {:keys [angle-from-down
                                                    err-angle]}]]
                                        (let [weight (/ 1.0
                                                        (pow err-angle
                                                             2.0))]
                                          [(+ partial-weighted-sum
                                              (* weight
                                                 angle-from-down))
                                           (+ partial-sum-of-weights
                                              weight)]))
                                      [0
                                       0]))]
    (/ weighted-sum
       sum-of-weights)))
#_
(->> @state/*selections
     state/sv-proj
     (reduce (fn [[partial-weighted-sum
                   partial-sum-of-weights]
                  [_
                   _
                   {:keys [angle
                           err-angle]}]]
               [(+ partial-weighted-sum
                   (* err-angle
                      angle))
                (+ partial-sum-of-weights
                   err-angle)])
             [0
              0]))
#_
(->> @state/*selections
     state/sv-proj
     angular-weighted-mean)




(defn
  inter-class-variance-weighted
  "POints are of the form
  ;; => [24.164284948952567
  ;;     -44.09616040647046
  ;;     {:index 0,
  ;;      :cycle-frac 0,
  ;;      :angle 5.213688218814581,
  ;;      :length 50.283039185070635,
  ;;      :err-x 12.984442896066495,
  ;;      :err-y 33.19259196387512,
  ;;      :err-angle 0.2803344675436626}]
  "
  [unclassified-points
   dichotomy-angle]
  (let [points-all (->> unclassified-points
                        (add-above? dichotomy-angle))]
        (let [{points-top true
               points-bot false} (->> points-all
                                      (add-above? dichotomy-angle)
                                      (group-by #(-> %
                                                     (get 2)
                                                     :above?)))]
    (let [num-top (count points-top)
          num-bot (count points-bot)]
      (let [num-tot (+ num-top
                       num-bot)]
        (let [mean-tot (->> points-all
                            angular-weighted-mean)
              #_(/ (->> points-all
                               (mapv #(get %
                                           2))
                               (mapv :angle-from-down)
                               (apply +))
                          num-tot)
              mean-top (->> points-top
                            angular-weighted-mean)
              mean-bot(->> points-bot
                            angular-weighted-mean)
              frac-top (/ num-top
                          num-tot)
              frac-bot (/ num-bot
                          num-tot)]
          {:angle-from-bottom      dichotomy-angle #_(mod (- dichotomy-angle
                               (/ PI
                                  2.0))
                            TWOPI)
           :points     points-all
           :centroid-a (-> mean-top
                           angle-from-bottom-to-unitvector)
           :centroid-b (-> mean-bot
                           angle-from-bottom-to-unitvector)
           :interclass-var (+ (* frac-top
                                 (pow (- mean-top
                                         mean-tot)
                                      2.0))
                              (* frac-bot
                                 (pow (- mean-bot
                                         mean-tot)
                                      2.0)))} ))))))

(defn
  otsu-weighted
  "return the angle that best splits the points
  in a way that minimizes the variance"
    [points]
    (let [dichotomy-angles (->> points
                                angle-dichotomies)]
      (->> dichotomy-angles
           (map (fn [dichotomy]
                  (inter-class-variance-weighted points
                                                 dichotomy)))
           (sort-by :interclass-var)
           last)))
