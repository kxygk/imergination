(ns matrix
  "The matrix of all the data
  First implementation based on `neanderthal`
  Will rewrite into something that can be more easily packaged.."
  (:require clojure.math))

(defprotocol Primitives
  "An interface for matrix libraries to do what the subset of things we need"
  (data [matrix])
  (mrows [matrix])
  (ncols [matrix])
;;  (cols  [matrix])
  (row [matrix
         index])
  (col [matrix
         index])
  (diag [matrix]) ;; should run on the `:sigma` and return a vector
  (set-value! [matrix
               x
               y
               value])
  (scale-to-value   [matrix
                     factor])
  (mult-elementwise [matrix
                     other-matrix])
  (mm [matrix1
       matrix2]
    [matrix1
     matrix2
     matrix3])
  (svd [matrix]) ;; Returns {:sigma _ :u _ :vt _} each also `Matrix`
  #_
  (sv1-inverter [matrix])
  (rezero   [matrix])
  (rezero-nonzero [matrix])
  (zero-top-left [matrix]))

(defprotocol Factory
  (build-matrix [factory ;; the "type" to switch on
                 num-rows
                 num-columns
                 data-vec])
  ;; Maybe this should have a default version ..
  ;; But normally the impl should make a diagonal matrix here
  ;; (instead a dense general matrix)
  (build-inverter [factory 
                   size]))

;; A global atom to hold the current active factory
(defonce matrix-factory (atom nil))

;; Function to set the switch
(defn set-factory! [f]
  (reset! matrix-factory f))

;;;; Higher order Functions

(defn build
  "Higher-level helper.
  Make a new mastrix,
  using the global switch"
  [num-rows
   num-cols
   data-vec]
  (if-let [factory @matrix-factory]
    (do (println (str "Factory type is: "
                      (type factory)))
        (build-matrix factory
                      num-rows
                      num-cols
                      data-vec))
    (throw (Exception. (str "No matrix factory initialized."
                            "Call `set-factory!` first."
                            "Namesapces like `matrix-neanderthal` self-register")))))
#_
(build 2
                       2
                       [1, 2, 3, 4])
#_
(scale-to-value (build 2
                       2
                       [1, 2, 3, 4])
                2)
#_
(abs-sums-of-cols (scale-to-value (build 2
                                         2
                                         [1, 2, 3, 4])
                                  2))

(defn inverter
  "This is the identity matrix,
with the first 0,0 flipped to -1"
  [size]
  (if-let [factory @matrix-factory]
    (build-inverter factory
                    size)
    (throw (Exception. (str "No matrix factory initialized."
                            "Call `set-factory!` first."
                            "Namesapces like `matrix-neanderthal` self-register")))))

;; Didn't reimplement b/c unused
#_
(defn
  project-onto-2d-basis
  "Does an oblique projections of 2D data points
   on to the two vectors of the new basis
   Input:
   DATA vector of pairs [[x1,y1] [x2,y2] .. ]
   BASIS-A a 2d [x y] vector pair for the basis direction
   BASIS-B a 2d [x y] vector pair for the basis direction
   Returns:
   2xn vector
   ..
   We need to generate an inverse of the 2x2 matrix
   For projecting the data on to the basis vectors
   ...
   if basis vectors are A and B
   Then we arrange them as two cols
   [u_ax u_bx
    u_ay u_by]
   ...
   a 2x2 matrix can be manually inverted
   https://en.wikipedia.org/wiki/Invertible_matrix"
  [basis-a
   basis-b
   points]
  (let [[a b] basis-a ;; names match standard notation
        [c d] basis-b]
    (let [basis-matrix (neand/dge 2
                                  2
                                  [[a b] [c d]])
          point-matrix (neand/dge 2
                                  (count points)
                                  (flatten (->> points
                                                (mapv #(take 2
                                                             %)))))]
      (let [proj-matrix (linalg/ls basis-matrix
                                   point-matrix)]
        #_
        {:point point-matrix
         :basis basis-matrix
         :proj  proj-matrix}
        #_
        proj-matrix
        ;;#_
        (mapv (fn stitch
                [original-point
                 new-x
                 new-y]
                (-> original-point
                    (assoc 0
                           new-x)
                    (assoc 1
                           new-y)))
              points
              (into []
                    (ncore/row proj-matrix
                               0))
              (into []
                    (ncore/row proj-matrix
                               1)))))))
#_
(project-onto-2d-basis [1.0, 2.0]
                       [3.0, 4.0]
                       [[5.0, 8.0, {:blah 2}]
                        [22.0, 6.0]
                        [20.0, 6.0]])
;; => [[-2.000000000000001 3.5000000000000004 {:blah 2}]
;;     [-38.000000000000036 30.000000000000025]
;;     [-34.000000000000036 27.000000000000025]];; => [[-2.000000000000001 3.5000000000000004 {:blah 2}]
;;     [-38.000000000000036 30.000000000000025]
;;     [-34.000000000000036 27.000000000000025]]

(into [1 2 3 ] [3 4 5])

(defn
  project-onto-2-patterns
  "Does an orthogonal projections of 2D data points
   on to the two vectors of the new basis
   Input:
   DATA vector of pairs [[x1,y1] [x2,y2] .. ]
   BASIS-A a 2d [x y] vector pair for the basis direction
   BASIS-B a 2d [x y] vector pair for the basis direction
   Returns:
   2xn vector"
  [pattern-a
   pattern-b
   points]
  (let [[a b] pattern-a ;; names match standard notation
        [c d] pattern-b]
    (let [vector-above (build 1
                              2
                              [a b]) ;; doesn't work with `dge` for some reason
          vector-below (build 1
                              2
                              [c d])
          point-matrix (build 2
                              (count points)
                              (flatten (->> points
                                                (mapv #(take 2
                                                             %))))
                              #_
                              (into (->> points
                                         (mapv first))
                                     (->> points
                                          (mapv second))))]
      ;; Before
      ;;[[x y]
      ;; [x y]
      ;; [x y]
      ;; [x y]]
      ;; *
      ;; [pat_x
      ;;  pat_y}
      ;; Now
      ;; [pat_x pat_y
      ;; *
      ;; [ x x x x x x x x ]
      ;; [ y y y y y y y y ]
      ;; This is then multiplied by the coordinate of the pattern
      (mapv (fn stitch
              [original-point
               new-x
               new-y]
              (-> original-point
                  (assoc 0
                         new-x)
                  (assoc 1
                         new-y)))
            points
            (data (mm vector-above
                      point-matrix))
            (data (mm vector-below
                      point-matrix))))))
#_
(let [points [[-0.03984475266550661 0.05052713742626782]
              [-0.03984475266550661 0.05052713742626782]
              [-0.07180659921926151 -0.022952287269459186]]
      patt-a [-0.6637395088755317 0.7479638121979351]
      patt-b [-0.8414805073840199 -0.5402874750470648]]
  (project-onto-2-patterns patt-a
                           patt-b
                           points))




(defn
  singular-vector-mix
  "Take SINGULAR-VECTOR-A and SINGULAR-VECTOR-B
  And mix them according to WEIGHT-A and WEIGHT-B
  (Doesn't actually use any backend)"
  [singular-vector-a
   singular-vector-b
   weight-a
   weight-b]
  (let [mixture (mapv (fn [sv1-point
                           sv2-point]
                        (/ (+ (* sv1-point
                                 weight-a)
                              (* sv2-point
                                 weight-b))
                           2.0))
                      singular-vector-a
                      singular-vector-b)]
    mixture))

(defn
  design-matrix
  "This is the matrix used for the regression
  https://en.wikipedia.org/wiki/Design_matrix
  Basically just have leading column of 1s for the constant"
  [xs]
  (build (count xs)
         2
         (concat (repeat (count xs)
                         1.0)
                 xs)))

(defn
  residual
  [xy-pairs
   predicted-ys]
  (mapv (fn [xy-pair
             predicted-y]
          (- (second xy-pair)
             predicted-y))
        xy-pairs
        predicted-ys))

(defn
  self-inner-product
  [xs]
  (->> xs
       (mapv (fn [x]
               (clojure.math/pow x
                                 2.0)))
       (reduce +)))
#_
(self-inner-product [1 2 3])

(defn
  vecvar
  "variance with mean zero"
  [somevec]
  (/ (self-inner-product somevec)
     (count somevec)))

(defn
  cols
  [data-matrix]
  (->> data-matrix
       ncols
       range
       (mapv #(col data-matrix
                   %))))

(defn
  abs-sums-of-cols
  ""
  [data-matrix]
  (->> data-matrix
       matrix/cols
       (mapv (fn [column]
               (->> column
                    (mapv abs)
                    (apply +)
               #_#_
                   seq
               (ncore/dot column
                          (neand/dv (repeat (ncore/dim column)
                                            1))))))))
#_
(->> [1 2 3 4]
     (build 2 2 )
     abs-sums-of-cols)
#_
(defn
  scaled-to-vec
  "Scale the columns in a matrix to by the values in a data-vec"
  [data-matrix
   data-vec]
  (let [num-datapoints (-> data-matrix
                           :matrix
                           ncore/ncols)
        num-pixels     (-> data-matrix
                           :matrix
                           ncore/mrows)]
    (let [mat-of-data-vecs (->> data-vec
                                (repeat num-datapoints)
                                (neand/dge num-pixels
                                           num-datapoints) #_
                                ncore/trans)]
      (-> data-matrix
          (assoc :matrix
                 (vect-math/mul mat-of-data-vecs
                                (:matrix data-matrix)))
          (dissoc :sigma ;; remove any stale svd info
                  :u
                  :vt)))))
