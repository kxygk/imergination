(ns
    matrix
  "The matrix of all the data
  First implementation based on `neanderthal`
  Will rewrite into something that can be more easily packaged.."
  (:require geogrid
            geogrid4seq
            [uncomplicate.neanderthal.core :as ncore]
            [uncomplicate.neanderthal.native :as neand]
            [uncomplicate.neanderthal.linalg :as linalg]
            [uncomplicate.neanderthal.vect-math :as vect-math]))

(defn-
  glue-geogrids-into-one-matrix
  "Take all the grids,
  pull out the data,
  slap them all together column by column"
  [geogrids]
  (let [[width-pix
         height-pix] (-> geogrids
                         first
                         geogrid/dimension-pix)
        all-data     (->> geogrids
                          (map geogrid/data)
                          (reduce into
                                  []))]
    (neand/dge (* width-pix ;;rows
                  height-pix)
               (count geogrids) ;;cols
               all-data)));; data

(defn
  from-geogrids
  "Read in a vector of geogrids into one large `matrix`"
  [geogrids]
  (let [local-matrix (glue-geogrids-into-one-matrix
                       geogrids)]
    {:matrix     local-matrix
     :dimension  (-> geogrids
                     first
                     geogrid/dimension-pix)
     :position   (-> geogrids
                     first
                     geogrid/corner)
     :resolution (-> geogrids
                     first
                     geogrid/eassou-res)}))
#_
(-> @state/*selections
    (fx/sub-ctx state/region-images)
    matrix/from-geogrids)

(defn
  get-min-max
  [matrix]
  (let [data-vec (-> matrix
                     :matrix
                     seq
                     vec
                     flatten
                     vec)]
    ;;data-vec
    [(apply min data-vec)
     (apply max data-vec)]))

(defn
  svd
  "Take a data matrix
  Returns
  {:sv matrix
  :weights vector}"
  [data-matrix]
  (merge (linalg/svd (:matrix data-matrix)
                     true
                     true)
         data-matrix))
#_
(linalg/svd (:matrix (-> @state/*selections
                         (cljfx.api/sub-ctx state/region-matrix)))
            true
            true)
#_
(-> @state/*selections
    (cljfx.api/sub-ctx state/region-matrix)
    svd
    keys)

(defn
  singular-values
  "Given an SVD return the singular-values
  Returned as pairs:
  [[sv-num sv]
   [sv-num sv]
   ..
   [sv-num sv]]
  Index starts at 1
  B/c this is what is directly plottable
  and this is the natural way we speak of svs"
  [svd]
  (->> svd
       :sigma
       ncore/dia
       (into [])
       (map-indexed (fn [index
                         value]
                      (vector (inc index)
                              value)))
       (into [])))
#_
(-> @state/*selections
    (cljfx.api/sub-ctx state/region-matrix)
    svd
    singular-values)

(defn
  singular-value
  [svd
   sv-index]
  (-> svd
      singular-values
      (get sv-index)
      second))
#_
(-> @state/*selections
    (cljfx.api/sub-ctx state/region-matrix)
    svd
    (singular-value 1))
;; => 9247.099897276306

(defn
  singular-values-stats
  [singular-values]
  (let [weights (->> singular-values
                     (map second))]
    (let [signal (->> weights
                      (take 2))
          noise  (->> weights
                      (drop 2))
          total  (apply +
                        weights)]
      {:total  total
       :signal (/ (apply +
                         signal)
                  total)
       :noise  (/ (apply +
                         noise)
                  total)})))

(defn
  singular-vector
  "Take the result of the SVD
  and return one singular vector
  It'll be the size of the original region
  ..
  NOTE: When num-pixels > num-images
  the number of singular vectors is limited to
  the number of images
  This is different from the Wiki definition of the SVD
  The U matrix is not square!!
  aka a `Thin SVD`"
  [svd
   sv-index]
  (into []
        (ncore/col (:u svd)
                   sv-index)))
#_
(let [region      locations/krabi-skinny-region
      data-dirstr "./data/late/"
      eas-res     0.1
      sou-res     0.1
      sv-index    0]
  (let [geogrids (->> data-dirstr
                      java.io.File.
                      .list
                      sort
                      (mapv #(str data-dirstr
                                  %))
                      (mapv #(geogrid4image/read-file %
                                                      eas-res
                                                      sou-res))
                      (mapv #(geogrid/subregion %
                                                region)))]
    (let [sv            (-> geogrids
                            matrix/from-geogrids
                            matrix/svd
                            (matrix/singular-vector sv-index))
          first-geogrid (-> geogrids
                            first)]
      (let [sv-grid   (geogrid4seq/build-grid (geogrid/params first-geogrid)
                                              sv)
            shoreline (plot/shoreline-map region
                                          "./data/shoreline-coarse.json"
                                          [])]
        (spit "out/test/matrix_singular-vector.svg"
              (-> sv-grid
                  (plot/grid-map shoreline)
                  quickthing/serialize))))))


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

(project-onto-2d-basis [-0.6637395088755317 0.7479638121979351]
                       [-0.8414805073840199 -0.5402874750470648]
                       [[-0.03984475266550661 0.05052713742626782]
                        [-0.07180659921926151 -0.022952287269459186]])

#_
(project-onto-2d-basis [1.0, 0.0]
                       [0.0, 1.0]
                       [[5.0, 8.0, {:blah 2}]
                        [22.0, 6.0]
                        [20.0, 6.0]])

(defn
  svd-to-weights
  "get the weight of a particular vector for each data point
  So for `0` it will give you the weight of the PC-1 for each point in time"
  [svd
   sv-index]
  (let [weight-matrix (:vt svd)]
    (ncore/row weight-matrix                          ;; data proj on sv1
               sv-index)))

(defn
  svd-to-2d-sv-space
  [svd]
  (let [weight-matrix (:vt svd)]
    (mapv vector
          (ncore/row weight-matrix                          ;; data proj on sv1
                     0)
          (ncore/row weight-matrix                          ;; data proj on sv2
                     1))))
#_
(-> @state/*selections
    (cljfx.api/sub-ctx state/region-matrix)
    svd
    svd-to-2d-sv-space)

(defn
  singular-vector-mix
  "Take SINGULAR-VECTOR-A and SINGULAR-VECTOR-B
  And mix them according to WEIGHT-A and WEIGHT-B"
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
  col-to-grid
  "Given a COLUMN-OF-DATA
  as well as map with keys for the grid's
  `:dimension` `:position` and `:resolution`
  Returns a `geogrid`"
  [column-of-data
   {:keys [;;matrix
           dimension
           position
           resolution]}]
  (let [[width-pix
         height-pix] dimension
        [eas-res
         sou-res]    resolution]
    (geogrid4seq/build-grid [width-pix
                             height-pix
                             eas-res
                             sou-res
                             position]
                            column-of-data)))

(defn
  extract-grid
  "Given a matrix"
  [{:keys [matrix
           dimension
           position
           resolution]
    :as   grid-params}
   column-index]
  (col-to-grid (into []
                     (ncore/col matrix
                                column-index))
               grid-params))

(defn
  extract-params
  "Get the geogrid params from the matrix
  The format matches the vector returned in `geogrid/params`
  Not sure why I made it that way.. Probably should be a map of values"
  [{:keys [matrix
           dimension
           position
           resolution]
    :as   grid-params}]
   (let [[width
          height] dimension
         [eas-res
          sou-res] resolution
         norwes-point position]
     ;; list from `geogrid/params`
     [width
      height
      eas-res
      sou-res
      norwes-point]))

(defn
  minus-1-sv
  "Take the SV matrices
  Zero out the first component
  Return the new data matrix of (maybe?) only noise"
  [svd]
  (let [truncated-sigma (-> svd
                            :sigma
                            (ncore/alter! 0
                                          0
                                          (fn ^double   ;; the type annotations are required for some reason
                                            [^double _] ;; crashed without them..
                                            0.0)))
        new-data-matrix (ncore/mm (:u svd)
                                  truncated-sigma
                                  (:vt svd))]
    (-> svd
        (assoc :sigma
               truncated-sigma)
        (assoc :matrix
               new-data-matrix))))
#_
(-> [1 2 3]
    neand/dv
    (ncore/alter! 0
                  ;;0
                  (fn ^double [^double _] 0.0 #_(inc x)) ))

(defn
  minus-2-sv
  "Take the SV matrices
  Zero out the first two components
  Return the new data matrix of only noise"
  [svd]
  (let [truncated-sigma (-> svd
                            :sigma
                            (ncore/alter! 0
                                          0
                                          (fn ^double   ;; the type annotations are required for some reason
                                            [^double _] ;; crashed without them..
                                            0.0))
                            (ncore/alter! 1
                                          1
                                          (fn ^double
                                            [^double _]
                                            0.0)))
        new-data-matrix (ncore/mm (:u svd)
                                  truncated-sigma
                                  (:vt svd))]
    (-> svd
        (assoc :sigma
               truncated-sigma)
        (assoc :matrix
               new-data-matrix))))

#_
(defn
  from-svd
  [svd
   geogrid]
  (let [{:keys [u
                vt
                sigma]} svd]
    (merge {:matrix     (ncore/mm u
                                  sigma
                                  vt)
            :dimension  (-> geogrids
                            first
                            geogrid/dimension-pix)
            :position   (-> geogrids
                            first
                            geogrid/corner)
            :resolution (-> geogrids
                            first
                            geogrid/eassou-res)}
           grid-params)))


(defn
  design-matrix
  "This is the matrix used for the regression
  https://en.wikipedia.org/wiki/Design_matrix
  Basically just have leading column of 1s for the constant"
  [xs]
  (neand/dge (count xs)
             2
             (concat (repeat (count xs)
                             1.0)
                     xs)))
#_
(design-matrix [1 2 3 4])


;; example data from
;; wikipedia.org/wiki/Simple_linear_regression#Example
;; going to use for demos below
(def
  data [[1.47 	52.21]
        [1.50 	53.12]
        [1.52 	54.48]
        [1.55 	55.84]
        [1.57 	57.20]
        [1.60 	58.57]
        [1.63 	59.93]
        [1.65 	61.29]
        [1.68 	63.11]
        [1.70 	64.47]
        [1.73 	66.28]
        [1.75 	68.10]
        [1.78 	69.92]
        [1.80 	72.19]
        [1.83 	74.46]])

(defn
  linear-fit
  "Fit a line to a set of [x y] pairs
  Return a column vector with
  first offset (alpha)
  second slope (beta)"
  [xy-pairs-vec]
  (let [xs                   (->> xy-pairs-vec
                                  (mapv first))
        ys                   (->> xy-pairs-vec
                                  (mapv second))
        least-squares-matrix (neand/dge (count xy-pairs-vec)
                                        2
                                        (design-matrix xs))
        y-vector             (neand/dge (count xy-pairs-vec)
                                        1
                                        ys)]

    (ncore/view-ge (linalg/ls least-squares-matrix
                              y-vector)
                   2
                   1)))
#_
(->> data
    linear-fit)
;; => #RealGEMatrix[double, mxn:2x1, layout:column]
;;       ▥       ↓       ┓    
;;       →     -39.06         
;;       →      61.27         
;;       ┗               ┛    
;; These match the wiki page!
;; First is offset and second is slope - as expected
#_
(->> data
    linear-fit
    seq
    flatten
    (zipmap [:offset
             :slope]))
;; => {:offset -39.061955918843935, :slope 61.27218654211063}

(defn
  predicted-values
  [fit-params
   xy-pairs]
  (-> (mapv first
            xy-pairs)
      design-matrix
      (ncore/mm fit-params)
      seq
      flatten
      vec))
#_
(-> data
    linear-fit
    (predicted-values data))

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
#_
(residual data
          (-> data
              linear-fit
              (predicted-values data)))

(defn
  self-inner-product
  [xs]
  (->> xs
       (mapv (fn [x]
               (clojure.math/pow x
                                 2.0)))
       (reduce +)))
#_
(self-inner-product (residual data
                              (-> data
                                  linear-fit
                                  (predicted-values data))))
;; => 7.490558403882591

(defn
  residual-variance
  [fit-params
   xy-pairs]
  (/ (self-inner-product (residual xy-pairs
                                   (predicted-values fit-params
                                                     xy-pairs)))
     (- (count xy-pairs)
        3.0)))
#_
(residual-variance data)

(defn
  subsets-for-linear-regression
  [sorted-xy-pairs]
  (->> sorted-xy-pairs
       count
       range
       (drop-last 3)
       (mapv (fn [num-to-drop]
               (let [subset     (->> sorted-xy-pairs
                                     (drop-last num-to-drop)
                                     vec)
                     fit-params (-> subset
                                    linear-fit)
                     [offset
                      slope]    (-> fit-params
                                    seq
                                    flatten
                                    vec)]
                 {:num-dropped       num-to-drop
                  :residual-variance (residual-variance fit-params
                                                        subset)
                  :fit-params        {:slope  slope
                                      :offset offset}
                  :subset            subset})))))
#_
(->> data
     subsets-for-linear-regression
     (mapv :residual-variance))
#_
(->> data
     subsets-for-linear-regression
     (apply min-key
            :residual-variance)
     keys)
;; => (:num-dropped :residual-variance :fit-params :subset)


(defn
  vecnorm
  "Normalize vector...
  Why does Neanderthal not have a function for this?"
  [somevec]
  (ncore/scal (/ 1.0
                 (ncore/nrm2 somevec))
              somevec))

(ncore/dim (neand/dv [1 2 3 4 5]))

(defn
  vecvar
  "variance with mean zero"
  [somevec]
  (/ (ncore/dot somevec ;; some of the squares
                somevec)
     (ncore/dim somevec)))

#_
(let [num (count data-seq)]
  (->> data-seq
       (reduce #(+ (/ (clojure.math/pow %2
                                        2.0)
                      num)
                   %1)
               0.0)))

(defn
  colvars
  "for each column in a matrix
  caluclate it's variance
  ..
  Assumes a mean of zero"
  [noisematrix]
  (->> noisematrix
       :matrix
       ncore/cols
       (mapv vecvar)))

(defn
  from-vecofvecs
  "Take a vector of vector of values and turn it into a matrix"
  [data-matrix-for-meta-data
   vecofvecs]
  (-> data-matrix-for-meta-data
      (assoc :matrix
             (neand/dge (-> vecofvecs
                            first
                            count)
                        (-> vecofvecs
                            count)
                        vecofvecs))))

(defn
  get-points-at-coord
  [{:keys [matrix
           dimension
           position
           resolution]}
   my-point]
  (let [[width-pix
         height-pix] dimension
        [eas-res
         sou-res]    resolution]
    (let [[eas sou] (geogrid/point-to-pix my-point
                                          (geogrid4seq/build-grid [width-pix
                                                                   height-pix
                                                                   eas-res
                                                                   sou-res
                                                                   position]
                                                                  nil))]
      #_matrix
      (println (str "\nDimensions: "
                    dimension
                    "\nPoint Coord: "
                    eas
                    " "
                    sou))
      (-> matrix
          (ncore/row
            (+ (int eas)
               (* width-pix
                  (int sou))))
          seq))))
#_
(let [test-poi (geoprim/point 8.100833
                              98.984722)]
  (-> @state/*selections
      (cljfx.api/sub-ctx state/region-matrix)
      (get-points-at-coord test-poi)))


(defn
  data-average-vec
  [{:keys [matrix
           dimension
           position
           resolution]}]
  (let [ndata (ncore/ncols matrix)]
    (ncore/ax (/ 1.0
                 ndata)
              (ncore/mv matrix
                        (neand/dv (repeat ndata
                                          1.0))))))
#_
(-> @state/*selections
    (cljfx.api/sub-ctx state/region-matrix)
    data-average-vec)

(defn
  data-average-geogrid
  [grid-params]
  (-> grid-params
      data-average-vec
      (col-to-grid grid-params)))
#_
(-> @state/*selections
    (cljfx.api/sub-ctx state/region-matrix)
    data-average-geogrid)
