(ns kxygk.imergination.datamats
  (:require [kxygk.imergination.matrix :as matrix]
            [geogrid]
            [geogrid4seq]))

(defn from-geogrids
  "Read in a vector of geogrids into one large `matrix`"
  [geogrids]
  (let [[width-pix
         height-pix] (-> geogrids
                         first
                         geogrid/dimension-pix)
        all-data     (->> geogrids
                          (map geogrid/data)
                          (reduce into
                                  [])
                          flatten)]
    (let [local-matrix (matrix/build (* width-pix ;;rows
                                        height-pix)
                                     (count geogrids) ;;cols
                                     all-data)]
      {:matrix     local-matrix
       :dimension  (-> geogrids
                       first
                       geogrid/dimension-pix)
       :position   (-> geogrids
                       first
                       geogrid/corner)
       :resolution (-> geogrids
                       first
                       geogrid/eassou-res)})))

(defn get-min-max
  [datamats]
  (let [data-vec (-> datamats
                     :matrix
                     matrix/data)]
    ;;data-vec
    [(apply min data-vec)
     (apply max data-vec)]))

(defn
  num-svs
  "Number of Singular Vectors/Values.
  This is either the number of data point,
  or the number of pixels.
  Whichever is smallest"
  [datamats]
  (let [matrix (-> datamats
                   :matrix)]
      (min (matrix/ncols matrix)
           (matrix/mrows matrix))))

;;Maybe if I make a map here,
;;it'll degenerate into the same vec of pairs
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
  [datamats]
  (->> datamats
       :sigma
       matrix/diag
       (map-indexed (fn [index
                         value]
                      (vector (inc index)
                              value)))
       vec))

(defn
  singular-value
  [datamats
   sv-index]
  (-> datamats
      singular-values
      (get sv-index)
      second))

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
  [datamats
   sv-index]
  (into []
        (matrix/col (:u datamats)
                    sv-index)))

(defn
  rezero
  "Removes the `min` value from all values"
  [datamats]
  (let [data-matrix (-> datamats
                        :matrix)]
    (let [data-vec (matrix/data data-matrix)]
      (let [min-pix (apply min data-vec)]
        (matrix/build (matrix/mrows data-matrix)
                      (matrix/ncols data-matrix)
                      (->> data-vec
                           (mapv #(- %
                                     min-pix))))))))

(defn
  rezero-nonzero
  "Removes the `min` value from all values"
  [datamats]
  (let [data-matrix (-> datamats
                        :matrix)]
    (let [data-vec (matrix/data data-matrix)]
      (let [min-pix (apply min data-vec)]
        (matrix/build (matrix/mrows data-matrix)
                      (matrix/ncols data-matrix)
                      (->> data-vec
                           (mapv #(if (zero? %)
                                    0.0
                                    (- %
                                       min-pix)))))))))

(defn-
  invert-sv1
  "This inverts the corresponding elements in `:u` `:v` .
  The original `:matrix` isn't recomputed as it shouldn't change"
  [svdmats]
  (let [#_#_
        num-pixels (-> datamats
                       :matrix
                       matrix/rows)
        num-observ (-> svdmats
                       :u
                       matrix/ncols)]
    (let [inverter (matrix/inverter num-observ)]
  (-> svdmats
      (update :u
              (fn [original-matrix]
                (matrix/mm original-matrix
                           inverter)))
      (update :vt
              (fn [original-matrix]
                (matrix/mm inverter
                           original-matrix)))
      ))))

(defn
  svd
  "Take a data matrix
  Returns
  {:sv matrix
  :weights vector}"
  [datamats]
  (let [raw-svd (matrix/svd (-> datamats
                                :matrix))]
    (if (pos? (apply +
                     (-> raw-svd
                         (singular-vector 0))))
      (merge raw-svd
             datamats)
      (merge (invert-sv1 raw-svd)
             datamats))))
  
(defn
  svd-to-weights
  "get the weight of a particular vector for each data point
  So for `0` it will give you the weight of the PC-1 for each point in time"
  [datamats
   sv-index]
  (let [weight-matrix (:vt datamats)]
    (-> weight-matrix
        (matrix/row sv-index)
        seq
        vec)))

(defn
  svd-to-2d-sv-space
  [datamats]
  (let [weight-matrix (:vt datamats)]
    (mapv vector
          (matrix/row weight-matrix                          ;; data proj on sv1
                      0)
          (matrix/row weight-matrix                          ;; data proj on sv2
                      1))))


(comment ;;rezero
         ;;rezero-nonzero
         ;;svd
         ;;invert-sv1
         ;;project-onto-2d-basis   ;;matrix
         ;;project-onto-2-patterns ;;matrix
         ;;svd-to-weights          ;;datamats
         ;;svd-to-2d-sv-space      ;;datamats
         ;;singular-vector-mix     ;;matrix
         ;;col-to-grid             ;;BACK TO state.clj
         ;;extract-grid            ;;state.clj
         ;;to-geogrid-vec          ;;datamats
         ;;extract-params          ;;datamats
         minus-1-sv              ;;datamats
         minus-2-sv              ;;datamats
         ;;design-matrix           ;;matrix
         linear-fit              ;;matrix
         predicted-values        ;;matrix
         ;;residual                ;;matrix
         ;;self-inner-product      ;;matrix
         ;;vecvar                  ;;matrix
         ;;colvars                 ;;datamats
         ;;from-vecofvecs          ;;datamats
         ;;get-points-at-coord
         ;data-average-vec
         ;;data-average-geogrid
         scaled-to-vec             ;;matrix
         self-inner-prod-of-cols   ;;matrix
         abs-sums-of-cols          ;;matrix
         scale-to-value            ;;
  
)

         
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
  [{:keys [matrix]
    :as   grid-params}
   column-index]
  (col-to-grid (into []
                     (matrix/col matrix
                                column-index))
               grid-params))

(defn
  to-geogrid-vec
  [grid-params]
  (->> grid-params
       :matrix
       matrix/ncols
       range
       (mapv #(extract-grid grid-params
                            %))))

(defn
  extract-params
  "Get the geogrid params from the matrix
  The format matches the vector returned in `geogrid/params`
  Not sure why I made it that way.. Probably should be a map of values"
  [{:keys [#_matrix
           dimension
           position
           resolution]
    #_#_
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
  colvars
  "for each column in a matrix
  caluclate it's variance
  ..
  Assumes a mean of zero"
  [noisedatamats]
  (->> noisedatamats
       :matrix
       matrix/cols
       (mapv matrix/vecvar)))

(defn
  from-vecofvecs
  "Take a vector of vector of values and turn it into a matrix"
  [data-matrix-for-meta-data
   vecofvecs]
  (println "Careful.. `from-vecofvecs` flattens data")
  (-> data-matrix-for-meta-data
      (assoc :matrix
             (matrix/build (-> vecofvecs
                        first
                        count)
                    (-> vecofvecs
                        count)
                    (flatten vecofvecs))))) ;; NOT SURE THIS IS RIGHT!!

;;UNUSED
#_
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
          (matrix/row
            (+ (int eas)
               (* width-pix
                  (int sou))))
          seq))))

#_
(defn
  data-average-vec
  [{:keys [matrix
           dimension
           position
           resolution]}]
  (let [ndata (matrix/ncols matrix)]
    (ncore/ax (/ 1.0                      ;;THIS GUY IS MISSING 
                 ndata)
              (matrix/mm matrix
                         (matrix/build ndata
                                       2
                                       (repeat ndata
                                               1.0))))))

#_
(defn
  data-average-geogrid
  [grid-params]
  (-> grid-params
      data-average-vec
      (col-to-grid grid-params)))

(defn errors-from-error-datamats
  [datamats
   singular-val]
  (-> datamats
      :matrix
      (matrix/scale-to-value (/ 1.0
                                singular-val))
      ;; pessimistic direct sum method
      ;;#_
      matrix/abs-sums-of-cols
      ;;quadrature sum method
      #_#_
      datamats/self-inner-prod-of-cols
      (mapv (fn [sum-of-squares]
              (-> sum-of-squares
                  Math/sqrt)))
      #_
      (map-indexed (fn [sv-index
                        scaled-error]
                     (/ scaled-error
                        singular-val)))
      vec))


(defn
  scaled-to-vec
  "Scale the columns in a matrix to by the values in a data-vec"
  [data-matrix
   data-vec]
  (let [num-datapoints (-> data-matrix
                           :matrix
                           matrix/ncols)
        num-pixels     (-> data-matrix
                           :matrix
                           matrix/mrows)]
    (let [mat-of-data-vecs (->> data-vec
                                (repeat num-datapoints)
                                (matrix/build num-pixels
                                              num-datapoints))]
      (-> data-matrix
          (assoc :matrix
                 (matrix/mult-elementwise (:matrix data-matrix)
                                          mat-of-data-vecs))
          (dissoc :sigma ;; remove any stale svd info
                  :u
                  :vt)))))

(defn
  minus-1-sv
  "Take the SV matrices
  Zero out the first component
  Return the new data matrix of (maybe?) only noise"
  [svdmats]
  (let [truncated-sigma (-> svdmats
                            :sigma
                            (matrix/set-value! 0
                                               0
                                               0.0))
        new-data-matrix (matrix/mm (:u svd)
                                   truncated-sigma
                                   (:vt svd))]
    (-> svdmats
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
                            (matrix/set-value! 0
                                               0
                                               0.0)
                            (matrix/set-value! 1
                                               1
                                               0.0))
        new-data-matrix (matrix/mm (matrix/mm (:u svd)
                                              truncated-sigma)
                                   (:vt svd))]
    (-> svd
        (assoc :sigma
               truncated-sigma)
        (assoc :matrix
               new-data-matrix))))
