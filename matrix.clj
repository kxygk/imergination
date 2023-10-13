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
  svd
  "Take a data matrix
  Returns
  {:sv matrix
  :weights vector}"
  [data-matrix]
  (linalg/svd (:matrix data-matrix)
              true
              true))
#_
(linalg/svd (:matrix (-> @state/*selections
                         (cljfx.api/sub-ctx state/region-matrix)))
            true
            true)
#_
(-> @state/*selections
    (cljfx.api/sub-ctx state/region-matrix)
    svd)

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
  It'll be the size of the original region"
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
      (let [sv-grid   (geogrid4seq/build-grid first-geogrid
                                              sv)
            shoreline (plot/shoreline-map region
                                          "./data/shoreline-coarse.json"
                                          [])]
        (spit "out/test/matrix_singular-vector.svg"
              (-> sv-grid
                  (plot/grid-map shoreline
                                 [])
                  quickthing/serialize))))))

(defn
  svd-to-2d-sv-space
  [svd]
  (let [weight-matrix (:vt svd)]
    (mapv vector
          (ncore/row weight-matrix                          ;; data proj on sv1
                     0)
          (ncore/row weight-matrix                          ;; data proj on sv1
                     1))))
#_
(-> @state/*selections
    (cljfx.api/sub-ctx state/region-matrix)
    svd
    svd-to-2d-sv-space)

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
    (geogrid4seq/build-grid
      width-pix
      height-pix
      eas-res
      sou-res
      position
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
  (col-to-grid
    (into
      []
      (ncore/col
        matrix
        column-index))
    grid-params))
