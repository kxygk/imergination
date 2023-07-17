(ns
    matrix
  "The matrix of all the data
  First implementation based on `neanderthal`
  Will rewrite into something that can be more easily packaged.."
  (:require geogrid
            geogrid4seq
            [uncomplicate.neanderthal.core   :as ncore]
            [uncomplicate.neanderthal.native   :as neand]
            [uncomplicate.neanderthal.linalg :as linalg]
            [uncomplicate.neanderthal.vect-math :as vect-math]))

(defn-
  glue-geogrids-into-one-matrix
  "Take all the grids,
  pull out the data,
  slap them all together column by column"
  [geogrids]
  (let[[width-pix
        height-pix] (->
                      geogrids
                      first
                      geogrid/dimension-pix)
       all-data     (->>
                      geogrids
                      (map
                        geogrid/data)
                      (reduce
                        into
                        []))]
    (neand/dge
      ;; rows
      (*
        width-pix
        height-pix)
      ;; columns
      (count
        geogrids)
      ;; data
      all-data)))


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
