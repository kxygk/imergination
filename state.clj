(ns
    state
  "Program and GUI state"
  (:require [clojure.java.io :as io]
            [cljfx.api       :as fx]
            [clojure.core.cache :as cache]
            [injest.path :refer [+> +>> x>> =>>]]
            geogrid4image
            svg2jfx
            matrix
            plot
            locations))

(def
  *selections
  (atom
    (fx/create-context
      {:window-width      1080.0
       :row-height        360
       :shoreline-filestr "./data/shoreline-coarse.json"
       :contour-filestr   nil
       :rain-dirstr       "/home/kxygk/Data/imerg/monthly/late/"
       :eas-res           0.1
       :sou-res           0.1
       :region            locations/krabi-skinny-region
       :datafile-idxs     []}
      #(cache/lru-cache-factory % :threshold 1000))))

;; (fx/sub-val
;;   *selections
;;   :shoreline-file)

(defn
  row-height
  [context]
  (fx/sub-val context
              :row-height))

(defn
  shoreline-filestr
  [context]
  (fx/sub-val context
              :shoreline-filestr))

(defn
  region
  [context]
  (fx/sub-val context
              :region))

(defn
  window-width
  [context]
  (fx/sub-val context
              :window-width))

(defn
  region-xy-ratio
  [context]
  (let [[lat
         lon] (-> context
                  (fx/sub-ctx region)
                  geoprim/dimension)]
    (/ lat
       lon)))

(defn
  region-display-width
  [context]
  (let [half-window        (/ (fx/sub-ctx context
                                          window-width)
                             2.0)
        max-image-height   (* (fx/sub-ctx context
                                          row-height)
                              0.75)
        drawing-area-ratio (/ half-window
                              max-image-height)
        image-ratio        (fx/sub-ctx context
                                       region-xy-ratio)]
    (if (> image-ratio
           drawing-area-ratio)
      half-window
      (* image-ratio
         max-image-height))))

(defn region-display-height
  [context]
  (/ (fx/sub-ctx context
                 region-display-width)
     (fx/sub-ctx context
                 region-xy-ratio)))

(defn
  region-to-display-scale-x
  "How much the region's `viewBox` (ie. lat range)
  needs to be scaled to match the display width"
  [context]
  (let [[lat
         _] (-> context
                (fx/sub-ctx region)
                geoprim/dimension)]
    (/ (fx/sub-ctx context
                   region-display-width)
       lat)))

(defn
  region-to-display-scale-y
  "How much the region's `viewBox` (ie. lat range)
  needs to be scaled to match the display width"
  [context]
  (let [[_
         lon] (-> context
                  (fx/sub-ctx region)
                  geoprim/dimension)]
    (/ (fx/sub-ctx context
                   region-display-height)
       lon)))

(defn
  eas-res
  [context]
  (fx/sub-val  context
               :eas-res))

(defn
  sou-res
  [context]
  (fx/sub-val context
              :sou-res))

(defn-
  world-svg-hiccup
  [context]
  (plot/shoreline-map locations/world-region
                      (fx/sub-ctx context
                                  shoreline-filestr)
                      [])) ;; no POI
  
(defn
  world-svg
  "Get a shoreline map of the whole world
  TODO: Maybe bake this in to the program?"
  [context]
  (-> context
      (fx/sub-ctx world-svg-hiccup)
      quickthing/serialize))

#_#_
(defn
  world-batik
  "Get the shoreline as a JFX group
  Not scaled in any way"
  [context]
  (let [shoreline-svg (fx/sub-ctx context
                                  world-svg)]
    (-> shoreline-svg
        svg2jfx/batik-load)))

(defn
  world-batik-fullwidth
  "Get the world shoreline as a JFX group
  Scaled to take the width of the window"
  [context]
  (let [batik-group (fx/sub-ctx context
                                world-batik)
        scale-x     (/ (fx/sub-ctx context
                                   state/window-width)
                       360.0)
        scale-y     (/
                      (fx/sub-ctx context
                                  state/window-width)
                      360.0)]
    (svg2jfx/batik-scale  batik-group
                          scale-x
                          scale-y)))

#_        
(defn
  region-shoreline-svg
  "Get a shoreline map of the region of interest
  TODO: This could be a higher resolution than the world map
  REMOVE************************"
  [context]
  (plot/shoreline-map
    [(fx/sub-ctx
       context
       region)
     nil] ;; no POIs
    (fx/sub-val
      context
      :shoreline-filestr)))



(defn-
  region-svg-hiccup
  "Get a shoreline map of the region of interest
  TODO: This could be a higher resolution than the world map"
  [context]
  (->
    (fx/sub-ctx context
                region)
    (plot/shoreline-map (fx/sub-ctx context
                                    shoreline-filestr)
                        []))) ;; no POI

(defn
region-svg
  "Get a shoreline map of the region of interest
  TODO: This could be a higher resolution than the world map"
  [context]
  (-> context
      (fx/sub-ctx region-svg-hiccup)
      quickthing/serialize))

#_#_
(defn
  region-batik
  [context]
  (let [shoreline-svg (fx/sub-ctx context
                                  region-svg)]
    (-> shoreline-svg
        svg2jfx/batik-load)))

(defn
  region-batik-halfwidth
  "This is scaled to fit the half area box"
  [context]
  (let [batik-group (fx/sub-ctx context
                                region-batik)
        scale-x     (fx/sub-ctx context
                                state/region-to-display-scale-x)
        scale-y     (fx/sub-ctx context
                                state/region-to-display-scale-y)]
    (svg2jfx/batik-scale batik-group
                         scale-x
                         scale-y)))

(defn
  data-dirstr
  "The string for the path to the directory with all the data"
  [context]
  (fx/sub-val context
              :rain-dirstr))


(defn
  datafile-strs
  "Gets the file listing in the selected directory
  NOTE: This does not automatically detect if
  the directory contents have been changed"
  [context]
  (-> ^String ;; I forget why I type hint..
      (fx/sub-ctx
        context
        data-dirstr)
      java.io.File.
      .list
      sort))

;; TODO Test out with caching the whole dataset..
;; see if the computer catches fire
#_
(defn
  all-images
  "Read in all the files, and store in a vector
  This will eat a ton of memory :)"
  [context]
  (->>
    (fx/sub-ctx
      context
      data-files-strs)
    (mapv
      #(str
         (fx/sub-ctx
           context
           data-dirstr)
         %))
    (mapv
      #(geogrid4image/read-file
         %
         0.1
         0.1))))

(defn
  region-geogrids
  "A vector of all the images of the region of interest
  in the same order as the file listing.
  Reading and cropping all the images take a min or two "
  [context]
  (->> (fx/sub-ctx context
                   datafile-strs)
       (mapv #(str (fx/sub-ctx context
                               data-dirstr)
                   %))
       (mapv #(geogrid4image/read-file %
                                       (fx/sub-ctx context
                                                   eas-res)
                                       (fx/sub-ctx context
                                                   sou-res)))
       (mapv #(geogrid/subregion %
                                 (fx/sub-ctx context
                                             region)))
       #_vec))
#_
(-> @state/*selections
    (fx/sub-ctx state/region-geogrids)
    first)

(defn
  datafile-idxs
  "Indeces of the data that's been selected" 
  [context]
  (fx/sub-val context
              :datafile-idxs))

(defn
  first-datafile-idx
  "Get the first selected data index
  Which in effect meaning the ~earliest~ in the list
  Or the lowest value
  If nothing has been selected then it returns `0`"
  [context]
  (get (fx/sub-ctx context
                   datafile-idxs)
       0))

(defn-
  first-datafile-geogrid
  [context]
  (get (fx/sub-ctx context
                   region-geogrids)
       (fx/sub-ctx context
                   first-datafile-idx)))
  
(defn
  first-datafile-svg
  "Get a shoreline map of the region of interest
  TODO: This could be a higher resolution than the world map"
  [context]
  (if (nil? (fx/sub-ctx context
                        first-datafile-idx))
    (fx/sub-ctx context
                region-svg)
    (-> (fx/sub-ctx context
                    first-datafile-geogrid)
        (plot/grid-map (fx/sub-ctx context
                                   region-svg-hiccup)
                       (fx/sub-ctx context
                                   region)
                       []) ;; no POI
        quickthing/serialize)))

#_#_#_
(defn
  first-datafile-batik
  [context]
  (let [svg (fx/sub-ctx context
                        first-datafile-svg)]
    (-> svg
        svg2jfx/batik-load)))

(defn
  first-datafile-batik-halfwidth
  "If nothing is selected, this blows up"
  [context]
  (let [batik-group (fx/sub-ctx  context
                                 first-datafile-batik)
        scale-x     (fx/sub-ctx context
                                state/region-to-display-scale-x)
        scale-y     (fx/sub-ctx  context
                                 state/region-to-display-scale-y)]
    (svg2jfx/batik-scale batik-group
                         scale-x
                         scale-y)))

(defn
  datapreview-batik-halfwidth
  [context]
  (if (nil? (fx/sub-ctx context
                        first-datafile-idx))
    (fx/sub-ctx context
                region-batik-halfwidth)
    (fx/sub-ctx  context
                 first-datafile-batik-halfwidth)))




(defn
  data-geogrid
  "Reads in geogrids from GeoTIFF files
  Cropped to the region of interest"
  [context]
  (let [dirstr  (fx/sub-ctx context
                            data-dirstr)
        eas-res (fx/sub-ctx context
                            eas-res)
        sou-res (fx/sub-ctx context
                            sou-res)]
    (->> dirstr
         java.io.File.
         .list
         sort
         (map
           #(geogrid4image/read-file
              (str
                dirstr
                %)
              eas-res
              sou-res)))))
#_
(defn
  data-matrix
  [context]
  (matrix/from-geogrids
    (fx/sub-ctx
      context
      data-geogrid)))
#_
(defn
  data-grid
  [context]
  )
#_
(defn-
  to-data-matrix
  "Turns a series of `geogrid` of identical size to one matrix"
  [grids]
  (let[[width-pix
        height-pix] (->
                      grids
                      first
                      geogrid/dimension-pix)
       all-data     (->>
                      grids
                      (map
                        geogrid/data)
                      (reduce
                        into
                        []))]
    (dge
      ;; rows
      (* 
        width-pix
        height-pix)
      ;; columns
      (count
        grids)
      ;; data
      all-data)))

#_#_#_
(defn
  data-matrix
  "Takes the read in geogrids
  and returns them packed in a matrix"
  [context]
  (to-data-matrix
    (fx/sub-ctx
      context
      data-geogrid)))


(defn-
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


(defn-
  matrix-col-to-grid
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
      (col
        matrix
        column-index))
    grid-params))
