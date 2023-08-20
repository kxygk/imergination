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
  Or the lowest value"
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
#_
(-> @state/*selections
    (fx/sub-ctx state/first-datafile-geogrid))

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
                       []) ;; no POI
        quickthing/serialize)))
#_
(-> @state/*selections
    (fx/sub-ctx state/first-datafile-svg))

(defn
  region-matrix
  "Matrix of all the data overa region
  Implementation is hidden in `matrix.clj`
  So that the underlying library can be swapped"
  [context]
  (-> context
      (fx/sub-ctx state/region-geogrids)
      matrix/from-geogrids))
#_
(-> @state/*selections
    (fx/sub-ctx state/region-matrix))

(defn
  region-svd
  "the SVD of the region matrix"
  [context]
  (-> context
      (fx/sub-ctx state/region-matrix)
      matrix/svd))
#_
(-> @state/*selections
    (fx/sub-ctx state/region-svd))
#_
(state/region-svd @state/*selections)



(defn
  singular-vector
  "Get a particular singular vector
  Based on `index`"
  [context
   sv-index]
  (-> context
      (fx/sub-ctx region-svd)
      (matrix/singular-vector sv-index)))
#_
(state/singular-vector @state/*selections
                       0)
#_
(-> @state/*selections
    (fx/sub-ctx state/singular-vector 0))

(defn
  singular-vector-geogrid
  [context
   sv-index]
  (let [sv              (fx/sub-ctx context
                                    singular-vector
                                    sv-index)
        ;;we use the first data grid to extra grid parameters
        first-data-grid (-> context
                            (fx/sub-ctx region-geogrids)
                            first)]
    (geogrid4seq/build-grid (-> context
                                (fx/sub-ctx region-geogrids)
                                first)
                            sv)))
#_
(-> @state/*selections
    (fx/sub-ctx state/singular-vector-geogrid
                0))
#_
(spit
  "out/first-sv.svg"
  (quickthing/serialize-with-line-breaks
    (plot/grid-map
      (-> @state/*selections
          (fx/sub-ctx state/re))
      (state/region @state/*selections) ;;input-region ;; it'll crop redundantly here..
      []
      "1st SV")))

(defn
  singular-vector-svg
  "Return an SVG of the singular vector INDEX
  If there are no datafiles and you can't do a SVD then return a region"
  [context
   sv-index]
  (if (empty? (fx/sub-ctx context
                          region-geogrids))
    (fx/sub-ctx context
                region-svg)
    (-> (fx/sub-ctx context
                    singular-vector-geogrid
                    sv-index)
        (plot/grid-map (fx/sub-ctx context
                                   region-svg-hiccup)
                       []) ;; no POI
        quickthing/serialize)))

(defn
  sv-proj
  [context]
  (-> context
      (fx/sub-ctx region-matrix)
      matrix/svd
      matrix/svd-to-2d-sv-space))
#_
(-> @state/*selections
    (fx/sub-ctx state/sv-projections))

(defn
  sv-proj-svg
  [context]
  (-> context
      (cljfx.api/sub-ctx sv-proj)
      (plot/two-d-plot (* 100 (fx/sub-ctx context
                                          state/window-width))
                       (* 100 (fx/sub-ctx context
                                          state/window-width)))
      quickthing/serialize))
#_
(-> @state/*selections
    (fx/sub-ctx state/sv-projections-svg))


(defn
  sv-weights
  [context]
  (-> context
      (fx/sub-ctx region-matrix)
      matrix/svd
      matrix/singular-values))
#_
(-> @state/*selections
    (fx/sub-ctx state/sv-weights))

(defn
  sv-weights-svg
  [context]
  (-> context
      (cljfx.api/sub-ctx sv-weights)
      (plot/sv-weights (* 100 (fx/sub-ctx context
                                          state/window-width))
                       (* 50 (fx/sub-ctx context
                                         state/window-width)))
      quickthing/svg2xml))
#_
(-> @state/*selections
    (fx/sub-ctx state/sv-weights-svg))

