(ns
    state
  "Program and GUI state"
  (:use [hashp.core])
  (:require [clojure.java.io :as io]
            [cljfx.api       :as fx]
            [clojure.core.cache :as cache]
            [injest.path :refer [+> +>> x>> =>>]]
            bisect
            geogrid4image
            geogrid4seq
            svg2jfx
            matrix
            plot
            locations))

(def debug?
  true)

(def
  *selections
  (atom (fx/create-context {:window-width      1080.0
                            :row-height        360
                            :shoreline-filestr "./data/shoreline-coarse.json"
                            :contour-filestr   nil
                            :rain-dirstr       "/home/kxygk/Data/imerg/monthly/late/"
                            :elevation-filestr "./data/World_e-Atlas-UCSD_SRTM30-plus_v8.tif"
                            :cycle-length      12
                            :cycle-phase       0
                            :eas-res           0.1
                            :sou-res           0.1
                            ;; TODO Debug `krabi-region`. Axis labels float off from the map
                            :region-key            :krabi-root-2 #_locations/krabi-skinny-region
                            #_locations/eastern-korea #_locations/krabi-root-2 #_locations/himalaya #_locations/rift-valley-small 
                            :mouse-click       nil
                            :datafile-idxs     []
                            :noise-idxs     []}
                           #(cache/lru-cache-factory % :threshold 1000))))

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
  elevation-filestr
  [context]
  (fx/sub-val context
              :elevation-filestr))

(defn
  region-key
  [context]
  (fx/sub-val context
              :region-key))

(defn
  region
  [context]
  (get locations/regions
       (fx/sub-ctx context
                   region-key)))


;; DEBUG HELPERS *************************
(defn
  spitstream
  "Take a string, spit it to FILENAME
  and return the STR back
  ..
  NOTE: Argument order reverse from `spit`
  b/c the `spit` order is inconvenient for most pipelines"
  [string
   filename]
  (assert (instance? String
                     string))
  (if debug?
    (spit (str "./debug/"
               (-> @state/*selections
                   (fx/sub-ctx state/region-key)
                   symbol)
               "/"
               filename)
          string)
    nil)
  string)

(if debug?
  (->> (-> @state/*selections
           (fx/sub-ctx state/region-key))
       symbol
       (str "./debug/")
       (java.io.File.)
       (.mkdir)))
;; ***************************************


(defn
  cycle-length
  [context]
  (fx/sub-val context
              :cycle-length))

(defn
  cycle-phase
  [context]
  (fx/sub-val context
              :cycle-phase))

(defn-
  cycle-frac
  [cycle-length
   cycle-phase
   idx]
    (/ (mod (+ idx
               cycle-phase)
            cycle-length)
       cycle-length))

(defn
  window-width
  [context]
  (fx/sub-val context
              :window-width))

(defn
  display-width
  [context]
  (* 0.9
     (fx/sub-ctx context
                 window-width)))

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
                              0.95)
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
  (plot/worldmap-region (plot/shoreline-map locations/world-region
                                            (fx/sub-ctx context
                                                        shoreline-filestr)
                                            [])
                        (-> context
                            (fx/sub-ctx region))))
#_
(-> @state/*selections
    (fx/sub-ctx state/world-svg-hiccup))

(defn
  world-svg
  "Get a shoreline map of the whole world
  TODO: Maybe bake this in to the program?"
  [context]
  (-> context
      (fx/sub-ctx world-svg-hiccup)
      quickthing/svg2xml
      (spitstream "world.svg")))
#_
(-> @state/*selections
    (fx/sub-ctx state/world-svg))
#_
(spit "out/test-world.svg"
      (-> @state/*selections
          (fx/sub-ctx state/world-svg)))

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
  TODO: This could be a higher resolution than the world map
  TODO: RENAME TO SOMETHING LIKE CONTOUR"
  [context]
  (->
    (fx/sub-ctx context
                region)
    (plot/shoreline-map (fx/sub-ctx context
                                    shoreline-filestr)
                        {:axis-visible? false}))) ;; no POI

(defn
  region-svg
  "Get a shoreline map of the region of interest
  TODO: This could be a higher resolution than the world map"
  [context]
  (-> context
      (fx/sub-ctx region-svg-hiccup)
      quickthing/svg2xml
      (spitstream "region.svg")))
#_
(type (-> @state/*selections
          (fx/sub-ctx state/region-svg)))

(defn
  contour-map-svg
  [context]
  (->
    (fx/sub-ctx context
                region)
    (plot/shoreline-map (fx/sub-ctx context
                                    shoreline-filestr)
                        {:axis-visible? true})
    quickthing/svg2xml
    (spitstream "contour.svg")))
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
  region-geogrid-params
  "This is a bit of a convoluted way to calculate the parameters,
  but it's the only way to ensure they're correct
  b/c image reading and segmentation is complicated
  and it's all done in `geogrid/subregion`"
  [context]
  (->> (-> context
           (fx/sub-ctx datafile-strs)
           first)
       vector
       (map #(str (fx/sub-ctx context
                               data-dirstr)
                   %))
       (map #(geogrid4image/read-file %
                                       (fx/sub-ctx context
                                                   eas-res)
                                       (fx/sub-ctx context
                                                   sou-res)))
       (map #(geogrid/subregion %
                                 (fx/sub-ctx context
                                             region)))
       first
       geogrid/params))
#_
(-> @state/*selections
    (fx/sub-ctx state/region-geogrid-params))

(defn
  region-matrix
  "Matrix of all the data over a region
  Implementation is hidden in `matrix.clj`
  So that the underlying library can be swapped"
  [context]
  (->> (fx/sub-ctx context
                   datafile-strs)
       (map #(str (fx/sub-ctx context
                              data-dirstr)
                  %))
       (map #(geogrid4image/read-file % ;; TODO: Trans
                                      (fx/sub-ctx context
                                                  eas-res)
                                      (fx/sub-ctx context
                                                  sou-res)))
       (map #(geogrid/subregion %
                                (fx/sub-ctx context
                                            region)))
       matrix/from-geogrids))
#_
(-> @state/*selections
    (fx/sub-ctx state/region-matrix))
;; => {:matrix #RealGEMatrix[double, mxn:2500x119, layout:column, offset:0]
;;       ▥       ↓       ↓       ↓       ↓       ↓       ┓    
;;       →      54.00    2.00    ⁙      74.00   12.00         
;;       →      47.00    4.00    ⁙      83.00    9.00         
;;       →       ⁙       ⁙       ⁙       ⁙       ⁙            
;;       →     179.00   13.00    ⁙     266.00  326.00         
;;       →     214.00   13.00    ⁙     281.00  292.00         
;;       ┗                                               ┛    
;;    ,
;;     :dimension [50 50],
;;     :position {:eas 276.5, :sou 79.0},
;;     :resolution [0.1 0.1]}

(defn
  region-svd
  "the SVD of the region matrix"
  [context]
  (-> context
      (fx/sub-ctx region-matrix)
      matrix/svd))
#_
(-> @state/*selections
    (fx/sub-ctx state/region-svd)
    keys)
#_
(state/region-svd @state/*selections)

(defn
  region-min-max
  [context]
  (-> context
      (fx/sub-ctx region-matrix)
      matrix/get-min-max))
#_
(-> @state/*selections
    (fx/sub-ctx state/region-min-max))

#_
(-> @state/*selections
    (fx/sub-ctx state/region-matrix)
    :matrix
    (uncomplicate.neanderthal.linalg/svd  true
                                          true)
    (matrix/from-svd))

(defn
  noise-matrix-2d
  "Noise matrix when there are two climate systems
  and the two leading SV are removed from the data"
  [context]
  (-> context
      (fx/sub-ctx region-svd)
      (matrix/minus-2-sv)))
#_
(-> @state/*selections
    (fx/sub-ctx state/noise-matrix-2d)
    keys)
;; => (:sigma :u :vt :master :matrix :dimension :position :resolution)

(defn
  noise-svg
  "Get the noise background of one data point"
  [context
   id]
  (-> (matrix/extract-grid (fx/sub-ctx context
                                       noise-matrix-2d)
                           id)
      (plot/grid-map (fx/sub-ctx context
                                 region-svg-hiccup))
      quickthing/svg2xml
      (spitstream (str "noise-"
                       id
                       "file.svg"))))
#_
(-> @state/*selections
    (fx/sub-ctx state/noise-svg
                3))

(defn
  noise-1d-matrix
  "This is a noise matric when only the first EOF is removed
  This shouldn't be used normally..
  b/c the code is designed to run on two climate regions"
  [context]
  (-> context
      (fx/sub-ctx region-svd)
      (matrix/minus-1-sv)))
#_
(-> @state/*selections
    (fx/sub-ctx state/noise-matrix-1d)
    keys)

(defn
  eof1-weights
  "Get the EOF1 component at each data point (ie. point in time)"
  [context]
  (-> context
      (fx/sub-ctx region-svd)
      (matrix/svd-to-weights 0)))
#_
(-> @state/*selections
    (fx/sub-ctx state/eof1-weights)
    seq
    vec)


(defn
  noise-1d-min-max
  [context]
  (-> context
      (fx/sub-ctx noise-1d-matrix)
      matrix/get-min-max))
#_
(-> @state/*selections
    (fx/sub-ctx state/noise-1d-min-max))

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
#_
(fx/sub-ctx @state/*selections
            first-datafile-idx)

(defn-
  first-datafile-geogrid
  [context]
  (matrix/extract-grid (fx/sub-ctx context
                                   region-matrix)
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
                contour-map-svg)
    (-> (fx/sub-ctx context
                    first-datafile-geogrid)
        (plot/grid-map (fx/sub-ctx context
                                   region-svg-hiccup))
        quickthing/svg2xml
        (spitstream "first-data-file.svg"))))
#_
(-> @state/*selections
    (fx/sub-ctx state/first-datafile-svg))


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
  sv-weight
  "Get a particular singular vector
  Based on `index`"
  [context
   sv-index]
  (-> context
      (fx/sub-ctx sv-weights)
      (get sv-index)
      second))
#_
(state/sv-weight @state/*selections
                       1)
;; => 9247.099897276306

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
  (let [sv (fx/sub-ctx context
                       singular-vector
                       sv-index)]
    (geogrid4seq/build-grid (-> context
                                (fx/sub-ctx region-geogrid-params))
                            sv)))
#_
(-> @state/*selections
    (fx/sub-ctx state/singular-vector-geogrid
                0))
#_ ;; BROKEN
(spit
  (str "out/"
       (-> @state/*selections
           (fx/sub-ctx state/region-key)
           symbol )
       "/first-sv.svg")
  (quickthing/svg2xml
    (plot/grid-map
      (-> @state/*selections
          (fx/sub-ctx state/region))
      (state/region @state/*selections) ;;input-region ;; it'll crop redundantly here..
      "1st SV")))

(defn
  singular-vector-svg
  "Return an SVG of the singular vector INDEX
  If there are no datafiles and you can't do a SVD then return a region"
  [context
   sv-index]
  (if (empty? (fx/sub-ctx context
                          datafile-strs))
    (fx/sub-ctx context
                region-svg)
    (-> (fx/sub-ctx context
                    singular-vector-geogrid
                    sv-index)
        (plot/grid-map (fx/sub-ctx context
                                   region-svg-hiccup))
        quickthing/svg2xml
        (spitstream (str "sv-"
                         sv-index
                         ".svg")))))

(defn
  first-sv-svg
  [context]
  (-> context
      (fx/sub-ctx singular-vector-svg
                  0)
      (spitstream "first-sv.svg")))
#_
(-> @state/*selections
    (fx/sub-ctx state/first-sv-svg))

(defn
  second-sv-svg
  [context]
  (-> context
      (fx/sub-ctx singular-vector-svg
                  1)
      (spitstream "second-sv.svg")))
#_
(-> @state/*selections
    (fx/sub-ctx state/second-sv-svg))

(defn
  singular-vector-mixture
  "Mixture of the first two singular vectors"
  [context
   svec-one
   svec-two
   sval-one
   sval-two]
  (println (str "\nin `singular-vector-mixture`"
                "\nsvec-one"
                svec-one
                "\nsvec-two"
                svec-two
                "\nsval-one"
                sval-one
                "\nsval-two"
                sval-two))
  (let [svec1 (fx/sub-ctx context
                        singular-vector
                        0)
        svec2 (fx/sub-ctx context
                        singular-vector
                        1)]
    (let [mixture (mapv (fn [svec1-point
                             svec2-point]
                          (/ (+ (* svec1-point
                                   svec-one
                                   sval-one)
                                (* svec2-point
                                   svec-two
                                   sval-two))
                             2.0))
                        svec1
                        svec2)]
      mixture)))

(defn
  singular-vector-mixture-geogrid
  [context
   sv-one
   sv-two
   sval-one
   sval-two]
  (geogrid4seq/build-grid (-> context
                              (fx/sub-ctx region-geogrid-params))
                          (fx/sub-ctx context
                                      singular-vector-mixture
                                      sv-one
                                      sv-two
   sval-one
   sval-two)))
#_
(-> @state/*selections
    (fx/sub-ctx state/singular-vector-mixture-geogrid
                0.5
                0.5
                1.0
                1.0))

(defn
  singular-vector-mixture-svg
  [context
   sv-one
   sv-two
   sval-one
   sval-two]
  (-> (fx/sub-ctx context
                  singular-vector-mixture-geogrid
                  sv-one
                  sv-two
                  sval-one
                  sval-two)
      (plot/grid-map (fx/sub-ctx context
                                 region-svg-hiccup))
      quickthing/svg2xml))
#_
(spit (str "out/"
           (-> @state/*selections
               (fx/sub-ctx state/region-key)
               symbol )
           "/fiftyfifty.svg")
      (-> @state/*selections
          (fx/sub-ctx state/singular-vector-mixture-svg
                      0.5
                      0.5
                      1.0
                      1.0)))

(defn
  sv-proj
  [context]
  (let [frac-generator (partial cycle-frac
                                (fx/sub-ctx context
                                            cycle-length)
                                (fx/sub-ctx context
                                            cycle-phase))]
  (->> (-> context
           (fx/sub-ctx region-matrix)
           matrix/svd
           matrix/svd-to-2d-sv-space)
       (map-indexed (fn add-cycle-frac
                      [idx point]
                      (conj point
                            {:cycle-frac (frac-generator idx)}))))))
#_
(-> @state/*selections
    (fx/sub-ctx state/sv-proj))

(defn
  sv-bisection
  [context]
  (-> context
      (fx/sub-ctx sv-proj)
      bisect/min-var))
#_
(-> @state/*selections
    (fx/sub-ctx state/sv-bisection)
    keys)
;; => (:angle :points-a :points-b :centroid-a :centroid-b)
#_
(let [{:keys [centroid-b]} (fx/sub-ctx @state/*selections
                                       state/sv-bisection)]
  (let [[x-coord
         y-coord] centroid-b]
    (/ x-coord
       (+ x-coord
          y-coord))))


(defn
  first-pattern
  [context]
  (let [{:keys [centroid-a]} (fx/sub-ctx context
                                         state/sv-bisection)]
    (let [sval-one (-> context
                          (fx/sub-ctx state/sv-weight 0))
          sval-two (-> context
                          (fx/sub-ctx state/sv-weight 1))
          [x-coord
           y-coord] centroid-a]
      (fx/sub-ctx context
                  singular-vector-mixture
                  x-coord
                  y-coord
                  sval-one
                  sval-two))))
#_
(-> @state/*selections
    (fx/sub-ctx state/first-pattern))

(defn
  first-pattern-svg
  [context]
  (-> (geogrid4seq/build-grid (-> context
                                  (fx/sub-ctx region-geogrid-params))
                              (-> context
                                  (fx/sub-ctx first-pattern)))
      (plot/grid-map (fx/sub-ctx context
                                 region-svg-hiccup))
      quickthing/svg2xml
      (spitstream "first-pattern.svg")))
#_
(-> @state/*selections
    (fx/sub-ctx state/first-pattern-svg))

(defn
  second-pattern
  [context]
  (let [{:keys [centroid-b]} (fx/sub-ctx context
                                         state/sv-bisection)]
    (let [sval-one (-> context
                          (fx/sub-ctx state/sv-weight 0))
          sval-two (-> context
                       (fx/sub-ctx state/sv-weight 1))
          [x-coord
           y-coord] centroid-b]
      (println (str "\nSecond Pattern Centroid: \n"
                    centroid-b
                    "\n"
                    "\nWeight 1: "
                    sval-one
                    "\nWeight 2: "
                    sval-two
                    "\nCoords: "
                    x-coord
                    "  "
                    y-coord))
      (fx/sub-ctx context
                  singular-vector-mixture
                  x-coord
                  y-coord
                  sval-one
                  sval-two))))
#_
(-> @state/*selections
    (fx/sub-ctx state/second-pattern))


(defn
  second-pattern-svg
  [context]
  (-> (geogrid4seq/build-grid (-> context
                                  (fx/sub-ctx region-geogrid-params))
                              (-> context
                                  (fx/sub-ctx second-pattern)))
      (plot/grid-map (fx/sub-ctx context
                                 region-svg-hiccup))
      quickthing/svg2xml
      (spitstream "second-pattern.svg")))
#_
(-> @state/*selections
    (fx/sub-ctx state/second-pattern-svg))


(defn
  pattern-proj
  "Projections of all the data
  onto the two extracted patterns
  Returns:
  a list of 2D points in the nonorthogonal coordinate system"
  [context]
  (let [{:keys [centroid-a
                centroid-b
                points]} (fx/sub-ctx context
                                     sv-bisection)]
    (let [projections (matrix/project-onto-2d-basis  centroid-a
                                                     centroid-b
                                                     points)]
      projections)))
    #_
(-> @state/*selections
    (fx/sub-ctx state/pattern-proj))


(defn
  pattern-proj-partitioned
  "Projections of all the data
  onto the two extracted patterns, but partitioned into two groups
  Returns:
  a two list of nonorthogonal coordinates"
  [context]
  (let [projections (fx/sub-ctx context
                                pattern-proj)
        proj-a (->> projections
                    (mapv (fn [proj]
                            (if (-> proj
                                    (get 2)
                                    :above?
                                    #_not)
                              (first proj)
                              0)))
                    (mapv (fn [proj]
                            (if (pos? proj)
                              proj
                              0.0))))
        proj-b (->> projections
                    (mapv (fn [proj]
                            (if (-> proj
                                    (get 2)
                                    :above?
                                    not)
                              (second proj)
                              0)))
                    (mapv (fn [proj]
                            (if (pos? proj)
                              proj
                              0.0))))]
    [proj-a
     proj-b]))
    #_
(-> @state/*selections
    (fx/sub-ctx state/pattern-proj-partitioned))

(defn
  pattern-proj-svg
  "Plot of the climate indeces"
  [context]
  (let [[proj-a
         proj-b] (fx/sub-ctx context
                             pattern-proj-partitioned)]
    (-> (plot/indeces (* 1.0
                         (fx/sub-ctx context
                                     state/window-width))
                      (* 1.0
                         (fx/sub-ctx context
                                     state/row-height))
                      proj-a
                      proj-b
                      2011
                      (fx/sub-ctx context
                                  cycle-length)
                      (fx/sub-ctx context
                                  cycle-phase))
        quickthing/svg2xml
        (spitstream "indeces.svg"))))
#_
(-> @state/*selections
    (fx/sub-ctx state/pattern-proj-svg))

(defn
  sv-proj-svg
  [context]
  (-> context
      (cljfx.api/sub-ctx sv-proj)
      (plot/sv-plot (* 0.5
                       (fx/sub-ctx context
                                   window-width))
                    (* 2.0
                       (fx/sub-ctx context
                                   row-height)))
      quickthing/svg2xml
      (spitstream "sv-projs.svg")))
#_
(-> @state/*selections
    (fx/sub-ctx state/sv-proj-svg))

(defn
  sv-weights-stats
  [context]
  (-> context
      (fx/sub-ctx sv-weights)
      matrix/singular-values-stats))
#_
(-> @state/*selections
    (fx/sub-ctx state/sv-weights-stats))

(defn
  sv-weights-svg
  [context]
  (-> (plot/sv-weights (take 10
                             (fx/sub-ctx context
                                         sv-weights))
                       (fx/sub-ctx context
                                   sv-weights-stats)
                       (* 0.5
                          (fx/sub-ctx context
                                      state/window-width))
                       (* 2.0
                          (fx/sub-ctx context
                                      state/row-height)))
      quickthing/svg2xml
      (spitstream "sv-weights.svg")))
#_
(spit (str "out/"
           (-> @state/*selections
               (fx/sub-ctx state/region-key)
               symbol)
           "/test-weights-actual.svg")
      (-> @state/*selections
          (fx/sub-ctx state/sv-weights-svg)))

(defn
  cycle-group-svg
  [context
   cycle-idx]
  (let [cycle-phase  (fx/sub-ctx context
                                 cycle-phase)
        cycle-length (fx/sub-ctx context
                                 cycle-length)]
    (let [cycle-start (+ cycle-phase
                         (* cycle-idx
                            cycle-length))]
      (let [cycle-end (+ cycle-start
                         cycle-length)]
        (-> (->> (range cycle-start
                        cycle-end)
                 (mapv (partial matrix/extract-grid
                                (fx/sub-ctx context
                                            region-matrix)))
                 (map-indexed (fn grids-to-maps
                                [idx
                                 grid]
                                (plot/grid-map grid
                                               (fx/sub-ctx context
                                                           region-svg-hiccup)
                                               {:label-top-right (str (inc idx))
                                                :cycle-frac      (/ idx
                                                                    12.0)})))
                 (into []))
            (plot/cyclic (clojure.math/ceil (clojure.math/pow cycle-length
                                                              0.5)))
            quickthing/svg2xml
            (spitstream "cycle.svg"))))))
#_
(-> @state/*selections
    (fx/sub-ctx state/cycle-group-svg
                0))

(def
  month-map {1  "Jan"
             2  "Feb"
             3  "Mar"
             4  "Apr"
             5  "May"
             6  "Jun"
             7  "Jul"
             8  "Aug"
             9  "Sep"
             10 "Oct"
             11 "Nov"
             12 "Dec"})

(defn
  annual-cycle
  [context
   year-idx]
  (let [cycle-phase  (fx/sub-ctx context
                                 cycle-phase)
        cycle-length (fx/sub-ctx context
                                 cycle-length)]
    (let [cycle-start (+ cycle-phase
                         (* year-idx
                            cycle-length))]
      (let [cycle-end (+ cycle-start
                         cycle-length)]
        (-> (->> (range cycle-start
                        cycle-end)
                 (mapv (partial matrix/extract-grid
                                (fx/sub-ctx context
                                            region-matrix))) ;;))))))
                 (map-indexed (fn grids-to-maps
                                [idx
                                 grid]
                                (plot/grid-map grid
                                               (fx/sub-ctx context
                                                           region-svg-hiccup)
                                               {:label-top-right (get month-map
                                                                      (inc idx))
                                                :label-attribs {:font-size 1.5}
                                                :cycle-frac      (/ idx
                                                                    12.0)})))
                 (into []))
            plot/annual-12-month-ring
            quickthing/svg2xml
            (spitstream (str "year"
                             year-idx
                             ".svg")))))))
#_
(-> @state/*selections
    (fx/sub-ctx state/annual-cycle
                0))

#_
(defn
  elevation-geogrid
  "A vector of all the images of the region of interest
  in the same order as the file listing.
  Reading and cropping all the images take a min or two "
  [context]
  (-> (fx/sub-ctx context
                  elevation-filestr)
      (geogrid4image/read-file (fx/sub-ctx context
                                           eas-res)
                               (fx/sub-ctx context
                                           sou-res))
      #_
      (geogrid/subregion (fx/sub-ctx context
                                     region))))
#_
(geogrid4image/read-file (str "/home/kxygk/Projects/imergination/data/"
                              "World_e-Atlas-UCSD_SRTM30-plus_v8.tif")
                          (fx/sub-ctx @state/*selections
                                      eas-res)
                          (fx/sub-ctx @state/*selections
                                      sou-res))
#_
(.getType (fx/sub-ctx @state/*selections
            elevation-geogrid))


;; DIAGNOSTIC CHARTS
;;
;; ONLY MAKES SENSE IN 1 CLIMATE REGIONS
(defn-
  var-from-zero
  [data-seq]
  (let [num (count data-seq)]
    (/ (->> data-seq
            (reduce #(+ (abs %1)
                        (abs %2))))
       num)))

(defn-
  var-from-mean
  [data-seq]
  (let [
        num (count data-seq)
        mean (/ (->> data-seq
                     (reduce +))
                num)] ;; N or N-1 ?
    (/ (->> data-seq
            (reduce #(+ (abs (- %1
                                mean))
                        (abs %2))))
       num)))

(defn
  eof1weight-vs-variance
  "Return a pair of the eof1weight and variance
   (relative to the EOF1 signal)
  for a given INDEX (ie. time point)"
  [context]
  (let [eof1-weight (-> context
                        (fx/sub-ctx state/sv-weight 0))
        eof1-components (->> (fx/sub-ctx context
                                         state/eof1-weights)
                             seq
                             vec
                             (mapv #(* %
                                       -1.0
                                       eof1-weight)))
        num-timesteps   (count eof1-components)]
    (let [var-matrix           (-> context
                                   (fx/sub-ctx #_state/region-matrix state/noise-1d-matrix)
                                   :matrix)
          variations-from-mean (->> num-timesteps
                                    range
                                    (mapv #(-> var-matrix
                                               (uncomplicate.neanderthal.core/col %)
                                               seq
                                               vec
                                               var-from-mean
                                               (clojure.math/pow 2))))]
      (mapv vector
            eof1-components
            variations-from-mean))))

(defn
  eof1-vs-var-svg
  "Plot and stream to file"
  [context]
(-> context
    (fx/sub-ctx eof1weight-vs-variance)
    (plot/eof1-vs-var (-> @state/*selections
                          (fx/sub-ctx state/region-key)
                          str)
                      1000 ;; needs values for graphic
                      1000)
    quickthing/svg2xml
    (spitstream "eof1-vs-var.svg")))
;;  Not in GUI display, so run code to save SVG to file
(fx/sub-ctx @state/*selections
            eof1-vs-var-svg)
