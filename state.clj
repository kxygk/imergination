(ns
    state
  "Program and GUI state"
  (:require [clojure.java.io :as io]
            [cljfx.api       :as fx]
            [clojure.core.cache :as cache]
            [injest.path :refer [+> +>> x>> =>>]]
            bisect
            geogrid4image
            svg2jfx
            matrix
            plot
            locations))

(def debug?
  true)

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
               filename)
          string)
    nil)
  string)

(def
  *selections
  (atom (fx/create-context {:window-width      1080.0
                            :row-height        360
                            :shoreline-filestr "./data/shoreline-coarse.json"
                            :contour-filestr   nil
                            :rain-dirstr       "/home/kxygk/Data/imerg/monthly/late/"
                            :eas-res           0.1
                            :sou-res           0.1
                            :region            locations/krabi-skinny-region
                            :mouse-click       nil
                            :datafile-idxs     []
                            :noise-idxs     []}
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
#_
(-> @state/*selections
    (fx/sub-ctx state/region)
    geoprim/dimension)


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
      quickthing/svg2xml
      (spitstream "region.svg")))
#_
(type (-> @state/*selections
          (fx/sub-ctx state/region-svg)))

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
  region-matrix
  "Matrix of all the data over a region
  Implementation is hidden in `matrix.clj`
  So that the underlying library can be swapped"
  [context]
  (-> context
      (fx/sub-ctx state/region-geogrids)
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


#_
(-> @state/*selections
    (fx/sub-ctx state/region-matrix)
    :matrix
    (uncomplicate.neanderthal.linalg/svd  true
                                          true)
    (matrix/from-svd))
;; => #RealGEMatrix[double, mxn:2500x119, layout:column, offset:0]
;;       ▥       ↓       ↓       ↓       ↓       ↓       ┓    
;;       →      54.00    2.00    ⁙      74.00   12.00         
;;       →      47.00    4.00    ⁙      83.00    9.00         
;;       →       ⁙       ⁙       ⁙       ⁙       ⁙            
;;       →     179.00   13.00    ⁙     266.00  326.00         
;;       →     214.00   13.00    ⁙     281.00  292.00         
;;       ┗                                               ┛

#_
(-> @state/*selections
    (fx/sub-ctx state/region-matrix)
    (matrix/svd)
    (matrix/from-svd))
;; => #RealGEMatrix[double, mxn:2500x119, layout:column, offset:0]
;;       ▥       ↓       ↓       ↓       ↓       ↓       ┓    
;;       →      54.00    2.00    ⁙      74.00   12.00         
;;       →      47.00    4.00    ⁙      83.00    9.00         
;;       →       ⁙       ⁙       ⁙       ⁙       ⁙            
;;       →     179.00   13.00    ⁙     266.00  326.00         
;;       →     214.00   13.00    ⁙     281.00  292.00         
;;       ┗                                               ┛    
#_
(-> @state/*selections
    (fx/sub-ctx state/region-svd)
    (matrix/from-svd))
;; => #RealGEMatrix[double, mxn:2500x119, layout:column, offset:0]
;;       ▥       ↓       ↓       ↓       ↓       ↓       ┓    
;;       →      19.54  -18.37    ⁙      -5.33    7.28         
;;       →      14.09  -15.72    ⁙       6.97    6.37         
;;       →       ⁙       ⁙       ⁙       ⁙       ⁙            
;;       →      57.02  -22.56    ⁙      21.29   44.10         
;;       →      80.54  -25.78    ⁙      13.39  -17.36         
;;       ┗                                               ┛

(defn
  noise-matrix
  [context]
  (-> context
      (fx/sub-ctx region-svd)
      (matrix/minus-2-sv)))
#_
(-> @state/*selections
    (fx/sub-ctx state/noise-matrix)
    keys)
;; => (:sigma :u :vt :master :matrix :dimension :position :resolution)

(defn
  noise-svg
  "Get the noise background of one data point"
  [context
   id]
  (-> (matrix/extract-grid (fx/sub-ctx context
                                       noise-matrix)
                           id)
      (plot/grid-map (fx/sub-ctx context
                                 region-svg-hiccup)
                     []) ;; no POI
      quickthing/svg2xml
      (spitstream (str "noise-"
                       id
                       "file.svg"))))
#_
(-> @state/*selections
    (fx/sub-ctx state/noise-svg
                3))

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
                region-svg)
    (-> (fx/sub-ctx context
                    first-datafile-geogrid)
        (plot/grid-map (fx/sub-ctx context
                                   region-svg-hiccup)
                       []) ;; no POI
        quickthing/svg2xml
        (spitstream "first-data-file.svg"))))
#_
(-> @state/*selections
    (fx/sub-ctx state/first-datafile-svg))

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
   sv-one
   sv-two]
  (let [sv1 (fx/sub-ctx context
                        singular-vector
                        0)
        sv2 (fx/sub-ctx context
                        singular-vector
                        1)]
    (let [mixture (mapv (fn [sv1-point
                             sv2-point]
                          (/ (+ (* sv1-point
                                   sv-one)
                                (* sv2-point
                                   sv-two))
                             2.0))
                        sv1
                        sv2)]
      mixture)))

(defn
  singular-vector-mixture-geogrid
  [context
   sv-one
   sv-two]
  (geogrid4seq/build-grid (-> context
                              (fx/sub-ctx region-geogrids)
                              first)
                          (fx/sub-ctx context
                                      singular-vector-mixture
                                      sv-one
                                      sv-two)))
#_
(-> @state/*selections
    (fx/sub-ctx state/singular-vector-mixture-geogrid
                0.5))

(defn
  singular-vector-mixture-svg
  [context
   sv-one
   sv-two]
  (-> (fx/sub-ctx context
                  singular-vector-mixture-geogrid
                  sv-one
                  sv-two)
      (plot/grid-map (fx/sub-ctx context
                                 region-svg-hiccup)
                     []) ;; no POI
      quickthing/svg2xml))
#_
(spit "out/fiftyfifty.svg"
      (-> @state/*selections
          (fx/sub-ctx state/singular-vector-mixture-svg
                      0.5
                      0.5)))

(defn
  sv-proj
  [context]
  (-> context
      (fx/sub-ctx region-matrix)
      matrix/svd
      matrix/svd-to-2d-sv-space))
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
    (let [[x-coord
           y-coord] centroid-a]
      (fx/sub-ctx context
                  singular-vector-mixture
                  x-coord
                  y-coord))))
#_
(-> @state/*selections
    (fx/sub-ctx state/first-pattern))

(defn
  first-pattern-svg
  [context]
  (-> (geogrid4seq/build-grid (-> context
                                  (fx/sub-ctx region-geogrids)
                                  first)
                              (-> context
                                  (fx/sub-ctx first-pattern)))
      (plot/grid-map (fx/sub-ctx context
                                 region-svg-hiccup)
                     []) ;; no POI
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
    (let [[x-coord
           y-coord] centroid-b]
      (fx/sub-ctx context
                  singular-vector-mixture
                  x-coord
                  y-coord))))
#_
(-> @state/*selections
    (fx/sub-ctx state/second-pattern))


(defn
  second-pattern-svg
  [context]
  (-> (geogrid4seq/build-grid (-> context
                                  (fx/sub-ctx region-geogrids)
                                  first)
                              (-> context
                                  (fx/sub-ctx second-pattern)))
      (plot/grid-map (fx/sub-ctx context
                                 region-svg-hiccup)
                     []) ;; no POI
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
  pattern-proj-svg
  "Plot of the climate indeces"
  [context]
  (let [projections (fx/sub-ctx context
                                pattern-proj)
        proj-a (->> projections
                    (mapv (fn [proj]
                            (if (-> proj
                                    (get 2)
                                    :above?
                                    not)
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
                                    :above?)
                              (second proj)
                              0)))
                    (mapv (fn [proj]
                            (if (pos? proj)
                              proj
                              0.0))))]
    (-> (plot/indeces (* 1.0
                         (fx/sub-ctx context
                                 state/window-width))
                      (* 1.0
                         (fx/sub-ctx context
                                     state/row-height))
                      proj-a
                      proj-b)
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
                                   state/window-width))
                    (* 2.0
                       (fx/sub-ctx context
                                   state/row-height)))
      quickthing/svg2xml
      (spitstream "sv-projs.svg")))
#_
(-> @state/*selections
    (fx/sub-ctx state/sv-proj-svg))

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
(spit "out/test-weights-actual.svg"
      (-> @state/*selections
          (fx/sub-ctx state/sv-weights-svg)))
