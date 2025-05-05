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
  (atom (fx/create-context {:window-width                   1080.0
                            :row-height                     360
                            :shoreline-filestr              "./data/shoreline-coarse.json"
                            :contour-filestr                nil
                            :rain-dirstr
                            #_
                            "/home/kxygk/Data/imerg/daily/late-more/"
                            #_
                            "/home/kxygk/Data/era5/hourly-201103/rot/"
                            #_
                            "/home/kxygk/Data/imerg/30min-subset/"
                            #_
                            "/home/kxygk/Data/era5/monthly/rot/"
                            #_
                            "/home/kxygk/Projects/raingrid/out/"
                            #_
                            "/home/kxygk/Projects/raingrid/out-pat1/"
                            #_
                            "/home/kxygk/Data/imerg/daily/late/"
                            ;;#_
                            "/home/kxygk/Data/imerg/monthly/late/"
                            :elevation-filestr              "./data/World_e-Atlas-UCSD_SRTM30-plus_v8.tif"
                            :cycle-length                   12;;24 ;;48
                            :cycle-phase                    0
                            :eas-res
                            ;;0.25
                            0.1
                            :sou-res
                            ;;0.25
                            0.1
                            ;; TODO Debug `krabi-region`. Axis labels float off from the map
                            :region                         nil
                            :region-key
                            ;;;;;;;;;;;;;;;;;;;;;;;
                            #_:world
                            #_:ocean1small
                            #_:ocean1small-1pat
                            #_:ocean1large
                            #_:ocean1largeextra
                            #_:ocean1largeextraextra
                            #_extraextraextra
                            #_:ocean2
                            #_:ocean1
                            #_:ghana-large
                            #_:togo
                            #_:jos
                            #_:taipei-region
                            #_:udaipur
                            #_:marrah
                            #_:marrah-big
                            #_:sichuan-wall
                            :krabi-root-2
                            #_:krabi-root-2-era5
                            #_:krabi-root-2-diurnal
                            #_:java
                            #_:java-era5
                            #_:birdhead
                            #_krabi-skinny-region
                            #_eastern-korea
                            #_:himalaya
                            #_:rift-valley-small
                            ;;;;;;;;;;;;;;;;;;;;;;;
                            :is-in-ram                      false
                            :mouse-click                    nil
                            :datafile-idxs                  [0]
                            :sv-selected-idxs               [0]
                            :noise-selected-idxs            [0]
                            :normalized-noise-selected-idxs [0]}
                           #(cache/lru-cache-factory % :threshold 1000))))

(defn
  is-in-ram
  [context]
  (fx/sub-val context
              :is-in-ram))
#_
(-> @state/*selections
    (fx/sub-ctx state/is-in-ram))

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
#_
(-> @state/*selections
    (fx/sub-ctx state/region-key))
;; => :krabi-root-2

(defn
  region
  [context]
  (if (nil? (fx/sub-ctx context
                        region-key))
    (fx/sub-val context
                :region)
    (:region (get locations/regions
                  (fx/sub-ctx context
                              region-key)))))
#_
(-> @state/*selections
    (fx/sub-ctx state/region))
;; => {:norwes {:eas 298.57142857142856, :sou 62.857142857142854},
;;     :soueas {:eas 303.4285714285714, :sou 69.14285714285714}}
;; => {:norwes {:eas 277.0, :sou 76.92893218813452},
;;     :soueas {:eas 281.0, :sou 84.0}}
;; => {:norwes {:eas 277.0, :sou 76.92893218813452},
;;     :soueas {:eas 281.0, :sou 84.0}}

(defn
  region-meta
  "Other data attached to the region"
  [context]
  (get locations/regions
       (fx/sub-ctx context
                   region-key)))
#_
(-> @state/*selections
    (fx/sub-ctx state/region-meta))

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
  (let [region-key (-> @state/*selections
                       (fx/sub-ctx state/region-key))
        subfolder  (if (nil? region-key)
                     "custom"
                     (symbol region-key))]
    (if debug?
      (future (spit (str "../imergination.wiki/"
                         subfolder
                         "/"
                         filename)
                    string))
      nil)
    string))

(if debug?
  (->> (-> @state/*selections
           (fx/sub-ctx state/region-key))
       symbol
       (str "../imergination.wiki/")
       (java.io.File.)
       (.mkdir)))

(defn
  spitsvgstream
  "Take an SVG hiccup
  Render it to XML and same to the `filename`
  And return the hiccup"
  [svg-hiccup
   filename]
  (println (str "Writing out SVG: "
                filename))
  (spitstream (-> svg-hiccup
                  quickthing/svg2xml)
              filename)
  svg-hiccup)
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
  (* 1
     (fx/sub-ctx context
                 window-width)))
#_
(-> @state/*selections
    (fx/sub-ctx state/display-width))

(defn
  region-xy-ratio
  [context]
  (let [[lat
         lon] (-> context
                  (fx/sub-ctx region)
                  geoprim/dimension)]
    (/ lat
       lon)))
#_
(-> @state/*selections
    (fx/sub-ctx region-xy-ratio))
#_
(-> @state/*selections
    (fx/sub-ctx state/region))
;; => {:norwes {:eas 297.66666666666663, :sou 136.33333333333331},
;;     :soueas {:eas 297.66666666666663, :sou 136.33333333333331}}
;;  geoprim/dimension)


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
#_
(-> @state/*selections
    (fx/sub-ctx state/region-display-width))

(defn region-display-height
  [context]
  (/ (fx/sub-ctx context
                 region-display-width)
     (fx/sub-ctx context
                 region-xy-ratio)))
#_
(-> @state/*selections
    (fx/sub-ctx state/region-display-height))

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
#_
(-> @state/*selections
    (fx/sub-ctx state/sou-res))

(defn-
  world-svg-hiccup
  [context]
  (plot/worldmap-region (plot/shoreline-map locations/world-region
                                            (fx/sub-ctx context
                                                        shoreline-filestr)
                                            [])
                        (-> context
                            (fx/sub-ctx region))
                        {:display-width (fx/sub-ctx context
                                                    display-width)}))
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
      (spitsvgstream "world.svg")))
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
      (spitsvgstream "region.svg")))
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
                        {:axis-visible? true
                         :display-width (fx/sub-ctx context
                                                    region-display-width)})
    (spitsvgstream "contour.svg")))
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
  (->> ^String ;; I forget why I type hint..
       (fx/sub-ctx
         context
         data-dirstr)
       java.io.File.
       .list
       sort))
#_
(->> (fx/sub-ctx @state/*selections
                 state/datafile-strs)
     count)
#_
(->> (fx/sub-ctx @state/*selections
                 state/datafile-strs)
     (map-indexed (fn append-index
                    [index
                     file-str]
                    (str "["
                         index
                         "]"
                         file-str)))
     vec)

;; TODO Test out with caching the whole dataset..
;; see if the computer catches fire.. EDIT: it did
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
  sv-strs
  "Gets listing of all possible SVs"
  [context]
  (let [num-of-svs (-> context
                       (fx/sub-ctx datafile-strs)
                       count)]
    (let [svs        (-> num-of-svs
                         range)
          max-digits (-> num-of-svs
                         clojure.math/log10
                         clojure.math/ceil
                         int)]
      (mapv (fn [svindex]
              (str "SV "
                   (format (str "%0"
                                max-digits
                                "d")
                           svindex)))
            svs))))
#_
(-> @state/*selections
    (fx/sub-ctx state/sv-strs))
#_
(-> @state/*selections
    (fx/sub-ctx datafile-strs)
    count
    clojure.math/log10
    clojure.math/ceil
    int)

#_
(defn
  sv-strs
  "Gets listing of all possible SVs"
  [context]
  (let [num-of-svs (-> context
                       (fx/sub-ctx datafile-strs)
                       count)]
    (let [svs        (-> num-of-svs
                         range)
          max-digits (-> num-of-svs
                         clojure.math/log10
                         clojure.math/ceil
                         int)]
      (mapv (fn [svindex]
              (str "SV "
                   (format (str "%0"
                                max-digits
                                "d")
                           svindex)))
            svs))))

(defn-
  lazy-world-reader
  "Returns a lazy collection for reading in all the rainmaps"
  [file-strs
   datadir-str
   easres
   soures]
  (map #(do #_(println (str "Reading in .. "
                            % ))
            (geogrid4image/read-file (str datadir-str
                                          % )
                                     easres
                                     soures))
       file-strs))

(defn
  world-geogrid-vec
  "All the data..."
  [context]
  (-> context
      (fx/sub-ctx datafile-strs)
      (lazy-world-reader (fx/sub-ctx context
                                     data-dirstr)
                         (fx/sub-ctx context
                                     eas-res)
                         (fx/sub-ctx context
                                     sou-res))))
#_
(realized? (-> @state/*selections
               (fx/sub-ctx state/world-geogrid-vec)))
#_
(-> @state/*selections
    (fx/sub-ctx is-in-ram))

(defn
  region-geogrid-vec
  "TODO: Ideally this could be removed entirely..
  All this data is in the `region-matrix`
  The problem is I used it in two places
  `region-geogrid-params`
  and
  `region-matrix`
  ..
  It's be best if it was only a transient data structure in `region-matrix`
  and params were deduces otherwise"
  [context]
  #_ ;; TODO. This probably does the same thing...
  (-> context
      (fx/sub-ctx region-matrix)
      matrix/to-geogrid-vec)
  (let [myregion (fx/sub-ctx context
                             region)]
    (if (fx/sub-ctx context
                    is-in-ram)
      (->> (fx/sub-ctx context
                       world-geogrid-vec)
           (map #(do #_(println "\nCutting out region ..")
                     (geogrid/subregion %
                                        myregion))))
      (->> (world-geogrid-vec context)
           (map #(do #_(println "\nCutting out region ..")
                     (geogrid/subregion %
                                        myregion)))))))
#_
(-> @state/*selections
    (fx/sub-ctx state/region-geogrid-vec))

(defn
  region-matrix
  "Matrix of all the data over a region
  Implementation is hidden in `matrix.clj`
  So that the underlying library can be swapped"
  [context]
  (->> (fx/sub-ctx context
                   region-geogrid-vec)
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
#_
(geogrid4image/read-file (->> (fx/sub-ctx @state/*selections
                                          datafile-strs)
                              (map #(str (fx/sub-ctx @state/*selections
                                                     data-dirstr)
                                         %))
                              first)
                         (fx/sub-ctx @state/*selections
                                     eas-res)
                         (fx/sub-ctx @state/*selections
                                     sou-res))

(defn
  region-geogrid-params
  "This is a bit of a convoluted way to calculate the parameters,
  but it's the only way to ensure they're correct
  b/c image reading and segmentation is complicated
  and it's all done in `geogrid/subregion`"
  [context]
  (->> (fx/sub-ctx context
                   region-matrix)
       matrix/extract-params))
#_
(-> @state/*selections
    (fx/sub-ctx state/region-geogrid-params))
;; => [40 71 0.1 0.1 {:eas 277.0, :sou 76.9}]

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
  noise-vars
  "Calculate the variances of the noise in the columns/datapoints"
  [context]
  (-> context
      (fx/sub-ctx noise-matrix-2d)
      :matrix
      (matrix/colvars)))
#_
(take 12
      (-> @state/*selections
          (fx/sub-ctx state/noise-vars)))

(defn
  num-points
  [context]
  (let [num-pix (apply *
                       (-> context
                           (fx/sub-ctx noise-matrix-2d)
                           :dimension))]
    (->> noise-vars
         (fx/sub-ctx context)
         (map #(/ %
                  num-pix)))))
#_
(take 12
      (-> @state/*selections
          (fx/sub-ctx state/num-points)))

(defn
  noise-svg
  "Get the noise background of one data point"
  [context
   id]
  (-> (matrix/extract-grid (fx/sub-ctx context
                                       noise-matrix-2d)
                           id)
      (plot/grid-map (fx/sub-ctx context
                                 region-svg-hiccup)
                     {:display-width (fx/sub-ctx context
                                                 region-display-width)})
      (spitsvgstream (str "noise-"
                          id
                          "file.svg"))))
#_
(-> @state/*selections
    (state/noise-svg 31))

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
  sv-weights
  "Get the SV components at each data point (ie. point in time)
  This is zero indexed
  So SV1 is index 0"
  [context
   sv-index]
  (-> context
      (fx/sub-ctx region-svd)
      (matrix/svd-to-weights sv-index)))
#_
(-> @state/*selections
    (fx/sub-ctx state/sv-weights 0))

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
#_
(fx/sub-ctx @state/*selections
            datafile-idxs)

(defn
  first-datafile-idx
  "Get the first selected data index
  Which in effect meaning the ~earliest~ in the list
  Or the lowest value"
  [context]
  (first (fx/sub-ctx context
                     datafile-idxs)))
#_
(fx/sub-ctx @state/*selections
            first-datafile-idx)

(defn
  sv-selected-idxs
  "Indeces of the data that's been selected"
  [context]
  (fx/sub-val context
              :sv-selected-idxs))
#_
(fx/sub-ctx @state/*selections
            datafile-idxs)

(defn
  first-sv-selected-idx
  "Get the first selected data index
  Which in effect meaning the ~earliest~ in the list
  Or the lowest value"
  [context]
  (first (fx/sub-ctx context
                     sv-selected-idxs)))
#_
(fx/sub-ctx @state/*selections
            first-datafile-idx)

(defn
  noise-selected-idxs
  "Indeces of the data that's been selected"
  [context]
  (fx/sub-val context
              :noise-selected-idxs))
#_
(fx/sub-ctx @state/*selections
            noise-selected-idxs)

(defn
  first-noise-selected-idx
  "Get the first selected data index
  Which in effect meaning the ~earliest~ in the list
  Or the lowest value"
  [context]
  (first (fx/sub-ctx context
                     noise-selected-idxs)))
#_
(fx/sub-ctx @state/*selections
            first-noise-selected-idx)

(defn
  normalized-noise-selected-idxs
  "Indeces of the data that's been selected"
  [context]
  (fx/sub-val context
              :normalized-noise-selected-idxs))
#_
(fx/sub-ctx @state/*selections
            normalized-noise-selected-idxs)

(defn
  first-normalized-noise-selected-idx
  "Get the first selected data index
  Which in effect meaning the ~earliest~ in the list
  Or the lowest value"
  [context]
  (first (fx/sub-ctx context
                     normalized-noise-selected-idxs)))
#_
(fx/sub-ctx @state/*selections
            first-normalized-noise-selected-idx)


(defn-
  datafile-geogrid
  [context
   id]
  (matrix/extract-grid (fx/sub-ctx context
                                   region-matrix)
                       id))
#_
(apply max
       (-> @state/*selections
           (fx/sub-ctx state/datafile-geogrid
                       9)
           :data-array
           vec))

;; => 30174.0
;; => -32344.0
#_
(-> @state/*selections
    (fx/sub-ctx region-matrix)
    :matrix
    uncomplicate.neanderthal.core/amax)

(defn
  average-geogrid
  [context]
  (-> context
      (fx/sub-ctx region-matrix)
      matrix/data-average-geogrid
      (plot/grid-map (fx/sub-ctx context
                                 region-svg-hiccup)
                     {:label-top-right "Average"
                      :display-width   (fx/sub-ctx context
                                                   region-display-width)})
      (spitsvgstream (str "average"
                          ".svg"))))
;;#_ ;;unused
(-> @state/*selections
    (state/average-geogrid))

(defn
  datafile-svg
  "Get an SVG of a given datafile
  (ex: day, month)
  Selected with an ID index"
  [context
   id]
  (println (str "svg of file: "
                id))
  (-> (fx/sub-ctx context
                  datafile-geogrid
                  id)
      (plot/grid-map (fx/sub-ctx context
                                 region-svg-hiccup)
                     {:display-width (fx/sub-ctx context
                                                 region-display-width)})
      (spitsvgstream (str "data-file-"
                          id
                          ".svg"))))
#_
(-> @state/*selections
    (state/datafile-svg 31))

(defn
  first-datafile-svg
  ""
  [context]
  (let [first-selections-idx (fx/sub-ctx context
                                         first-datafile-idx)]
    (if (nil? first-selections-idx)
      (fx/sub-ctx context
                  contour-map-svg)
      (fx/sub-ctx context
                  datafile-svg
                  first-selections-idx))))
#_
(-> @state/*selections
    (fx/sub-ctx state/first-datafile-svg))

(defn
  singular-values
  [context]
  (-> context
      (fx/sub-ctx region-matrix)
      matrix/svd
      matrix/singular-values))
#_
(-> @state/*selections
    (fx/sub-ctx state/singular-values))


(defn
  singular-value
  "Get a particular singular vector
  Based on `index`"
  [context
   sv-index]
  (-> context
      (fx/sub-ctx singular-values)
      (get sv-index)
      second))
#_
(state/singular-value @state/*selections
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
                                   region-svg-hiccup)
                       {:display-width (fx/sub-ctx context
                                                   region-display-width)})
        (spitsvgstream (str "sv-"
                            sv-index
                            ".svg")))))

(defn
  first-sv-svg
  [context]
  (-> context
      (fx/sub-ctx singular-vector-svg
                  0)
      (spitsvgstream "first-sv.svg")))
#_
(-> @state/*selections
    (fx/sub-ctx state/first-sv-svg))

(defn
  second-sv-svg
  [context]
  (-> context
      (fx/sub-ctx singular-vector-svg
                  1)
      (spitsvgstream "second-sv.svg")))
#_
(-> @state/*selections
    (fx/sub-ctx state/second-sv-svg))

(defn
  first-sv-selected-svg
  ""
  [context]
  (let [first-selections-idx (fx/sub-ctx context
                                         first-sv-selected-idx)]
    (if (nil? first-selections-idx)
      (fx/sub-ctx context
                  contour-map-svg)
      (fx/sub-ctx context
                  singular-vector-svg
                  first-selections-idx))))
#_
(-> @state/*selections
    (fx/sub-ctx state/first-datafile-svg))

(defn
  first-noise-selected-svg
  ""
  [context]
  (let [first-selections-idx (fx/sub-ctx context
                                         first-noise-selected-idx)]
    (if (nil? first-selections-idx)
      (fx/sub-ctx context
                  contour-map-svg)
      (fx/sub-ctx context
                  noise-svg
                  first-selections-idx))))
#_
(-> @state/*selections
    (fx/sub-ctx state/first-noise-selected-svg))

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
                                 region-svg-hiccup)
                     {:display-width (fx/sub-ctx context
                                                 region-display-width)})))
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

(defn sv-proj-vec
  [context]
  (-> context
      (fx/sub-ctx region-matrix)
      matrix/svd
      matrix/svd-to-2d-sv-space))

(defn-
  add-cycle-data
  "add metadata to a vector that in the same order as the original data"
  [context
   input-vec]
  (let [frac-generator (partial cycle-frac
                                (fx/sub-ctx context
                                            cycle-length)
                                (fx/sub-ctx context
                                            cycle-phase))]
    (->> input-vec
         (map-indexed (fn add-cycle-frac
                        [idx point]
                        (conj point
                              {:cycle-frac (frac-generator idx)})))
         vec)))

(defn
  sv-proj
  "adds cycle meta-data to `sv-proj-vec`"
  [context]
  (add-cycle-data context
                  (fx/sub-ctx context
                              sv-proj-vec)))
#_
(-> @state/*selections
    (fx/sub-ctx state/sv-proj))

(defn
  sv12-plot-svg
  "Plot of SV1 and SV2 and how they chaneg over time"
  [context]
  (let [sv-projs (fx/sub-ctx context
                             sv-proj-vec)]
    (-> (plot/sv1sv2-1scale (* 1.0
                               (fx/sub-ctx context
                                           state/window-width))
                            (* 1.0
                               (fx/sub-ctx context
                                           state/row-height))
                            sv-projs
                            2011
                            (fx/sub-ctx context
                                        cycle-length)
                            (fx/sub-ctx context
                                        cycle-phase))
        (spitsvgstream "sv1sv2.svg"))))
;;#_ ;;unused
(-> @state/*selections
    (fx/sub-ctx state/sv12-plot-svg))

(defn
  sv12-plot-2scale-svg
  "Plot of SV1 and SV2 and how they chaneg over time
  This is an extra display with SV1 and SV2 with different Y axis"
  [context]
  (let [sv-projs (fx/sub-ctx context
                             sv-proj-vec)]
    (-> (plot/sv1sv2-2scale (* 1.0
                               (fx/sub-ctx context
                                           state/window-width))
                            (* 1.0
                               (fx/sub-ctx context
                                           state/row-height))
                            sv-projs
                            2011
                            (fx/sub-ctx context
                                        cycle-length)
                            (fx/sub-ctx context
                                        cycle-phase))
        (spitsvgstream "sv1sv2-2scale.svg"))))
;;#_ ;;unused
(-> @state/*selections
    (fx/sub-ctx state/sv12-plot-2scale-svg))

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
  top-pattern
  [context]
  (let [{:keys [centroid-a]} (fx/sub-ctx context
                                         state/sv-bisection)]
    (let [sval-one  (-> context
                        (fx/sub-ctx state/singular-value 0))
          sval-two  (-> context
                        (fx/sub-ctx state/singular-value 1))
          [x-coord
           y-coord] centroid-a]
      (let [mixture (fx/sub-ctx context
                                singular-vector-mixture
                                x-coord
                                y-coord
                                sval-one
                                sval-two)]
        mixture)))) ;; TODO: Normalize? I think..
#_
(-> @state/*selections
    (fx/sub-ctx state/top-pattern))

(defn
  top-pattern-svg
  [context]
  (-> (geogrid4seq/build-grid (-> context
                                  (fx/sub-ctx region-geogrid-params))
                              (-> context
                                  (fx/sub-ctx top-pattern)))
      (plot/grid-map (fx/sub-ctx context
                                 region-svg-hiccup)
                     {:label-top-right "Top Pattern"
                      :label-attribs   {:fill "#00aa88"}
                      :display-width   (fx/sub-ctx context
                                                   region-display-width)})
      (spitsvgstream "top-pattern.svg")))
#_
(-> @state/*selections
    (fx/sub-ctx state/top-pattern-svg))

(defn
  top-pattern-weighted-noise
  "Weight the remaining noise by the first pattern"
  [context
   index]
  (-> context
      (fx/sub-ctx noise-matrix-2d)
      (matrix/extract-grid index)
      :data-array
      (#(mapv *
              %
              (fx/sub-ctx context top-pattern)))))
#_
(-> @state/*selections
    (fx/sub-ctx state/top-pattern-weighted-noise 6))

(defn
  top-pattern-weighted-noise-svg
  [context
   index]
  (-> (geogrid4seq/build-grid (-> context
                                  (fx/sub-ctx region-geogrid-params))
                              (-> context
                                  (fx/sub-ctx top-pattern-weighted-noise
                                              index)))
      (plot/grid-map (fx/sub-ctx context
                                 region-svg-hiccup)
                     {:display-width (fx/sub-ctx context
                                                 region-display-width)})
      (spitsvgstream (str "top-pattern-weighted-noise-"
                          index
                          ".svg"))))
#_
(-> @state/*selections
    (state/datafile-svg 6))
#_
(-> @state/*selections
    (state/noise-svg 6))
#_
(-> @state/*selections
    (state/top-pattern-weighted-noise-svg 6))
#_
(-> @state/*selections
    (state/datafile-svg 31))
#_
(-> @state/*selections
    (state/noise-svg 31))
#_
(-> @state/*selections
    (state/top-pattern-weighted-noise-svg 31))

(defn
  bottom-pattern
  [context]
  (let [{:keys [centroid-b]} (fx/sub-ctx context
                                         state/sv-bisection)]
    (let [sval-one  (-> context
                        (fx/sub-ctx state/singular-value 0))
          sval-two  (-> context
                        (fx/sub-ctx state/singular-value 1))
          [x-coord
           y-coord] centroid-b]
      (fx/sub-ctx context
                  singular-vector-mixture
                  x-coord
                  y-coord
                  sval-one
                  sval-two))))
#_
(-> @state/*selections
    (fx/sub-ctx state/bottom-pattern))

(defn
  bottom-pattern-svg
  [context]
  (-> (geogrid4seq/build-grid (-> context
                                  (fx/sub-ctx region-geogrid-params))
                              (-> context
                                  (fx/sub-ctx bottom-pattern)))
      (plot/grid-map (fx/sub-ctx context
                                 region-svg-hiccup)
                     {:label-top-right "Bottom Pattern"
                      :label-attribs   {:fill "#aa8800"}
                      :display-width   (fx/sub-ctx context
                                                   region-display-width)})
      (spitsvgstream "bottom-pattern.svg")))
#_
(-> @state/*selections
    (fx/sub-ctx state/bottom-pattern-svg))

(defn
  bottom-pattern-weighted-noise
  "Weight the remaining noise by the first pattern"
  [context
   index]
  (-> context
      (fx/sub-ctx noise-matrix-2d)
      (matrix/extract-grid index)
      :data-array
      (#(mapv *
              %
              (fx/sub-ctx context
                          bottom-pattern)))))
#_
(-> @state/*selections
    (fx/sub-ctx state/bottom-pattern-weighted-noise 6))

(defn
  bottom-pattern-weighted-noise-svg
  [context
   index]
  (-> (geogrid4seq/build-grid (-> context
                                  (fx/sub-ctx region-geogrid-params))
                              (-> context
                                  (fx/sub-ctx bottom-pattern-weighted-noise
                                              index)))
      (plot/grid-map (fx/sub-ctx context
                                 region-svg-hiccup)
                     {:display-width (fx/sub-ctx context
                                                 region-display-width)})
      (spitsvgstream (str "bottom-pattern-weighted-noise-"
                          index
                          ".svg"))))
#_
(-> @state/*selections
    (state/datafile-svg 10))
#_
(-> @state/*selections
    (state/noise-svg 10))
#_
(-> @state/*selections
    (state/bottom-pattern-weighted-noise-svg 10))
#_
(-> @state/*selections
    (state/datafile-svg 11))
#_
(-> @state/*selections
    (state/noise-svg 11))
#_
(-> @state/*selections
    (state/bottom-pattern-weighted-noise-svg 11))

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
    (fx/sub-ctx state/pattern-proj)
    first)
;; => [-0.009947702296436598
;;     0.0614008470602693
;;     {:cycle-frac 0, :above? false}]

(defn
  binary-index-vector
  "A vector of true/false values.
  Indicating the given data index was labeled `above` or `below`"
  [context]
  (->> (fx/sub-ctx context
                   pattern-proj)
       (mapv (fn [datapoint]
               (-> datapoint
                   (get 2)
                   :above?)))))
#_
(-> @state/*selections
    binary-index-vector)

(defn
  climate-noise-matrix-2d-normalized
  "The noise matrix is normalized according to its corresponding climate pattern
  Areas where the climate doesn't manifest are in effect given a lower weight
  As it can not skew the climate index in those areas
  NOTE: This could probably be rewritten in terms of matrix operations if it's slow"
  [context]
  (let [binary-classifications (fx/sub-ctx context
                                           binary-index-vector)]
    (->> binary-classifications
         (map-indexed (fn [index
                           is-top-pattern]
                        (if is-top-pattern
                          (top-pattern-weighted-noise context
                                                      index)
                          (bottom-pattern-weighted-noise context
                                                         index))))
         (matrix/from-vecofvecs (fx/sub-ctx context
                                            region-matrix)))))
#_
(->> @state/*selections
     climate-noise-matrix-2d-normalized)
;;Validating the matric is built correctly row by row (on `:krabi-root2`)
;; => {:matrix #RealGEMatrix[double, mxn:2840x119, layout:column]
;;       ▥       ↓       ↓       ↓       ↓       ↓       ┓    
;;       →    2.34E+4 -9.23E+4   ⁙    -5.83E+41.69E+4         
;;       →    1.81E+4 -9.04E+4   ⁙    -4.47E+43.57E+4         
;;       →       ⁙       ⁙       ⁙       ⁙       ⁙            
;;       →    -2.83E+47.42E+4    ⁙    1.75E+4 -2.40E+4        
;;       →    -1.17E+41.46E+4    ⁙    -1.20E+4-6.52E+3        
;;       ┗                                               ┛    
;;    ,
;;     :dimension [40 71],
;;     :position {:eas 277.0, :sou 76.9},
;;     :resolution [0.1 0.1]}
;; We repeat the projections/normalization manually
#_
(-> @state/*selections
    binary-index-vector
    first)
;; => false
;; (so January is classified as "summer")
#_
(take 2
      (-> @state/*selections
          (top-pattern-weighted-noise 0)))
;; => (23429.16540445495 18104.86334445968)
#_
(take-last 2
           (-> @state/*selections
               (top-pattern-weighted-noise 0)))
;; => (-28273.311179534514 -11668.173447261712)

(defn
  climate-noise-svg
  "Get the noise background of one data point"
  [context
   id]
  (-> (matrix/extract-grid (fx/sub-ctx context
                                       climate-noise-matrix-2d-normalized)
                           id)
      (plot/grid-map (fx/sub-ctx context
                                 region-svg-hiccup)
                     {:display-width (fx/sub-ctx context
                                                 region-display-width)})
      (spitsvgstream (str "climate-noise-"
                          id
                          "file.svg"))))
#_
(-> @state/*selections
    (state/climate-noise-svg 31))

(defn
  first-normalized-noise-selected-svg
  ""
  [context]
  (let [first-selections-idx (fx/sub-ctx context
                                         first-normalized-noise-selected-idx)]
    (if (nil? first-selections-idx)
      (fx/sub-ctx context
                  contour-map-svg)
      (fx/sub-ctx context
                  climate-noise-svg
                  first-selections-idx))))
#_
(-> @state/*selections
    (fx/sub-ctx state/first-normalized-noise-selected-svg))

(defn
  climate-noise-vars
  [context]
  (-> context
      (fx/sub-ctx climate-noise-matrix-2d-normalized)
      (matrix/colvars)))
#_
(->> @state/*selections
     climate-noise-vars)

(defn climate-noise-var-svg
  [context]
  (-> (plot/index (* 1.0
                     (fx/sub-ctx context
                                 state/window-width))
                  (* 1.0
                     (fx/sub-ctx context
                                 state/row-height))
                  (-> context
                      (fx/sub-ctx climate-noise-vars))
                  2011
                  (fx/sub-ctx context
                              cycle-length)
                  (fx/sub-ctx context
                              cycle-phase))
      (spitsvgstream "indeces-vars.svg")))
#_
(->> @state/*selections
     climate-noise-var-svg)

(defn
  pattern-proj-partitioned
  "Projections of all the data
  onto the two extracted patterns, but partitioned into two groups
  Returns:
  a two list of nonorthogonal coordinates"
  [context]
  (let [projections (fx/sub-ctx context
                                pattern-proj)
        proj-a      (->> projections
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
        proj-b      (->> projections
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
        (spitsvgstream "indeces.svg"))))
#_
(-> @state/*selections
    (fx/sub-ctx state/pattern-proj-svg))

(defn
  sv-proj-svg
  [context]
  (-> context
      (cljfx.api/sub-ctx sv-proj)
      (plot/sv-plot (* 1.0
                       (fx/sub-ctx context
                                   window-width))
                    (* 4.0
                       (fx/sub-ctx context
                                   row-height)))
      (spitsvgstream "sv-projs.svg")))
#_
(-> @state/*selections
    (fx/sub-ctx state/sv-proj-svg))

(defn
  singular-values-stats
  [context]
  (-> context
      (fx/sub-ctx singular-values)
      matrix/singular-values-stats))
#_
(-> @state/*selections
    (fx/sub-ctx state/singular-valuesstats))

(defn
  singular-values-svg
  [context]
  (-> (plot/sv-weights (fx/sub-ctx context
                                   singular-values)
                       50
                       (fx/sub-ctx context
                                   singular-values-stats)
                       (* 1.0
                          (fx/sub-ctx context
                                      state/window-width))
                       (* 1.0
                          (fx/sub-ctx context
                                      state/row-height)))
      (spitsvgstream "singular-values.svg")))
#_
(spit (str "out/"
           (-> @state/*selections
               (fx/sub-ctx state/region-key)
               symbol)
           "/test-singular-values-actual.svg")
      (-> @state/*selections
          (fx/sub-ctx state/singular-valuess-svg)))

(defn
  all-svg
  [context
   geogrid-vec]
  (let [cycle-length 12 #_ (fx/sub-ctx context
                                       cycle-length)]
    (-> (->> geogrid-vec
             (map-indexed (fn [idx
                               grid]
                            (plot/grid-map grid
                                           (fx/sub-ctx context
                                                       region-svg-hiccup)
                                           {:label-top-right (str (inc idx))
                                            :axis-visible?   true
                                            :cycle-frac      (/ idx
                                                                12.0)}))))
        (plot/cyclic 12))))

(defn
  precipitation-all-svg
  [context]
  (-> context
      (all-svg (-> context
                   (fx/sub-ctx region-matrix)
                   matrix/to-geogrid-vec))
      (spitsvgstream "precipitation-all.svg")))
;;#_ ;;unused
(-> @state/*selections
    (fx/sub-ctx state/precipitation-all-svg))


#_(fx/sub-ctx context
              region-geogrid-vec)

(defn
  noise-all-svg
  [context]
  (-> context
      (all-svg (-> context
                   (fx/sub-ctx noise-matrix-2d)
                   matrix/to-geogrid-vec))
      (spitsvgstream "noise-all.svg")))
;;#_ ;;unused
(-> @state/*selections
    (fx/sub-ctx state/noise-all-svg))

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
                                                                    12.0)
                                                #_#_
                                                :display-width   (fx/sub-ctx context
                                                                             region-display-width)})))
                 (into []))
            (plot/cyclic (clojure.math/ceil (clojure.math/pow cycle-length
                                                              0.5)))
            (spitsvgstream "cycle.svg"))))))
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
                                                :label-attribs   {:font-size 1.5}
                                                :cycle-frac      (/ idx
                                                                    12.0)
                                                :display-width   (/ (fx/sub-ctx context
                                                                                region-display-width)
                                                                    4)})))
                 (into []))
            plot/annual-12-month-ring
            (spitsvgstream (str "year"
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

(defn
  power-of-sv-weights-scaled
  "Return a pair of the eof1weight and variance
   (relative to the EOF1 signal)
  for a given INDEX (ie. time point)"
  [context]
  (let [num-of-svs    (-> context
                          (fx/sub-ctx datafile-strs)
                          count)
        singular-vals (->> (fx/sub-ctx context
                                       singular-values)
                           (mapv second))]
    (->> num-of-svs
         range
         (mapv (fn [sv-index]
                 (let [weight (get singular-vals
                                   sv-index)]
                   (->> (sv-weights context
                                    sv-index)
                        (mapv (partial *
                                       weight))
                        (mapv #(Math/pow %
                                         2)))))))))
#_
(->> (-> @state/*selections
         (fx/sub-ctx power-of-sv-weights-scaled))
     (mapv (partial take 5)))

(defn
  sv12-vs-other
  [context]
  (let [all-sv-weights-power (fx/sub-ctx context
                                         power-of-sv-weights-scaled)]
    (let [sv1       (first all-sv-weights-power)
          sv2       (second all-sv-weights-power)
          sv12      (mapv +
                          sv1
                          sv2)
          ;;#_#_
          other-svs (apply (partial mapv
                                    +)
                           (drop 2
                                 all-sv-weights-power))]
      (map vector
           sv12
           other-svs))))
#_
(->> (-> @state/*selections
         (fx/sub-ctx sv12-vs-other))
     (mapv (fn [pair]
             (/ (first pair)
                (second pair)))))



(defn
  sv12-vs-other-svg
  [context]
  (-> (add-cycle-data context
                      (fx/sub-ctx context
                                  sv12-vs-other))
      (plot/add-cycle-color)
      (plot/scatter 1000
                    1000
                    {:title-str "SV Power Plot"
                     :x-name    "SV1 and SV2"
                     :y-name    "Other SVs"})
      (spitsvgstream (str "power-sv12-vs-other"
                          ".svg"))))
;;#_ ;;usused
(-> @state/*selections
    (fx/sub-ctx sv12-vs-other-svg))
