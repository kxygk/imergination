(ns
    kxygk.imergination.state
  "Program and GUI state"
  (:use [hashp.core]
        [clojure.math])
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [cljfx.api       :as fx]
            [clojure.core.cache :as cache]
;;            [injest.path :refer [+> +>> x>> =>>]]
            [kxygk.imergination.bisect :as bisect]
            [geogrid4image]
            [geogrid4seq]
            [kxygk.imergination.svg2jfx]
            [kxygk.imergination.datamats :as datamats]
            #_
            kxygk.imergination.matrix4neanderthal
            ;;#_
            kxygk.imergination.matrix4ojalgo ;; this sets the matrix backend
            [kxygk.imergination.matrix :as matrix] ;; only used in one spot
            [kxygk.imergination.plot :as plot]
            [kxygk.imergination.locations :as locations]))

(def debug?
  true)

(def
  config-dir
  (str "/home/kxygk/Projects/imergination.wiki/"
       #_
       "krabi-monthly-2year"
       #_
       "krabi-daily-2year"
       #_
       "krabi-pentad-10year"
       #_
       "krabi-pentads-2year"
       #_
       "krabins-short-pentad"
       #_
       "krabi-short-daily"
       #_
       "krabi-gpcp"
       #_
       "fakerain"
       #_
       "rift-valley"
       #_
       "imerg-blip"
       #_
       "krabi-gpcc"
       #_
       "marrah"
       #_
       "krabdaily"
       #_
       "haihai-norm"
       #_
       "krabins-norm"
       #_
       "krab-mon-norm"
       ;;#_
       "krabi-monthly"
       #_
       "krabi-monthly-final-v7"
       #_
       "krabi-monthly"
       #_
       "scs-rainbow"
       #_
       "krabins-v7"
       #_
       "krabins"
       #_
       "sichuan"
       #_
       "taipei"
       #_
       "taiwan"
       #_
       "scs-skinny"
       #_
       "hainan-skinny-nonorm"
       #_
       "hainan-skinny"))

(def
  *selections
  (atom (fx/create-context (merge {;; Defaults
                                   :window-width                   1080.0
                                   :row-height                     360
                                   :shoreline-filestr              "./data/shoreline-coarse.json"
                                   :contour-filestr                nil
                                   :non-zero-min?                  false
                                   :normalize-data?                true
                                   #_#_#_#_:rain-dirstr                    "/home/kxygk/Data/sst/monthly/geotiff-rot/"
                                   :elevation-filestr              "./data/World_e-Atlas-UCSD_SRTM30-plus_v8.tif"
                                   :bin-size                       1
                                   :cycle-length                   12
                                   :cycle-phase                    0
                                   :eas-res                        0.1
                                   :sou-res                        0.1
                                   :region                         nil
                                   :region-key                     :krabi-root-2
                                   :is-in-ram                      false
                                   :mouse-click                    nil
                                   :datafile-idxs                  [0]
                                   :sv-selected-idxs               [0]
                                   :noise-selected-idxs            [0]
                                   :normalized-noise-selected-idxs [0]}
                                  (-> config-dir
                                      (str "/config.edn")
                                      slurp
                                      clojure.edn/read-string))
                           #(cache/lru-cache-factory % :threshold 1000))))



(defn
  is-in-ram
  [context]
  (fx/sub-val context
              :is-in-ram))
#_
(-> @*selections
    (fx/sub-ctx is-in-ram))

(defn
  non-zero-min?
  [context]
  (fx/sub-val context
              :non-zero-min?))

(defn
  normalize-data?
  [context]
  (fx/sub-val context
              :normalize-data?))
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
(-> @*selections
    (fx/sub-ctx region-key))
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
(-> @*selections
    (fx/sub-ctx region))
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
(-> @*selections
    (fx/sub-ctx region-meta))

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
  (let [region-key (-> @*selections
                       (fx/sub-ctx region-key))
        subfolder  (if (nil? region-key)
                     "custom"
                     (symbol region-key))]
    (if debug?
      (future (spit (str config-dir
                         "/"
                         filename)
                    #_(str "../imergination.wiki/"
                           subfolder
                           "/"
                           filename)
                    string))
      nil)
    string))

#_
(if debug?
  (->> (-> @*selections
           (fx/sub-ctx region-key))
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
  (println (str "Making SVG: "
                filename))
  (let [realized-hiccup (do (println "Generating Hiccup")
                            (doall svg-hiccup))]
    (let [xml (do (println "Generating XML")
                  (quickthing/svg2xml realized-hiccup))]
      #_xml
      (println "Writing to File")
      (spitstream xml
                  filename)
      svg-hiccup)))
;; ***************************************


;; TODO Make these checks on first run
;;#_#_
(if (not (zero? (mod (fx/sub-val @*selections
                                 :cycle-length)
                     (fx/sub-val @*selections
                                 :bin-size))))
  (println "ERROR: The `bin-size` doesn't cleanly divide the cycle length"))


(if (not (zero? (mod (fx/sub-val @*selections
                                 :cycle-length)
                     (fx/sub-val @*selections
                                 :bin-size))))
  (println "ERROR: The `bin-size` doesn't cleanly divide the cycle length"))

(defn
  bin-size
  [context]
  (fx/sub-val context
              :bin-size))

(defn
  cycle-length
  [context]
  (/ (fx/sub-val context
                 :cycle-length)
     (fx/sub-ctx context
                 bin-size)))

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
(-> @*selections
    (fx/sub-ctx display-width))

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
(-> @*selections
    (fx/sub-ctx region-xy-ratio))
#_
(-> @*selections
    (fx/sub-ctx region))
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
(-> @*selections
    (fx/sub-ctx region-display-width))

(defn region-display-height
  [context]
  (/ (fx/sub-ctx context
                 region-display-width)
     (fx/sub-ctx context
                 region-xy-ratio)))
#_
(-> @*selections
    (fx/sub-ctx region-display-height))

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
(-> @*selections
    (fx/sub-ctx sou-res))

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
(-> @*selections
    (fx/sub-ctx world-svg-hiccup))

(defn
  world-svg
  "Get a shoreline map of the whole world
  TODO: Maybe bake this in to the program?"
  [context]
  (-> context
      (fx/sub-ctx world-svg-hiccup)
      (spitsvgstream "world.svg")))
#_
(-> @*selections
    (fx/sub-ctx world-svg))
#_
(spit "out/test-world.svg"
      (-> @*selections
          (fx/sub-ctx world-svg)))

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
                                   window-width)
                       360.0)
        scale-y     (/
                      (fx/sub-ctx context
                                  window-width)
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
(type (-> @*selections
          (fx/sub-ctx region-svg)))

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
                                region-to-display-scale-x)
        scale-y     (fx/sub-ctx context
                                region-to-display-scale-y)]
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
(->> (fx/sub-ctx @*selections
                 datafile-strs)
     count)
#_
(->> (fx/sub-ctx @*selections
                 datafile-strs)
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

(defn-
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
(realized? (-> @*selections
               (fx/sub-ctx world-geogrid-vec)))
#_
(-> @*selections
    (fx/sub-ctx is-in-ram))

(defn-
  region-geogrid-vec
  "TODO: I think this can be folded into `region-matrix` now?
  TODO: Ideally this could be removed entirely..
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
      datamats/to-geogrid-vec)
  (let [myregion   (fx/sub-ctx context
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
(-> @*selections
    (fx/sub-ctx region-geogrid-vec)
    first
    keys)

(defn-
  bin-sum
  "Build a new `geogrid`.
  It'll be the sum of the given `geogrids`"
  [geogrids]
  (geogrid4seq/build-grid (-> geogrids
                              first
                              geogrid/params)
                          (->> geogrids
                               (mapv geogrid/data)
                               (apply mapv +))))

(defn-
  bin-geogrids
  [bin-size
   geogrids]
  (if (== bin-size
          1)
    geogrids
    (->> geogrids
         (partition bin-size)
         (mapv bin-sum))))

(defn-
  normalize-geogrid
  "Returns a map
  {:geogrid _
   :scale   _
   :shift   _ } "
  [non-zero-min?
   grid]
  (if non-zero-min?
    (geogrid4seq/convert-to-normalized grid)
    (geogrid4seq/convert-to-minzero-normalized grid)))

(defn
  region-matrix
  "Matrix of all the data over a region
  Implementation is hidden in `matrix.clj`
  So that the underlying library can be swapped"
  [context]
  (let [bin-size (fx/sub-ctx context
                             bin-size)
        grids (fx/sub-ctx context
                          region-geogrid-vec)
        binned-grids (bin-geogrids bin-size
                                   grids)
        normalized-grids (if (fx/sub-ctx context
                                         normalize-data?)
                           (map (partial normalize-geogrid
                                         (fx/sub-ctx context
                                                     non-zero-min?))
                                binned-grids)
                           (map (fn [grid]
                                  {:grid grid
                                   :scale   1.0
                                   :shift   0.0})
                                binned-grids))]
    (-> (map :grid
             normalized-grids)
        datamats/from-geogrids
        (assoc :scales (->> normalized-grids
                            (mapv :scale)))
        (assoc :shifts (->> normalized-grids
                            (mapv :shift))))))
#_
(-> @*selections
    (fx/sub-ctx region-matrix))
;; => (:matrix :dimension :position :resolution :scales :shifts)


(-> @*selections
    (fx/sub-ctx normalize-data?))

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
(geogrid4image/read-file (->> (fx/sub-ctx @*selections
                                          datafile-strs)
                              (map #(str (fx/sub-ctx @*selections
                                                     data-dirstr)
                                         %))
                              first)
                         (fx/sub-ctx @*selections
                                     eas-res)
                         (fx/sub-ctx @*selections
                                     sou-res))
#_
(-> @*selections
    (fx/sub-ctx region-geogrids-and-scales-vec)
    datamats/from-geogrids)


(defn
  region-geogrid-params
  "This is a bit of a convoluted way to calculate the parameters,
  but it's the only way to ensure they're correct
  b/c image reading and segmentation is complicated
  and it's all done in `geogrid/subregion`"
  [context]
  (->> (fx/sub-ctx context
                   region-matrix)
       datamats/extract-params))
#_
(-> @*selections
    (fx/sub-ctx region-geogrid-params))
;; => [40 71 0.1 0.1 {:eas 277.0, :sou 76.9}]

(defn
  num-svs
  "Number of Singular Vectors/Values.
  This is either the number of data point,
  or the number of pixels.
  Whichever is smallest"
  [context]
  (-> context
      (fx/sub-ctx region-matrix)
      datamats/num-svs))

(defn
  sv-strs
  "Gets listing of all possible SVs"
  [context]
  (let [num-of-svs (-> context
                       (fx/sub-ctx num-svs))]
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
(-> @*selections
    (fx/sub-ctx sv-strs))
#_
(-> @*selections
    (fx/sub-ctx num-svs)
    clojure.math/log10
    clojure.math/ceil
    int)

(defn
  region-svd
  "the SVD of the region matrix"
  [context]
  (-> context
      (fx/sub-ctx region-matrix)
      datamats/svd))
#_
(-> @*selections
    (fx/sub-ctx region-svd)
    keys)
#_
(region-svd @*selections)

(defn
  region-min-max
  [context]
  (-> context
      (fx/sub-ctx region-matrix)
      datamats/get-min-max))
#_
(-> @*selections
    (fx/sub-ctx region-min-max))
;; => [0.0 1802.0]
#_
(-> @*selections
    (fx/sub-ctx region-matrix)
    :matrix
    (uncomplicate.neanderthal.linalg/svd  true
                                          true)
    (datamats/from-svd))

(defn
  sv-weights
  "Get the SV components at each data point (ie. point in time)
  This is zero indexed
  So SV1 is index 0"
  [context
   sv-index]
  (-> context
      (fx/sub-ctx region-svd)
      (datamats/svd-to-weights sv-index)))
#_
(-> @*selections
    (fx/sub-ctx sv-weights 0))

(defn
  datafile-idxs
  "Indeces of the data that's been selected" 
  [context]
  (fx/sub-val context
              :datafile-idxs))
#_
(fx/sub-ctx @*selections
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
(fx/sub-ctx @*selections
            first-datafile-idx)

(defn
  sv-selected-idxs
  "Indeces of the data that's been selected"
  [context]
  (fx/sub-val context
              :sv-selected-idxs))
#_
(fx/sub-ctx @*selections
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
(fx/sub-ctx @*selections
            first-datafile-idx)


(defn-
  datafile-geogrid
  [context
   id]
  (datamats/extract-grid (fx/sub-ctx context
                                   region-matrix)
                       id))
#_
(apply max
       (-> @*selections
           (fx/sub-ctx datafile-geogrid
                       9)
           :data-array
           vec))

;; => 30174.0
;; => -32344.0
#_
(-> @*selections
    (fx/sub-ctx region-matrix)
    :matrix
    uncomplicate.neanderthal.core/amax)

(defn
  zero-point-mask
  "Make a mask based on the first datafile where the pix are 0"
  [context]
  (mapv zero?
        (-> context
            (fx/sub-ctx datafile-geogrid
                        0)
            :data-array)))
#_
(-> @*selections
    (fx/sub-ctx zero-point-mask))

#_#_
(defn
  average-geogrid
  [context]
  (-> context
      (fx/sub-ctx region-matrix)
      datamats/data-average-geogrid
      (plot/grid-map (fx/sub-ctx context
                                 region-svg-hiccup)
                     {:label-top-right "Average"
                      :display-width   (fx/sub-ctx context
                                                   region-display-width)})
      #_
      (spitsvgstream (str "average"
                          ".svg"))))
;;#_ ;;unused
(-> @*selections
    (average-geogrid))

(defn
  datafile-svg
  "Get an SVG of a given datafile
  (ex: day, month)
  Selected with an ID index"
  [context
   id]
  (-> (fx/sub-ctx context
                  datafile-geogrid
                  id)
      (plot/grid-map (fx/sub-ctx context
                                 region-svg-hiccup)
                     {:max-val       (->  context
                                          (fx/sub-ctx region-min-max)
                                          second)
                      :display-width (fx/sub-ctx context
                                                 region-display-width)})
      (spitsvgstream (str "data-file-"
                          id
                          ".svg"))))
#_
(-> @*selections
    (datafile-svg 31))

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
(-> @*selections
    (fx/sub-ctx first-datafile-svg))

(defn
  singular-values
  [context]
  (-> context
      (fx/sub-ctx region-matrix)
      datamats/svd
      datamats/singular-values))
#_
(-> @*selections
    (fx/sub-ctx singular-values))


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
(singular-value @*selections
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
      (datamats/singular-vector sv-index)))
#_
(singular-vector @*selections
                       0)
#_
(-> @*selections
    (fx/sub-ctx singular-vector 0))

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
(-> @*selections
    (fx/sub-ctx singular-vector-geogrid
                0))
#_ ;; BROKEN
(spit
  (str "out/"
       (-> @*selections
           (fx/sub-ctx region-key)
           symbol )
       "/first-sv.svg")
  (quickthing/svg2xml
    (plot/grid-map
      (-> @*selections
          (fx/sub-ctx region))
      (region @*selections) ;;input-region ;; it'll crop redundantly here..
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
                       {:label-top-right (str "SV"
                                              (inc sv-index))
                        :label-attribs {:fill "black"
                                        :stroke "white" #_#_
                                        :font-size 1.1}
                        :display-width (fx/sub-ctx context
                                                   region-display-width)})
        (spitsvgstream (str "sv-"
                            sv-index
                            ".svg")))))
;;#_
(->> 7
     range
     (run! (fn [sv-index]
             (fx/sub-ctx @*selections
                         singular-vector-svg sv-index))))

(defn
  first-sv-svg
  [context]
  (-> context
      (fx/sub-ctx singular-vector-svg
                  0)
      (spitsvgstream "first-sv.svg")))
;;#_
(-> @*selections
    (fx/sub-ctx first-sv-svg))

(defn
  second-sv-svg
  [context]
  (-> context
      (fx/sub-ctx singular-vector-svg
                  1)
      (spitsvgstream "second-sv.svg")))
;;#_
(-> @*selections
    (fx/sub-ctx second-sv-svg))

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
(-> @*selections
    (fx/sub-ctx first-datafile-svg))


(defn
  singular-vector-mixture
  "Mixture of the first two singular vectors"
  [context
   svec-one
   svec-two
   sval-one
   sval-two]
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
(-> @*selections
    (fx/sub-ctx singular-vector-mixture-geogrid
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
           (-> @*selections
               (fx/sub-ctx region-key)
               symbol )
           "/fiftyfifty.svg")
      (-> @*selections
          (fx/sub-ctx singular-vector-mixture-svg
                      0.5
                      0.5
                      1.0
                      1.0)))

(defn
  sv-proj-vec
  "Get the projection vector for each point.
  ie. the first two columns of the weight matrix.
  These are NOT scaled by the singular value at all"
  [context]
  (-> context
      (fx/sub-ctx region-matrix)
      datamats/svd
      datamats/svd-to-2d-sv-space))
#_
(-> @*selections
    sv-proj-vec)

#_
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
  sv12-plot-svg
  "Plot of SV1 and SV2 and how they chaneg over time"
  [context]
  (let [sv-projs (fx/sub-ctx context
                             sv-proj-vec)]
    (-> (plot/sv1sv2-1scale (* 1.0
                               (fx/sub-ctx context
                                           window-width))
                            (* 1.0
                               (fx/sub-ctx context
                                           row-height))
                            sv-projs
                            2011
                            (fx/sub-ctx context
                                        cycle-length)
                            (fx/sub-ctx context
                                        cycle-phase))
        (spitsvgstream "sv1sv2.svg"))))
;;#_ ;;unused
(-> @*selections
    (fx/sub-ctx sv12-plot-svg))

(defn
  sv12-plot-2scale-svg
  "Plot of SV1 and SV2 and how they chaneg over time
  This is an extra display with SV1 and SV2 with different Y axis"
  [context]
  (let [sv-projs (fx/sub-ctx context
                             sv-proj-vec)]
    (-> (plot/sv1sv2-2scale (* 1.0
                               (fx/sub-ctx context
                                           window-width))
                            (* 1.0
                               (fx/sub-ctx context
                                           row-height))
                            sv-projs
                            2011
                            (fx/sub-ctx context
                                        cycle-length)
                            (fx/sub-ctx context
                                        cycle-phase))
        (spitsvgstream "sv1sv2-2scale.svg"))))
;;#_ ;;unused
(-> @*selections
    (fx/sub-ctx sv12-plot-2scale-svg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;      NOISE 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn
  noise-matrix-2d
  "Noise matrix when there are two climate systems
  and the two leading SV are removed from the data"
  [context]
  (-> context
      (fx/sub-ctx region-svd)
      (datamats/minus-2-sv)))
#_
(-> @*selections
    (fx/sub-ctx noise-matrix-2d)
    keys)
#_
(filter nil?
      (-> @*selections
          (fx/sub-ctx region-matrix)
          :matrix
          matrix/data))

(defn
  noise-vars
  "Calculate the variances of the noise in the columns/datapoints"
  [context]
  (-> context
      (fx/sub-ctx noise-matrix-2d)
      :matrix
      (datamats/colvars)))
#_
(take 12
      (-> @*selections
          (fx/sub-ctx noise-vars)))

(defn
  noise-svg
  "Get the noise background of one data point"
  [context
   id]
  (-> (datamats/extract-grid (fx/sub-ctx context
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
(-> @*selections
    (noise-svg 31))

(defn
  noise-selected-idxs
  "Indeces of the data that's been selected"
  [context]
  (fx/sub-val context
              :noise-selected-idxs))
#_
(fx/sub-ctx @*selections
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
(fx/sub-ctx @*selections
            first-noise-selected-idx)

(defn
  normalized-noise-selected-idxs
  "Indeces of the data that's been selected"
  [context]
  (fx/sub-val context
              :normalized-noise-selected-idxs))
#_
(fx/sub-ctx @*selections
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
(fx/sub-ctx @*selections
            first-normalized-noise-selected-idx)


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
(-> @*selections
    (fx/sub-ctx first-noise-selected-svg))


(defn
  noise-matrix-scaled-to-sv1
  [context]
  (let [sv (fx/sub-ctx context
                       singular-vector
                       0)]
    (->  context
         (fx/sub-ctx noise-matrix-2d)
         (datamats/scaled-to-vec sv))))
#_
(-> @*selections
    (fx/sub-ctx noise-matrix-scaled-to-sv1)
    ;;#_
    :matrix
    ;;#_
    matrix/abs-sums-of-cols
    #_
    (datamats/errors-from-error-datamats 10))

(defn
  noise-scaled-to-sv1-svg
  "Get the noise background of one data point"
  [context
   id]
  (-> (datamats/extract-grid (fx/sub-ctx context
                                       noise-matrix-scaled-to-sv1)
                           id)
      (plot/grid-map (fx/sub-ctx context
                                 region-svg-hiccup)
                     {:display-width (fx/sub-ctx context
                                                 region-display-width)})
      (spitsvgstream (str "noise-sv1-"
                          id
                          "file.svg"))))
#_
(-> @*selections
    (noise-svg 31))

(defn
  noise-matrix-scaled-to-sv2
  [context]
  (let [sv (fx/sub-ctx context
                       singular-vector
                       1)]
    (-> context
        (fx/sub-ctx noise-matrix-2d)
        (datamats/scaled-to-vec sv))))
#_
(fx/sub-ctx @*selections
            noise-matrix-scaled-to-sv2)

(defn
  noise-scaled-to-sv2-svg
  "Get the noise background of one data point"
  [context
   id]
  (-> (datamats/extract-grid (fx/sub-ctx context
                                       noise-matrix-scaled-to-sv2)
                           id)
      (plot/grid-map (fx/sub-ctx context
                                 region-svg-hiccup)
                     {:display-width (fx/sub-ctx context
                                                 region-display-width)})
      (spitsvgstream (str "noise-sv2-"
                          id
                          "file.svg"))))
#_
(-> @*selections
    (noise-svg 31))

(defn
  errors-in-sv1-proj
  [context]
    (let [singular-val (fx/sub-ctx context
                                 singular-value
                                 0)]
      (println (str "Singular Value 1 : "
                    singular-val))
      (datamats/errors-from-error-datamats (fx/sub-ctx context
                                                       noise-matrix-scaled-to-sv1)
                                           singular-val)))
#_
(-> @*selections
    (fx/sub-ctx errors-in-sv1-proj)
    first
    println)

(defn
  errors-in-sv2-proj
  [context]
    (let [singular-val (fx/sub-ctx context
                                 singular-value
                                 1)]
      (datamats/errors-from-error-datamats (fx/sub-ctx context
                                                       noise-matrix-scaled-to-sv1)
                                           singular-val)))
#_
(fx/sub-ctx @*selections
            errors-in-sv1-proj)

#_
(defn
  errors-in-sv2-proj
  [context]
  (let [singular-val (fx/sub-ctx context
                                 singular-value
                                 1)]
    (->> (fx/sub-ctx context
                     noise-matrix-scaled-to-sv2)
         (datamats/scale-to-value (/ 1.0
                                   singular-val))
         ;; pessimistic direct sum method
         ;;#_
         datamats/abs-sums-of-cols
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
         vec)))
#_
(fx/sub-ctx @*selections
            errors-in-sv2-proj)

#_
(defn
  noise-1d-matrix
  "This is a noise matric when only the first EOF is removed
  This shouldn't be used normally..
  b/c the code is designed to run on two climate regions"
  [context]
  (-> context
      (fx/sub-ctx region-svd)
      (datamats/minus-1-sv)))
#_
(-> @*selections
    (fx/sub-ctx noise-matrix-1d)
    keys)

#_
(defn
  noise-1d-min-max
  [context]
  (-> context
      (fx/sub-ctx noise-1d-matrix)
      datamats/get-min-max))
#_
(-> @*selections
    (fx/sub-ctx noise-1d-min-max))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defn
  sv-proj
  "adds cycle meta-data to `sv-proj-vec`"
  [context]
  (let [projs  (fx/sub-ctx context
                           sv-proj-vec)
        scales (:scales (fx/sub-ctx context
                                    region-matrix))]
    (filterv #(-> %
                  last
                  :radius
                  zero?
                  not)
             (map (fn [[proj-x
                        proj-y
                        :as projection]
                       data-index
                       scale
                       cycle-fraction
                       sv1-error
                       sv2-error]
                    (let [polar-coords (bisect/to-polar projection)]
                      (if (neg? proj-x)
                        (println (str "WARNING:"
                                      \newline
                                      "You have a point outside of the expected range"
                                      \newline
                                      "Index: "
                                      data-index
                                      \newline
                                      "Coords: "
                                      projection
                                      \newline
                                      "This point was a negative SV1 component."
                                      "Please report this case to the author")))
                      (assoc [(* proj-x
                                 scale)
                              (* proj-y
                                 scale)]
                             2
                             (merge {:index      data-index
                                     :cycle-frac cycle-fraction
                                     :scale      scale
                                     :err-x      (* sv1-error
                                                    scale)
                                     :err-y      (* sv2-error
                                                    scale)
                                     :err-angle  (let [radius (:radius polar-coords)]
                                                   (if (zero? radius)
                                                     (do (println (str "Bin (Pentad or Day): "
                                                                       data-index
                                                                       " has no rain!"))
                                                         nil)
                                                     (clojure.math/atan (/ (quickthing/orthogonal-error-length [proj-x
                                                                                                                proj-y
                                                                                                                {:err-x sv1-error
                                                                                                                 :err-y sv2-error}])
                                                                           radius))))}
                                    (-> polar-coords
                                        (update :radius
                                                #(* %
                                                    scale)))))))
                  projs
                  (->> projs
                       count
                       range)
                  scales
                  (->> projs
                       count
                       range
                       (mapv #(cycle-frac (fx/sub-ctx context
                                                      cycle-length)
                                          (fx/sub-ctx context
                                                      cycle-phase)
                                          %)))
                  (fx/sub-ctx context
                              errors-in-sv1-proj)
                  (fx/sub-ctx context
                              errors-in-sv2-proj)))))
#_
(-> @*selections
    (fx/sub-ctx sv-proj)
    first)
;; => [113.12006734716704
;;     -203.48496680096278
;;     {:index 0,
;;      :cycle-frac 0,
;;      :err-x 79.35568753767804,
;;      :err-y 157.21263605633916,
;;      :err-angle 0.35919181354944224,
;;      :radius 232.8138341048842,
;;      :angle-from-down 0.5073720887345978}]


;; => [24.164284948952567
;;     -44.09616040647046
;;     {:index 0,
;;      :cycle-frac 0,
;;      :err-x 12.984442896066495,
;;      :err-y 33.19259196387512,
;;      :err-angle 0.2803344675436626,
;;      :radius 50.283039185070635,
;;      :angle-from-down 0.5012992384298911}]


;; => [24.164284948952567
;;     -44.09616040647046
;;     {:index 0,
;;      :cycle-frac 0,
;;      :angle-from-down 0.5012992384298913,
;;      :length 50.283039185070635,
;;      :err-x 12.984442896066495,
;;      :err-y 33.19259196387512,
;;      :err-angle 0.2803344675436626}]

;; => [24.164284948952567
;;     -44.09616040647046
;;     {:index 0,
;;      :cycle-frac 0,
;;      :angle 5.213688218814581,
;;      :length 50.283039185070635,
;;      :err-x 12.984442896066495,
;;      :err-y 33.19259196387512,
;;      :err-angle 0.2803344675436626}]

#_
(->>(fx/sub-ctx @*selections
                sv-proj)
    (mapv #(get %
                2))
    (mapv :angle)
    (mapv #(if (> %
                  clojure.math/PI)
             (- %
                bisect/TWOPI)
             %)))

#_
(defn
  sv-2d-bisection
  [context]
  (-> context
      (fx/sub-ctx sv-proj)
      bisect/min-var))
#_
(-> @*selections
    (fx/sub-ctx sv-2d-bisection)
    :angle)


;; => 4.667193224617784;; => 2.827520782342667


#_
(-> @*selections
    (fx/sub-ctx sv-proj)
    bisect/angle-dichotomies)

(defn
  sv-angular-bisection
  [context]
  (-> context
      (fx/sub-ctx sv-proj)
      #_
      bisect/otsu
      ;;#_
      bisect/otsu-weighted
      #_
      bisect/min-angular-var))
#_
(-> @*selections
    (fx/sub-ctx sv-angular-bisection)
    :points
    second)
;; => 3.0963968978228875

;; => 1.5256005710279907

;; => 3.0876082629567905;; => 2.887023616887416

(defn
  sv-bisection
  [context]
  (-> context
      (fx/sub-ctx sv-angular-bisection)))
#_
(->> @*selections
     sv-bisection
     :centroid-a)


#_
(->> @*selections
     sv-bisection
     :points
     (map last)
     (map :angle-from-down))
;; => [113.12006734716704
;;     -203.48496680096278
;;     {:index 0,
;;      :cycle-frac 0,
;;      :err-x 79.35568753767804,
;;      :err-y 157.21263605633916,
;;      :err-angle 0.35919181354944224,
;;      :radius 232.8138341048842,
;;      :angle-from-down 0.5073720887345978,
;;      :above? false}]


;; => 6.1714157010825055

(defn
  sv-proj-svg
  [context]
  (-> (plot/sv-plot (* 0.750
                       (fx/sub-ctx context
                                   window-width))
                    (* 1.5
                       (fx/sub-ctx context
                                   window-width))
                    #_(* 4.0
                           (fx/sub-ctx context
                                       row-height))
                    (fx/sub-ctx context
                                sv-bisection))
      ;;      #_
      (spitsvgstream "sv-projs.svg")))
;;#_
(-> @*selections
    (fx/sub-ctx sv-proj-svg)
    nil?)


#_
(defn
  angular-historgram
  [context]
  (let [{:keys [angle
                points
                centroid-a
                centroid-b]} (-> context
                                 (fx/sub-ctx sv-angular-bisection))]
    (let [grouped (->> points
                       (group-by #(-> %
                                      (nth 2)
                                      :above?)))]
      (let [above-angles (->> (get grouped
                                   true)
                              (mapv #(-> %
                                         (nth 2)
                                         :delta-angle)))
            below-angles (->> (get grouped
                                   false)
                              (mapv #(-> %
                                         (nth 2)
                                         :delta-angle)))]
        (let [above-num (count above-angles)
              below-num (count below-angles)]
          (let [above-mean               (/ (->> above-angles
                                                 (apply +))
                                            above-num)
                below-mean               (/ (->> below-angles
                                                 (apply +))
                                            below-num)
                above-interquartile-mean (/ (->> above-angles
                                                 (drop (/ above-num
                                                          4))
                                                 (drop-last (/ above-num
                                                               4))
                                                 (apply +))
                                            (- above-num
                                               (* 2
                                                  (/ above-num
                                                     4))))
                below-interquartile-mean (/ (->> below-angles
                                                 (drop (/ below-num
                                                          4))
                                                 (drop-last (/ below-num
                                                               4))
                                                 (apply +))
                                            (- below-num
                                               (* 2
                                                  (/ below-num
                                                     4))))
                above-centroid-angle     (-> centroid-a
                                             bisect/to-angle
                                             (- angle))
                below-centroid-angle     (-> centroid-b
                                             bisect/to-angle
                                             (- angle))
                above-median             (-> above-angles
                                             sort
                                             (nth (-> above-num
                                                      (/ 2.0)
                                                      int)))
                below-median             (-> below-angles
                                             sort
                                             (nth (-> below-num
                                                      (/ 2.0)
                                                      int)))]
            (-> points
                (plot/angular-hist [(* 1.0
                                       (fx/sub-ctx context
                                                   window-width))
                                    (* 1.0
                                       (fx/sub-ctx context
                                                   row-height))]
                                   {:notes [[above-mean
                                             "AMean"]
                                            [below-mean
                                             "AMean"]
                                            [above-interquartile-mean
                                             "InterMean"]
                                            [below-interquartile-mean
                                             "InterMean"]
                                            [above-median
                                             "Median"]
                                            [below-median
                                             "Median"]
                                            [above-centroid-angle
                                             "Centroid"]
                                            [below-centroid-angle
                                             "Centroid"]]})
                (spitsvgstream "angles-hist.svg"))))))))
#_
(-> @*selections
    (fx/sub-ctx angular-historgram)
    nil?)
;; => ([4.066817326755436
;;      {:cycle-frac 0, :delta-angle 4.066817326755436, :above? false}]
;;     [3.979364672052008
;;      {:cycle-frac 1/73, :delta-angle 3.979364672052008, :above? false}]
;;     [2.9663820135949375
;;      {:cycle-frac 2/73, :delta-angle 2.9663820135949375, :above? true}])

(defn-
  rezero-vec
  "Remove the non-zero minimum value from the vector elements"
  [input-vec
   & {:keys [mask]
      :or   {mask (->> input-vec
                       (mapv #(or (zero? %)
                                  (neg? %))))}}]
  (let [vec-min (->> (map (fn [pix
                               mask]
                            (if mask
                              nil
                              pix))
                          input-vec
                          mask)
                     (filter some?)
                     (apply min))]
    (mapv (fn [pix
               mask]
            (if mask
              0.0
              (- pix
                 vec-min)))
          input-vec
          mask)))
#_
(rezero-vec [ 0 3 4 5 5])

(defn
  top-pattern
  [context]
  (let [{:keys [centroid-a]} (fx/sub-ctx context
                                         sv-bisection)]
    (let [sval-one  (-> context
                        (fx/sub-ctx singular-value 0))
          sval-two  (-> context
                        (fx/sub-ctx singular-value 1))
          [x-coord
           y-coord] centroid-a]
      (let [patt (fx/sub-ctx context
                             singular-vector-mixture
                             x-coord
                             y-coord
                             sval-one
                             sval-two)]
        (if (fx/sub-ctx context
                        non-zero-min?)
          (rezero-vec patt
                      {:mask (fx/sub-ctx context
                                         zero-point-mask)})
          patt))))) ;; TODO: Normalize? I think..
#_
(-> @*selections
    (fx/sub-ctx top-pattern))

(defn
  top-pattern-svg
  [context]
  (let [input-grid (geogrid4seq/build-grid (-> context
                                               (fx/sub-ctx region-geogrid-params))
                                           (-> context
                                               (fx/sub-ctx top-pattern)))]
    (let [[width
           height] (geoprim/dimension (geogrid/covered-region input-grid))]
      (-> input-grid
          (plot/grid-map (fx/sub-ctx context
                                     region-svg-hiccup)
                         {:label-top-right "Top Pattern"
                          :label-attribs   {:fill      "#00aa88"
                                            :font-size (/ (min width
                                                               height)
                                                          9)}
                          #_#_
                          :colormap        (into quickthing/rainbow
                                                 quickthing/rainbow)
                          :display-width   (fx/sub-ctx context
                                                       region-display-width)})
          (spitsvgstream "top-pattern.svg")))))
;;#_
(-> @*selections
    (fx/sub-ctx top-pattern-svg)
    empty?)

(defn
  top-pattern-weighted-noise
  "Weight the remaining noise by the first pattern"
  [context
   index]
  (-> context
      (fx/sub-ctx noise-matrix-2d)
      (datamats/extract-grid index)
      :data-array
      (#(mapv *
              %
              (fx/sub-ctx context top-pattern)))))
#_
(-> @*selections
    (fx/sub-ctx top-pattern-weighted-noise 6))

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
(-> @*selections
    (datafile-svg 6))
#_
(-> @*selections
    (noise-svg 6))
#_
(-> @*selections
    (top-pattern-weighted-noise-svg 6))
#_
(-> @*selections
    (datafile-svg 31))
#_
(-> @*selections
    (noise-svg 31))
#_
(-> @*selections
    (top-pattern-weighted-noise-svg 31))

(defn
  bottom-pattern
  [context]
  (let [{:keys [centroid-b]} (fx/sub-ctx context
                                         sv-bisection)]
    (let [sval-one  (-> context
                        (fx/sub-ctx singular-value 0))
          sval-two  (-> context
                        (fx/sub-ctx singular-value 1))
          [x-coord
           y-coord] centroid-b]
      (let [patt (fx/sub-ctx context
                             singular-vector-mixture
                             x-coord
                             y-coord
                             sval-one
                             sval-two)]
        (if (fx/sub-ctx context
                        non-zero-min?)
          (rezero-vec patt
                      {:mask (fx/sub-ctx context
                                         zero-point-mask)})
          patt)))))
#_
(-> @*selections
    (fx/sub-ctx bottom-pattern))

#_
(->>
  (-> @*selections
      (fx/sub-ctx bottom-pattern))
  (filter pos?)
  (apply min))
;; => 5.912743094506505E-16


(defn
  bottom-pattern-svg
  [context]
    (let [input-grid (geogrid4seq/build-grid (-> context
                                               (fx/sub-ctx region-geogrid-params))
                                           (-> context
                                               (fx/sub-ctx bottom-pattern)))]
    (let [[width
           height] (geoprim/dimension (geogrid/covered-region input-grid))]
      (-> input-grid
          (plot/grid-map (fx/sub-ctx context
                                     region-svg-hiccup)
                         {:label-top-right "Bottom Pattern"
                          :label-attribs   {:fill      "#aa8800"
                                            :font-size (/ (min width
                                                               height)
                                                          9)}
                          #_#_
                          :colormap        (into quickthing/rainbow
                                                 quickthing/rainbow)
                          :display-width   (fx/sub-ctx context
                                                       region-display-width)})
          (spitsvgstream "bottom-pattern.svg")))))
;;#_
(-> @*selections
    (fx/sub-ctx bottom-pattern-svg)
    empty?)

(defn
  bottom-pattern-weighted-noise
  "Weight the remaining noise by the first pattern"
  [context
   index]
  (-> context
      (fx/sub-ctx noise-matrix-2d)
      (datamats/extract-grid index)
      :data-array
      (#(mapv *
              %
              (fx/sub-ctx context
                          bottom-pattern)))))
#_
(-> @*selections
    (fx/sub-ctx bottom-pattern-weighted-noise 6))

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
(-> @*selections
    (datafile-svg 10))
#_
(-> @*selections
    (noise-svg 10))
#_
(-> @*selections
    (bottom-pattern-weighted-noise-svg 10))
#_
(-> @*selections
    (datafile-svg 11))
#_
(-> @*selections
    (noise-svg 11))
#_
(-> @*selections
    (bottom-pattern-weighted-noise-svg 11))

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
    (let [centroid-a-angle (-> centroid-a
                               bisect/to-polar
                               :angle-from-down
                               (- (/ PI
                                     2.0)))
          centroid-b-angle (-> centroid-b
                               bisect/to-polar
                               :angle-from-down
                               (- (/ PI
                                     2.0)))]
      (let [projections    (matrix/project-onto-2-patterns  centroid-a ;; datamats/project-onto-2d-basis
                                                            centroid-b
                                                            points)
            err-centroid-a (->> points
                                (mapv (fn [point]
                                        (quickthing/parallel-error-length centroid-a-angle
                                                                          point))))
            err-centroid-b (->> points
                                (mapv (fn [point]
                                        (quickthing/parallel-error-length centroid-b-angle
                                                                          point))))]
        #_projections
        (mapv (fn [point
                   err-cent-a
                   err-cent-b]
                (-> point
                    (update 2
                            #(assoc %
                                    :err-centroid-a
                                    err-cent-a))
                    (update 2
                            #(assoc %
                                    :err-centroid-b
                                    err-cent-b))))
              projections
              err-centroid-a
              err-centroid-b)))))

#_
(-> @*selections
    sv-bisection
    :centroid-a)
;; => [0.7085216175364683 0.705689108236415]

#_
(-> @*selections
    sv-bisection
    :centroid-b)
;; => [0.5739485119789848 -0.8188913881566402]


#_
(-> @*selections
    sv-bisection
    :points
    (get 2))
;; => [147.88696751163914
;;     -174.6602634890784
;;     {:index 2,
;;      :cycle-frac 1/6,
;;      :err-x 52.162313764093206,
;;      :err-y 127.42670092948086,
;;      :err-angle 0.2750300327604253,
;;      :radius 228.85970113120158,
;;      :angle-from-down 0.7025821993082306,
;;      :above? false}]

(map *
     [0.5739485119789848 -0.8188913881566402]
     [147.88696751163914 -174.6602634890784])
;; => (84.87950494438975 143.02778562437595)

#_
(-> @*selections
    sv-bisection
    :points
    (get 71))
;; => [112.57764719667338
;;     -253.63562272487084
;;     {:index 71,
;;      :cycle-frac 11/12,
;;      :err-x 65.33889317833521,
;;      :err-y 176.27591651947827,
;;      :err-angle 0.24892029918671557,
;;      :radius 277.49730766869015,
;;      :angle-from-down 0.4177326901041829,
;;      :above? false}]

(map *
     [0.5739485119789848 -0.8188913881566402]
     [112.57764719667338 -253.63562272487084])
;; => (64.61377309062581 207.70002717914335)

#_
(-> @*selections
    pattern-proj
    (get 2))
;; => [-18.474732152035315
;;     227.9072905687657
;;     {:cycle-frac 1/6,
;;      :angle-from-down 0.7025821993082306,
;;      :index 2,
;;      :err-y 127.42670092948086,
;;      :radius 228.85970113120158,
;;      :err-centroid-a 68.17283427493027,
;;      :err-centroid-b 78.47856683923837,
;;      :err-x 52.162313764093206,
;;      :above? false,
;;      :err-angle 0.2750300327604253}]

#_
(-> @*selections
    pattern-proj
    (get 71))
;; => [-99.224199727465
;;     272.31380026976916
;;     {:cycle-frac 11/12,
;;      :angle-from-down 0.4177326901041829,
;;      :index 71,
;;      :err-y 176.27591651947827,
;;      :radius 277.49730766869015,
;;      :err-centroid-a 86.51137672966664,
;;      :err-centroid-b 100.63471700224113,
;;      :err-x 65.33889317833521,
;;      :above? false,
;;      :err-angle 0.24892029918671557}]

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
(-> @*selections
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
         (datamats/from-vecofvecs (fx/sub-ctx context
                                            region-matrix)))))
#_
(->> @*selections
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
(-> @*selections
    binary-index-vector
    first)
;; => false
;; (so January is classified as "summer")
#_
(take 2
      (-> @*selections
          (top-pattern-weighted-noise 0)))
;; => (23429.16540445495 18104.86334445968)
#_
(take-last 2
           (-> @*selections
               (top-pattern-weighted-noise 0)))
;; => (-28273.311179534514 -11668.173447261712)

(defn
  climate-noise-svg
  "Get the noise background of one data point"
  [context
   id]
  (-> (datamats/extract-grid (fx/sub-ctx context
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
(-> @*selections
    (climate-noise-svg 31))

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
(-> @*selections
    (fx/sub-ctx first-normalized-noise-selected-svg))

(defn
  climate-noise-vars
  [context]
  (-> context
      (fx/sub-ctx climate-noise-matrix-2d-normalized)
      (datamats/colvars)))
#_
(->> @*selections
     climate-noise-vars)

(defn climate-noise-var-svg
  [context]
  (-> (plot/index (* 1.0
                     (fx/sub-ctx context
                                 window-width))
                  (* 1.0
                     (fx/sub-ctx context
                                 row-height))
                  (-> context
                      (fx/sub-ctx climate-noise-vars))
                  2011
                  (fx/sub-ctx context
                              cycle-length)
                  (fx/sub-ctx context
                              cycle-phase))
      (spitsvgstream "indeces-vars.svg")))
#_
(->> @*selections
     climate-noise-var-svg)


;;      :err-centroid-a 0.018324016829075744,
;;      :err-centroid-b 0.020913045151059212,

(defn
  pattern-proj-partitioned
  "Projections of all the data
  onto the two extracted patterns, but partitioned into two groups
  Returns:
  a two list of nonorthogonal coordinates
  and a list of errors"
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
                                   0.0))))
        errors      (->> projections
                         (mapv (fn [proj]
                                 (if (-> proj
                                         (get 2)
                                         :above?)
                                   (-> proj
                                       (get 2)
                                       :err-centroid-a)
                                   (-> proj
                                       (get 2)
                                       :err-centroid-b)))))]
    (with-open [writer (io/writer (str config-dir
                                       "/climate-index.csv"))]
      (println (str "Writing out climate index to CSV file .. "))
      (csv/write-csv writer
                     (mapv vector
                           proj-a
                           proj-b
                           errors)))
    [proj-a
     proj-b
     errors]))
#_
(-> @*selections
    pattern-proj-partitioned
    (nth 2))

#_
(-> @*selections
(fx/sub-ctx pattern-proj-partitioned)
first
count)
;; => 146
#_
(-> @*selections
(fx/sub-ctx pattern-proj-partitioned)
second
count)
;; => 146
#_
(-> @*selections
(fx/sub-ctx pattern-proj-partitioned)
(nth 0)#_
count)

(defn
  pattern-proj-svg
  "Plot of the climate indeces"
  [context]
  (let [[proj-a
         proj-b
         errors] (fx/sub-ctx context
                             pattern-proj-partitioned)
        width    (fx/sub-ctx context
                             window-width)
        height   (fx/sub-ctx context
                             row-height)]
    (-> (plot/indeces width
                      height
                      proj-a
                      proj-b
                      errors
                      2011
                      (fx/sub-ctx context
                                  cycle-length)
                      (fx/sub-ctx context
                                  cycle-phase)
                      {:bar-width (* 0.5
                                     (/ width
                                        (count proj-a)))})
        (spitsvgstream "indeces.svg"))))
;;#_
(-> @*selections
    (fx/sub-ctx pattern-proj-svg)
    nil?)


(defn
  singular-values-stats
  [context]
  (-> context
      (fx/sub-ctx singular-values)
      datamats/singular-values-stats))
#_
(-> @*selections
    (fx/sub-ctx singular-valuesstats))

(defn
  singular-values-svg
  [context]
  (-> (plot/sv-weights (fx/sub-ctx context
                                   singular-values)
                       20
                       (fx/sub-ctx context
                                   singular-values-stats)
                       (* 1.0
                          (fx/sub-ctx context
                                      window-width))
                       (* 1.0
                          (fx/sub-ctx context
                                      row-height)))
      (spitsvgstream "singular-values.svg")))
;;#_
(-> @*selections
    (fx/sub-ctx singular-values-svg))
#_
(spit (str "out/"
           (-> @*selections
               (fx/sub-ctx region-key)
               symbol)
           "/test-singular-values-actual.svg")
      (-> @*selections
          (fx/sub-ctx singular-valuess-svg)))

(defn
  observation-svg
  "May be different from `datafile-svg`
  b/c you may have binning"
  [context
   observation-idx
   & {:keys [cycle-length]
      :or {cycle-length (fx/sub-ctx context
                                    cycle-length)}}]
  (-> context
      (fx/sub-ctx region-matrix)
      datamats/to-geogrid-vec
      (get observation-idx)
      (plot/grid-map (fx/sub-ctx context
                                 region-svg-hiccup)
                     {:label-top-right (str (inc observation-idx))
                      #_#_
                      :max-val         (->  context
                                            (fx/sub-ctx region-min-max)
                                            second)
                        :label-attribs {#_#_:font-size 0.7}
                      :axis-visible?   false
                      :cycle-frac      (/ observation-idx
                                          12.0)})
      (spitsvgstream (str "observation-"
                          observation-idx
                          ".svg"))))
;;#_
(let [cycle-length 12 #_(fx/sub-ctx @*selections
                               cycle-length)]
  (->> [0 1 2]
       (mapv (fn [cycle-num]
               [(-> cycle-num
                    (* cycle-length)
                    (+ 3))
                (-> cycle-num
                    (* cycle-length)
                    (+ 4))
                (-> cycle-num
                    (* cycle-length)
                    (+ 5))
                (-> cycle-num
                    (* cycle-length)
                    (+ 9))
                (-> cycle-num
                    (* cycle-length)
                    (+ 10))
                (-> cycle-num
                    (* cycle-length)
                    (+ 11))]))
       flatten
       (mapv (fn [index]
               ;;index #_
               (-> @*selections
                   (fx/sub-ctx observation-svg (int index)))))))
;; => [3 4 5 9 10 11 15 16 17 21 22 23 27 28 29 33 34 35]

#_
(let [cycle-length (fx/sub-ctx @*selections
                               cycle-length)]
  (->> [0 1 2]
       (mapv (fn [cycle-num]
               [(-> cycle-num
                    (* cycle-length))
                (-> cycle-num
                    (* cycle-length)
                    inc)
                (-> cycle-num
                    (* cycle-length)
                    inc
                    inc)
                (-> cycle-num
                    (* cycle-length)
                    (+ (/ cycle-length
                          2)))
                (-> cycle-num
                    (* cycle-length)
                    (+ (/ cycle-length
                          2))
                    inc)
                (-> cycle-num
                    (* cycle-length)
                    (+ (/ cycle-length
                          2))
                    inc
                    inc)
                (-> cycle-num
                    inc
                    (* cycle-length)
                    dec)
                (-> cycle-num
                    inc
                    (* cycle-length)
                    dec
                    dec)
                (-> cycle-num
                    inc
                    (* cycle-length)
                    dec
                    dec
                    dec)]))
       flatten
       (mapv (fn [index]
               (-> @*selections
                   (fx/sub-ctx observation-svg (int index)))))))


(-> @*selections
    (fx/sub-ctx region-matrix))
  
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
                                            :max-val         (->  context
                                                                  (fx/sub-ctx region-min-max)
                                                                  second)
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
                   datamats/to-geogrid-vec))
      (spitsvgstream "precipitation-all.svg")))
;;#_ ;;unused
(if (-> @*selections
        (fx/sub-ctx datafile-strs)
        count
        (< 200))
  (-> @*selections
      (fx/sub-ctx precipitation-all-svg)))

(defn
  noise-all-svg
  [context]
  (-> context
      (all-svg (-> context
                   (fx/sub-ctx noise-matrix-2d)
                   datamats/to-geogrid-vec))
      (spitsvgstream "noise-all.svg")))
;;unused
(if (-> @*selections
        (fx/sub-ctx num-svs)
        (< 200))
  (-> @*selections
      (fx/sub-ctx noise-all-svg)))

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
                 (mapv (partial datamats/extract-grid
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
(-> @*selections
    (fx/sub-ctx cycle-group-svg
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
                 (mapv (partial datamats/extract-grid
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
(-> @*selections
    (fx/sub-ctx annual-cycle
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
                         (fx/sub-ctx @*selections
                                     eas-res)
                         (fx/sub-ctx @*selections
                                     sou-res))
#_
(.getType (fx/sub-ctx @*selections
                      elevation-geogrid))

(defn
  power-of-sv-weights-scaled
  "Return a pair of the eof1weight and variance
   (relative to the EOF1 signal)
  for a given INDEX (ie. time point)"
  [context]
  (let [num-of-svs    (-> context
                          (fx/sub-ctx num-svs))
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
(->> (-> @*selections
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
(->> (-> @*selections
         (fx/sub-ctx sv12-vs-other))
     (mapv (fn [pair]
             (/ (first pair)
                (second pair)))))



(defn
  sv12-vs-other-svg
  [context]
  (let [points (fx/sub-ctx context
                           sv12-vs-other)]
    (-> (mapv (fn [points
                   cycle-fraction]
                (update points
                        2
                        #(assoc %
                                :cycle-frac
                                cycle-fraction)))
              points
              (->> points
                   count
                   range
                   (mapv #(cycle-frac (fx/sub-ctx context
                                                  cycle-length)
                                      (fx/sub-ctx context
                                                  cycle-phase)
                                      %))))
        (plot/add-cycle-color)
        (plot/scatter 1000
                      1000
                      {:title-str "SV Power Plot"
                       :x-name    "SV1 and SV2"
                       :y-name    "Other SVs"})
        (spitsvgstream (str "power-sv12-vs-other"
                            ".svg")))))
#_;;usused
(-> @*selections
    (fx/sub-ctx sv12-vs-other-svg))
