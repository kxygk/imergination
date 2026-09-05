(ns
    kxygk.imergination.stateom
  "Program and GUI state"
  (:use [clojure.math])
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [cljfx.api       :as fx]
            [clojure.core.cache :as cache]
            ;;            [injest.path :refer [+> +>> x>> =>>]]
            [kxygk.imergination.bisect :as bisect]
            [kxygk.imergination.zip :as zip]
            [geogrid4image]
            [geogrid4seq]
            [kxygk.imergination.datamats :as datamats]
            #_
            kxygk.imergination.matrix4neanderthal
            ;;#_
            kxygk.imergination.matrix4ojalgo ;; this sets the matrix backend
            [kxygk.imergination.matrix :as matrix] ;; only used in one spot
            [kxygk.imergination.plot :as plot]
            [kxygk.imergination.locations :as locations]
            ;;
            kxygk.pathmore.cache
            kxygk.pathmore.async
            [com.wsscode.pathom3.connect.built-in.resolvers :as pbir]
            [com.wsscode.pathom3.interface.smart-map :as psm]
            [com.wsscode.pathom3.connect.runner :as pcr]
            [com.wsscode.pathom3.connect.operation :as pco]
            [com.wsscode.pathom3.interface.async.eql :as p.a.eql]
            [com.wsscode.pathom3.interface.eql :as p.eql]
            [com.wsscode.pathom3.connect.planner :as pcp]
            [com.wsscode.pathom3.connect.indexes :as pci]
            [promesa.core :as p]))

#_(time (check :region-matrix))
#_(time (gensummary))

(def pathom-plan-cache*
  (atom {}))

(def debug?
  true)

(def
  config-dir
  (str "/home/kxygk/Projects/imergination.wiki/"
       #_
       "nao-anomaly-monthly"
       #_
       "krabi-monthly-2year"
       #_
       "tianshan-monthly"
       #_
       "tianshan-pentads-10year"
       #_
       "tianshan-pentads-2year"
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
       #_
       "krabi-monthly"
       #_
       "krabi-monthly-final-v7"
       ;;#_
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
  (atom (merge {;; Defaults
                :barchart-height-width-ratio 3.0
                :plot-zoom-factor            360
                :shoreline-filestr           nil
                :contour-filestr             nil
                :non-zero-min?               false
                :normalize-data?             true
                #_#_#_#_:rain-dirstr         "/home/kxygk/Data/sst/monthly/geotiff-rot/"
                :elevation-filestr           "./data/World_e-Atlas-UCSD_SRTM30-plus_v8.tif"
                :bin-size                    1
                :cycle-length                12
                :cycle-phase                 0
                :eas-res                     0.1
                :sou-res                     0.1
                :region-key                  :krabi-root-2
                :is-in-ram                   false
                :mouse-click                 nil
                :datafile-idxs               [0]
                :sv-selected-idxs            [0]
                :noise-selected-idxs         [0]
                :climate-noise-selected-idxs [0]}
               (if (nil? config-dir)
                 {}
                 (-> config-dir
                     (str "/config.edn")
                     slurp
                     clojure.edn/read-string)))))
#_
(-> @(p.a.eql/process env
                      @*selections
                      [:is-in-ram
                       :non-zero-min?
                       :normalize-data?
                       :row-height
                       :shoreline-filestr
                       :region-key]))

(pco/defresolver $barchart-height
  [{:keys [plot-zoom-factor]}]
  {:barchart-height plot-zoom-factor})

(pco/defresolver $barchart-width
  [{:keys [plot-zoom-factor
           barchart-height-width-ratio]}]
  {:barchart-width (* plot-zoom-factor
                      barchart-height-width-ratio)})

(def $shoreline
  (pbir/single-attr-resolver :shoreline-filestr
                             :shoreline
                             (fn [shoreline-filestr]
                               (if (nil? shoreline-filestr)
                                 (slurp (io/resource "data/shoreline-coarse.json"))
                                 (slurp (io/file shoreline-filestr))))))
;; (check :shoreline)

(pco/defresolver $dummy-barchart-svg
  [{:keys [barchart-width
           barchart-height]}]
  {::pco/output [{:dummy-barchart-svg [:hiccup]}]}
  {:dummy-barchart-svg {:hiccup (plot/empty-svg barchart-width
                                               barchart-height)}})

(pco/defresolver $dummy-sv-proj-svg
  [{:keys [barchart-width
           barchart-height]}]
  {::pco/output [{:dummy-sv-proj-svg [:hiccup]}]}
  {:dummy-sv-proj-svg {:hiccup (plot/empty-svg barchart-width
                                               (* barchart-width
                                                  2.0))}})

#_
(-> @(p.a.eql/process env
                      @*selections
                      [:region-key]))


(defn regularize-region-bounds
  [{:keys [start-lat
           start-lon
           ended-lat
           ended-lon]}]
  (if (and (not= start-lat
                 ended-lat)
           (not= start-lon
                 ended-lon)
           (< -90
              start-lat
              90)
           (< -90
              ended-lat
              90)
           (< -180
              start-lon
              180)
           (< -180
              ended-lon
              180))
    {:start-lat (max start-lat
                     ended-lat)
     :start-lon (min start-lon
                     ended-lon)
     :ended-lat (min start-lat
                     ended-lat)
     :ended-lon (max start-lon
                     ended-lon)}))


(def $region
  (pbir/single-attr-resolver :region-key
                             :region
                             (fn [region-key]
                               (:region (get locations/regions
                                             region-key)))))
#_
(-> @(p.a.eql/process env
                      @*selections
                      [:region]))
;;{:region #geoprim.nwse-region{:norwes #geoprim.eassou-point{:eas 277.5, :sou 76.6}, :soueas #geoprim.eassou-point{:eas 281.0, :sou 84.6}}}



(pco/defresolver $decompose-region
  [{:keys [region]}]
  {::pco/output [:start-lat
                 :start-lon
                 :ended-lat
                 :ended-lon]}
  (let [[top-left
         _
         bot-right
         _] (geoprim/four-corners region)]
    (let [[start-lat
           start-lon] (geoprim/as-latlon top-left)
          [ended-lat
           ended-lon] (geoprim/as-latlon bot-right)]
      {:start-lat start-lat
       :start-lon start-lon
       :ended-lat ended-lat
       :ended-lon ended-lon})))
#_
(-> @(p.a.eql/process env
                      @*selections
                      [:start-lat
                       :start-lon
                       :ended-lat
                       :ended-lon])
    regularize-region-bounds)
;;{:start-lat 12.400000000000006, :start-lon 97.5, :ended-lat 5.400000000000006, :ended-lon 101.0}
;;{:start-lat 12.400000000000006, :start-lon 97.5, :ended-lat 5.400000000000006, :ended-lon 101.0}

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
  (let [region-key nil #_ (-> @*selections
                              (fx/sub-ctx region-key))
        subfolder  (if (nil? region-key)
                     "custom"
                     (symbol region-key))]
    (if debug?
      (do #_(println "Writing to File")
          (p/vthread (spit (str config-dir
                                "/"
                                filename)
                           #_(str "../imergination.wiki/"
                                  subfolder
                                  "/"
                                  filename)
                           string)))
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
  (if debug?
    (do (println (str "Writing.. "
                      filename))
        (let [realized-hiccup (do #_(println "Generating Hiccup")
                                  (doall svg-hiccup))]
          (let [xml (do #_(println "Generating XML")
                        (quickthing/svg2xml realized-hiccup))]
            #_xml
            (spitstream xml
                        filename)
            svg-hiccup)))
    svg-hiccup))
;; ***************************************

;; TODO Make these checks on first run
;;#_#_
(if (not (zero? (mod (:cycle-length @*selections)
                     (:bin-size @*selections))))
  (println "ERROR: The `bin-size` doesn't cleanly divide the cycle length"))

#_
(-> @(p.a.eql/process env
                      @*selections
                      [:bin-size
                       :cycle-length
                       :cycle-phase]))

(pco/defresolver $cycle-length-bins
  [{:keys [bin-size
           cycle-length]}]
  {:cycle-length-bins (/ cycle-length
                         bin-size)})
#_
(-> @(p.a.eql/process env
                      @*selections
                      [:cycle-length-bins]))

(defn-
  cycle-frac
  [cycle-length
   cycle-phase
   idx]
  (/ (mod (+ idx
             cycle-phase)
          cycle-length)
     cycle-length))

#_
(-> @(p.a.eql/process env
                      @*selections
                      [:window-width]))

(def $region-xy-ratio
  (pbir/single-attr-resolver :region
                             :region-xy-ratio
                             (fn [region]
                               (let [[lat
                                      lon] (geoprim/dimension region)]
                                 (/ lat
                                    lon)))))
#_
(-> @(p.a.eql/process env
                      @*selections
                      [:region-xy-ratio]))

#_
(-> @(p.a.eql/process env
                      @*selections
                      [:eas-res
                       :sou-res]))

(pco/defresolver $world-svg
  [{:keys [shoreline]}]
  {::pco/output [{:world-svg [:hiccup]}]}
  {:world-svg {:hiccup (-> locations/world-region
                           (plot/shoreline-map shoreline
                                               [])
                           (spitsvgstream "world.svg"))}})
#_
(check :world-svg)


(pco/defresolver $world-with-region-highlight-svg
  [{:keys [shoreline
           region]}]
  {::pco/output [{:world-with-region-highlight-svg [:hiccup]}]}
  {:world-with-region-highlight-svg {:hiccup (-> locations/world-region
                                                 (plot/shoreline-map shoreline
                                                                     [])
                                                 (plot/worldmap-region region)
                                                 (spitsvgstream "world-with-region.svg"))}})
#_
(check :world-svg)


(pco/defresolver $contour-bare-svg
  "A bare contour of the region. No legends or axis"
  [{:keys [shoreline
           region]}]
  {::pco/output [{:contour-bare-svg [:hiccup]}]}
  {:contour-bare-svg {:hiccup (-> region
                                  (plot/shoreline-map shoreline
                                                      {:axis-visible? false})
                                  #_;; not interesting
                                  (spitsvgstream "contour-bare.svg"))}})
#_
(check :contour-bare-svg)

(pco/defresolver $contour-svg
  "A contour on a map (with lat/lon). Use as loading placeholder"
  [{:keys [shoreline
           region]}]
  {::pco/output [{:contour-svg [:hiccup]}]}
  (println "CONTOUR-SVG resolver running, region:" (some-> region .hashCode))
  {:contour-svg {:hiccup (-> region
                             (plot/shoreline-map shoreline
                                                 {:axis-visible? true})
                             ;; not interesting
                             (spitsvgstream "contour.svg"))}})
#_
(let [shoreline (check :shoreline)]
  (time (check :contour-svg
               (assoc shoreline
                      :region
                      (:region (:java locations/regions))))))



(def $data-dirstr
  (pbir/single-attr-resolver :rain-dirstr
                             :data-dirstr
                             (fn [specified-dir]
                               (if (some? specified-dir) ;; is directory specified?
                                 specified-dir
                                 ;; if not, unzip our backup data
                                 (-> "data/imerg-late-v06b-10yrs-2011-through-2021.zip"
                                     io/resource
                                     zip/unzip
                                     .getPath)))))
#_
@(p.a.eql/process env
                  @*selections
                  [:data-dirstr])

(def $datafile-strs
  (pbir/single-attr-resolver :data-dirstr
                             :datafile-strs
                             (fn [maybe-dirstr]
                               (if (nil? maybe-dirstr)
                                 ;; should fall back on to the baked in dataset
                                 (println "The Default dataset was missing!")
                                 (->> maybe-dirstr
                                      java.io.File.
                                      .list
                                      sort)))))
#_
@(p.a.eql/process env
                  @*selections
                  [:datafile-strs])

(def $datafile-strs-formatted
  (pbir/single-attr-resolver :datafile-strs
                             :datafile-strs-formatted
                             #(->> %
                                   (map-indexed (fn append-index
                                                  [index
                                                   file-str]
                                                  (str "["
                                                       index
                                                       "]"
                                                       file-str)))
                                   vec)))

(pco/defresolver $data-locations
  [{:keys [data-dirstr
           datafile-strs]}]
  {:data-locations (let [directory data-dirstr]
                     (mapv #(clojure.java.io/file (str directory
                                                       "/"
                                                       %))
                           datafile-strs))});; no POI
#_
(-> @(p.a.eql/process env
                      @*selections
                      [:data-locations]))





(defn-
  lazy-world-reader
  "Returns a lazy collection for reading in all the rainmaps"
  [file-locations
   easres
   soures]
  (let [num-files (count file-locations)]
    #_(println "FILE-READ thread:" (.getName (Thread/currentThread)))
    (vec (map-indexed (fn read-in-a-file
                   [index
                    file-location]
                   (do (println (str "Reading "
                                     (inc index)
                                     " of "
                                     num-files))
                       (geogrid4image/read-location file-location
                                                    easres
                                                    soures)))
                 file-locations))))
;; #_
;; (geogrid4image/read-location (clojure.java.io/file
;;                                  "/home/kxygk/Data/20CRv2c/SLP/pressure/geotiff-rot-subset/pres_only.nc-block0074-rot.tiff")
;;                              2.0
;;                              2.0)
;; #_
;; #geogrid4image.imagegrid{:norwes-point #geoprim.eassou-point{:eas 0.0,
;;                                                              :sou 0.0},
;;                          :image #object[java.awt.image.BufferedImage
;;                                         0x3b76a9e5
;;                                         "BufferedImage@3b76a9e5: type = 11 ColorModel: #pixelBits = 16 numComponents = 1 color space = java.awt.color.ICC_ColorSpace@ef7b344 transparency = 1 has alpha = false isAlphaPre = false ShortInterleavedRaster: width = 180 height = 91 #numDataElements 1"],
;;                          :eas-res 0.2,
;;                          :sou-res 0.2}
;; #_
;; (geogrid4image/read-location (clojure.java.io/file
;;                                 "/home/kxygk/Data/imerg/monthly/late-02years/3B-MO-L.GIS.IMERG.20110401.V06B.tif")
;;                              0.1
;;                              0.1)
;; #_
;; #geogrid4image.imagegrid{:norwes-point #geoprim.eassou-point{:eas 0.0,
;;                                                              :sou 0.0},
;;                          :image #object[java.awt.image.BufferedImage
;;                                         0x4d488d61
;;                                         "BufferedImage@4d488d61: type = 11 ColorModel: #pixelBits = 16 numComponents = 1 color space = java.awt.color.ICC_ColorSpace@ef7b344 transparency = 1 has alpha = false isAlphaPre = false ShortInterleavedRaster: width = 3600 height = 1800 #numDataElements 1"],
;;                          :eas-res 0.1,
;;                          :sou-res 0.1}

#_
(->> (-> "/home/kxygk/Data/20CRv2c/SLP/pressure/geotiff-rot-subset/pres_only.nc-block0074-rot.tiff"
         clojure.java.io/file
         (geogrid4image/read-location 2.0
                                      2.0)
         :image
         .getData
         .getDataBuffer
         .getData)
     (into-array
       Double/TYPE)
     seq
     first)

#_
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
(-> @*selections
    (fx/sub-ctx region-geogrid-vec)
    first
    keys)
#_
(-> @*selections
    (fx/sub-ctx region-geogrid-vec)
    first)
;; #_
;; #geogrid4image.imagegrid{:norwes-point #geoprim.eassou-point{:eas 80.0,
;;                                                              :sou 10.0},
;;                          :image #object[java.awt.image.BufferedImage 0x714b2122
;;                                         "BufferedImage@714b2122: type = 11 ColorModel: #pixelBits = 16 numComponents = 1 color space = java.awt.color.ICC_ColorSpace@ef7b344 transparency = 1 has alpha = false isAlphaPre = false ShortInterleavedRaster: width = 70 height = 35 #numDataElements 1"],
;;                          :eas-res 2.0,
;;                          :sou-res 2.0}

(pco/defresolver $world-geogrid-vec
  [{:keys [data-locations
           eas-res
           sou-res]}]
  {:world-geogrid-vec (lazy-world-reader data-locations
                                         eas-res
                                         sou-res)})
#_
(-> @(p.a.eql/process env
                      @*selections
                      [:world-geogrid-vec]))


;; TODO: The lazyness is probably broken right now
(pco/defresolver $region-geogrid-vec
  [{:keys [region
           is-in-ram
           world-geogrid-vec]}]
  {:region-geogrid-vec (if is-in-ram
                         (->> world-geogrid-vec
                              (map #(do #_(println "\nCutting out region ..")
                                        (geogrid/subregion %
                                                           region))))
                         (->> world-geogrid-vec
                              (map #(do #_(println "\nCutting out region ..")
                                        (geogrid/subregion %
                                                           region)))))})
#_
(-> @(p.a.eql/process env
                      @*selections
                      [:region-geogrid-vec]))

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

#_
(-> @(p.a.eql/process env
                      @*selections
                      [:non-zero-min?]))

(pco/defresolver $region-matrix
  [{:keys [bin-size
           region-geogrid-vec
           normalize-data?
           non-zero-min?]}]
  {:region-matrix (let [binned-grids     (bin-geogrids bin-size
                                                       region-geogrid-vec)
                        normalized-grids (if normalize-data?
                                           (map (partial normalize-geogrid
                                                         non-zero-min?)
                                                binned-grids)
                                           (map (fn [grid]
                                                  {:grid  grid
                                                   :scale 1.0
                                                   :shift 0.0})
                                                binned-grids))]
                    (-> (map :grid
                             normalized-grids)
                        datamats/from-geogrids
                        (assoc :scales (->> normalized-grids
                                            (mapv :scale)))
                        (assoc :shifts (->> normalized-grids
                                            (mapv :shift)))))})
#_
(-> @(p.a.eql/process env
                      @*selections
                      [:region-matrix]))
;; WARNING:
;; With the OjAlgo backend these matrixes won't print!
;;
;; use Neanderthal to print/inspect matrices.
;; Ex:
;;
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

(def $region-geogrid-params
  (pbir/single-attr-resolver :region-matrix
                             :region-geogrid-params
                             #(datamats/extract-params %)))
#_
@(p.a.eql/process env
                  @*selections
                  [:region-geogrid-params])


(def $num-svs
  (pbir/single-attr-resolver :region-matrix
                             :num-svs
                             datamats/num-svs))
;;(check :num-svs)

(def $sv-strs
  (pbir/single-attr-resolver :num-svs
                             :sv-strs
                             (fn generate-sv-strs
                               [num-of-svs]
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
                                       svs)))))

(def $region-svd
  (pbir/single-attr-resolver :region-matrix
                             :region-svd
                             datamats/svd))
#_(check :region-svd)
;; OjAlgo won't print to REPL

(def $region-min-max
  (pbir/single-attr-resolver :region-matrix
                             :region-min-max
                             datamats/get-min-max))
#_(check :region-min-max)


(def $first-datafile-idx
  (pbir/single-attr-resolver :datafile-idxs
                             :first-datafile-idx
                             first))
#_(check :first-datafile-idx)


(def $first-svec-selected-idx
  (pbir/single-attr-resolver :sv-selected-idxs
                             :first-svec-selected-idx
                             first))
#_(check :first-svec-selected-idx)


;;;;;;;;;; FOR OBSERVATION

(pco/defresolver $$datafile-geogrid
  [{:keys [datafile-id
           region-matrix]}]
  {:inject-cache :lru4}
  {:datafile-geogrid (datamats/extract-grid region-matrix
                                            datafile-id)})

#_
(defn-
  datafile-geogrid
  [region-matrix
   id]
  (datamats/extract-grid region-matrix
                         id))

;; TODO This could be read in before all the data is injested
;; to speed things up..
(pco/defresolver $first-datafile-geogrid
  [{:keys [region-matrix]}]
  {::pco/input  [:region-matrix]
   ::pco/output [{:first-datafile-geogrid [:region-matrix
                                           :id]}]}
  {:first-datafile-geogrid {:region-matrix region-matrix
                            :id            0}})

#_(-> (check {:first-datafile-geogrid [:datafile-geogrid]})
      :first-datafile-geogrid
      :datafile-geogrid
      keys)

(def $zero-point-mask
  (pbir/single-attr-resolver :first-datafile-geogrid
                             :zero-point-mask
                             (fn [first-datafile-geogrid]
                               (mapv zero?
                                     (:data-array first-datafile-geogrid)))))
#_(check :zero-point-mask)

(pco/defresolver $$datafile-svg
  [{:keys [datafile-id
           datafile-geogrid
           region
           contour-svg
           region-min-max]}]
  {::pco/input   [:datafile-id
                  :datafile-geogrid
                  :region
                  {:contour-svg [:hiccup]}
                  :region-min-max]
   ::pco/output  [:hiccup]
   #_#_ ;; prolly can regenerate each time you look at a new data
   :inject-cache :lru1}
  (if (nil? datafile-id)
    contour-svg
    {:hiccup (-> datafile-geogrid
                 (plot/grid-map region
                                contour-svg
                                {:max-val (second region-min-max)})
                 (spitsvgstream (str "data-file-"
                                     datafile-id
                                     ".svg")))}))
#_(check :hiccup
         {:datafile-id 0})

(pco/defresolver $first-datafile-svg
  [inputs]
  {::pco/input  [:region
                 :region-matrix
                 {:contour-svg [:hiccup]}
                 :region-min-max
                 :first-datafile-idx]
   ::pco/output [{:first-datafile-svg [:datafile-id
                                       :region
                                       :region-matrix
                                       {:contour-svg [:hiccup]}
                                       :region-min-max
                                       :first-datafile-idx]}]}
  {:first-datafile-svg (merge inputs
                              {:datafile-id (:first-datafile-idx inputs)})})
#_(check {:first-datafile-svg [:hiccup]})

(pco/defresolver $$singular-vector
  [{:keys [sv-index
           region-svd]}]
  {:inject-cache :lru4}
  {:singular-vector (datamats/singular-vector region-svd
                                              sv-index)})

(pco/defresolver $first-svec
  [{:keys [region-svd]}]
  {::pco/input  [:region-svd]
   ::pco/output [{:first-svec [:sv-index
                               :region-svd]}]}
  {:first-svec {:sv-index   0
                :region-svd region-svd}})
#_(check {:first-svec [:singular-vector]})

(pco/defresolver $second-svec
  [{:keys [region-svd]}]
  {::pco/input  [:region-svd]
   ::pco/output [{:second-svec [:sv-index
                                :region-svd]}]}
  {:second-svec {:sv-index   1
                 :region-svd region-svd}})
#_(check {:second-svec [:singular-vector]})

(pco/defresolver $$singular-vector-geogrid
  [{:keys [#_sv-index ;; used indirectly
           singular-vector
           region-geogrid-params]}]
  {:inject-cache :lru4}
  {:singular-vector-geogrid (geogrid4seq/build-grid region-geogrid-params
                                                    singular-vector)})
#_(check :singular-vector-geogrid
         {:sv-index 0})


(pco/defresolver $$singular-vector-svg
  [{:keys [sv-index
           datafile-strs
           region
           contour-svg
           singular-vector-geogrid]}]
  {::pco/input   [:sv-index
                  :datafile-strs
                  :region
                  {:contour-svg [:hiccup]}
                  :singular-vector-geogrid]
   ::pco/output  [:hiccup]
   :inject-cache :lru4}
  {:hiccup (if (empty? datafile-strs)
             contour-svg
             (-> singular-vector-geogrid
                 (plot/grid-map region
                                contour-svg
                                {:label-top-right (str "SV"
                                                       (inc sv-index))
                                 :label-attribs   {:fill      "black"
                                                   :stroke    "white" #_#_
                                                   :font-size 1.1}})
                 (spitsvgstream (str "sv-"
                                     sv-index
                                     ".svg"))))})
#_(check :hiccup
         {:sv-index 1})

(pco/defresolver $first-svec-svg
  [inputs]
  {::pco/input  [:datafile-strs
                 :region
                 {:contour-svg [:hiccup]}
                 :region-svd
                 :region-geogrid-params
                 :contour-svg]
   ::pco/output [{:first-svec-svg [:sv-index
                                   :datafile-strs
                                   :region
                                   {:contour-svg [:hiccup]}
                                   :region-svd
                                   :region-geogrid-params
                                   :contour-svg]}]}
  {:first-svec-svg (merge inputs
                          {:sv-index 0})})
#_(check {:first-svec-svg [:hiccup]})

(pco/defresolver $second-svec-svg
  [inputs]
  {::pco/input  [:datafile-strs
                 :region
                 {:contour-svg [:hiccup]}
                 :region-svd
                 :region-geogrid-params]
   ::pco/output [{:second-svec-svg [:datafile-strs
                                    :region
                                    {:contour-svg [:hiccup]}
                                    :region-svd
                                    :region-geogrid-params]}]}
  {:second-svec-svg (merge inputs
                           {:sv-index 1})})
#_(check {:second-svec-svg [:hiccup]})


(pco/defresolver $first-svec-selected-svg
  [{:keys [first-svec-selected-idx
           contour-svg]
    :as   inputs}]
  {::pco/input  [:first-svec-selected-idx
                 {:contour-svg [:hiccup]}
                 :region-matrix
                 :region
                 :datafile-strs]
   ::pco/output [{:first-svec-selected-svg [:sv-index
                                            {:contour-svg [:hiccup]}
                                            :region-matrix
                                            :region
                                            :datafile-strs]}]}
  {:first-svec-selected-svg (if (nil? first-svec-selected-idx)
                              contour-svg ;; BROKEN ; doesn't match output keys. Unclear how to fix
                              (merge inputs
                                     {:sv-index (:first-svec-selected-idx inputs)}))})
#_(check {:first-svec-selected-svg [:hiccup]})

(pco/defresolver $$sv-weights
  [{:keys [sv-index
           region-svd]}]
  {:inject-cache :lru4}
  {:sv-weights (datamats/svd-to-weights region-svd
                                        sv-index)})
#_(check :sv-weights
         {:sv-index 0})

(def $sv-proj-vec
  (pbir/single-attr-resolver :region-matrix
                             :sv-proj-vec
                             (fn [region-matrix]
                               (-> region-matrix
                                   datamats/svd
                                   datamats/svd-to-2d-sv-space))))
#_(check :sv-proj-vec)

(pco/defresolver $sv12-plot-svg
  [{:keys [sv-proj-vec
           region-svd
           barchart-width
           barchart-height
           cycle-length
           cycle-phase]}]
  {::pco/output [{:sv12-plot-svg [:hiccup]}]}
  {:sv12-plot-svg {:hiccup (-> (plot/sv1sv2-1scale barchart-width
                                                   barchart-height
                                                   sv-proj-vec
                                                   2011
                                                   cycle-length
                                                   cycle-phase)
                               (spitsvgstream "sv1sv2.svg"))}})
#_(check :sv12-plot-svg)

(pco/defresolver $sv12-plot-2scale-svg
  [{:keys [sv-proj-vec
           region-svd
           barchart-width
           barchart-height
           cycle-length
           cycle-phase]}]
  {::pco/output [{:sv12-plot-2scale-svg [:hiccup]}]}
  {:sv12-plot-2scale-svg {:hiccup (-> (plot/sv1sv2-2scale barchart-width
                                                          barchart-height
                                                          sv-proj-vec
                                                          2011
                                                          cycle-length
                                                          cycle-phase)
                                      (spitsvgstream "sv1sv2.svg"))}})
#_(check :sv12-plot-2scale-svg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;      NOISE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def $noise-matrix-2d
  (pbir/single-attr-resolver :region-svd
                             :noise-matrix-2d
                             datamats/minus-2-sv))
#_(datamats/extract-grid (:noise-matrix-2d (check :noise-matrix-2d))
                         0)
#_(matrix/data (:sigma (:noise-matrix-2d (check :noise-matrix-2d))))

(def $noise-vars
  (pbir/single-attr-resolver :noise-matrix-2d
                             :noise-vars
                             #(-> %
                                  :matrix
                                  (datamats/colvars))))
#_(keys (check :noise-matrix-2d))

(pco/defresolver $$noise-svg
  [{:keys [noise-index
           region
           contour-svg
           noise-matrix-2d]}]
  {::pco/input   [:noise-index
                  :region
                  {:contour-svg [:hiccup]}
                  :noise-matrix-2d]
   ::pco/output  [:hiccup]
   :inject-cache :lru1}
  (if (nil? noise-index)
    contour-svg
    {:hiccup (-> (datamats/extract-grid noise-matrix-2d
                                        noise-index)
                 (plot/grid-map region
                                contour-svg) ;; maybe no need?
                 (spitsvgstream (str "noise-"
                                     noise-index
                                     "-file.svg")))}))
#_(check :hiccup
         {:noise-index 0})

(def $first-noise-selected-idx
  (pbir/single-attr-resolver :noise-selected-idxs
                             :first-noise-selected-idx
                             first))
#_(check :first-noise-selected-idx)

(def $first-climate-noise-selected-idx
  (pbir/single-attr-resolver :climate-noise-selected-idxs
                             :first-climate-noise-selected-idx
                             first))
#_(check :first-climate-noise-selected-idx)

(pco/defresolver $first-noise-selected-svg
  [inputs]
  {::pco/input  [:first-noise-selected-idx
                 :region
                 {:contour-svg [:hiccup]}
                 :noise-matrix-2d]
   ::pco/output [{:first-noise-selected-svg [#_:noise-svg ;; will convert to this!
                                             :noise-index ;; injected here
                                             :region
                                             {:contour-svg [:hiccup]}
                                             :noise-matrix-2d]}]}
  {:first-noise-selected-svg (merge inputs
                                    {:noise-index (:first-noise-selected-idx inputs)})})
#_(check {:first-noise-selected-svg [:hiccup]})

(pco/defresolver $noise-matrix-scaled-to-sv1
  [{:keys [noise-matrix-2d
           first-svec]}]
  {::pco/input  [{:first-svec [:singular-vector]}]
   ::pco/output [:noise-matrix-scaled-to-sv1]}
  {:noise-matrix-scaled-to-sv1 (datamats/scaled-to-vec noise-matrix-2d
                                                       (:singular-vector first-svec))})
#_(keys (:noise-matrix-scaled-to-sv1 (check :noise-matrix-scaled-to-sv1)))

(pco/defresolver $$noise-scaled-to-sv1-svg
  [{:keys [noise-sv1-index
           region
           contour-svg
           noise-matrix-scaled-to-sv1]}]
  {::pco/input   [:noise-sv1-index
                  :region
                  {:contour-svg [:hiccup]}
                  :noise-matrix-scaled-to-sv1]
   ::pco/output  [:hiccup]
   :inject-cache :lru1}
  {:hiccup (if (nil? noise-sv1-index)
             contour-svg
             (-> (datamats/extract-grid noise-matrix-scaled-to-sv1
                                        noise-sv1-index)
                 (plot/grid-map region
                                contour-svg)
                 (spitsvgstream (str "noise-sv1-"
                                     noise-sv1-index
                                     "-file.svg"))))})
#_(check :hiccup
         {:noise-sv1-index 0})

(pco/defresolver $noise-matrix-scaled-to-sv2
  [{:keys [noise-matrix-2d
           second-svec]}]
  {::pco/input  [{:second-svec [:singular-vector]}]
   ::pco/output [:noise-matrix-scaled-to-sv2]}
  {:noise-matrix-scaled-to-sv2 (datamats/scaled-to-vec noise-matrix-2d
                                                       (:singular-vector second-svec))})
#_(keys (:noise-matrix-scaled-to-sv2 (check :noise-matrix-scaled-to-sv2)))

(pco/defresolver $$noise-scaled-to-sv2-svg
  [{:keys [noise-sv2-index
           region
           contour-svg
           noise-matrix-scaled-to-sv2]}]
  {::pco/input   [:noise-sv2-index
                  :region
                  {:contour-svg [:hiccup]}
                  :noise-matrix-scaled-to-sv2]
   ::pco/output  [:hiccup]
   :inject-cache :lru4}
  {:hiccup (if (nil? noise-sv2-index)
             contour-svg
             (-> (datamats/extract-grid noise-matrix-scaled-to-sv2
                                        noise-sv2-index)
                 (plot/grid-map region
                                contour-svg)
                 (spitsvgstream (str "noise-sv2-"
                                     noise-sv2-index
                                     "-file.svg"))))})
#_(check :hiccup
         {:noise-sv2-index 0})

(def $singular-values
  (pbir/single-attr-resolver :region-svd
                             :singular-values
                             datamats/singular-values))
#_(check :singular-values)

#_(datamats/singular-values (:noise-matrix-2d (check :noise-matrix-2d)))
#_(datamats/singular-values (check :region-svd))

(pco/defresolver $$singular-value
  [{:keys [sv-index
           singular-values]}]
  {:inject-cache :lru4}
  {:singular-value (-> singular-values
                       (get sv-index)
                       second)})
#_(check :singular-value
         {:sv-index 0})

(pco/defresolver $first-sval
  [{:keys [singular-values]
    :as   inputs}]
  {::pco/input  [:singular-values]
   ::pco/output [{:first-sval [:singular-values
                               :sv-index]}]}
  {:first-sval (merge inputs
                      {:sv-index 0})})
#_(check {:first-sval [:singular-value]})
#_(keys (:first-sval (check :first-sval)))

(pco/defresolver $second-sval
  [{:keys [singular-values]
    :as   inputs}]
  {::pco/input  [:singular-values]
   ::pco/output [{:second-sval [:singular-values
                                :sv-index]}]}
  {:second-sval (merge inputs
                       {:sv-index 1})})
#_(check {:second-sval [:singular-value]})

(pco/defresolver $errors-in-sv1-proj
  [{:keys [noise-matrix-scaled-to-sv1
           first-sval]}]
  {::pco/input  [:noise-matrix-scaled-to-sv1
                 {:first-sval [:singular-value]}]
   ::pco/output [:errors-in-sv1-proj]}
  {:errors-in-sv1-proj (datamats/errors-from-error-datamats noise-matrix-scaled-to-sv1
                                                            (:singular-value first-sval))})
#_ (check :errors-in-sv1-proj)

(pco/defresolver $errors-in-sv2-proj
  [{:keys [noise-matrix-scaled-to-sv2
           second-sval]}]
  {::pco/input  [:noise-matrix-scaled-to-sv2
                 {:second-sval [:singular-value]}]
   ::pco/output [:errors-in-sv2-proj]}
  {:errors-in-sv2-proj (datamats/errors-from-error-datamats noise-matrix-scaled-to-sv2
                                                            (:singular-value second-sval))})
#_ (check :errors-in-sv2-proj)

(pco/defresolver $sv-proj
  [{:keys [sv-proj-vec
           region-matrix
           cycle-length
           cycle-phase
           errors-in-sv1-proj
           errors-in-sv2-proj]}]
  {:sv-proj (let [projs  sv-proj-vec
                  scales (:scales region-matrix)]
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
                            sv-proj-vec
                            (->> sv-proj-vec
                                 count
                                 range)
                            scales
                            (->> sv-proj-vec
                                 count
                                 range
                                 (mapv #(cycle-frac cycle-length
                                                    cycle-phase
                                                    %)))
                            errors-in-sv1-proj
                            errors-in-sv2-proj)))})
#_(check :sv-proj)

(def $sv-angular-bisection
  (pbir/single-attr-resolver :sv-proj
                             :sv-angular-bisection
                             bisect/otsu-weighted))
#_(check :sv-angular-bisection)
#_(-> :sv-angular-bisection
      check
      :sv-angular-bisection
      keys)

(pco/defresolver $sv-bisection
  [{:keys [sv-angular-bisection]}]
  {::pco/input  [:sv-angular-bisection]
   ::pco/output [{:sv-bisection [:angle-from-bottom
                                 :points
                                 :centroid-a
                                 :centroid-b
                                 :interclass-var]}]}
  {:sv-bisection sv-angular-bisection})

#_
(def $sv-bisection ;; Kinda useless.. but it's an injection joint
  (pbir/single-attr-resolver :sv-angular-bisection
                             :sv-bisection
                             identity))
#_(->> :sv-bisection
       check
       :sv-bisection
       keys)

(pco/defresolver $sv-proj-svg
  [{:keys [sv-bisection
           barchart-width]}]
  {::pco/output [{:sv-proj-svg [:hiccup]}]}
  {:sv-proj-svg {:hiccup (-> (plot/sv-plot barchart-width
                                           (* barchart-width
                                              2.0)
                                           sv-bisection)
                             (spitsvgstream "sv-projs.svg"))}})
#_(check :sv-proj-svg)


#_
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
;; TODO
;; Add this back in...
;; If `non-zero-min?` is `true`
;; then the mixture should be run through this function

(pco/defresolver $$singular-vector-mixture
  [{:keys [first-svec
           second-svec
           svec-one
           svec-two
           sval-one
           sval-two]}]
  {::pco/input   [{:first-svec [:singular-vector]}
                  {:second-svec [:singular-vector]}
                  :svec-one
                  :svec-two
                  :sval-one
                  :sval-two]
   ::pco/output  [:singular-vector-mixture]
   :inject-cache :lru4}
  {:singular-vector-mixture (mapv (fn [svec1-point
                                       svec2-point]
                                    (/ (+ (* svec1-point
                                             svec-one
                                             sval-one)
                                          (* svec2-point
                                             svec-two
                                             sval-two))
                                       2.0))
                                  (:singular-vector first-svec)
                                  (:singular-vector second-svec))})

(pco/defresolver $top-pattern
  [{:keys [sv-bisection
           first-svec
           second-svec
           first-sval
           second-sval]}]
  {::pco/input  [:sv-bisection
                 {:first-svec [:singular-vector]}  ;; sval is unpacked
                 {:second-svec [:singular-vector]} ;; but svec is unpacked deeper :/
                 {:first-sval [:singular-value]}
                 {:second-sval [:singular-value]}]
   ::pco/output [{:top-pattern [{:first-svec [:singular-vector]}
                                {:second-svec [:singular-vector]}
                                :svec-one
                                :svec-two
                                :sval-one
                                :sval-two]}]}
  (let [centroid (:centroid-a sv-bisection)]
    {:top-pattern {:first-svec  first-svec
                   :second-svec second-svec
                   :svec-one    (first centroid)
                   :svec-two    (second centroid)
                   :sval-one    (:singular-value first-sval)
                   :sval-two    (:singular-value second-sval)}}))

(pco/defresolver $top-pattern-svg
  [{:keys [region
           region-geogrid-params
           top-pattern
           contour-svg]}]
  {::pco/input  [:region-geogrid-params
                 {:top-pattern [:singular-vector-mixture]}
                 {:contour-svg [:hiccup]}]
   ::pco/output [{:top-pattern-svg [:hiccup]}]}
  {:top-pattern-svg {:hiccup (let [input-grid (geogrid4seq/build-grid region-geogrid-params
                                                                      (-> top-pattern
                                                                          :singular-vector-mixture))]
                               (let [[width
                                      height] (geoprim/dimension (geogrid/covered-region input-grid))]
                                 (-> input-grid
                                     (plot/grid-map region
                                                    contour-svg
                                                    {:label-top-right "Top Pattern"
                                                     :label-attribs   {:fill      "#00aa88"
                                                                       ;; TODO: Ugly Font Size
                                                                       :font-size (/ (min width
                                                                                          height)
                                                                                     9)}
                                                     #_#_
                                                     :colormap        (into quickthing/rainbow
                                                                            quickthing/rainbow)})
                                     (spitsvgstream "top-pattern.svg"))))}})
#_(check :top-pattern-svg)


(pco/defresolver $$top-pattern-weighted-noise
  [{:keys [top-pattern-weighted-noise-index
           noise-matrix-2d
           top-pattern]}]
  {::pco/input [:top-pattern-weighted-noise-index
                :noise-matrix-2d
                {:top-pattern [:singular-vector-mixture]}]}
  {:top-pattern-weighted-noise (-> noise-matrix-2d
                                   (datamats/extract-grid top-pattern-weighted-noise-index)
                                   :data-array
                                   (#(mapv *
                                           %
                                           (:singular-vector-mixture top-pattern))))})
#_(check :top-pattern-weighted-noise
         {:top-pattern-weighted-noise-index 0})

(pco/defresolver $$top-pattern-weighted-noise-svg
  [{:keys [top-pattern-weighted-noise-index
           region
           contour-svg
           region-geogrid-params
           top-pattern-weighted-noise]}]
  {::pco/input  [:top-pattern-weighted-noise-index
                 :region
                 {:contour-svg [:hiccup]}
                 :region-geogrid-params
                 :top-pattern-weighted-noise]
   ::pco/output [{:top-pattern-weighted-noise-svg [:hiccup]}]}
  {:top-pattern-weighted-noise-svg {:hiccup (-> (geogrid4seq/build-grid region-geogrid-params
                                                                        top-pattern-weighted-noise)
                                                (plot/grid-map region
                                                               contour-svg)
                                                (spitsvgstream (str "top-pattern-weighted-noise-"
                                                                    top-pattern-weighted-noise-index
                                                                    ".svg")))}})
#_(check :top-pattern-weighted-noise-svg
         {:top-pattern-weighted-noise-index 0})


(pco/defresolver $bot-pattern
  [{:keys [sv-bisection
           first-svec
           second-svec
           first-sval
           second-sval]}]
  {::pco/input  [:sv-bisection
                 {:first-svec [:singular-vector]}  ;; sval is unpacked
                 {:second-svec [:singular-vector]} ;; but svec is unpacked deeper :/
                 {:first-sval [:singular-value]}
                 {:second-sval [:singular-value]}]
   ::pco/output [{:bot-pattern [{:first-svec [:singular-vector]} 
                                {:second-svec [:singular-vector]}
                                :svec-one
                                :svec-two
                                :sval-one
                                :sval-two]}]}
  (let [centroid (:centroid-b sv-bisection)]
    {:bot-pattern {:first-svec  first-svec
                   :second-svec second-svec
                   :svec-one    (first centroid)
                   :svec-two    (second centroid)
                   :sval-one    (:singular-value first-sval)
                   :sval-two    (:singular-value second-sval)}}))

(pco/defresolver $bot-pattern-svg
  [{:keys [region
           contour-svg
           region-geogrid-params
           bot-pattern]}]
  {::pco/input  [:region
                 {:contour-svg [:hiccup]}
                 :region-geogrid-params
                 {:bot-pattern [:singular-vector-mixture]}]
   ::pco/output [{:bot-pattern-svg [:hiccup]}]}
  {:bot-pattern-svg {:hiccup (let [input-grid (geogrid4seq/build-grid region-geogrid-params
                                                                      (-> bot-pattern
                                                                          :singular-vector-mixture))]
                               (let [[width
                                      height] (geoprim/dimension (geogrid/covered-region input-grid))]
                                 (-> input-grid
                                     (plot/grid-map region
                                                    contour-svg
                                                    {:label-top-right "Bottom Pattern"
                                                     :label-attribs   {:fill      "#aa8800"
                                                                       ;; TODO: Ugly Font Size
                                                                       :font-size (/ (min width
                                                                                          height)
                                                                                     9)}
                                                     #_#_
                                                     :colormap        (into quickthing/rainbow
                                                                            quickthing/rainbow)})
                                     (spitsvgstream "bot-pattern.svg"))))}})
#_(check :bot-pattern-svg)


(pco/defresolver $$bot-pattern-weighted-noise
  [{:keys [bot-pattern-weighted-noise-index
           noise-matrix-2d
           bot-pattern]}]
  {::pco/input [:bot-pattern-weighted-noise-index
                :noise-matrix-2d
                {:bot-pattern [:singular-vector-mixture]}]}
  {:bot-pattern-weighted-noise (-> noise-matrix-2d
                                   (datamats/extract-grid bot-pattern-weighted-noise-index)
                                   :data-array
                                   (#(mapv *
                                           %
                                           (:singular-vector-mixture bot-pattern))))})
#_(check :bot-pattern-weighted-noise
         {:bot-pattern-weighted-noise-index 0})

(pco/defresolver $$bot-pattern-weighted-noise-svg
  [{:keys [bot-pattern-weighted-noise-index
           region
           contour-svg
           region-geogrid-params
           bot-pattern-weighted-noise]}]
  {::pco/inputs [:bot-pattern-weighted-noise-index
                 :region
                 {:contour-svg [:hiccup]}
                 :region-geogrid-params
                 :bot-pattern-weighted-noise]
   ::pco/output [{:bot-pattern-weighted-noise-svg [:hiccup]}]}
  {:bot-pattern-weighted-noise-svg {:hiccup (-> (geogrid4seq/build-grid region-geogrid-params
                                                                        bot-pattern-weighted-noise)
                                                (plot/grid-map region
                                                               contour-svg)
                                                (spitsvgstream (str "bot-pattern-weighted-noise-"
                                                                    bot-pattern-weighted-noise
                                                                    ".svg")))}})
#_(check :bot-pattern-weighted-noise-svg
         {:bot-pattern-weighted-noise-index 0})

(def $pattern-proj
  (pbir/single-attr-resolver :sv-bisection
                             :pattern-proj
                             (fn [{:keys [centroid-a
                                          centroid-b
                                          points]}]
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
                                 (let [projections    (matrix/project-onto-2-patterns  centroid-a
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
                                         err-centroid-b))))))
#_(check :pattern-proj)

(def $binary-index-vector
  (pbir/single-attr-resolver :pattern-proj
                             :binary-index-vector
                             (fn [pattern-proj]
                               (->> pattern-proj
                                    (mapv (fn [datapoint]
                                            (-> datapoint
                                                (get 2)
                                                :above?)))))))
#_(check :binary-index-vector)

;; TODO: This could be way more efficient.
;; Probably need some particular BLAS operator to do elementwise vector products
(pco/defresolver $climate-noise-matrix-2d-normalized
  [{:keys [binary-index-vector
           noise-matrix-2d
           top-pattern
           bot-pattern
           region-matrix]}]
  {::pco/input  [:binary-index-vector
                 :noise-matrix-2d
                 {:top-pattern [:singular-vector-mixture]}
                 {:bot-pattern [:singular-vector-mixture]}
                 :region-matrix]
   ::pco/output [:climate-noise-matrix-2d-normalized]}
  {:climate-noise-matrix-2d-normalized
   (->> binary-index-vector
        (map-indexed (fn [index
                          is-top-pattern]
                       (-> noise-matrix-2d
                           (datamats/extract-grid index)
                           :data-array
                           (#(mapv *
                                   %
                                   (if is-top-pattern
                                     (:singular-vector-mixture top-pattern)
                                     (:singular-vector-mixture bot-pattern)))))))
        (datamats/from-vecofvecs region-matrix))})
#_(-> :climate-noise-matrix-2d-normalized
      check
      :climate-noise-matrix-2d-normalized
      vec)

(pco/defresolver $$climate-noise-svg
  [{:keys [climate-noise-index
           region
           contour-svg
           climate-noise-matrix-2d-normalized]}]
  {::pco/input  [:climate-noise-index
                 :region
                 {:contour-svg [:hiccup]}
                 :climate-noise-matrix-2d-normalized]
   ::pco/output [:hiccup]}
  {:hiccup (-> (datamats/extract-grid  climate-noise-matrix-2d-normalized
                                       climate-noise-index)
               (plot/grid-map region
                              contour-svg)
               (spitsvgstream (str "climate-noise-"
                                   climate-noise-index
                                   "-file.svg")))})
#_(check :hiccup
         {:climate-noise-index 6})


(pco/defresolver $first-climate-noise-selected-svg
  [inputs]
  {::pco/input  [:first-climate-noise-selected-idx
                 :region
                 {:contour-svg [:hiccup]}
                 :climate-noise-matrix-2d-normalized]
   ::pco/output [{:first-climate-noise-selected-svg [:climate-noise-index
                                                     :region
                                                     {:contour-svg [:hiccup]}
                                                     :climate-noise-matrix-2d-normalized]}]}
  {:first-climate-noise-selected-svg (merge inputs
                                            {:climate-noise-index (:first-climate-noise-selected-idx inputs)})})
#_(check {:first-climate-noise-selected-svg [:hiccup]})

#_
(defn
  first-normalized-noise-selected-svg
  ""
  [context]
  (let [first-selections-idx (fx/sub-ctx context
                                         first-normalized-noise-selected-idx)]
    (if (nil? first-selections-idx)
      (fx/sub-ctx context
                  contour-svg)
      (fx/sub-ctx context
                  climate-noise-svg
                  first-selections-idx))))
#_
(-> @*selections
    (fx/sub-ctx first-normalized-noise-selected-svg))

(def $climate-noise-vars
  (pbir/single-attr-resolver :climate-noise-matrix-2d-normalized
                             :climate-noise-vars
                             datamats/colvars))
#_(check :climate-noise-vars)


;; TODO This redraws on window resize!
;; Should let the GUI resize and keep the same SVG/Render
(pco/defresolver $climate-noise-var-svg
  [{:keys [barchart-width
           barchart-height
           climate-noise-vars
           cycle-length
           cycle-phase]}]
  {::pco/output [{:climate-noise-var-svg [:hiccup]}]}
  {:climate-noise-var-svg {:hiccup (-> (plot/index barchart-width
                                                   barchart-height
                                                   climate-noise-vars
                                                   2011
                                                   cycle-length
                                                   cycle-phase)
                                       (spitsvgstream "indeces-vars.svg"))}})
#_(:climate-noise-var-svg (check :climate-noise-var-svg
                                 {:index 6}))

(def $pattern-proj-partitioned
  (pbir/single-attr-resolver :pattern-proj
                             :pattern-proj-partitioned
                             (fn [projections]
                               (let [proj-a (->> projections
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
                                                           0.0))))
                                     errors (->> projections
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
                                 (if debug?
                                   (with-open [writer (io/writer (str config-dir
                                                                      "/climate-index.csv"))]
                                     (println (str "Writing out climate index to CSV file .. "))
                                     (csv/write-csv writer
                                                    (mapv vector
                                                          proj-a
                                                          proj-b
                                                          errors))))
                                 [proj-a
                                  proj-b
                                  errors]))))
#_(check :pattern-proj-partitioned)

;; TODO This redraws on window resize!
;; Should let the GUI resize and keep the same SVG/Render
(pco/defresolver $pattern-proj-svg
  [{:keys [pattern-proj-partitioned
           barchart-width
           barchart-height
           cycle-length
           cycle-phase]}]
  {::pco/output [{:pattern-proj-svg [:hiccup]}]}
  {:pattern-proj-svg {:hiccup (let [[proj-a
                                     proj-b
                                     errors] pattern-proj-partitioned]
                                (-> (plot/indeces barchart-width
                                                  barchart-height
                                                  proj-a
                                                  proj-b
                                                  errors
                                                  2011
                                                  cycle-length
                                                  cycle-phase
                                                  {:bar-width (* 0.5
                                                                 (/ barchart-width
                                                                    (count proj-a)))})
                                    (spitsvgstream "indeces.svg")))}})
#_(check {:pattern-proj-svg [:hiccup]})


(pco/defresolver $pattern-proj-with-errors-svg
  [{:keys [pattern-proj-partitioned
           barchart-width
           barchart-height
           cycle-length
           cycle-phase]}]
  {::pco/output [{:pattern-proj-with-errors-svg [:hiccup]}]}
  {:pattern-proj-with-errors-svg {:hiccup (let [[proj-a
                                                 proj-b
                                                 errors] pattern-proj-partitioned]
                                            (-> (plot/indeces barchart-width
                                                              barchart-height
                                                              proj-a
                                                              proj-b
                                                              errors
                                                              2011
                                                              cycle-length
                                                              cycle-phase
                                                              {:error-bars? true
                                                               :bar-width (* 0.5
                                                                             (/ barchart-width
                                                                                (count proj-a)))})
                                                (spitsvgstream "indeces-with-errors.svg")))}})
#_(check {:pattern-proj-with-errors-svg [:hiccup]})

(pco/defresolver $sv-proj-with-errors-svg
  [{:keys [sv-bisection
           barchart-width]}]
  {::pco/output [{:sv-proj-with-errors-svg [:hiccup]}]}
  {:sv-proj-with-errors-svg {:hiccup (-> (plot/sv-plot barchart-width
                                                       (* barchart-width
                                                          2.0)
                                                       sv-bisection
                                                       {:error-bars? true})
                                         (spitsvgstream "sv-projs-with-errors.svg"))}})
#_(check :sv-proj-with-errors-svg)


(def $singular-values-stats
  (pbir/single-attr-resolver :singular-values
                             :singular-values-stats
                             datamats/singular-values-stats))
#_(check :singular-values-stats)

;; TODO This redraws on window resize!
;; Should let the GUI resize and keep the same SVG/Render
(pco/defresolver $singular-values-svg
  [{:keys [singular-values
           singular-values-stats
           barchart-width
           barchart-height]}]
  {::pco/output [{:singular-values-svg [:hiccup]}]}
  {:singular-values-svg {:hiccup (-> (plot/sv-weights singular-values
                                                      20
                                                      singular-values-stats
                                                      barchart-width
                                                      barchart-height)
                                     (spitsvgstream "singular-values.svg"))}})
#_(check :singular-values-svg)

(pco/defresolver $$observation-svg
  [{:keys [observation-index
           region
           region-matrix
           contour-svg
           cycle-length]}]
  {::pco/output [:hiccup]}
  {:hiccup (->  region-matrix
                datamats/to-geogrid-vec
                (get observation-index)
                (plot/grid-map region
                               contour-svg
                               {:label-top-right (str (inc observation-index))
                                #_#_
                                :max-val         (->  context
                                                      (fx/sub-ctx region-min-max)
                                                      second)
                                :label-attribs   {#_#_:font-size 0.7}
                                :axis-visible?   false
                                :cycle-frac      (/ observation-index
                                                    cycle-length)})
                (spitsvgstream (str "observation-"
                                    observation-index
                                    ".svg")))})
#_(check :hiccup
         {:observation-index 0})

;;;;;
(identity @*selections)
;;;
(def input-env
  (-> (pci/register {::p.a.eql/parallel? true}
                    [$data-dirstr
                     $datafile-strs
                     $datafile-strs-formatted
                     $data-locations
                     $world-geogrid-vec
                     $region-geogrid-vec
                     $region-matrix])
      (pcp/with-plan-cache pathom-plan-cache*)
      kxygk.pathmore.cache/inject-for-all-resolvers))
(def env
  (-> (pci/register {::p.a.eql/parallel? true}
                    [$barchart-height
                     $barchart-width
                     $dummy-barchart-svg
                     $dummy-sv-proj-svg
                     $shoreline
                     $region
                     $decompose-region
                     $cycle-length-bins
                     $region-xy-ratio
                     $world-svg
                     $world-with-region-highlight-svg
                     $contour-bare-svg
                     $contour-svg
                     input-env ;; stuff related to reading in the data.. this is slow
                     $region-geogrid-params
                     $num-svs
                     $sv-strs
                     $region-svd
                     $region-min-max
                     $first-datafile-idx
                     $first-svec-selected-idx
                     $$datafile-geogrid
                     $first-datafile-geogrid
                     $zero-point-mask
                     $$datafile-svg
                     $first-datafile-svg
                     $$singular-vector
                     $first-svec
                     $second-svec
                     $$singular-vector-geogrid
                     $$singular-vector-svg
                     $first-svec-svg
                     $second-svec-svg
                     $first-svec-selected-svg
                     $$sv-weights
                     $sv-proj-vec
                     $sv12-plot-svg
                     $sv12-plot-2scale-svg
                     $noise-matrix-2d
                     $noise-vars
                     $$noise-svg
                     $first-noise-selected-idx
                     $first-climate-noise-selected-idx
                     $first-noise-selected-svg
                     $noise-matrix-scaled-to-sv1
                     $$noise-scaled-to-sv1-svg
                     $noise-matrix-scaled-to-sv2
                     $$noise-scaled-to-sv2-svg
                     $singular-values
                     $$singular-value
                     $first-sval
                     $second-sval
                     $errors-in-sv1-proj
                     $errors-in-sv2-proj
                     $sv-proj
                     $sv-angular-bisection
                     $sv-bisection
                     $sv-proj-svg
                     $$singular-vector-mixture
                     $top-pattern
                     $top-pattern-svg
                     $$top-pattern-weighted-noise
                     $$top-pattern-weighted-noise-svg
                     $bot-pattern
                     $bot-pattern-svg
                     $$bot-pattern-weighted-noise
                     $$bot-pattern-weighted-noise-svg
                     $pattern-proj
                     $binary-index-vector
                     $climate-noise-matrix-2d-normalized
                     $$climate-noise-svg
                     $first-climate-noise-selected-svg
                     $climate-noise-vars
                     $climate-noise-var-svg
                     $pattern-proj-partitioned
                     $pattern-proj-svg
                     $pattern-proj-with-errors-svg
                     $sv-proj-with-errors-svg
                     $singular-values-stats
                     $singular-values-svg
                     $$observation-svg])
      (pcp/with-plan-cache pathom-plan-cache*)
      kxygk.pathmore.cache/inject-for-all-resolvers
      kxygk.pathmore.async/wrap-all-resolvers-async))

#_
(time
  (p.a.eql/process env
                   @*selections
                   [{:first-datafile-svg [:imagebuf]}]))

(defn check
  "simple util func to check a key in this file"
  ([some-key]
   @(p.a.eql/process env
                     @*selections
                     [some-key]))
  ([some-key
    extra-state]
   @(p.a.eql/process env
                     (merge @*selections
                            extra-state)
                     [some-key])))
#_(check [:first-datafile-svg [:datafile-svg]])
#_(check {:first-datafile-svg [:datafile-svg]})

(defn gensummary
  []
  (do @(p.a.eql/process env
                        @*selections
                        [{:first-svec-svg [:hiccup]}
                         {:second-svec-svg [:hiccup]}
                         :sv-proj-svg
                         :top-pattern-svg
                         :bot-pattern-svg
                         :pattern-proj-svg
                         :singular-values-svg
                         :sv12-plot-svg
                         :sv12-plot-2scale-svg])
      (let [cycle-length (-> :cycle-length
                             check
                             :cycle-length)]
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
             (run! (fn [index]
                     (check :hiccup
                            {:observation-index index})))))))

#_(gensummary)

(defn fetch
  "get a key..
  Probably shouldn't be used b/c it locks the main thread
  But it's an easy placeholder"
  ([state
    some-key]
   (some-key @(p.a.eql/process env
                               state
                               [some-key])))
  ([state
    some-key
    extra-state]
   (some-key @(p.a.eql/process env
                               (merge state
                                      extra-state)
                               [some-key]))))

#_(fetch @*selections
         [:first-datafile-svg [:datafile-svg]])
#_(fetch @*selections
         :datafile-strs-formatted)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These are old function that maybe should be converted.
;; But are not essential for the main function of the applications
;; Some are hardcoded for 12month cycles.. so they're a bit ugly
;;
;; This is stuff to revisit later


#_
(defn-
  add-cycle-data
  "add metadata to a vector that in the same order as the original data
NOTE: I Think I already do this in `sv-proj` now?"
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
#_
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
                                 contour-svg)
                     {:display-width (fx/sub-ctx context
                                                 region-display-width)})))



;; TODO:
;; Thing that prints some set obervations for the summary
#_
(let [cycle-length 12 #_ (fx/sub-ctx @*selections
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

#_#_#_#_
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
                                                       contour-svg)
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

#_
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
                                                           contour-svg)
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
#_
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
#_
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
                                                           contour-svg)
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

#_
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

#_
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


#_
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





