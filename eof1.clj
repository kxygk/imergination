(ns
    eof1
  (:use [hashp.core])
  (:require [clojure.java.io :as io]
            [cljfx.api       :as fx]
            [clojure.core.cache :as cache]
            [injest.path :refer [+> +>> x>> =>>]]
            state
            bisect
            svg2jfx
            matrix
            plot
            locations))

(def debug?
  true)

;; DEBUG HELPERS *************************
(defn
  spitstream
  "This is an override of the one in `state`
  This makes plot be spit to the `eof1` subfolder"
  [string
   filename]
  (assert (instance? String
                     string))
  (if debug?
    (spit (str "../imergination.wiki/"
               (-> @state/*selections
                   (fx/sub-ctx state/region-key)
                   symbol)
               "/eof1/"
               filename)
          string)
    nil)
  string)

(defn
  spitsvgstream
  "Take an SVG hiccup
  Render it to XML and same to the `filename`
  And return the hiccup"
  [svg-hiccup
   filename]
  (spitstream (-> svg-hiccup
                  quickthing/svg2xml)
              filename)
  svg-hiccup)

;; make output directory
(if debug?
  (->> (-> @state/*selections
           (fx/sub-ctx state/region-key))
       symbol
       (str "../imergination.wiki/")
       (java.io.File.)
       (.mkdir)))

;; make output directory
(if debug?
  (->> (str "../imergination.wiki/"
            (-> @state/*selections
                (fx/sub-ctx state/region-key)
                symbol)
            "/eof1/")
       (java.io.File.)
       (.mkdirs)))

;; ***************************************

(defn-
  right-to-left-line-fitted-plot
  "Generic plot of some aspect of the data
  Given arbitrary XY values
  - scatter plot
  - fit a line right to left with minimal residual variance
  - highlight indexes designated in the region metadata
  Not clear if this is used more widely.."
  [context
   xy-pairs]
  (let [fit-params (->> xy-pairs
                        matrix/linear-fit
                        seq
                        flatten
                        (zipmap [:offset
                                 :slope]))]
    (println (str " FIT PARAMS "
                  fit-params))
      (-> xy-pairs
          (plot/eof1-vs-var (str (-> @state/*selections
                                     (fx/sub-ctx state/region-key)
                                     symbol
                                     str
                                     clojure.string/upper-case)
                                 ": "
                                 (->> fit-params
                                      :slope
                                      (format "%.3G"))
                                 "x + "
                                 (->> fit-params
                                      :offset
                                      (format "%.3G")))
                            1000 ;; needs values for graphic.. and for `svg/group`
                            1000
                            {:y-name              "variance"
                             :highlighted-idx-vec (-> @state/*selections
                                                      (fx/sub-ctx state/region-meta)
                                                      :interesting-times)
                             #_#_
                             :traced-id-vec       subset
                             :fit-params          fit-params})
          #_(spitsvgstream "eof1-vs-std-from-zero.svg"))))
#_
(fx/sub-ctx @state/*selections
            right-to-left-line-fitted-plot
            (fx/sub-ctx @state/*selections
                        eof1weight-vs-variance-from-zero))

;; DIAGNOSTIC CHARTS
;;
;; ONLY MAKES SENSE IN 1 CLIMATE REGIONS
(defn-
  var-from-zero
  [data-seq]
  (let [num (count data-seq)]
    (->> data-seq
         (reduce #(+ (/ (clojure.math/pow %2
                                          2.0)
                        num)
                     %1)
                 0.0))))

(defn-
  std-from-zero
  [data-seq]
  (-> data-seq
      var-from-zero
      clojure.math/sqrt))

(defn
  eof1weight-vs-variance-from-zero
  "Return a pair of the eof1weight and variance
   (relative to the EOF1 signal)
  for a given INDEX (ie. time point)"
  [context]
  (let [eof1-weight     (-> context
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
                                   (fx/sub-ctx state/noise-1d-matrix)
                                   :matrix)
          variations-from-mean (->> num-timesteps
                                    range
                                    (mapv #(-> var-matrix
                                               (uncomplicate.neanderthal.core/col %)
                                               seq
                                               vec
                                               var-from-zero)))]
      (mapv vector
            eof1-components
            variations-from-mean))))
#_
(-> @state/*selections
    (fx/sub-ctx eof1weight-vs-variance-from-zero))

(defn
  eof1-vs-var-zero-svg
  "Plot and stream to file"
  [context]
  (let [xy-pairs (-> context
                     (fx/sub-ctx eof1weight-vs-variance-from-zero))]
    (-> context
        (fx/sub-ctx right-to-left-line-fitted-plot
                    xy-pairs))))
#_
  (-> context
      (fx/sub-ctx eof1weight-vs-variance-from-zero)
      (plot/eof1-vs-var (-> @state/*selections
                            (fx/sub-ctx state/region-key)
                            str)
                        1000 ;; needs values for graphic
                        1000
                        {:y-name              "variance"
                         :highlighted-idx-vec (-> @state/*selections
                                                  (fx/sub-ctx state/region-meta)
                                                  :interesting-times)})
      #_(spitsvgstream "eof1-vs-var-from-zero.svg"))
;;  Not in GUI display, so run code to save SVG to file
(fx/sub-ctx @state/*selections
            eof1-vs-var-zero-svg)

(defn-
  var-from-mean
  [data-seq]
  (let [
        num  (count data-seq)
        mean (/ (->> data-seq
                     (reduce +))
                num)] ;; N or N-1 ?
    (->> data-seq
         (reduce #(+ (/ (clojure.math/pow (- %2
                                             mean)
                                          2.0)
                        num)
                     %1)
                 0.0))))

(defn-
  std-from-mean
  [data-seq]
  (-> data-seq
      var-from-zero
      clojure.math/sqrt))

(defn
  eof1weight-vs-variance-from-mean
  "Return a pair of the eof1weight and variance
   (relative to the EOF1 signal)
  for a given INDEX (ie. time point)"
  [context]
  (let [eof1-weight     (-> context
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
                                   (fx/sub-ctx state/noise-1d-matrix)
                                   :matrix)
          variations-from-mean (->> num-timesteps
                                    range
                                    (mapv #(-> var-matrix
                                               (uncomplicate.neanderthal.core/col %)
                                               seq
                                               vec
                                               var-from-mean)))]
      (mapv vector
            eof1-components
            variations-from-mean))))

(defn
  eof1-vs-var-mean-svg
  "Plot and stream to file"
  [context]
  (-> context
      (fx/sub-ctx eof1weight-vs-variance-from-mean) ;; #_#_#_
      (plot/eof1-vs-var (-> @state/*selections
                            (fx/sub-ctx state/region-key)
                            str)
                        1000 ;; needs values for graphic
                        1000
                        {:y-name              "variance"
                         :highlighted-idx-vec (-> @state/*selections
                                                  (fx/sub-ctx state/region-meta)
                                                  :interesting-times)})
      #_(spitsvgstream "eof1-vs-var-from-mean.svg")))
;;  Not in GUI display, so run code to save SVG to file
(fx/sub-ctx @state/*selections
            eof1-vs-var-mean-svg)

(defn
  noise-1d-hist-svg
  [context
   time-index
   y-max]
  (-> time-index
      (plot/histogram-of-monthly-rain-amounts (-> context
                                                  (cljfx.api/sub-ctx state/noise-1d-matrix)
                                                  :matrix
                                                  (uncomplicate.neanderthal.core/col time-index)
                                                  seq
                                                  vec)
                                              (-> context
                                                  (cljfx.api/sub-ctx state/noise-1d-min-max))
                                              [0.0, 30.0]
                                              [1000
                                               1000])
      #_(spitsvgstream (str "noise-hist-t"
                          time-index
                          ".svg"))))
#_
(-> @state/*selections
    (fx/sub-ctx noise-1d-hist-svg 5))

(defn
  noise-1d-var-stats
  [context]
  (let [eof1-vs-var-svg (-> context
                            (fx/sub-ctx eof1-vs-var-zero-svg))
        hist-svg-vec    (->> (fx/sub-ctx context
                                         state/region-meta)
                             :interesting-times
                             (#(if (nil? %)
                                 [0 1 2 3 4 5 6 7]
                                 %))
                             (plot/histograms-of-monthly-rain-amounts (-> context
                                                                          (fx/sub-ctx state/noise-1d-matrix)
                                                                          :matrix)
                                                                      [1000,1000]))]
    (spitsvgstream (plot/non-eof1-stats eof1-vs-var-svg
                                        hist-svg-vec)
                   "noise-1d-var-stats.svg")))
;;#_
(-> @state/*selections
    (fx/sub-ctx noise-1d-var-stats))

(defn
  eof-index
  "Simple extraction of the EOF1 component"
  [context]
  (->> (fx/sub-ctx context
                   state/sv-proj)
       (mapv (fn remove-ys
               [proj]
               (-> proj
                   (get 0)
                   abs)))
       #_(vector)))
#_
(-> @state/*selections
    (fx/sub-ctx eof-index))

(defn
  eof-index-svg
  [context]
  (let [proj (->> (fx/sub-ctx context
                              eof-index)
                  (map-indexed vector))]
    (-> (plot/index (* 1.0
                         (fx/sub-ctx context
                                     state/window-width))
                      (* 1.0
                         (fx/sub-ctx context
                                     state/row-height))
                      proj
                      2011
                      (fx/sub-ctx context
                                  state/cycle-length)
                      (fx/sub-ctx context
                                  state/cycle-phase))
        (spitsvgstream "index.svg"))))
#_{:region (region (point 90
                          -180)
                   (point -90
                          180))}
(-> @state/*selections
    (fx/sub-ctx eof-index-svg))

(defn
  eof-index-hist
  [context]
  (let [proj (fx/sub-ctx context
                         eof-index)]
    (-> (plot/histogram-of-index-values proj
                                        [1000, 400])
        (spitsvgstream "index-pdf.svg"))))
;;#_
(-> @state/*selections
    (fx/sub-ctx eof-index-hist))
