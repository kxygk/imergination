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
(-> context
    (fx/sub-ctx eof1weight-vs-variance-from-zero)
    (plot/eof1-vs-var (-> @state/*selections
                          (fx/sub-ctx state/region-key)
                          str)
                      1000 ;; needs values for graphic
                      1000
                      {:y-name "variance"
                       :highlighted-idx-vec (-> @state/*selections
                                                  (fx/sub-ctx state/region-meta)
                                                  :interesting-times)})
    (state/spitsvgstream "eof1-vs-var-from-zero.svg")))
;;  Not in GUI display, so run code to save SVG to file
(fx/sub-ctx @state/*selections
            eof1-vs-var-zero-svg)


(defn
  eof1weight-vs-std-from-zero
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
                                               std-from-zero)))]
      (mapv vector
            eof1-components
            variations-from-mean))))
#_
(-> @state/*selections
    (fx/sub-ctx eof1weight-vs-variance-from-zero))



#_
(->> (fx/sub-ctx @state/*selections
                 eof1weight-vs-variance-from-zero)
     (sort #(> (first %1)
               (first %2)))
     matrix/subsets-for-linear-regression
     (apply min-key
            :residual-variance)
     :fit-params)


(defn
  eof1-vs-std-zero-svg
  "Plot and stream to file"
  [context]
(-> context
    (fx/sub-ctx eof1weight-vs-std-from-zero)
    (plot/eof1-vs-var (-> @state/*selections
                          (fx/sub-ctx state/region-key)
                          str)
                      1000 ;; needs values for graphic
                      1000
                      {:y-name "standard deviation"
                       :highlighted-idx-vec (-> @state/*selections
                                                  (fx/sub-ctx state/region-meta)
                                                  :interesting-times)})
    (state/spitsvgstream "eof1-vs-std-from-zero.svg")))
;;  Not in GUI display, so run code to save SVG to file
(fx/sub-ctx @state/*selections
            eof1-vs-std-zero-svg)



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
                        {:y-name "variance"
                         :highlighted-idx-vec (-> @state/*selections
                                                  (fx/sub-ctx state/region-meta)
                                                  :interesting-times)})
      (state/spitsvgstream "eof1-vs-var-from-mean.svg")))
;;  Not in GUI display, so run code to save SVG to file
(fx/sub-ctx @state/*selections
            eof1-vs-var-mean-svg)

(defn
  noise-1d-hist-svg
  [context
   time-index]
  (plot/histogram-of-monthly-rain-amounts time-index
                                          (-> context
                                              (cljfx.api/sub-ctx state/noise-1d-matrix)
                                              :matrix
                                              (uncomplicate.neanderthal.core/col time-index)
                                              seq
                                              vec)
                                          (-> context
                                              (cljfx.api/sub-ctx state/noise-1d-min-max))
                                          [0.0, 30.0]
                                          [1000
                                           1000]))
#_
(-> @state/*selections
    (fx/sub-ctx noise-1d-hist-svg 5)
    (spitsvgstream (str "noise-hist-t"
                        5
                        ".svg")))

(defn
  noise-1d-var-stats
  [context]
  (let [eof1-vs-var-svg (-> context
                            (fx/sub-ctx eof1-vs-var-zero-svg))
        hist-svg-vec (->> (fx/sub-ctx context
                                      state/region-meta)
                          :interesting-times
                          (#(if (nil? %)
                              [0 1 2 3 4 5 6 7]
                              %))
                          (mapv (fn get-hist-at-time-point
                                  [time-index]
                                  (-> context
                                      (noise-1d-hist-svg time-index)))))]
    (plot/non-eof1-stats eof1-vs-var-svg
                                  hist-svg-vec)))
#_
(-> @state/*selections
    (fx/sub-ctx noise-1d-var-stats)
    (spitsvgstream (str "noise-1d-var-stats.svg")))



(defn
  noise-1d-std-stats
  [context]
  (let [eof1-vs-std-svg (-> context
                            (fx/sub-ctx eof1-vs-std-zero-svg))
        hist-svg-vec (->> (fx/sub-ctx context
                                      state/region-meta)
                          :interesting-times
                          (#(if (nil? %)
                              [0 1 2 3 4 5 6 7]
                              %))
                          (mapv (fn get-hist-at-time-point
                                  [time-index]
                                  (-> context
                                      (noise-1d-hist-svg time-index)))))]
    (plot/non-eof1-stats eof1-vs-std-svg
                         hist-svg-vec)))
#_
(-> @state/*selections
    (fx/sub-ctx noise-1d-std-stats)
    (spitsvgstream (str "noise-1d-std-stats.svg")))
-
