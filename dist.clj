(ns dist
  "poking at the rain distributions.."
  (:require [clojure.java.io :as io]
            [cljfx.api       :as fx]
            [quickthing]
            [thi.ng.geom.viz.core :as viz]
            [thi.ng.geom.svg.core :as svg]))


(fx/sub-ctx @state/*selections
            state/region-matrix)
;; => {:matrix #RealGEMatrix[double, mxn:2840x3653, layout:column]
;;       ▥       ↓       ↓       ↓       ↓       ↓       ┓    
;;       →       0.00    0.00    ⁙       0.00    0.00         
;;       →       0.00    0.00    ⁙       0.00    0.00         
;;       →       ⁙       ⁙       ⁙       ⁙       ⁙            
;;       →       0.00    1.00    ⁙       0.00    0.00         
;;       →       0.00    1.00    ⁙       0.00    0.00         
;;       ┗                                               ┛    
;;    ,
;;     :dimension [40 71],
;;     :position {:eas 277.0, :sou 76.9},
;;     :resolution [0.1 0.1]}

(* 40
   71)
;; => 2840

;; So the rows are the pixels and the columns are the "days"
;; this makes sense since we have around 10 years of data ~365*10


;; krabi airport - eyeballing `locations/krabi-root-2`
;; is around 21 pixels across & 38 down

;; assuming images are row-major

(let [row 38
      col 21]
(+ col
   (* 40
      (dec row))))
;; => 1501

;; So to get daily rain at the airport we do
#_
(let [rain-days   (-> @state/*selections
                      (fx/sub-ctx state/region-matrix)
                      :matrix
                      (uncomplicate.neanderthal.core/row 1501)
                      seq
                      vec)
      rain-int    (->> rain-days #_
                       (mapv int))
      rain-max    (apply max
                         rain-int)
      rain-counts (->> (update-vals (->> rain-int
                                         (group-by identity))
                                    count)
                       (into (sorted-map-by <)))]
  (let [width     1000
        height    500
        fake-data (drop 1 (vec rain-counts))
        axis      (-> fake-data
                      (quickthing/primary-axis {:x-name "YEAR"
                                                :y-name "RAND"
                                                :title  "TEST-PLOT"
                                                :color  "#0008"}))]
    (spit "out/rain-raw-counts.svg"
          (-> axis
              (assoc :data
                     (quickthing/hist fake-data))
              viz/svg-plot2d-cartesian
              (quickthing/svg-wrap [width
                                    height])
              quickthing/svg2xml))))

#_
;; Repeat the same with the signal removed
(let [rain-days   (-> @state/*selections
                      (fx/sub-ctx state/noise-matrix-2d)
                      :matrix
                      (uncomplicate.neanderthal.core/row 1501)
                      seq
                      vec)
      rain-int    (->> rain-days
                       (mapv clojure.math/round))
      rain-max    (apply max
                         rain-int)
      rain-counts (->> (update-vals (->> rain-int
                                         (group-by identity))
                                    count)
                       (into (sorted-map-by <)))]
  (let [width     1000
        height    500
        fake-data (drop 1 (vec rain-counts))
        axis      (-> fake-data
                      (quickthing/primary-axis {:x-name "YEAR"
                                                :y-name "RAND"
                                                :title  "TEST-PLOT"
                                                :color  "#0008"}))]
    rain-int
    (spit "out/rain-noise-counts.svg"
          (-> axis
              (assoc :data
                     (quickthing/hist fake-data))
              viz/svg-plot2d-cartesian
              (quickthing/svg-wrap [width
                                    height])
              quickthing/svg2xml))))


(defn across-field-1d
  "Build a histogram of the 1D noise matrix for a time INDEX"
  [index]
  (let [[x-min
         x-max] (-> @state/*selections
                    (fx/sub-ctx state/noise-1d-min-max))
        y-min 0
        y-max 30
        data-bounds [[x-min, y-min]
                     [x-max, y-max]]
        variation-from-mean (-> @state/*selections
                                  (fx/sub-ctx #_state/region-matrix state/noise-matrix-1d)
                                  :matrix
                                  (uncomplicate.neanderthal.core/col index)
                                  seq
                                  vec)
        eof1-component (* (-> @state/*selections
                             (fx/sub-ctx state/sv-weight 0))
                         (-> @state/*selections
                             (fx/sub-ctx state/eof1-weights)
                             seq
                             vec
                             (get index)))
        rounded    (->> variation-from-mean
                        (mapv (partial * 1))
                        (mapv clojure.math/round)
                        (mapv (partial * 1)))
        ;; rain-max    (apply max
        ;;                    rounded)
        counts (->> (update-vals (->> rounded
                                      (group-by identity))
                                 count)
                    (into (sorted-map-by <)))]
    (let [width     1000
          height    500
          data counts #_(drop 1 (vec rain-counts))
          axis      (-> data-bounds
                        (quickthing/primary-axis {:x-name "deviation from mean"
                                                  :y-name "Counts"
                                                  :title  (str "EOF1: "
                                                               eof1-component)
                                                  :color  "#0008"}))]
      (-> axis
          (update :data
                  #(into %
                         (quickthing/hist data)))
          viz/svg-plot2d-cartesian
          (quickthing/svg-wrap [width
                                height])
          quickthing/svg2xml))))

(spit "out/1d-rain-noise-counts.svg"
      (across-field-1d 19))

(var-from-zero [-1 2 -3 4.0])

(def eof1weight-vs-variance
  "Return a pair of the eof1weight and variance
   (relative to the EOF1 signal)
  for a given INDEX (ie. time point)"
  ;;[index]
  (let [eof1-weight (-> @state/*selections
                        (fx/sub-ctx state/sv-weight 0))
        eof1-components (->> (fx/sub-ctx @state/*selections
                                         state/eof1-weights)
                             seq
                             vec
                             (mapv #(* %
                                       -1.0
                                       eof1-weight)))
        num-timesteps   (count eof1-components)]
    (let [var-matrix           (-> @state/*selections
                                   (fx/sub-ctx #_state/region-matrix state/noise-matrix-1d)
                                   :matrix)
          variations-from-mean (->> num-timesteps
                                    range
                                    (mapv #(-> var-matrix
                                               (uncomplicate.neanderthal.core/col %)
                                               seq
                                               vec
                                               var-from-zero
                                               (clojure.math/pow 2))))]
      (mapv vector
            eof1-components
            variations-from-mean))))
#_
(identity eof1weight-vs-variance)


(spit "out/index-vs-var.svg"
      (let [width  1000
            height 500
            axis   (-> eof1weight-vs-variance
                       (quickthing/primary-axis {:x-name "deviation from mean"
                                                 :y-name "Counts"
                                                 :title  "INDEX vs VAR"
                                                 :color  "#0008"}))]
        (-> axis
            (update :data
                    #(into %
                           (quickthing/adjustable-circles eof1weight-vs-variance
                                                          {:scale 10})))
            viz/svg-plot2d-cartesian
            (quickthing/svg-wrap [width
                                  height])
            quickthing/svg2xml))))
