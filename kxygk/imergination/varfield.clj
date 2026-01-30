(ns kxygk.imergination.varfield
  "Functions to prompt for:
  Variance across the whole field
  And other related statistics"
  (:use [hashp.core])
  (:require [clojure.java.io :as io]
            [cljfx.api       :as fx]
            [clojure.core.cache :as cache]
            [injest.path :refer [+> +>> x>> =>>]]
            [uncomplicate.neanderthal.core :as ncore]
            [uncomplicate.neanderthal.native  :as nnative]
            kxygk.imergination.bisect
            kxygk.imergination.geogrid4image
            kxygk.imergination.geogrid4seq
            kxygk.imergination.svg2jfx
            kxygk.imergination.matrix
            kxygk.imergination.plot
            kxygk.imergination.locations))

#_
(-> @state/*selections
    (fx/sub-ctx state/sv-bisection)
    keys)
;; => (:angle :points :centroid-a :centroid-b)

#_
(-> @state/*selections
    (fx/sub-ctx state/sv-bisection)
    :points
    first)
;; => [-0.024578662253719318
;;     0.05558278260624202
;;     {:cycle-frac 0, :above? false}]

#_
(-> @state/*selections
    (fx/sub-ctx state/sv-bisection)
    :points
    first
    (get 2)
    :above?)

#_
(->> (fx/sub-ctx @state/*selections
                 state/sv-bisection)
     :points
     (group-by #(-> %
                    (get 2)
                    :above?))
     keys)
;; => (false true)

(defn
  above-below-data
  [context]
  (let [bisection     (->> (fx/sub-ctx context
                                   state/sv-bisection))
        svd2d-points  (-> bisection
                         :points)
        above?        (->> svd2d-points
                    (mapv #(-> %
                               (get 2)
                               :above?)))
        region-matrix (->> (fx/sub-ctx context
                                       state/region-matrix)
                           :matrix)
        above-data    (->> region-matrix 
                        uncomplicate.neanderthal.core/cols
                        (mapv vector above?)
                        (filter first)
                        (mapv second))
        below-data    (->> region-matrix 
                        uncomplicate.neanderthal.core/cols
                        (mapv vector above?)
                        (filter #(-> %
                                     first
                                     not))
                        (mapv second))
        mrows         (-> region-matrix
                  ncore/mrows)
        ncols         (-> region-matrix
                  ncore/ncols)
        above-cols    (count above-data)]
    {:above (nnative/dge mrows
                                                 (count above-data)
                                                 (->> above-data
                                                      (mapv seq)
                                                      flatten))
     :below (nnative/dge mrows
                                                 (count below-data)
                                                 (->> below-data
                                                      (mapv seq)
                                                      flatten))}))
#_
(-> @state/*selections
    (fx/sub-ctx above-below-data))


#_
(-> @state/*selections
    (fx/sub-ctx state/sv-bisection)
    :centroid-a)
;; => [-0.9069057768547362 -0.4213334925062419]

#_
(-> @state/*selections
    (fx/sub-ctx state/first-pattern))


(defn
  above-below-noise
  [context]
  (let [{:keys [above
                below]}   (-> context
                              (fx/sub-ctx above-below-data))
        above-pattern-vec (-> context
                              (fx/sub-ctx state/first-pattern)
                              nnative/dv
                              matrix/vecnorm)
        below-pattern-vec (-> context
                              (fx/sub-ctx state/second-pattern)
                              nnative/dv
                              matrix/vecnorm)]
    (let [above-index (ncore/mv (ncore/trans above)
                                above-pattern-vec)
          below-index (ncore/mv (ncore/trans below)
                                below-pattern-vec)]
      (let [above-signal (ncore/rk above-pattern-vec
                                   above-index)
            below-signal (ncore/rk below-pattern-vec
                                   below-index)
            above-noise (ncore/axpy -1.0
                                    above-signal
                                    above)
            below-noise (ncore/axpy -1.0
                                    below
                                    (ncore/rk below-pattern-vec
                                              below-index))]
        {:above above
         :above-pattern above-pattern-vec
         :above-index above-index
         :above-signal above-signal
         :above-noise above-noise
         :below below
         :below-pattern below-pattern-vec
         :below-index below-index
         :below-signal below-signal
         :below-noise below-noise}))))
#_
(-> @state/*selections
    above-below-noise)
;; => {:above #RealGEMatrix[double, mxn:2840x56, layout:column]
;;       ▥       ↓       ↓       ↓       ↓       ↓       ┓    
;;       →     120.00  470.00    ⁙     416.00  236.00         
;;       →     155.00  335.00    ⁙     394.00  242.00         
;;       →       ⁙       ⁙       ⁙       ⁙       ⁙            
;;       →     168.00  197.00    ⁙     386.00  217.00         
;;       →      98.00  175.00    ⁙     387.00  185.00         
;;       ┗                                               ┛    
;;    , :pattern #RealBlockVector[double, n:2840, stride:1]
;;    [3.37E-2 3.40E-2 3.48E-2    ⋯   1.52E-2 1.41E-2 ]
;;    , :above-index #RealBlockVector[double, n:56, stride:1]
;;    [4.27E+3 9.62E+3 1.40E+4    ⋯   1.76E+4 1.58E+4 ]
;;    , :above-signal #RealGEMatrix[double, mxn:2840x56, layout:column]
;;       ▥       ↓       ↓       ↓       ↓       ↓       ┓    
;;       →     144.05  324.51    ⁙     593.54  533.13         
;;       →     145.22  327.13    ⁙     598.34  537.44         
;;       →       ⁙       ⁙       ⁙       ⁙       ⁙            
;;       →      64.79  145.96    ⁙     266.96  239.79         
;;       →      60.25  135.72    ⁙     248.24  222.97         
;;       ┗                                               ┛    
;;    , :above-noise #RealGEMatrix[double, mxn:2840x56, layout:column]
;;       ▥       ↓       ↓       ↓       ↓       ↓       ┓    
;;       →     -24.05  145.49    ⁙    -177.54 -297.13         
;;       →       9.78    7.87    ⁙    -204.34 -295.44         
;;       →       ⁙       ⁙       ⁙       ⁙       ⁙            
;;       →     103.21   51.04    ⁙     119.04  -22.79         
;;       →      37.75   39.28    ⁙     138.76  -37.97         
;;       ┗                                               ┛    
;;    }

(defn
  above-below-variances
  [context]
  (let [{:keys [above
                above-pattern
                above-index
                above-signal
                above-noise
                below
                below-pattern
                below-index
                below-signal
                below-noise]} (-> context
                                  (fx/sub-ctx above-below-noise))]
    {:above-vars (mapv vector
                       above-index
                       (matrix/colvars above-noise))
     :below-vars  (mapv vector
                        below-index
                        (matrix/colvars below-noise))} ))
#_
(-> @state/*selections
    above-below-variances
    keys)
;; => (:above-vars :below-vars)

(defn
  plot-variance-summaries
  [context]
  (let [width                1000
        height               2000
        {:keys [above-vars
                below-vars]} (-> context
                                 (fx/sub-ctx above-below-variances)) ]
    (let [above-fit-params (->> above-vars
                                matrix/linear-fit
                                seq
                                flatten
                                (zipmap [:offset
                                         :slope]))
          below-fit-params (->> below-vars
                                matrix/linear-fit
                                seq
                                flatten
                                (zipmap [:offset
                                         :slope]))]
      (-> (quickthing/group-plots-grid [[(plot/eof1-vs-var above-vars
                                                           (str "Above: "
                                                                (->> above-fit-params
                                                                     :slope
                                                                     (format "%.3G"))
                                                                "x + "
                                                                (->> above-fit-params
                                                                     :offset
                                                                     (format "%.3G")))
                                                           width
                                                           (/ height
                                                              2.0)
                                                           {:y-name     "variance"
                                                            :fit-params above-fit-params})]
                                        [(plot/eof1-vs-var below-vars
                                                           (str "Below: "
                                                                (->> below-fit-params
                                                                     :slope
                                                                     (format "%.3G"))
                                                                "x + "
                                                                (->> below-fit-params
                                                                     :offset
                                                                     (format "%.3G")))
                                                           width
                                                           (/ height
                                                              2.0)
                                                           {:y-name     "variance"
                                                            :fit-params below-fit-params})]])))))
;;#_
(-> @state/*selections
    plot-variance-summaries
    (state/spitsvgstream "pattern-variances.svg"))
