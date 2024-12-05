(ns varfield
  "Functions to prompt for:
  Variance across the whole field
  And other related statistics"
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
                  uncomplicate.neanderthal.core/mrows)
        ncols         (-> region-matrix
                  uncomplicate.neanderthal.core/ncols)
        above-cols    (count above-data)]
    {:above (uncomplicate.neanderthal.native/dge mrows
                                                 (count above-data)
                                                 (->> above-data
                                                      (mapv seq)
                                                      flatten))
     :below (uncomplicate.neanderthal.native/dge mrows
                                                 (count below-data)
                                                 (->> below-data
                                                      (mapv seq)
                                                      flatten))}))

(-> @state/*selections
    (fx/sub-ctx above-below-data))
