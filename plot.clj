(ns
    plot
  "Some project-specific plotting fuctions"
  (:use     geoprim)
  (:require [geogrid]
            [geogrid2svg]
            [geojson2svg]
            [quickthing]
            [svgmaps]
            [thi.ng.geom.viz.core :as viz]
            [thi.ng.geom.svg.core :as svg]))

(def
  shoreline-filestr
  "Hardcoded path to a shoreline file.. TODO"
  "./data/shoreline-coarse.json")

(defn
  shoreline-map
  "Given a region and vector of geopoints(POIS)
  Draw a simple shoreline map"
  [region
   shoreline-filestr
   & [{:keys [pois
              label-top-right
              cycle-frac
              axis-visible?]
       :or   {pois            nil
              label-top-right ""
              cycle-frac      nil
              axis-visible? false}}]]
  (->
    (svg/group
      {}
      (if axis-visible?
        (svgmaps/latlon-axis ;; draws lat/lon axis
          region)
        (svg/group {}
                   nil))
      (geojson2svg/read-file
        shoreline-filestr
        region)
      (if pois
        (svgmaps/points-of-interest
          pois
          region)
        (svg/group {}
                  nil)))
    (quickthing/svg-wrap
      (dimension
        region))
    #_quickthing/serialize-with-line-breaks))

(defn
  worldmap-region
  "Adds a region to the world map
  The world map goes EAS 0-360 and SOU 0-180
  TODO: Make something a bit more generic.."
  [world-map
   region]
  (let [{:keys [norwes
                soueas]} region
        x-start          (:eas norwes)
        y-start          (:sou norwes)]
    ;;{:norwes {:eas 278.0, :sou 81.5}, :soueas {:eas 279.0, :sou 82.5}}
    (quickthing/svg-wrap
        (svg/group {}
                   world-map
                   (svg/rect [x-start
                              y-start]
                             (- (:eas soueas)
                                x-start)
                             (- (:sou soueas)
                                y-start)
                             {:fill "red"
                              :fill-opacity "0.25"}))
        [360.0
         180.0])))

(defn
  map-label
  "Label to add to the top right of maps"
  [region
   text
   & [attribs]]
  (let [[width
         height] (dimension region)
        spacing  (/ (min width
                         height)
                    10.0)]
    (svg/text [(- width
                  (/  spacing
                      2.0))
               (/ spacing
                  3.0)]
              text
              (merge {:font-size         spacing
                      :text-anchor       "end"
                      :stroke            "black"
                      :stroke-width      (/ spacing
                                            50)
                      :fill              "white"
                      :dominant-baseline "hanging"}
                     attribs))))

(defn
  empty-map
  "Just a blank of the right size
  Needed as a spacer in the grid of maps
  The REGION determines the dimension of the spacer"
  [input-region]
  (-> (svg/group {})
      (quickthing/svg-wrap (dimension region))))

(defn
  grid-map
  "Draw a contour map with a grid overlay"
  [input-grid
   contour-svg
   & [{:keys [pois
              label-top-right
              label-attribs
              cycle-frac
              axis-visible?]
       :or   {pois            []
              label-top-right ""
              label-attribs   nil
              cycle-frac      nil
              axis-visible? false}}]]
  (let [region             (geogrid/covered-region input-grid)
        local-rain-grid    (geogrid/subregion input-grid
                                              region)
        {:keys [overruns]} (geogrid/adjusted-crop-region-to-grid region
                                                                 local-rain-grid)]
    (-> (svg/group {}
                   (if input-grid ;; TODO: Make it work without a grid..
                     (geogrid2svg/to-heatmap local-rain-grid
                                             overruns)
                     (svg/group {}
                                nil))
                   (if axis-visible?
                     (svgmaps/latlon-axis region)
                     (svg/group {}
                                nil))
                   (if contour-svg
                     contour-svg
                     (svg/group {}
                                nil))
                   (svgmaps/points-of-interest pois
                                               region)
                   (map-label region
                              label-top-right
                              (merge label-attribs
                                     {:fill (quickthing/color-cycle cycle-frac)})))
        (quickthing/svg-wrap (dimension region)
                             360))))

(defn
  sv-plot
  "Data points should be in the form:
  [x, y, {:cycle-frac 69}]
  if the `:cycle-frac` is missing, it will be colored black"
  [data
   width
   height]
  (let [{:keys [angle
                points-a
                points-b
                centroid-a
                centroid-b]} (bisect/min-var data)]
    (println "ANGLE "
             angle)
    (->> (-> (quickthing/zero-axis data
                                   {:width       width
                                    :height      height
                                    :margin-frac 0.0})
             (assoc :data
                    (into []
                          cat
                          [(quickthing/adjustable-circles (->> data
                                                               (map-indexed (fn [index
                                                                                 [data-x
                                                                                  data-y
                                                                                  attribs]]
                                                                              [data-x
                                                                               data-y
                                                                               nil ;; default radius
                                                                               {:stroke "#777"
                                                                                :fill   (quickthing/color-cycle
                                                                                          (-> attribs
                                                                                              :cycle-frac))}])))
                                                          {:scale 20})
                           (quickthing/index-text data
                                                  {:scale 20})
                           (quickthing/line-through-point data
                                                          (->> angle
                                                               bisect/angle-to-unitvector)
                                                          {:attribs {:stroke           "red"
                                                                     :stroke-dasharray (str 50.0
                                                                                            " "
                                                                                            50.0)}})
                           (quickthing/line-through-point data
                                                          centroid-a
                                                          {:attribs {:stroke           "black"
                                                                     :stroke-dasharray (str 7.0
                                                                                            " "
                                                                                            7.0)}})
                           (quickthing/line-through-point data
                                                          centroid-b
                                                          {:attribs {:stroke           "black"
                                                                     :stroke-dasharray (str 7.0
                                                                                            " "
                                                                                            7.0)}})]))
             (assoc :grid ;; turn off grid
                    nil))
         (viz/svg-plot2d-cartesian)
         (svg/svg {:width  width
                   :height height}))))

(defn
  sv-weights
  [weights
   sv-weights-stats
   width
   height]
  (let [first-two (take 2 weights)
        the-rest  (drop 2 weights)]
    (-> weights
        (quickthing/primary-axis {:width  width
                                  :height height
                                  :margin-frac 0.00})
        #_#_
        (assoc-in [:x-axis
                   :major]
                  nil)
        (assoc-in [:y-axis
                   :major]
                  nil)
        (update :data
                #(into %
                       (quickthing/hist first-two
                                        {:attribs {:stroke "red"}})))
        (update :data
                #(into %
                       (quickthing/hist the-rest)))
        viz/svg-plot2d-cartesian
        (quickthing/svg-wrap [width
                              height]))))
#_
(let [width   1000
      height  500
      weights [[1 5000.0]
               [2 3000.0]
               [3 500.00]
               [4 300.00]
               [5 200.00]
               [6 100.00]]
      stats   (-> weights
                  matrix/singular-values-stats)]
  (spit "out/test-weights.svg"
        (-> (plot/sv-weights weights
                             stats
                             width
                             height)
            (quickthing/svg-wrap [width
                                  height])
            quickthing/svg2xml)))

(defn
  indeces
  [width
   height
   proj-a
   proj-b
   cycle-length ;; Maybe make these optional?
   cycle-phase]
  (let [index-a (into [] (map-indexed vector
                                      proj-a))
        index-b (map-indexed vector
                             proj-b)]
    (let [axis (-> (quickthing/primary-axis (into index-a
                                                   index-b)
                                             {:width width
                                              :height height
                                              :x-ticks [1.0]
                                              :y-ticks [1.0]
                                              :color  "#0008"})
                   (assoc-in [:x-axis
                              :major]
                             (range cycle-phase
                                    (count proj-a)
                                    cycle-length)))]
      (-> axis
          (assoc :data
                 (into []
                       cat
                       [(quickthing/hist index-a
                                         {:attribs {:opacity "0.5"
                                                    :stroke "red"}})
                        (quickthing/hist index-b
                                         {:attribs {:opacity "0.5"
                                                    :stroke "blue"}})]))
          viz/svg-plot2d-cartesian
          (quickthing/svg-wrap [width
                                height])))))

(defn
  cyclic
  [maps
   cols]
  (let [two-d (->> maps ;; TODO: replace with `partitionv-all`
                   (partition-all cols) ;; when Clojure 1.12 lands
                   (mapv (partial into  ;;needs to be vec-of-vecs
                                  [])))] ;;otherwise crashes
    two-d
    (quickthing/group-plots-grid two-d)))
#_
(-> [[:test1, {:height 100
               :width  100}]
     [:test2, {:height 100
               :width  100}]
     [:test3, {:height 100
               :width  100}]
     [:test4, {:height 100
               :width  100}]
     [:test5, {:height 100
               :width  100}]
     [:test6, {:height 100
               :width  100}]
     [:test7, {:height 100
               :width  100}]
     [:test8, {:height 100
               :width  100}]
     [:test9, {:height 100
               :width  100}]]
    (cyclic 4))

(defn
  annual-12-month-ring
  "A ring of 12 months
  08 07 06 05
  09 -- -- 04
  10 -- -- 03
  11 12 01 02
  in a counterclockwise fashion
  (why does counterclockwise seem better?)"
  [maps]
  (assert (== 12
              (count maps))
          "Can't plot a ring if it's not 12 maps")
  (let [submap (partial get      ;; helper to make layout easier to see below
                        maps)
        blank-spot (assoc (first maps)
                          2
                          (svg/group {}))]
    (let [map-matrix [[(submap 7), (submap 6), (submap 5), (submap 4)]
                      [(submap 8), blank-spot, blank-spot, (submap 3)]
                      [(submap 9), blank-spot, blank-spot, (submap 2)]
                      [(submap 10), (submap 11), (submap 0), (submap 1)]]]
    (quickthing/group-plots-grid map-matrix))))
