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
   pois]
  (->
    (svg/group
      {}
      (svgmaps/latlon-axis ;; draws lat/lon axis
        region)
      (geojson2svg/read-file
        shoreline-filestr
        region)
      (svgmaps/points-of-interest
        pois
        region))
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
   text]
  (let [[width
         height] (dimension
                   region)
        spacing  (/
                   (min
                     width
                     height)
                   10.0)]
    (svg/text
      [(-
         width
         (/
           spacing
           2.0))
       (/
         spacing
         3.0)]
      text
      {:font-size         spacing
       :text-anchor       "end"
       :stroke            "black"
       :stroke-width      (/ spacing
                             50)
       :fill              "white"
       :dominant-baseline "hanging"})))

(defn
  empty-map
  "Just a blank of the right size
  Needed as a spacer in the grid of maps
  The REGION determines the dimension of the spacer"
  [input-region]
  (->
    (svg/group
      {})
    (quickthing/svg-wrap
      (dimension
        region))))

(defn
  grid-map
  "Draw a contour map with a grid overlay"
  ([input-grid
    contour-svg
    pois]
   (grid-map
     input-grid
     contour-svg
     pois
     ""))
  ([input-grid
    contour-svg
    pois
    text]
   (let [region             (geogrid/covered-region input-grid)
         local-rain-grid    (geogrid/subregion
                              input-grid
                              region)
         {:keys [overruns]} (geogrid/adjusted-crop-region-to-grid
                              region
                              local-rain-grid)]
     (->
       (svg/group
         {}
         (geogrid2svg/to-heatmap
           local-rain-grid
           overruns)
         (svgmaps/latlon-axis ;; draws lat/lon axis
           region)
         contour-svg
         (svgmaps/points-of-interest
           pois
           region)
         (map-label
           region
           text))
       (quickthing/svg-wrap
         (dimension
           region))
       #_quickthing/serialize-with-line-breaks))))

(defn
  two-d-plot
  [data
   width
   height]
  (->> (-> ;; create the axis
         (quickthing/zero-axis data
                               {:width width
                                :height height
                                :margin-frac 0.0})
         ;; add data to the plot
         (assoc :data
                [(quickthing/adjustable-circles (->> data
                                                     (map-indexed (fn [index
                                                                       data-point]
                                                                    (conj data-point
                                                                          nil ;; default radius
                                                                          {:stroke "#777"
                                                                           :fill   (quickthing/color-cycle
                                                                                     (- 12.0
                                                                                        (/ (+ index
                                                                                              3.0) ;; so it starts in blue
                                                                                           12.0)))})))))
                 (quickthing/index-text
                   data)])
         (assoc :grid ;; turn off grid
                nil))
    ;; turns the plot specification to svg hiccup
    (viz/svg-plot2d-cartesian)
    ;; wraps in an `<svg>` element
    (svg/svg {:width  width
              :height height})))

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
