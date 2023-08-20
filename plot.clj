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
   (let [region (geogrid/covered-region input-grid)
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
  (->>
    (->
      ;; create the axis
      (quickthing/zero-axis
        data
        1000
        1000)
      ;; add data to the plot
      (assoc
        :data
        [#_(quickthing/dashed-line
             data)
         (quickthing/adjustable-circles
           (map-indexed
             (fn [index
                  data-point]
               (conj
                 data-point
                 nil ;; default radius
                 {:stroke "#777"
                  :fill   (quickthing/color-cycle
                            (-
                              12.0
                              (/
                                (+
                                  index
                                  3.0) ;; so it starts in blue
                                12.0)))}))
             data))
         (quickthing/index-text
           data)
         ;;ICA vectors as Points
         #_(quickthing/adjustable-circles
             (mapv
               #(conj
                  %
                  100
                  {:fill "black"})
               (mapv
                 #(mapv
                    double
                    %)
                 (ica-vectors
                   data))))])
      (assoc ;; turn off grid
        :grid
        nil))
    ;; turns the plot specification to svg hiccup
    (viz/svg-plot2d-cartesian)
    ;; wraps in an `<svg>` element
    (svg/svg
      {:width  width
       :height height})))

(defn
  sv-weights
  [weights
   width
   height]
  (-> weights
      (quickthing/primary-axis {:x-name "YEAR"
                                :y-name "RAND"
                                :title  "TEST-PLOT"
                                :color  "#0008"})
      #_
      (assoc-in [:x-axis
                 :major]
                (range 2000
                       2016))
      (assoc :data
             (quickthing/hist weights))
      viz/svg-plot2d-cartesian
      (quickthing/svg-wrap [width
                            height])))
