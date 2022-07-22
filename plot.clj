(ns
    plot
  (:use
   geoprim)
  (:require
   [thi.ng.geom.viz.core
    :as
    viz]
   [thi.ng.geom.svg.core
    :as
    svg]))

(def
  shoreline-filestr
  "Hardcoded path to a shoreline file.. TODO"
  "./data/shoreline-coarse.json")

(defn
  shoreline-map
  "Given a region and vector of geopoints(POIS)
  Draw a simple shoreline map"
  [region
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
    (svghelpers/set-display-width
      1000
      (dimension
        region))
    #_svghelpers/serialize-with-line-breaks))

(defn
  map-label
  "Label to add to the top right of maps"
  [region
   text]
  (let [;;
        [width
         height]
        (dimension
          region)
        ;;
        spacing
        (/
          (min
            width
            height)
          10.0)
        ]
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
       :fill             "white"
       :dominant-baseline "hanging"})))

(defn
  grid-map
  "Draw a contour map with a grid overlay"
  ([input-grid
    region
    pois]
   (grid-map
     input-grid
     region
     pois
     ""))
  ([input-grid
    region
    pois
    text]
   (let [;;
         local-rain-grid
         (geogrid/subregion 
           input-grid
           region)
         ;;
         {:keys [overruns]}
         (geogrid/adjusted-crop-region-to-grid
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
         (geojson2svg/read-file
           shoreline-filestr
           region)
         (svgmaps/points-of-interest
           pois
           region)
         (map-label
           region
           text))
       (svghelpers/set-display-width
         1000
         (dimension
           region))
       #_svghelpers/serialize-with-line-breaks))))

(defn
  two-d-plot
  [data
   width
   height]
  (->>
    (->
      ;; create the axis
      (quickthing/standard-axis
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
