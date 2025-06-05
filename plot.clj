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
              display-width
              cycle-frac
              axis-visible?]
       :or   {pois            nil
              label-top-right ""
              display-width   nil
              cycle-frac      nil
              axis-visible?   false}}]]
  (let [svg-nodes (-> (svg/group
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
                                     nil))))]
    (if (nil? display-width)
      (quickthing/svg-wrap svg-nodes
                           (dimension region))
      (quickthing/svg-wrap svg-nodes
                           (dimension region)
                           display-width))))

(defn
  worldmap-region
  "Adds a region to the world map
  The world map goes EAS 0-360 and SOU 0-180
  TODO: Make something a bit more generic.."
  [world-map
   region
   & [{:keys [display-width]
       :or   {display-width 360}}]]
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
                           {:fill         "red"
                            :fill-opacity "0.25"}))
      [360.0
       180.0]
      display-width)))

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
              max-val
              label-top-right
              label-attribs
              display-width
              cycle-frac
              colormap
              axis-visible?]
       :or   {pois            []
              max-val         nil
              label-top-right ""
              label-attribs   nil
              display-width   360
              cycle-frac      nil
              axis-visible?   false}}]]
  (let [region             (geogrid/covered-region input-grid)
        local-rain-grid    (geogrid/subregion input-grid
                                              region)
        {:keys [overruns]} (geogrid/adjusted-crop-region-to-grid region
                                                                 local-rain-grid)]
    (-> (svg/group {}
                   (if input-grid ;; TODO: Make it work without a grid..
                     (geogrid2svg/to-heatmap local-rain-grid
                                             (cond-> overruns
                                               (some? max-val) (assoc :max-val
                                                                      max-val)
                                               (some? colormap) (assoc :colormap
                                                                       colormap)))
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
                              (merge {:fill (quickthing/color-cycle cycle-frac)}
                                     label-attribs)))
        (quickthing/svg-wrap (dimension region)
                             display-width))))

(defn
  sv-plot
  "Data points should be in the form:
  [x, y, {:cycle-frac 69}]
  if the `:cycle-frac` is missing, it will be colored black"
  [data
   width
   height
   & [{:keys [errors]
       :or   {errors []}}]]
  (let [{:keys [angle
                points-a
                points-b
                centroid-a
                centroid-b]} (bisect/min-var data)]
    (->> (-> (quickthing/zero-axis data
                                   {:width       width
                                    :height      height
                                    :margin-frac 0.0})
             (update :data
                     #(into %
                            (quickthing/adjustable-circles (->> data
                                                                (map-indexed (fn [index
                                                                                  [data-x
                                                                                   data-y
                                                                                   attribs]]
                                                                               [data-x
                                                                                data-y
                                                                                nil ;; default radius
                                                                                {:stroke #_"transparent" "#777"
                                                                                 ;; TODO thread the whole thing
                                                                                 :fill   (-> attribs
                                                                                             :cycle-frac
                                                                                             quickthing/color-cycle)}])))
                                                           {:scale 15})))
             #_
             (update :data
                     #(into %
                            (flatten (quickthing/error-bars errors))))
             #_
             (update :data
                     #(into %
                            (quickthing/index-text data
                                                   {:scale 40})))
             (update :data
                     #(into %
                             (quickthing/line-through-point data
                                                          (->> angle
                                                               bisect/angle-to-unitvector)
                                                          {:attribs {:stroke           "red"
                                                                     :stroke-dasharray (str 50.0
                                                                                            " "
                                                                                            50.0)}})))
             (update :data
                     #(into %
                           (quickthing/line-through-point data
                                                          centroid-a
                                                          {:attribs {:stroke           "black"
                                                                     :stroke-dasharray (str 7.0
                                                                                            " "
                                                                                            7.0)}})))
             (update :data
                     #(into %
                            (quickthing/line-through-point data
                                                          centroid-b
                                                          {:attribs {:stroke           "black"
                                                                     :stroke-dasharray (str 7.0
                                                                                            " "
                                                                                            7.0)}})))
             (assoc :grid ;; turn off grid
                    nil))
         (viz/svg-plot2d-cartesian)
         (svg/svg {:width  width
                   :height height}))))

;; NOTE: This doesn't work b/c the ellipse errors aren't in the plotting units
;; So you get circles but they're not properly scaled
;; They look deceptively right...
;; DON'T USE
#_
(defn
  sv-ellipse-plot
  "Data points should be in the form:
  [x, y, err-x, err-y, {:cycle-frac 69}]
  if the `:cycle-frac` is missing, it will be colored black"
  [data-with-errors
   width
   height]
  (let [{:keys [angle
                points-a
                points-b
                centroid-a
                centroid-b]} (bisect/min-var (->> data-with-errors
                                                 (mapv (fn [[x y]]
                                                         [x
                                                          y]))))]
    (->> (-> (quickthing/zero-axis data-with-errors
                                   {:width       width
                                    :height      height
                                    :margin-frac 0.0})
             (update :data
                     #(into %
                            (quickthing/adjustable-ellipses (->> data-with-errors
                                                                 (map-indexed (fn [index
                                                                                   [data-x
                                                                                    data-y
                                                                                    err-x
                                                                                    err-y
                                                                                    attribs]]
                                                                               [data-x
                                                                                data-y
                                                                                err-x
                                                                                err-y
                                                                                {:stroke #_ "transparent" "#777"
                                                                                 ;; TODO thread the whole thing
                                                                                 :fill   (-> attribs
                                                                                             :cycle-frac
                                                                                             quickthing/color-cycle)}]))
                                                                 doall)
                                                           {:scale 30})))
             #_
             (update :data
                     #(into %
                            (flatten (quickthing/error-bars errors))))
             #_
             (update :data
                     #(into %
                            (quickthing/index-text data
                                                   {:scale 40})))
             (update :data
                     #(into %
                             (quickthing/line-through-point data-with-errors
                                                          (->> angle
                                                               bisect/angle-to-unitvector)
                                                          {:attribs {:stroke           "red"
                                                                     :stroke-dasharray (str 50.0
                                                                                            " "
                                                                                            50.0)}})))
             (update :data
                     #(into %
                           (quickthing/line-through-point data-with-errors
                                                          centroid-a
                                                          {:attribs {:stroke           "black"
                                                                     :stroke-dasharray (str 7.0
                                                                                            " "
                                                                                            7.0)}})))
             (update :data
                     #(into %
                            (quickthing/line-through-point data-with-errors
                                                          centroid-b
                                                          {:attribs {:stroke           "black"
                                                                     :stroke-dasharray (str 7.0
                                                                                            " "
                                                                                            7.0)}})))
             (assoc :grid ;; turn off grid
                    nil))
         (viz/svg-plot2d-cartesian)
         (svg/svg {:width  width
                   :height height}))))




(defn
  sv-weights
  [weights
   num-display
   sv-weights-stats
   width
   height]
  (let [total-weight (->> weights
                          (mapv second)
                          (reduce +))]
    (let [weight-perc (->> weights
                           (take num-display)
                           (mapv (fn [weight-pair]
                                   (update weight-pair
                                           1
                                           #(* 100
                                               (/ %
                                                  total-weight))))))]
      (let [first-two (take 2 weight-perc)
            the-rest  (drop 2 weight-perc)]
        (-> weight-perc
            (quickthing/primary-axis  {:width       width
                                       :height      height
                                       :margin-frac 0.05})
            (assoc :grid ;; turn off grid
                   nil)
            (assoc-in [:x-axis
                       :major]
                      [])
            (assoc-in [:x-axis
                       :minor]
                      [])
            (update :data
                    #(into %
                           (quickthing/hist first-two
                                            {:attribs {:stroke "red"}})))
            (update :data
                    #(into %
                           (quickthing/hist the-rest)))
            viz/svg-plot2d-cartesian
            (quickthing/svg-wrap [width
                                  height]
                                 width))))))
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
                             6
                             stats
                             width
                             height)
            (quickthing/svg-wrap [width
                                  height])
            quickthing/svg2xml)))

(defn
  index
  [width
   height
   proj
   cycle-start-value
   cycle-length ;; Maybe make these optional?
   cycle-phase]
  (let [indexed (map-indexed vector
                             proj)]
    (let [axis (-> (quickthing/primary-axis indexed
                                            {:width       width
                                             :height      height
                                             :x-ticks     [1.0]
                                             :y-ticks     [1.0]
                                             :margin-frac 0.07
                                             :color       "#0008"})
                   (assoc-in [:x-axis
                              :label]
                             (thi.ng.geom.viz.core/default-svg-label #(+ cycle-start-value
                                                                         (/ %
                                                                            cycle-length))))
                   (assoc-in [:x-axis
                              :major]
                             (into [] (range cycle-phase
                                             (count proj)
                                             cycle-length)))
                   (assoc-in [:y-axis
                              :major]
                             []))]
      (-> axis
          (assoc :data
                 (into []
                       cat
                       [(quickthing/hist indexed
                                         {:attribs {;;:opacity "0.5"
                                                    :stroke-width 10 #_0.4
                                                    :stroke       "gray"}})]))
          viz/svg-plot2d-cartesian
          (quickthing/svg-wrap [width
                                height]
                               width)))))

(defn
  indeces
  [width
   height
   proj-a
   proj-b
   cycle-start-value
   cycle-length ;; Maybe make these optional?
   cycle-phase]
  (let [index-a (into [] (map-indexed vector
                                      proj-a))
        index-b (map-indexed vector
                             proj-b)]
    (let [axis (-> (quickthing/primary-axis (into index-a
                                                  index-b)
                                            {:width       width
                                             :height      height
                                             :x-ticks     [1.0]
                                             :y-ticks     [1.0]
                                             :margin-frac 0.07
                                             :color       "#0008"})
                   (assoc-in [:x-axis
                              :label]
                             (thi.ng.geom.viz.core/default-svg-label #(+ cycle-start-value
                                                                         (/ %
                                                                            cycle-length))))
                   (assoc-in [:x-axis
                              :major]
                             (range cycle-phase
                                    (count proj-a)
                                    cycle-length))
                   (assoc-in [:y-axis
                              :major]
                             []))]
      (-> axis
          (assoc :data
                 (into []
                       cat
                       [(quickthing/hist index-a
                                         {:attribs {:stroke-width 10
                                                    :stroke       "#00aa88"}})
                        (quickthing/hist index-b
                                         {:attribs {:stroke-width 10
                                                    :stroke       "#aa8800"}})]))
          viz/svg-plot2d-cartesian
          (quickthing/svg-wrap [width
                                height]
                               width)))))

(defn
  sv1sv2-1scale
  "Two line plots of SV1 and SV2"
  [width
   height
   sv-projs
   cycle-start-value
   cycle-length ;; Maybe make these optional?
   cycle-phase]
  (let [proj-sv1 (->> sv-projs
                      (map first)
                      (map-indexed vector))
        proj-sv2 (->> sv-projs
                      (map second)
                      (map-indexed vector))]
    (let [sv-axis (-> (quickthing/primary-axis (into proj-sv1
                                                     proj-sv2)
                                               {:width           width
                                                :height          height
                                                :legend          [["First Singular Vector"
                                                                   {:fill "#0000aa"}]
                                                                  ["Second Singular Vector"
                                                                   {:fill "#aa0000"}]]
                                                ;;:title           "SV1 SV2 weights"
                                                ;;:x-name      "Data index"
                                                ;; :x-ticks     [1.0]
                                                ;; :y-ticks     [1.0]
                                                #_#_:margin-frac 0.07
                                                #_#_:color       "#0000aa"})
                      (assoc-in [:grid]
                                nil)
                      (assoc-in [:x-axis
                                 :label]
                                (thi.ng.geom.viz.core/default-svg-label #(+ cycle-start-value
                                                                            (/ %
                                                                               cycle-length))))
                      (assoc-in [:x-axis
                                 :major]
                                (range cycle-phase
                                       (count sv-projs)
                                       cycle-length)))]
      (-> sv-axis
          (update :data
                  #(into %
                         (quickthing/solid-line proj-sv1
                                                {:attribs {:stroke-width 7
                                                           :stroke       "#0000aa"}})))
          (update :data
                  #(into %
                         (quickthing/solid-line proj-sv2
                                                {:attribs {:stroke-width 7
                                                           :stroke       "#aa0000"}})))
          viz/svg-plot2d-cartesian
          (quickthing/svg-wrap [width
                                height]
                               width)))))

(defn
  sv1sv2-2scale
  "Two line plots of SV1 and SV2"
  [width
   height
   sv-projs
   cycle-start-value
   cycle-length ;; Maybe make these optional?
   cycle-phase]
  (let [proj-sv1 (->> sv-projs
                      (map first)
                      (map-indexed vector))
        proj-sv2 (->> sv-projs
                      (map second)
                      (map-indexed vector))]
    (let [sv1-axis (-> (quickthing/primary-axis proj-sv1
                                                {:width       width
                                                 :height      height
                                                 :legend      [["Singular Vec 1"
                                                                {:fill "#0000aa"}]
                                                               ["Singular Vec 2"
                                                                {:fill "#aa0000"}]]
                                                 ;;:title       "SV1 SV2 weights"
                                                 ;;:x-name      "Data index"
                                                 ;; :x-ticks     [1.0]
                                                 ;; :y-ticks     [1.0]
                                                 :margin-frac 0.06
                                                 :color       "#0000aa"})
                       (assoc-in [:grid]
                                 nil)
                       (assoc-in [:x-axis
                                  :label]
                                 (thi.ng.geom.viz.core/default-svg-label #(+ cycle-start-value
                                                                             (/ %
                                                                                cycle-length))))
                       (assoc-in [:x-axis
                                  :major]
                                 (range cycle-phase
                                        (count sv-projs)
                                        cycle-length)))
          sv2-axis (-> (quickthing/secondary-axis proj-sv2
                                                  {:width       width
                                                   :height      height
                                                   :margin-frac 0.06
                                                   :color       "#aa0000"})
                       (assoc-in [:grid]
                                 nil)
                       (assoc-in [:x-axis
                                  :label]
                                 (thi.ng.geom.viz.core/default-svg-label #(+ cycle-start-value
                                                                             (/ %
                                                                                cycle-length))))
                       (assoc-in [:x-axis
                                  :major]
                                 (range cycle-phase
                                        (count sv-projs)
                                        cycle-length)))]
      (-> (svg/group {}
                     (-> sv1-axis
                         (update :data
                                 #(into %
                                        (quickthing/solid-line proj-sv1
                                                               {:attribs {:stroke-width 7
                                                                          :stroke       "#0000aa"}})))
                         #_
                         (update :data
                                 #(into %
                                        (quickthing/solid-line proj-sv2
                                                               {:attribs {:stroke-width 7
                                                                          :stroke       "#aa0000"}})))
                         viz/svg-plot2d-cartesian)
                     (-> sv2-axis
                         #_
                         (update :data
                                 #(into %
                                        (quickthing/solid-line proj-sv1
                                                               {:attribs {:stroke-width 7
                                                                          :stroke       "#0000aa"}})))
                         (update :data
                                 #(into %
                                        (quickthing/solid-line proj-sv2
                                                               {:attribs {:stroke-width 7
                                                                          :stroke       "#aa0000"}})))
                         viz/svg-plot2d-cartesian))
          (quickthing/svg-wrap [width
                                height]
                               width)))))

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
  (let [submap     (partial get      ;; helper to make layout easier to see below
                            maps)
        blank-spot (assoc (first maps)
                          2
                          (svg/group {}))]
    (let [map-matrix [[(submap 7), (submap 6), (submap 5), (submap 4)]
                      [(submap 8), blank-spot, blank-spot, (submap 3)]
                      [(submap 9), blank-spot, blank-spot, (submap 2)]
                      [(submap 10), (submap 11), (submap 0), (submap 1)]]]
      (quickthing/group-plots-grid map-matrix))))

(defn
  add-cycle-color
  "if there is cycle data in the meta data,
  add a `:fill` color"
  [data-vec]
  (->> data-vec
       (mapv (fn [data-entry]
               (let [cycle-frac (-> data-entry
                                    last
                                    :cycle-frac)]
                 (if (nil? cycle-frac)
                   data-entry
                   (update data-entry
                           (-> data-entry
                               count
                               dec)
                           (fn [meta-data-map]
                             (assoc meta-data-map
                                    :fill
                                    (quickthing/color-cycle cycle-frac))))))))))

(defn
  scatter
  "An [x y] scatter plot"
  [xy-vec
   width
   height
   & [{:keys [title-str
              x-name
              y-name
              highlighted-idx-vec
              traced-id-vec
              fit-params]}]]
  (let [xy-radius      (->> xy-vec
                            (mapv (fn [xy-entry]
                                    [(first xy-entry)
                                     (second xy-entry)
                                     nil
                                     (last xy-entry)])))
        xy-indexed-vec (->> xy-vec
                            (map-indexed (fn [index
                                              xy-entry]
                                           [(first xy-entry)
                                            (second xy-entry)
                                            index]))
                            vec)]
    (let [axis (-> (into [[0 0]]
                         xy-vec)
                   (quickthing/primary-axis {:width     width
                                             :height    height
                                             :x-name    x-name
                                             :y-name    y-name
                                             :title     title-str
                                             #_#_:color "#0008"}))]
      (-> (cond-> axis
            #_#_
            (some? fit-params) (update :data
                                       #(into %
                                              (quickthing/polyline xy-vec
                                                                   [(:offset fit-params)
                                                                    (:slope fit-params)]
                                                                   {:scale   50
                                                                    :attribs {:dy -10.0}})))
            #_
            (update :data
                    #(into %
                           (quickthing/adjustable-circles traced-id-vec
                                                          {:scale   20
                                                           :attribs {:stroke       "#0004"
                                                                     :stroke-width 3
                                                                     :fill         "none"}})))
            #_#_
            true               (update :data
                                       #(into %
                                              (quickthing/adjustable-circles (-> xy-vec
                                                                                 (select-keys highlighted-idx-vec)
                                                                                 vals
                                                                                 vec)
                                                                             #_
                                                                             {:scale   70
                                                                              :attribs {:stroke       "#f004"
                                                                                        :stroke-width 8
                                                                                        :fill         "none"}})))
            true               (update :data
                                       #(into %
                                              (quickthing/adjustable-circles xy-radius
                                                                             {:attribs {:stroke "#777"}})))
            true               (update :data
                                       #(into %
                                              (quickthing/index-text xy-radius
                                                                     {:scale 40}))))
          ;;#_#_
          viz/svg-plot2d-cartesian
          (quickthing/svg-wrap [width
                                height]
                               width)))))

;;(conj [1 2] 3)

#_
(-> [[12 3242] [213 423] [2342 525]]
    (eof1-vs-var :blah 1000 1000))

#_
(-> @state/*selections
    (cljfx.api/sub-ctx state/eof1weight-vs-variance-from-zero)
    (eof1-vs-var :blah 1000 1000))

#_
(-> @state/*selections
    (cljfx.api/sub-ctx state/eof1weight-vs-variance-from-zero))


(defn
  histogram-of-index-values
  [index-values
   [width, height]
   & [{:keys [title]}]]
  (let [max-index (->> index-values
                       (apply max))]
    (let [bin-size (/ max-index
                      10.0)]
      (let [hist-data (->> (update-vals (->> index-values
                                             (mapv #(/ %
                                                       bin-size))
                                             (mapv clojure.math/round)
                                             (group-by identity))
                                        count)
                           (into (sorted-map-by <))
                           (into []))]
        (let [axis (-> (into hist-data
                             [[0, 0]]) ;; make sure 0,0 is included
                       (quickthing/primary-axis {:width  width
                                                 :height height
                                                 :x-name "deviation from mean"
                                                 :y-name "Counts"
                                                 :title  title}))]
          (-> axis
              (update :data
                      #(into  %
                              (quickthing/hist hist-data
                                               {:attribs {;;:opacity "0.5"
                                                          :stroke-width 20 #_0.4
                                                          :stroke       "black"}})))
              viz/svg-plot2d-cartesian
              (quickthing/svg-wrap [width
                                    height]
                                   width)))))))

(defn
  histogram-of-monthly-rain-amounts
  [counts
   [x-min, x-max]
   [y-min, y-max]
   [width, height]
   & [{:keys [title]}]]
  (let [data counts
        axis (-> [[x-min, y-min]
                  [x-max, y-max]]
                 (quickthing/primary-axis {:width  width
                                           :height height
                                           :x-name "deviation from mean"
                                           :y-name "Counts"
                                           :title  title}))]
    (-> axis
        (update :data
                #(into %
                       (quickthing/hist data)))
        viz/svg-plot2d-cartesian
        (quickthing/svg-wrap [width
                              height]
                             width))))

(defn
  histograms-of-monthly-rain-amounts
  [data-matrix
   [width,height]
   title-prefix
   indeces]
  (let [counts-for-each-index (->> indeces
                                   (mapv (fn [time-index]
                                           (let [rain-vector (-> data-matrix
                                                                 (uncomplicate.neanderthal.core/col time-index)
                                                                 seq
                                                                 vec)]
                                             (let [bin-size (inc (clojure.math/ceil (/ (clojure.math/log (count rain-vector))
                                                                                       (clojure.math/log 2))))] ;; Sturges' rule
                                               (->> (update-vals (->> (uncomplicate.neanderthal.core/col data-matrix
                                                                                                         time-index)
                                                                      seq
                                                                      vec
                                                                      (mapv #(/ %
                                                                                bin-size))
                                                                      (mapv int )
                                                                      (mapv (partial *
                                                                                     bin-size))
                                                                      (group-by identity))
                                                                 count)
                                                    (into (sorted-map-by <))))))))]
    (let [max-count (->> counts-for-each-index
                         (mapv vals)
                         flatten
                         (apply max))
          x-bins    (->> counts-for-each-index
                         (mapv keys)
                         flatten)]
      (println (str "Max Count: "
                    max-count
                    " Zero count: "
                    (->> counts-for-each-index
                         (mapv #(get %
                                     0)))))
      (mapv (fn [counts,
                 index]
              (histogram-of-monthly-rain-amounts counts
                                                 [(apply min x-bins)
                                                  (apply max x-bins)]
                                                 [0.0, max-count]
                                                 [width, height]
                                                 {:title (str title-prefix
                                                              "#"
                                                              index)}))
            counts-for-each-index
            indeces))))
#_
(histograms-of-monthly-rain-amounts (-> @state/*selections
                                        (cljfx.api/sub-ctx state/noise-1d-matrix)
                                        :matrix)
                                    [100,100]
                                    [1, 2, 3])

(defn
  non-eof1-stats
  "A ring of 12 months
  08 07 06 05
  09 -- -- 04
  10 -- -- 03
  11 12 01 02
  in a counterclockwise fashion
  (why does counterclockwise seem better?)"
  [eof1-vs-var-svg
   hist-svg-vec]
  (assert (<= 8
              (count hist-svg-vec))
          "Can't plot a ring of less than 8 histograms")
  (let [hists (partial get      ;; helper to make layout easier to see below
                       hist-svg-vec)]
    (let [map-matrix [[(hists 0),    (hists 1),    (hists 2)]
                      [(hists 7), eof1-vs-var-svg, (hists 3)]
                      [(hists 6),    (hists 5),    (hists 4)]]]
      (quickthing/group-plots-grid map-matrix))))
