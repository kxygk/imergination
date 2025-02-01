(ns
    gui
  "Imergination GUI tree"
  (:require [clojure.java.io :as io]
            [cljfx.api       :as fx]
            [cljfx.ext.list-view :as fx.ext.list-view]
            quickthing
            bisect
            plot
            locations
            svg2jfx
            state)
  (:gen-class :main true))`

(set!
  *warn-on-reflection*
  true)


(defn
  svg
  "The actual workmap svg (as a JFX Group)
  This is addition Group that functions as a wrapper post-scaling
  This wrapped group is essential! Prevents spill out after scaling"
  [{:keys [fx/context
           svg-hiccup
           scale-x
           scale-y]}]
  {:fx/type  :group
   :children [{:fx/type fx/ext-instance-factory
               :create  (fn []
                          (-> svg-hiccup
                              quickthing/svg2xml
                              svg2jfx/batik-load
                              (svg2jfx/batik-scale
                                scale-x
                                scale-y)))}]})

(defn
  click-to-soueas
  [click-event
   display-width]
  (let [click-x (.getX click-event)
        click-y (.getY click-event)]
    (let [scaling (/ 360.0
                     display-width)]
      (geoprim/point-eassou (* click-x
                               scaling)
                            (* click-y
                               scaling)))))


;; {:effect (fn [snapshot
;;                                          event]
;;                                       (-> snapshot
;;                                           (fx/swap-context
;;                                             assoc
;;                                             :datafile-idxs
;;                                             (:fx/event
;;                                              event))))}]


(defn
  calc-new-region
  [click-point
   mouse-release-point]
  (let [[onn-x
         onn-y] (geoprim/as-eassou click-point)
        [off-x
         off-y] (geoprim/as-eassou mouse-release-point)]
    (geoprim/region (geoprim/point-eassou (min onn-x
                                               off-x)
                                          (min onn-y
                                               off-y))
                    (geoprim/point-eassou (max onn-x
                                               off-x)
                                          (max onn-y
                                               off-y)))))

(defn
  event-worldmap-mouse-press
  "When the mouse is pressed on the worldmap
  Record the position"
  [snapshot
   {:keys [fx/event]}]
  (let [click-coord (click-to-soueas event
                                     (fx/sub-ctx snapshot
                                                 state/display-width))]
    (print "Wow you clicked.."
           click-coord)
    (-> snapshot
        (fx/swap-context assoc
                         :mouse-click
                         click-coord))))

(defn
  event-worldmap-mouse-release
  "When the mouse is released on the worldmap
  Calculate the selected region and update the state
  Also reset the mousepress to `nil`"
  [snapshot
   {:keys [fx/event]}]
  (println "you released the mouse button..."
           "\n"
           "The new region: "
           (calc-new-region (fx/sub-val snapshot
                                        :mouse-click)
                            (click-to-soueas event
                                             (fx/sub-ctx snapshot
                                                         state/display-width))))
  (-> snapshot
      (fx/swap-context assoc
                       :region
                       (calc-new-region (fx/sub-val snapshot
                                                    :mouse-click)
                                        (click-to-soueas event
                                                         (fx/sub-ctx snapshot
                                                                     state/display-width))))))
(defn
  worldmap
  "A map of the world
  - filepath to shoreline file
  - filepath to contour file
  - region limits"
  [{:keys [fx/context]}]
  {:fx/type           :v-box
   :max-height        (/ (fx/sub-ctx context
                                     state/display-width)
                         2.0)
   :on-mouse-pressed  {:effect event-worldmap-mouse-press}
   :on-mouse-released {:effect event-worldmap-mouse-release}
   :children          [{:fx/type    svg
                        :svg-hiccup (fx/sub-ctx context
                                                state/world-svg)
                        :scale-x    (-> context
                                        (fx/sub-ctx state/display-width)
                                        (/ 360.0))
                        :scale-y    (-> context
                                        (fx/sub-ctx state/display-width)
                                        (/ 360.0))}]})

(defn
  region
  "Where we select the region we will be looking at
  As well as the contour file to use"
  [{:keys [fx/context]}]
  {:fx/type worldmap}
  {:fx/type   :v-box
   :alignment :top-center
   ;;   :style {:-fx-background-color :purple}
   :children  [{:fx/type     :stack-pane
                :alignment   :center
                :v-box/vgrow :always
                :children    [{:fx/type     svg
                               :v-box/hgrow :always
                               :svg-hiccup  (fx/sub-ctx context
                                                        state/region-svg)
                               :scale-x     (fx/sub-ctx context
                                                        state/region-to-display-scale-x)
                               :scale-y     (fx/sub-ctx context
                                                        state/region-to-display-scale-y)}]}]})

(defn
  datadir-list
  "Lists have to be wrapped in an `extension lifecycle`..
  (I don't understand why)
  see: `cljfx/examples/e27_selection_models.clj`
  for details.."
  [{:keys [fx/context]}]
  (let [select-file-effect {:effect (fn [snapshot
                                         event]
                                      (-> snapshot
                                          (fx/swap-context
                                            assoc
                                            :datafile-idxs
                                            (:fx/event
                                             event))))}]
    {:fx/type fx.ext.list-view/with-selection-props
     :style   {:-fx-background-color :red}
     :props   {:selection-mode              :multiple
               :on-selected-indices-changed select-file-effect}
     :desc    {:fx/type      :list-view
               #_#_
               :cell-factory {:fx/cell-type :list-cell
                              :describe     (fn [path]
                                              {:text path})}
               :items       (->> (fx/sub-ctx
                                   context
                                  state/datafile-strs)
                                 (map-indexed (fn append-index
                                                [index
                                                 file-str]
                                                (str "["
                                                     index
                                                     "]"
                                                     file-str)))
                                 vec)}}))

(defn
  datadir
  "Where we select the data to read in..
  We can inspect how it looks in our region"
  [{:keys [fx/context]}]
  {:fx/type   :v-box
   :alignment :center-left
   ;;   :style {:-fx-background-color :green}
   :children  [{:fx/type :text-field
                :disable true
                ;;                :alignment :center-left
                :text    (fx/sub-ctx
                           context
                           state/data-dirstr)}
               {:fx/type     datadir-list
                :v-box/vgrow :always}]})

(defn
  datapreview
  "Where we select the data to read in..
  We can inspect how it looks in our region
  TODO: revisit why I can't do this with `svg` and need to use `svg2jfx/xml`. With the `svg` element it doesn't update properly in the GUI"
  [{:keys [fx/context]}]
  {:fx/type   :v-box
   :alignment :center
   :children  [{:fx/type svg2jfx/xml
                :svg     (-> (fx/sub-ctx context
                                         state/first-datafile-svg)
                             quickthing/svg2xml)
                :scale-x (fx/sub-ctx context
                                     state/region-to-display-scale-x)
                :scale-y (fx/sub-ctx context
                                     state/region-to-display-scale-y)}]})

(defn
  svlist
  "Lists have to be wrapped in an `extension lifecycle`..
  (I don't understand why)
  see: `cljfx/examples/e27_selection_models.clj`
  for details.."
  [{:keys [fx/context]}]
  (let [select-file-effect {:effect (fn [snapshot
                                         event]
                                      (-> snapshot
                                          (fx/swap-context assoc
                                                           :sv-selected-idxs
                                                           (:fx/event event))))}]
    {:fx/type fx.ext.list-view/with-selection-props
     :props   {:selection-mode              :multiple
               :on-selected-indices-changed select-file-effect}
     :desc    {:fx/type      :list-view
               #_#_
               :cell-factory {:fx/cell-type :list-cell
                              :describe     (fn [path]
                                              {:text path})}
               :items       (->> (fx/sub-ctx context
                                             state/sv-strs))}}))

(defn
  svpreview
  "The display of the selected SV"
  [{:keys [fx/context]}]
  {:fx/type   :v-box
   :alignment :center
   :children  [{:fx/type svg2jfx/xml
                :svg     (-> (fx/sub-ctx context
                                         state/first-sv-selected-svg)
                             quickthing/svg2xml)
                :scale-x (fx/sub-ctx context
                                     state/region-to-display-scale-x)
                :scale-y (fx/sub-ctx context
                                     state/region-to-display-scale-y)}]})

(defn
  sv
  "Diplay of the Singular Vector of index `:sv-num`
  Index is ZERO indexed.. (so PC1 is `{:sv-num 0}`)"
  [{:keys [fx/context
           sv-num]}]
  {:fx/type   :v-box
   :alignment :top-center
   ;;   :style     {:-fx-background-color :blue}
   :children  [#_{:fx/type   :h-box
                  :alignment :top-left
                  :children  [{:fx/type :label
                               :text    " Contour File: "}
                              {:fx/type     :text-field
                               :disable     true
                               :h-box/hgrow :always
                               ;;:pref-width Double/MAX_VALUE
                               :text        (->> context
                                                 state/shoreline-filestr
                                                 (str " "))} ;;..spacer
                              {:fx/type :button
                               :text    "Select"}]}
               {:fx/type     svg
                :v-box/hgrow :always
                :svg-hiccup  (fx/sub-ctx context
                                         state/singular-vector-svg
                                         sv-num)
                :scale-x     (fx/sub-ctx context
                                         state/region-to-display-scale-x)
                :scale-y     (fx/sub-ctx context
                                         state/region-to-display-scale-y)}
               #_{:fx/type     :label
                  :v-box/vgrow :always
                  :text        "Preview Map!!"}]})

(defn
  sv-one
  "Where we select the data to read in..
  We can inspect how it looks in our region"
  [{:keys [fx/context]}]
  {:fx/type   :v-box
   :alignment :top-center
   :children  [{:fx/type     svg
                :v-box/hgrow :always
                :svg-hiccup  (fx/sub-ctx context
                                         state/first-sv-svg)
                :scale-x     (fx/sub-ctx context
                                         state/region-to-display-scale-x)
                :scale-y     (fx/sub-ctx context
                                         state/region-to-display-scale-y)}]})

(defn
  sv-two
  "Where we select the data to read in..
  We can inspect how it looks in our region"
  [{:keys [fx/context]}]
  {:fx/type   :v-box
   :alignment :top-center
   :children  [{:fx/type     svg
                :v-box/hgrow :always
                :svg-hiccup  (fx/sub-ctx context
                                         state/second-sv-svg)
                :scale-x     (fx/sub-ctx context
                                         state/region-to-display-scale-x)
                :scale-y     (fx/sub-ctx context
                                         state/region-to-display-scale-y)}]})

(defn
  sv-mix-one
  "Where we select the data to read in..
  We can inspect how it looks in our region"
  [{:keys [fx/context]}]
  {:fx/type   :v-box
   :alignment :top-center
   :children  [{:fx/type     svg
                :v-box/hgrow :always
                :svg-hiccup  (fx/sub-ctx context
                                         state/top-pattern-svg)
                :scale-x     (fx/sub-ctx context
                                         state/region-to-display-scale-x)
                :scale-y     (fx/sub-ctx context
                                         state/region-to-display-scale-y)}]})

(defn
  sv-mix-two
  "Where we select the data to read in..
  We can inspect how it looks in our region"
  [{:keys [fx/context]}]
  {:fx/type   :v-box
   :alignment :top-center
   :children  [{:fx/type     svg
                :v-box/hgrow :always
                :svg-hiccup  (fx/sub-ctx context
                                         state/bottom-pattern-svg)
                :scale-x     (fx/sub-ctx context
                                         state/region-to-display-scale-x)
                :scale-y     (fx/sub-ctx context
                                         state/region-to-display-scale-y)}]})

(defn
  sv-projections
  "Where we select the data to read in..
  We can inspect how it looks in our region"
  [{:keys [fx/context]}]
  {:fx/type     svg
   :v-box/hgrow :always
   :svg-hiccup  (-> context
                    (fx/sub-ctx state/sv-proj-svg))
   :scale-x     1.0
   :scale-y     1.0})


(defn
  sv-weights
  "Where we select the data to read in..
  We can inspect how it looks in our region"
  [{:keys [fx/context]}]
  {:fx/type     svg
   :v-box/hgrow :always
   :svg-hiccup  (-> context
                    (fx/sub-ctx state/sv-weights-svg))
   :scale-x     1.0
   :scale-y     1.0})

(defn
  climate-index
  [{:keys [fx/context]}]
  {:fx/type     svg
   :v-box/hgrow :always
   :svg-hiccup  (-> context
                    (fx/sub-ctx state/pattern-proj-svg))
   :scale-x     1.0
   :scale-y     1.0})


(defn
  noiselist
  "Lists have to be wrapped in an `extension lifecycle`..
  (I don't understand why)
  see: `cljfx/examples/e27_selection_models.clj`
  for details.."
  [{:keys [fx/context]}]
  (let [select-file-effect {:effect (fn [snapshot
                                         event]
                                      (-> snapshot
                                          (fx/swap-context assoc
                                                           :noise-selected-idxs
                                                           (:fx/event event))))}]
    {:fx/type fx.ext.list-view/with-selection-props
     :props   {:selection-mode              :multiple
               :on-selected-indices-changed select-file-effect}
     :desc    {:fx/type      :list-view
               :items       (->> (fx/sub-ctx context
                                             state/datafile-strs))}}))

(defn
  normalizednoisepreview
  "The display of the selected SV"
  [{:keys [fx/context]}]
  {:fx/type   :v-box
   :alignment :center
   :children  [{:fx/type svg2jfx/xml
                :svg     (-> (fx/sub-ctx context
                                         state/first-normalized-noise-selected-svg)
                             quickthing/svg2xml)
                :scale-x (fx/sub-ctx context
                                     state/region-to-display-scale-x)
                :scale-y (fx/sub-ctx context
                                     state/region-to-display-scale-y)}]})

(defn
  normalizednoiselist
  "Lists have to be wrapped in an `extension lifecycle`..
  (I don't understand why)
  see: `cljfx/examples/e27_selection_models.clj`
  for details.."
  [{:keys [fx/context]}]
  (let [select-file-effect {:effect (fn [snapshot
                                         event]
                                      (-> snapshot
                                          (fx/swap-context assoc
                                                           :normalized-noise-selected-idxs
                                                           (:fx/event event))))}]
    {:fx/type fx.ext.list-view/with-selection-props
     :props   {:selection-mode              :multiple
               :on-selected-indices-changed select-file-effect}
     :desc    {:fx/type      :list-view
               :items       (->> (fx/sub-ctx context
                                             state/datafile-strs))}}))

(defn
  noisepreview
  "The display of the selected SV"
  [{:keys [fx/context]}]
  {:fx/type   :v-box
   :alignment :center
   :children  [{:fx/type svg2jfx/xml
                :svg     (-> (fx/sub-ctx context
                                         state/first-noise-selected-svg)
                             quickthing/svg2xml)
                :scale-x (fx/sub-ctx context
                                     state/region-to-display-scale-x)
                :scale-y (fx/sub-ctx context
                                     state/region-to-display-scale-y)}]})

(defn
  main-vertical-display
  "Tha main vertical window"
  [{:keys [fx/context]}]
  {:fx/type  :v-box
   :children [{:fx/type               worldmap
               #_#_#_#_#_#_           :grid-pane/row 0
               :grid-pane/column      0
               :grid-pane/column-span 2}
              {:fx/type            :grid-pane
               :column-constraints [{:fx/type       :column-constraints
                                     :percent-width 100/2}
                                    {:fx/type       :column-constraints
                                     :percent-width 100/2}]
               :row-constraints    (repeat
                                     2 ;; number of rows.. hardcoded :(
                                     {:fx/type    :row-constraints
                                      :max-height (fx/sub-ctx
                                                    context
                                                    state/row-height)})
               :children           [{:fx/type          datadir
                                     :grid-pane/row    1
                                     :grid-pane/column 0}
                                    {:fx/type          datapreview
                                     :grid-pane/row    1
                                     :grid-pane/column 1}
                                    {:fx/type          sv
                                     :sv-num           0
                                     :grid-pane/row    2
                                     :grid-pane/column 0}
                                    {:fx/type          sv
                                     :sv-num           1
                                     :grid-pane/row    2
                                     :grid-pane/column 1}
                                    {:fx/type          svlist
                                     :grid-pane/row    3
                                     :grid-pane/column 0}
                                    {:fx/type          svpreview
                                     :grid-pane/row    3
                                     :grid-pane/column 1}
                                    {:fx/type               sv-weights
                                     :grid-pane/row         4
                                     :grid-pane/row-span    1
                                     :grid-pane/column      0
                                     :grid-pane/column-span 2}
                                    {:fx/type               sv-projections
                                     :grid-pane/row         5
                                     :grid-pane/row-span    2
                                     :grid-pane/column      0
                                     :grid-pane/column-span 2}
                                    {:fx/type          sv-mix-one
                                     :grid-pane/row    7
                                     :grid-pane/column 0}
                                    {:fx/type          sv-mix-two
                                     :grid-pane/row    7
                                     :grid-pane/column 1}
                                    {:fx/type               climate-index
                                     :grid-pane/row         8
                                     :grid-pane/row-span    1
                                     :grid-pane/column      0
                                     :grid-pane/column-span 2}
                                    {:fx/type          noiselist
                                     :grid-pane/row    9
                                     :grid-pane/column 0}
                                    {:fx/type          noisepreview
                                     :grid-pane/row    9
                                     :grid-pane/column 1}
                                    {:fx/type          normalizednoiselist
                                     :grid-pane/row    10
                                     :grid-pane/column 0}
                                    {:fx/type          normalizednoisepreview
                                     :grid-pane/row    10
                                     :grid-pane/column 1}
                                    #_#_
                                    {:fx/type          svg
                                     :grid-pane/row    9
                                     :grid-pane/column 0
                                     :v-box/hgrow      :always
                                     :svg-hiccup       (fx/sub-ctx context
                                                                   state/noise-svg
                                                                   3)
                                     :scale-x          (fx/sub-ctx context
                                                                   state/region-to-display-scale-x)
                                     :scale-y          (fx/sub-ctx context
                                                                   state/region-to-display-scale-y)}
                                    {:fx/type          svg
                                     :grid-pane/row    9
                                     :grid-pane/column 1
                                     :v-box/hgrow      :always
                                     :svg-hiccup       (fx/sub-ctx context
                                                                   state/noise-svg
                                                                   8)
                                     :scale-x          (fx/sub-ctx context
                                                                   state/region-to-display-scale-x)
                                     :scale-y          (fx/sub-ctx context
                                                                   state/region-to-display-scale-y)}]}]})


(defn
  effect-window-width
  "Effect that updates the current window width state"
  [snapshot
   event]
  (-> snapshot
      (fx/swap-context assoc
                       :window-width
                       (:fx/event event))))

(defn
  root
  "The start of the GUI tree
  (using the `cljfx` naming convention)"
  [{:keys [fx/context]}]
  {:fx/type :stage
   :showing true
   :title   "Imergination"
   :scene   {:fx/type          :scene
             :on-width-changed {:effect effect-window-width}
             :root             {:fx/type      :scroll-pane
                                :fit-to-width true ;; otherwise you get horizontal scrollbars
                                :content      {:fx/type main-vertical-display}}}})

#_
(core/renderer)
