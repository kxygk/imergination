(ns
    gui
  "Imergination GUI tree"
  (:require [clojure.java.io :as io]
            [cljfx.api       :as fx]
            [cljfx.ext.list-view :as fx.ext.list-view]
            quickthing
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
  "The actual workmap svg (as a JFX component"
  [{:keys [fx/context
           svg-group]}]
  {:fx/type fx/ext-instance-factory
   :create  (fn []
              svg-group)})

(defn
  worldmap
  "A map of the world
  - filepath to shoreline file
  - filepath to contour file
  - region limits"
  [{:keys [fx/context]}]
  {:fx/type    :v-box
   :max-height (/
                 (fx/sub-ctx
                   context
                   state/window-width)
                 2.0)
   ;;   :alignment :top-center
   ;;   :style {:-fx-background-color :red}
   :children   [#_{:fx/type   :h-box
                   :alignment :center
                   :children  [{:fx/type :label
                                :text    " Contour File: "}
                               {:fx/type     :text-field
                                :disable     true
                                :h-box/hgrow :always
                                ;;:pref-width Double/MAX_VALUE
                                :text        (str
                                               " "
                                               (state/world-shoreline-filestr
                                                 context))}
                               {:fx/type :button
                                :text    "Select"}]}       
                {:fx/type     :stack-pane
                 :pref-height (/
                                (fx/sub-ctx
                                  context
                                  state/window-width)
                                2.0)
                 :alignment   :center
                 :v-box/vgrow :always
                 :children    [{:fx/type   svg
                                :svg-group (fx/sub-ctx
                                             context
                                             state/world-batik-fullwidth)}
                               #_
                               {:fx/type svg2jfx/xml ;;2img
                                :scale-x (/
                                           (fx/sub-ctx
                                             context
                                             state/window-width)
                                           360.0)
                                :scale-y (/
                                           (fx/sub-ctx
                                             context
                                             state/window-width)
                                           360.0)
                                :svg
                                #_       (slurp "out-backup/summer.svg")
                                #_       (slurp "summer-notext.svg")
                                (->
                                  locations/world-region
                                  (plot/shoreline-map
                                    [#_locations/krabi])
                                  quickthing/serialize)}]}
                #_{:fx/type :label
                   :text    "World Map!"}]})

#_#_world-map-fn (svg2jfx/xml2img
                   (quickthing/serialize-with-line-breaks
                     (plot/shoreline-map
                       locations/world-region
                       [locations/krabi])))
#_       (slurp "first-year.svg")
#_       (slurp "out/summer.svg")
#_       (slurp "summer-notext.svg")
#_       (slurp "world-shorelines.svg")

#_
{:fx/type     fx/ext-instance-factory
 :style       {:-fx-background-color :red}
 :v-box/vgrow :always
 :create      world-map-fn}


(defn
  region
  "Where we select the region we will be looking at
  As well as the contour file to use"
  [{:keys [fx/context]}]
  {:fx/type worldmap}
  {:fx/type   :v-box
   :alignment :top-center
   ;;   :style {:-fx-background-color :purple}
   :children  [{:fx/type   :h-box
                :alignment :top-left
                :children  [{:fx/type :label
                             :text    " Contour File: "}
                            {:fx/type     :text-field
                             :disable     true
                             :h-box/hgrow :always
                             ;;:pref-width Double/MAX_VALUE
                             :text        (str
                                            " "
                                            (state/world-shoreline-filestr
                                              context))}
                            {:fx/type :button
                             :text    "Select"}]}       
               {:fx/type     :stack-pane
                :alignment   :center
                :v-box/vgrow :always
                :children    [{:fx/type svg2jfx/xml
                               :scale-x (fx/sub-ctx
                                          context
                                          state/region-to-display-scale-x)
                               :scale-y (fx/sub-ctx
                                          context
                                          state/region-to-display-scale-y)
                               :svg
                               #_
                               (slurp "summer.svg")
                               #_
                               (slurp "small-map-notext.svg")
                               #_
                               (slurp "small-map.svg")
                               #_
                               (slurp "circle-big.svg")
                               #_
                               (slurp "summer-notext.svg")
                               (->
                                 (fx/sub-ctx
                                   context
                                   state/region)
                                 (plot/shoreline-map
                                   [#_locations/krabi])
                                 quickthing/serialize)}]}
               {:fx/type     :label
                :v-box/vgrow :always
                :text        "Region Map!!"}]})



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
               :on-selected-indices-changed select-file-effect }
     :desc    {:fx/type      :list-view
               #_#_
               :cell-factory {:fx/cell-type :list-cell
                              :describe     (fn [path]
                                              {:text path})}
               :items        (fx/sub-ctx
                               context
                               state/datafile-strs)}}))

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
  We can inspect how it looks in our region"
  [{:keys [fx/context]}]
  {:fx/type   :v-box
   :alignment :top-center
   ;;   :style     {:-fx-background-color :blue}
   :children  [{:fx/type   :h-box
                :alignment :top-left
                :children  [{:fx/type :label
                             :text    " Contour File: "}
                            {:fx/type     :text-field
                             :disable     true
                             :h-box/hgrow :always
                             ;;:pref-width Double/MAX_VALUE
                             :text        (str
                                            " "
                                            (state/world-shoreline-filestr
                                              context))}
                            {:fx/type :button
                             :text    "Select"}]}       
               {:fx/type     :stack-pane
                :alignment   :center
                :v-box/vgrow :always
                :children    [{:fx/type   svg
                               :svg-group (fx/sub-ctx
                                            context
                                            state/datapreview-batik-halfwidth)}]}
               {:fx/type     :label
                :v-box/vgrow :always
                :text        "Preview Map!!"}]})

(defn
  root
  "The start of the GUI tree
  (using the `cljfx` naming convention)"
  [{:keys [fx/context]}]
  {:fx/type :stage
   :showing true
   :title   "Imergination"
   :scene   {:fx/type          :scene
             :on-width-changed {:effect (fn [snapshot
                                             event]
                                          (-> snapshot
                                              (fx/swap-context
                                                assoc
                                                :window-width
                                                (:fx/event
                                                 event))))}
             :root             #_ {:fx/type :label
                                   :text    "Hello World?"}
             
             {:fx/type      :scroll-pane
              :fit-to-width true
              :content      {:fx/type  :v-box
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
                                                               2 ;; number of rows of stuff going on.. hardcoded :(
                                                               {:fx/type    :row-constraints
                                                                :max-height (fx/sub-ctx
                                                                              context
                                                                              state/row-height)})
                                         
                                         :children [{:fx/type          datadir
                                                     :grid-pane/row    1
                                                     :grid-pane/column 0}
                                                    {:fx/type          datapreview
                                                     :grid-pane/row    1
                                                     :grid-pane/column 1}
                                                    #_#_#_#_
                                                    {:fx/type          datapreview
                                                     :grid-pane/row    2
                                                     :grid-pane/column 0}
                                                    {:fx/type          datapreview
                                                     :grid-pane/row    2
                                                     :grid-pane/column 1}
                                                    {:fx/type          datapreview
                                                     :grid-pane/row    3
                                                     :grid-pane/column 0}
                                                    {:fx/type          datapreview
                                                     :grid-pane/row    3
                                                     :grid-pane/column 1}]}]}}}})

#_
(core/renderer)
