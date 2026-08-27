(ns
    kxygk.imergination.guiom
  "Imergination GUI tree"
  (:require [clojure.java.io :as io]
            [cljfx.api       :as fx]
            [cljfx.ext.list-view :as fx.ext.list-view]
            [com.wsscode.pathom3.connect.built-in.resolvers :as pbir]
            [com.wsscode.pathom3.interface.smart-map :as psm]
            [com.wsscode.pathom3.connect.runner :as pcr]
            [com.wsscode.pathom3.connect.operation :as pco]
            [com.wsscode.pathom3.interface.async.eql :as p.a.eql]
            [com.wsscode.pathom3.interface.eql :as p.eql]
            [com.wsscode.pathom3.connect.planner :as pcp]
            [com.wsscode.pathom3.connect.indexes :as pci]
            [quickthing]
            [geoprim]
            kxygk.pathmore.async
            [kxygk.imergination.svg2jfx :as svg2jfx]
            [kxygk.pathomfx.pathprom :as pathprom]
            [kxygk.imergination.stateom :as stateom]
            [kxygk.imergination.imagepane :as imagepane])
  (:gen-class :main true))`



(set!
  *warn-on-reflection*
  true)

;; This is the "Default" view while things are loading
;; The atom is reused around the UI.
;; The atom is ppdated in the root
(defonce contour-imagebuf-atom
  (atom nil))

(defn contour-ui
  [{:keys [value]}]
  (if value
    {:fx/type  imagepane/imagebuf
     :imagebuf value}
    {:fx/type :stack-pane
     :style   {:-fx-background-color "#eee7e9"}}))

(defn contour-loading-ui
  [_]
  {:fx/type fx/ext-watcher
   :ref     contour-imagebuf-atom
   :desc    {:fx/type contour-ui}})
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def pathom-env
  (-> (pci/register {::p.a.eql/parallel? true}
                    [stateom/env
                     (pbir/single-attr-resolver :hiccup
                                                :imagebuf
                                                ;; Note: No caching b/c imagebuf is huge
                                                ;; And `pathprom` caches this part
                                                (fn render-svg-hiccup
                                                  [svg-hiccup]
                                                  (-> svg-hiccup
                                                      quickthing/svg2xml
                                                      svg2jfx/jsvg-jxfimg)))])
      (pcp/with-plan-cache stateom/pathom-plan-cache*)
      kxygk.pathmore.async/wrap-all-resolvers-async))

#_
@(p.a.eql/process pathom-env
                  @stateom/*selections
                  [{:contour-svg [:imagebuf]}])


#_
(time
  (p.a.eql/process pathom-env
                   @stateom/*selections
                   [{:first-datafile-svg [:imagebuf]}]))

(def map-clicks
  (atom {}))

(defn region-rectangle
  [{:keys [value]}]
  (let [x1 (:start-x value)
        y1 (:start-y value)
        x2 (:ended-x value)
        y2 (:ended-y value)]
    (if (and x1
             x2
             y1
             y2)
      {:fx/type           :rectangle
       :x                 (min x1 x2)
       :y                 (min y1 y2)
       :width             (abs (- x2 x1))
       :height            (abs (- y2 y1))
       :fill              :transparent
       :stroke            :red
       :stroke-width      2
       :mouse-transparent true}
      {:fx/type           :rectangle
       :x                 0
       :y                 0
       :width             0
       :height            0
       :fill              :transparent
       :stroke            :red
       :stroke-width      2
       :mouse-transparent true})))

(defn clamp
  "clamp a number between `minimum` and `maximum`"
  [number
   minimum
   maximum]
  (max minimum
       (min maximum
            number)))

(defn
  worldmap
  "A map of the world
  - filepath to shoreline file
  - filepath to contour file
  - region limits"
  [{:keys [state]}]
  {:fx/type          :stack-pane
   :alignment        :top-left
   #_#_
   :fill-width       true
   :style            {:-fx-background-color :blue}
   :on-mouse-pressed (fn event-worldmap-mouse-press
                       [event]
                       (let [pick-result (.getPickResult event)
                             img-view    (.getIntersectedNode pick-result)]
                         (when (instance? javafx.scene.image.ImageView
                                          img-view)
                           (let [point        (.getIntersectedPoint pick-result)
                                 click-x      (.getX point)
                                 click-y      (.getY point)
                                 image-width  (.getFitWidth img-view)
                                 image-height (.getFitHeight img-view)]
                             (when (instance? javafx.scene.image.ImageView
                                              img-view)
                               (swap! map-clicks
                                      #(merge %
                                              {:start-x   click-x
                                               :start-y   click-y
                                               :ended-x   nil
                                               :ended-y   nil
                                               :eas-first (* 360
                                                             (/ click-x
                                                                image-width))
                                               :sou-first (* 180
                                                             (/ click-y
                                                                image-height))})))))))
   :on-mouse-dragged (fn event-worldmap-mouse-dragged
                       [event]
                       (let [pick-result (.getPickResult event)
                             img-view    (.getIntersectedNode pick-result)]
                         (if (or (nil? img-view)
                                 (not (instance? javafx.scene.image.ImageView
                                                 img-view))) ;; left the ImageView
                           (reset! map-clicks
                                   {})
                           (let [point        (.getIntersectedPoint pick-result)
                                 click-x      (.getX point)
                                 click-y      (.getY point)
                                 image-width  (.getFitWidth img-view)
                                 image-height (.getFitHeight img-view)]
                             (if (:start-x @map-clicks)
                               (do (swap! map-clicks
                                          #(merge %
                                                  {:ended-x    click-x
                                                   :ended-y    click-y
                                                   :eas-second (* 360
                                                                  (/ click-x
                                                                     image-width))
                                                   :sou-second (* 180
                                                                  (/ click-y
                                                                     image-height))}))))))))

   :on-mouse-released (fn event-worldmap-mouse-released
                        [event]
                        (let [{:keys [eas-first
                                      eas-second
                                      sou-first
                                      sou-second]} @map-clicks]
                          (when (and eas-first
                                     eas-second
                                     sou-first
                                     sou-second
                                     (not= eas-first
                                           eas-second)
                                     (not= sou-first
                                           sou-second))
                            ( swap! stateom/*selections
                             assoc
                             :region
                             (geoprim/region (geoprim/point-eassou (clamp (min eas-first
                                                                               eas-second)
                                                                          0
                                                                          360)
                                                                   (clamp (min sou-first
                                                                               sou-second)
                                                                          0
                                                                          180))
                                             (geoprim/point-eassou (clamp (max eas-first
                                                                               eas-second)
                                                                          0
                                                                          360)
                                                                   (clamp (max sou-first
                                                                               sou-second)
                                                                          0
                                                                          180))))))
                        (reset! map-clicks
                                {}))

   :on-mouse-exited (fn [_]
                      (reset! map-clicks
                              {}))
   :children        [{:fx/type        pathprom/later
                      :env            pathom-env
                      :inputmap       state
                      :tx             [{:world-with-region-highlight-svg [:imagebuf]}]
                      :loading-ui     {:fx/type fx/ext-get-ref
                                       :ref     ::world}
                      :realized-ui-fn (fn [pathom-map]
                                        {:fx/type  imagepane/imagebuf
                                         :imagebuf (-> pathom-map
                                                       :world-with-region-highlight-svg
                                                       :imagebuf)})}
                     {:fx/type           :pane
                      :mouse-transparent true
                      :children          [{:fx/type fx/ext-watcher
                                           :ref     map-clicks
                                           :desc    {:fx/type region-rectangle}}]}]})
#_
@(p.a.eql/process pathom-env
                  @stateom/*selections
                  [{:world-svg [:imagebuf]}])



(defn
  datadirlist
  "Lists have to be wrapped in an `extension lifecycle`..
  (I don't understand why)
  see: `cljfx/examples/e27_selection_models.clj`
  for details.."
  [{:keys [state]}]
  {:fx/type        pathprom/later
   :env            pathom-env
   :inputmap       state
   :tx             [:datafile-strs-formatted]
   :loading-ui     {:fx/type     :list-view
                    :min-height  0
                    :pref-height 0
                    :items       ["Loading..."]}
   :realized-ui-fn (fn [pathom-map]
                     {:fx/type fx.ext.list-view/with-selection-props
                      :style   {:-fx-background-color :red}
                      :props   {:selection-mode              :multiple
                                :on-selected-indices-changed (fn update-datafile-selections
                                                               [selected-indices]
                                                               (swap! stateom/*selections
                                                                      assoc
                                                                      :datafile-idxs
                                                                      selected-indices))}
                      :desc    {:fx/type     :list-view
                                :min-height  0
                                :pref-height 0
                                :items       (:datafile-strs-formatted pathom-map)}})})

(defn
  datadir
  "Where we select the data to read in..
  We can inspect how it looks in our region"
  [{:keys [state]}]
  {:fx/type    :v-box
   :style      {:-fx-background-color :green}
   :min-height 0
   :children   [{:fx/type        pathprom/later
                 :env            pathom-env
                 :inputmap       state
                 :tx             [:data-dirstr]
                 :loading-ui     {:fx/type :text-field
                                  :disable true
                                  :text    "Loading..."}
                 :realized-ui-fn (fn [pathom-map]
                                   {:fx/type :text-field
                                    :disable true
                                    :text    (:data-dirstr pathom-map)})}
                {:fx/type     datadirlist
                 :state       state
                 :v-box/vgrow :always
                 :min-height  0}
                ]})
#_
(defn datapreview [{:keys [state]}]
  {:fx/type  :v-box
   :children [{:fx/type :label :text "dummy"}]})

(defn
  datapreview
  "Where we select the data to read in..
  We can inspect how it looks in our region
  TODO: revisit why I can't do this with `svg` and need to use `svg2jfx/xml`.
  With the `svg` element it doesn't update properly in the GUI"
  [{:keys [state]}]
  {:fx/type    :v-box
   :fill-width true
   :style      {:-fx-background-color :red}
   :children   [{:fx/type        pathprom/later
                 :env            pathom-env
                 :inputmap       state
                 :tx             [{:first-datafile-svg [:imagebuf]}]
                 :loading-ui     {:fx/type contour-loading-ui}
                 :realized-ui-fn (fn [pathom-map]
                                   {:fx/type  imagepane/imagebuf
                                    :imagebuf (-> pathom-map
                                                  :first-datafile-svg
                                                  :imagebuf)})}]})
#_
@(p.a.eql/process pathom-env
                  @stateom/*selections
                  [{:first-datafile-svg [:imagebuf]}])


(defn
  firstsv
  "Where we select the data to read in..
  We can inspect how it looks in our region
  TODO: revisit why I can't do this with `svg` and need to use `svg2jfx/xml`.
  With the `svg` element it doesn't update properly in the GUI"
  [{:keys [state]}]
  {:fx/type    :v-box
   :fill-width true
   :style      {:-fx-background-color :red}
   :children   [{:fx/type        pathprom/later
                 :env            pathom-env
                 :inputmap       state
                 :tx             [{:first-svec-svg [:imagebuf]}]
                 :loading-ui     {:fx/type contour-loading-ui}
                 :realized-ui-fn (fn [pathom-map]
                                   {:fx/type  imagepane/imagebuf
                                    :imagebuf (-> pathom-map
                                                  :first-svec-svg
                                                  :imagebuf)})}]})


(defn
  secondsv
  "Where we select the data to read in..
  We can inspect how it looks in our region
  TODO: revisit why I can't do this with `svg` and need to use `svg2jfx/xml`.
  With the `svg` element it doesn't update properly in the GUI"
  [{:keys [state]}]
  {:fx/type    :v-box
   :fill-width true
   :style      {:-fx-background-color :red}
   :children   [{:fx/type        pathprom/later
                 :env            pathom-env
                 :inputmap       state
                 :tx             [{:second-svec-svg [:imagebuf]}]
                 :loading-ui     {:fx/type contour-loading-ui}
                 :realized-ui-fn (fn [pathom-map]
                                   {:fx/type  imagepane/imagebuf
                                    :imagebuf (-> pathom-map
                                                  :second-svec-svg
                                                  :imagebuf)})}]})

(defn
  svlist
  "Lists have to be wrapped in an `extension lifecycle`..
  (I don't understand why)
  see: `cljfx/examples/e27_selection_models.clj`
  for details.."
  [{:keys [state]}]
  {:fx/type        pathprom/later
   :env            pathom-env
   :inputmap       state
   :tx             [:sv-strs]
   :loading-ui     {:fx/type     :list-view
                    :min-height  0
                    :pref-height 0
                    :items       ["Loading..."]}
   :realized-ui-fn (fn [pathom-map]
                     {:fx/type fx.ext.list-view/with-selection-props
                      :style   {:-fx-background-color :red}
                      :props   {:selection-mode              :multiple
                                :on-selected-indices-changed (fn update-datafile-selections
                                                               [selected-indices]
                                                               (swap! stateom/*selections
                                                                      assoc
                                                                      :sv-selected-idxs
                                                                      selected-indices))}
                      :desc    {:fx/type     :list-view
                                :min-height  0
                                :pref-height 0
                                :items       (:sv-strs pathom-map)}})})

#_
@(p.a.eql/process pathom-env
                  @stateom/*selections
                  [{:contour-svg [:imagebuf]}])


(defn
  svpreview
  "Where we select the data to read in..
  We can inspect how it looks in our region
  TODO: revisit why I can't do this with `svg` and need to use `svg2jfx/xml`.
  With the `svg` element it doesn't update properly in the GUI"
  [{:keys [state]}]
  {:fx/type    :v-box
   :fill-width true
   :style      {:-fx-background-color :red}
   :children   [{:fx/type        pathprom/later
                 :env            pathom-env
                 :inputmap       state
                 :tx             [{:first-svec-selected-svg [:imagebuf]}]
                 :loading-ui     {:fx/type contour-loading-ui}
                 :realized-ui-fn (fn [pathom-map]
                                   {:fx/type  imagepane/imagebuf
                                    :imagebuf (-> pathom-map
                                                  :first-svec-selected-svg
                                                  :imagebuf)})}]})


(defn
  main-vertical-display
  "Tha main vertical window"
  [{:keys [state]}]
  {:fx/type      :scroll-pane
   :fit-to-width true ;; Ensures content stretches to window width
   :hbar-policy  :never
   :vbar-policy  :always
   :content
   {:fx/type    :v-box
    :style      {:-fx-background-color "#eee7e9"}
    :fill-width true
    :children   [#_{:fx/type worldmap
                    :state   state}
                 {:fx/type            :grid-pane
                  #_#_
                  :row-constraints    [{:fx/type :row-constraints :percent-height 50}
                                       {:fx/type :row-constraints :percent-height 50}]
                  :column-constraints [{:fx/type       :column-constraints
                                        :percent-width 100/2}
                                       {:fx/type       :column-constraints
                                        :percent-width 100/2}]
                  :children           [{:fx/type               worldmap
                                        :state                 state
                                        :grid-pane/row         0
                                        :grid-pane/column      0
                                        :grid-pane/column-span 2}
                                       {:fx/type          datadir
                                        :alignment        :top-center
                                        :state            state
                                        :grid-pane/row    1
                                        :grid-pane/column 0}
                                       {:fx/type          datapreview
                                        :state            state
                                        :grid-pane/row    1
                                        :grid-pane/column 1}
                                       {:fx/type          firstsv
                                        :state            state
                                        :grid-pane/row    2
                                        :grid-pane/column 0}
                                       {:fx/type          secondsv
                                        :state            state
                                        :grid-pane/row    2
                                        :grid-pane/column 1}
                                       {:fx/type          svlist
                                        :state            state
                                        :grid-pane/row    3
                                        :grid-pane/column 0}
                                       {:fx/type          svpreview
                                        :state            state
                                        :grid-pane/row    3
                                        :grid-pane/column 1}
                                       ]}]
    }
   })

(defn app-root
  "The absolute root of the Cljfx application.
   This provides the OS Window (Stage) and Canvas (Scene).
  I also calculate some for the reusable static images here"
  [{:keys [value]}]
  {:fx/type :stage
   :title   "Imergination"
   :showing true        ;; <--- THIS IS WHAT MAKES THE WINDOW APPEAR
   :width   800
   :height  600
   :scene   {:fx/type :scene
             :root    {:fx/type fx/ext-let-refs
                       :refs    {::world      {:fx/type        pathprom/now
                                               :env            pathom-env
                                               :inputmap       value
                                               :tx             [{:world-svg [:imagebuf]}]
                                               :realized-ui-fn (fn [pathom-map]
                                                                 {:fx/type  imagepane/imagebuf
                                                                  :imagebuf (-> pathom-map
                                                                                :world-svg
                                                                                :imagebuf)})}
                                 ::loading-ui {:fx/type        pathprom/now
                                               :env            pathom-env
                                               :inputmap       value
                                               :tx             [{:contour-svg [:imagebuf]}]
                                               :realized-ui-fn (fn updating-contour-atom
                                                                 [pathom-map]
                                                                 (reset! contour-imagebuf-atom
                                                                         (-> pathom-map
                                                                             :contour-svg
                                                                             :imagebuf))
                                                                 {:fx/type :stack-pane})}}
                       :desc    {:fx/type main-vertical-display
                                 :state   value}}}})

(defn root-state-watcher
  "This `fx/ext-watcher` is an element that just watches an IRef.
  In this case it's watching our core state.
  This setup obviates the need for a renderer.
  You could have parts of the UI tree have their own states/`fx/ext-watcher`"
  [{:keys [state]}]
  {:fx/type fx/ext-watcher
   :ref     state
   :desc    {:fx/type app-root}})

(def app
  (-> {:fx/type root-state-watcher
       :state   stateom/*selections}
      fx/create-component
      fx/on-fx-thread))

#_
@(com.wsscode.pathom3.interface.async.eql/process stateom/env
                                                  @stateom/*selections
                                                  [:world-svg])

;; (defn -main [& args]
;;   (javafx.application.Platform/setImplicitExit false)
;;   @app)


#_
(let [shoreline (check :shoreline)]
  (time (check {:contour-svg [:imagebuf]}
               (assoc shoreline
                      :region
                      (:region (:java locations/regions))))))
