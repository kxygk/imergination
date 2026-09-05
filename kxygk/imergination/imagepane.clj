(ns kxygk.imergination.imagepane
  "This was made in coordination with Qwen.
  Though i reviewed all the code carefully and tweaked it a bit.
  ..
  Super complicated wrapper for Images.
  - Given a width preserved height
  - Allows the Image to fit-to-width of a Parent
  - It allows them to resize to parent nodes dynamically
  - Cleans up all the machinery when the GUI node is removed"
  (:require [cljfx.api       :as fx]
            [cljfx.ext.list-view :as fx.ext.list-view]
            [kxygk.imergination.svg2jfx :as svg2jfx]
            quickthing))

;; Debug flag
(defonce debug-svg-aspect?
  (atom false))

;; Useful for diagnosing leaks:
;; Check with: @svg-aspect-stats
(defonce svg-aspect-stats
  (atom {:created 0
         :cleaned 0}))

(defn- svg-aspect-dbg
  "Print DBG text,
  when `debug-svg-aspect?` is set to `true`"
  [& args]
  (when @debug-svg-aspect?
    (apply println
           "SVG-ASPECT:"
           args)))

(defn- image-ratio
  "Get an `Image` height/width ratio"
  [^javafx.scene.image.Image img]
  (if (and img
           (> (.getWidth img)
              0.0))
    (double (/ (.getHeight img)
               (.getWidth img)))
    0.0))

(defn- request-layout-parent!
  [^javafx.scene.Node node]
  (when node
    (.requestLayout node)
    (when-let [p (.getParent node)]
      (.requestLayout p))))

(defn- region-ancestor-width
  ^double
  [^javafx.scene.Node node]
  (loop [current (.getParent node)
         depth   0]
    (if (nil? current)
      (do (svg-aspect-dbg "ancestor-width: hit nil at depth" depth)
          0.0)
      (if (instance? javafx.scene.layout.Region current)
        (let [w (.getWidth ^javafx.scene.layout.Region current)]
          (svg-aspect-dbg "ancestor-width: depth" depth
                          "class" (.getSimpleName (class current))
                          "width" w)
          (if (> w 0.0)
            w
            (recur (.getParent current) (inc depth))))
        (do (svg-aspect-dbg "ancestor-width: depth" depth
                            "class" (.getSimpleName (class current))
                            "not a Region, skipping")
            (recur (.getParent current) (inc depth)))))))

(defn- effective-width-info
  "Checks a proposed width,
  and returns a [value, key] pair.
  The key tells you which condition hit..
  CASE 1: `proposed` > `0.0`, returns `proposed`
  CASE 2: Else returns current `node` width
  CASE 3: Else (if `0.0`) parent width (zero or otherwise)"
  [^javafx.scene.layout.Region node
   ^double proposed]
  (cond (> proposed
           0.0)
        [proposed
         :proposed]
        (> (.getWidth node)
           0.0)
        [(.getWidth node)
         :current-width]
        :else
        (let [pw (region-ancestor-width node)]
          (if (> pw
                 0.0)
            [pw
             :parent-width]
            [0.0
             :parent-width-zero]))))

(defn- close-double?
  "Checks if two values are more or less equal..
  with a `tolerance`"
  [^double a
   ^double b
   ^double tolerance]
  (< (Math/abs (- a
                  b))
     tolerance))

(defn- build-imgview-pane
  [img-view]
  (proxy [javafx.scene.layout.Pane]
      []
    (^double computePrefWidth [^double height]
     (let [[w
            source] (effective-width-info this
                                          height)]
       (svg-aspect-dbg "computePrefWidth"
                       "proposed height:"
                       height
                       "using width:"
                       w
                       "source:"
                       source)
       (double w)))
    (^double computePrefHeight [^double width]
     (let [[w
            source] (effective-width-info this
                                          width)
           img      (.getImage img-view)
           r        (image-ratio img)
           h        (double (* w
                               r))]
       (svg-aspect-dbg "computePrefHeight"
                       "proposed width:"
                       width
                       "using width:"
                       w
                       "source:"
                       source
                       "image:"
                       (when img
                         [(.getWidth img)
                          (.getHeight img)])
                       "ratio:"
                       r
                       "-> height:"
                       h)
       h))
    (^void layoutChildren []
     (let [w   (.getWidth ^javafx.scene.layout.Region this)
           h   (.getHeight ^javafx.scene.layout.Region this)
           img (.getImage img-view)
           r   (image-ratio img)]
       (when (and (> w
                     0.0)
                  (> r
                     0.0))
         (let [expected-h (double (* w
                                     r))]
           (svg-aspect-dbg "layoutChildren"
                           "w:" w
                           "h:" h
                           "expected-h:" expected-h)
           (.setFitWidth img-view
                         w)
           (.setFitHeight img-view
                          expected-h)
           (.relocate img-view
                      0.0
                      0.0)))))))

(defn- wrap-image-with-pane
  "A `Pane` that wraps an `ImageView` tha wraps an `image`
  This Pane resizes according to its parents"
  [^javafx.scene.image.Image image]
  ;; debugging atoms
  (swap! svg-aspect-stats
         update
         :created inc)
  (svg-aspect-dbg "creating aspect pane. stats:"
                  @svg-aspect-stats
                  "image size:"
                  (.getWidth image)
                  "x"
                  (.getHeight image)
                  "ratio:"
                  (image-ratio image))
  (let [^javafx.scene.image.ImageView img-view (doto (javafx.scene.image.ImageView. image)
                                                 (.setPreserveRatio true) ;; don't stretch image
                                                 (.setSmooth true)
                                                 (.setManaged false)) ;; don't have JavaFX look at children for size
        ^javafx.scene.layout.Pane pane         (build-imgview-pane img-view)]
    (.add (.getChildren pane)
          img-view)
    (let [cleaned?        (atom false) ;; tracks if Pane has been removed by the UI system
          ;; listeners need to be saved so they can be explicitely removed on cleanup
          width-listener  (atom nil)
          height-listener (atom nil)
          image-listener  (atom nil)
          parent-listener (atom nil)
          #_#_ ;; ideally.. would be added back to clear memory faster
          scene-listener  (atom nil)
          ;; Changes the width (and by extension height) of the embedded Image/ImageView
          force-width!    (fn [source
                               ^double w]
                            (when-not @cleaned?
                              (let [img (.getImage img-view)
                                    r   (image-ratio img)]
                                (when (and (> w
                                              0.0)
                                           (> r
                                              0.0))
                                  (let [h (double (* w
                                                     r))]
                                    (svg-aspect-dbg "force-width!"
                                                    source
                                                    "w:"
                                                    w
                                                    "h:"
                                                    h)
                                    (.setFitWidth img-view
                                                  w)
                                    (.setFitHeight img-view
                                                   h)
                                    (.relocate img-view
                                               0.0
                                               0.0)
                                    (when (or (< (.getPrefHeight pane)
                                                 0.0)
                                              (not (close-double? (.getPrefHeight pane)
                                                                  h
                                                                  0.5)))
                                      (.setPrefHeight pane
                                                      h)
                                      (.setMinHeight pane
                                                     h)
                                      (.setMaxHeight pane
                                                     h)
                                      (request-layout-parent! pane)))))))
          ;; Unhooks the Pane and Image so that things can be garbage collected
          cleanup!        (fn []
                            (when (compare-and-set! cleaned?
                                                    false
                                                    true)
                              ;; debug tracking
                              (swap! svg-aspect-stats
                                     update
                                     :cleaned
                                     inc)
                              (svg-aspect-dbg "cleanup! stats:"
                                              @svg-aspect-stats)
                              ;; Remove listeners. Allows for garbage collection
                              (when-let [l @width-listener]
                                (.removeListener (.widthProperty pane)
                                                 l))
                              (when-let [l @height-listener]
                                (.removeListener (.heightProperty pane)
                                                 l))
                              (when-let [l @image-listener]
                                (.removeListener (.imageProperty img-view)
                                                 l))
                              (when-let [l @parent-listener]
                                (.removeListener (.parentProperty pane)
                                                 l))
                              #_
                              (when-let [l @scene-listener]
                                (.removeListener (.sceneProperty pane) l))
                              ;; Release the expensive image reference.
                              (.setImage img-view
                                         nil)
                              ;; Drop child references too.
                              (.clear (.getChildren pane))))]
      ;; Create listeners that change width and height of Image/ImageView when Pane changes.
      ;; These are saved in atoms so they can be cleaned up
      (reset! width-listener
              (reify javafx.beans.value.ChangeListener
                (^void changed [_
                                ^javafx.beans.value.ObservableValue obs
                                ^Object old-val
                                ^Object new-val]
                 (let [w (double new-val)]
                   (svg-aspect-dbg "widthProperty changed:"
                                   w)
                   (when (and (> w
                                 0.0)
                              (not @cleaned?))
                     (force-width! "widthProperty"
                                   w))))))
      (reset! height-listener
              (reify javafx.beans.value.ChangeListener
                (^void changed [_
                                ^javafx.beans.value.ObservableValue obs
                                ^Object old-val
                                ^Object new-val]
                 (let [w   (.getWidth pane)
                       h   (double new-val)
                       img (.getImage img-view)
                       r   (image-ratio img)]
                   (svg-aspect-dbg "heightProperty changed:"
                                   h)
                   (when (and (> w
                                 0.0)
                              (> r
                                 0.0)
                              (not @cleaned?))
                     (let [expected-h (double (* w
                                                 r))]
                       ;; Do not blindly trust the actual height.
                       ;; It may be stale during a resize transition.
                       (.setFitWidth img-view w)
                       (.setFitHeight img-view expected-h)
                       (.relocate img-view 0.0 0.0)))))))
      (reset! image-listener
              (reify javafx.beans.value.ChangeListener
                (^void changed [_
                                ^javafx.beans.value.ObservableValue obs
                                ^Object old-val
                                ^Object new-val]
                 (svg-aspect-dbg "imageProperty changed")
                 (when-not @cleaned?
                   (.requestLayout pane)
                   #_
                   (request-layout-chain! pane)
                   (let [w (cond (> (.getWidth pane)
                                    0.0)
                                 (.getWidth pane)
                                 :else
                                 (region-ancestor-width pane))]
                     (when (> w
                              0.0)
                       (force-width! "imageProperty"
                                     w)))))))
      (reset! parent-listener
              (reify javafx.beans.value.ChangeListener
                (^void changed [_
                                ^javafx.beans.value.ObservableValue obs
                                ^Object old-val
                                ^Object new-val]
                 (svg-aspect-dbg "parentProperty changed:"
                                 "old:"
                                 (class old-val)
                                 "new:"
                                 (class new-val))
                 ;; Node added somewhere.
                 (when (and new-val
                            (not @cleaned?))
                   (.requestLayout pane)
                   #_
                   (request-layout-chain! pane)
                   (let [pw (region-ancestor-width pane)]
                     (when (> pw
                              0.0)
                       (force-width! "parentProperty"
                                     pw))))
                 ;; Node removed.
                 ;; Delay slightly to avoid cleaning up during a temporary remove/add.
                 (when (and old-val
                            (nil? new-val)
                            (not @cleaned?))
                   (javafx.application.Platform/runLater
                     (reify Runnable
                       (^void run [_]
                        (when (and (nil? (.getParent pane))
                                   (nil? (.getScene pane))
                                   (not @cleaned?))
                          (cleanup!)))))))))
      (.addListener (.widthProperty pane)
                    @width-listener)
      (.addListener (.heightProperty pane)
                    @height-listener)
      (.addListener (.imageProperty img-view)
                    @image-listener)
      (.addListener (.parentProperty pane)
                    @parent-listener))
    (doto pane
      (.setMinWidth 0.0)
      (.setMinHeight 0.0)
      (.setMaxWidth 100000.0)
      (.setMaxHeight javafx.scene.layout.Region/USE_PREF_SIZE))))

(defn svg [{:keys [svg]}]
  (let [fx-node (-> svg
                    quickthing/svg2xml
                    svg2jfx/jsvg-fxcanvas)]
    {:fx/type fx/ext-instance-factory
     :create  (fn [] fx-node)}))

(defn imagebuf [{:keys [imagebuf]}]
  {:fx/type fx/ext-recreate-on-key-changed
   :key     imagebuf
   :desc    {:fx/type fx/ext-instance-factory
             :create  (fn []
                        (wrap-image-with-pane imagebuf))}})
