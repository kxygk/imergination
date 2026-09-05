(ns
    kxygk.imergination.svg2jfx
  "Make a GUI node from an SVG"
  (:require clojure.java.io
            quickthing)
  (:import java.awt.image.BufferedImage
           javafx.stage.Screen
           [javafx.scene CacheHint]
           com.github.weisj.jsvg.parser.SVGLoader
           com.github.weisj.jsvg.renderer.SVGRenderingHints
           com.github.weisj.jsvg.parser.LoaderContext
           com.github.weisj.jsvg.parser.DocumentLimits
           [com.github.weisj.jsvg SVGDocument]
           [com.github.weisj.jsvg.ui.jfx FXSVGCanvas FXSVGCanvas$RenderBackend]))

;; I guess this is a static loader object thing?
(def jsvg-loader
  (SVGLoader.))



(defn target-render-width []
  (long (max 1 (:width @max-render-size))))

(defn
  jsvg-bufimg
  [svg-str]
  (let [svg-doc       (.load jsvg-loader
                             (-> svg-str
                                 .getBytes
                                 clojure.java.io/input-stream)
                             nil
                             (.documentLimits (LoaderContext/builder)
                                              (DocumentLimits. 99
                                                               99
                                                               99999)))
        doc-size      (-> svg-doc
                          .size)
        width         (-> doc-size
                          .width
                          int)
        height        (-> doc-size
                          .height
                          int)
        target-width  (target-render-width)
        zoom          (max 1
                           (int (/ target-width
                                   width)))
        imgbuf        (BufferedImage. (* width
                                         zoom)
                                      (* height
                                         zoom)
                                      java.awt.image.BufferedImage/TYPE_INT_RGB)
        graphics2d    (.createGraphics imgbuf)
        ;; output-file   (File. "test" #_file-str)
        rbspectrum    quickthing/red-blue-colors
        neutral-white (get rbspectrum
                           (/ (count rbspectrum)
                              2))]
    ;; maybe unneeded from the docs https://github.com/weisJ/jsvg
    (.setRenderingHint graphics2d
                       SVGRenderingHints/KEY_SOFT_CLIPPING
                       SVGRenderingHints/VALUE_SOFT_CLIPPING_ON)
    (.setRenderingHint graphics2d
                       SVGRenderingHints/KEY_IMAGE_ANTIALIASING
                       SVGRenderingHints/VALUE_IMAGE_ANTIALIASING_ON)
    (.setRenderingHint graphics2d
                       SVGRenderingHints/KEY_MASK_CLIP_RENDERING
                       SVGRenderingHints/VALUE_MASK_CLIP_RENDERING_ACCURACY)
    (.scale graphics2d
            zoom
            zoom)
    (.setColor graphics2d
               (java.awt.Color. (float (:r neutral-white))
                                (float (:g neutral-white))
                                (float (:b neutral-white))
                                #_Color/WHITE))
    (.fillRect graphics2d
               0
               0
               (* width
                  zoom)
               (* height
                  zoom))
    (.render svg-doc
             nil
             graphics2d)
    (.dispose graphics2d)
    imgbuf))

(defn
  jsvg-jxfimg
  [svg-str]
  (-> svg-str
      jsvg-bufimg
      (javafx.embed.swing.SwingFXUtils/toFXImage
        nil)))

(defn jsvg-fxcanvas
  "Converts an SVG string directly to a JavaFX FXSVGCanvas node.
   Bypasses BufferedImage entirely."
  [svg-str]
  (let [loader (SVGLoader.) ;; Thread-safe: new instance per call
        doc    (.load loader
                      (-> svg-str
                          .getBytes
                          clojure.java.io/input-stream))
        canvas (FXSVGCanvas.)]
    (.setRenderBackend canvas
                       FXSVGCanvas$RenderBackend/JavaFX)
    (.setShowTransparentPattern canvas
                                false)
    (.setDocument canvas
                  doc)
    (.setCache canvas
               true)
    (.setCacheHint canvas
                   CacheHint/SCALE)
    canvas))
