(ns
    svg2jfx
  "Make a GUI node from an SVG"
  (:require [cljfx.api            :as fx]
            [cljfx.composite      :as com]
            [thi.ng.geom.svg.core :as svgthing]
            quickthing)
  (:import com.github.weisj.jsvg.parser.SVGLoader
           java.net.URI
           java.awt.image.BufferedImage
           java.io.File
           javax.imageio.ImageIO
           com.github.weisj.jsvg.parser.LoaderContext
           com.github.weisj.jsvg.parser.DocumentLimits
           java.awt.Color
           [javafx.embed.swing JFXPanel]
           com.kitfox.svg.SVGCache))

#_
(defn
  file
  [^String filestr]
  (->
    (java.io.File. filestr)
    afester.javafx.svg.SVGLoader/load))

(defn
  girod2jfx-fn
  "Given a `svg` and a `width`
  Returns a functor that generates an JFX Group of the SVG"
  [^String svg
   width]
  (fn []
    (let [params (org.girod.javafx.svgimage.LoaderParameters.)]
      #_(set!
          (.
            params
            scale)
          width)
      (let [svgimg (org.girod.javafx.svgimage.SVGLoader/load
                     ^String svg
                     #_params)]
        (if (nil?
              width)
          svgimg
          (org.girod.javafx.svgimage.SVGImage/.scaleTo svgimg
                                                       width))))))

;; ## Converting SVGs to JavaFX objects
;; Takes a string and turns it into an input stream
(defn string->stream
  ([^java.lang.String s] (string->stream s "UTF-8"))
  ([^java.lang.String s
    ^java.lang.String encoding]
   (-> s
       (.getBytes encoding)
       (java.io.ByteArrayInputStream.))))

;; 
(defn svg2batik-fn
  "Use the FranzXaver library to turn an SVG
  into a JavaFX compatible Group Node
  ..  (which shows up as a picture)
  This is using Batik under the hood somehow"
  [^java.lang.String svg-xml-string
   scale-x
   scale-y]
  (fn
    []
    (let [^javafx.scene.Group
          img (.loadSvg
                (afester.javafx.svg.SvgLoader.)
                ^java.io.ByteArrayInputStream
                (string->stream
                  svg-xml-string))]
      (.setScaleX
        img
        scale-x)
      (.setScaleY
        img
        scale-y)
      img)))

(defn
  batik-load
  [svg-string]
  (.loadSvg
    (afester.javafx.svg.SvgLoader.)
    ^java.io.ByteArrayInputStream
    (string->stream
      svg-string)))

(defn
  batik-scale
  [^javafx.scene.Group batik-group
   scale-x
   scale-y]
  (.setScaleX
    batik-group
    scale-x)
  (.setScaleY
    batik-group
    scale-y)
  batik-group)  

(defn
  xml
  "Converts an SVG string to a JavaFX Group-like object
  `:svg` for the svg string
  `:width` for an optional width parameter
  NOTE: The library seems to work weird with widths
  if you specify the display width with `:width`
  then the `<svg>` tag shouldn't specify its own height/width"
  [{:keys [fx/context
           svg
           scale-x
           scale-y]}]
  {:fx/type fx/ext-instance-factory
   :create  (svg2batik-fn
              svg
              scale-x
              scale-y)
   #_       (girod2jfx-fn
              svg
              width)})
#_#_params (LoaderParameters.) ;; seemingly unused


(defn-
  salamander-buffered-image
  "How to turn an SVG XML string to an image
   Uses `svgSalamander`"
  [svg-xml-str
   width
   height]
  (let [universe       (com.kitfox.svg.SVGCache/getSVGUniverse)
        uri            (.loadSVG
                         universe
                         (java.io.StringReader.
                           svg-xml-str)
                         "unused-placeholder")
        diagram        (.getDiagram
                         universe
                         uri)
        buffered-image (java.awt.image.BufferedImage.
                         width
                         height
                         java.awt.image.BufferedImage/TYPE_4BYTE_ABGR)
        graphics-2d    (.createGraphics
                         buffered-image)]
    (.setRenderingHint
      graphics-2d
      java.awt.RenderingHints/KEY_ANTIALIASING
      java.awt.RenderingHints/VALUE_ANTIALIAS_ON)
    (.render
      diagram
      graphics-2d)
    (.removeDocument
      universe
      uri)
    buffered-image))

#_
(javax.imageio.ImageIO/write
  (salamander-buffered-image
    (slurp
      "small-map-notext.svg")
    100
    100)
  "jpg"
  (java.io.File.
    "buff.jpg"))

(defn-
  salamander2jfx-image
  "How to turn an SVG XML string to an image
   Uses `svgSalamander`"
  [svg-xml-str
   width]
  (->
    svg-xml-str
    #_#_
    (#(svgthing/svg
        {:width  width
         :height width} ;;height
        %))
    (svgthing/serialize)
    (salamander-buffered-image
      width
      width) ;;height
    ((fn
       [buff-img]
       (println
         "Writing image to disk")
       (javax.imageio.ImageIO/write
         ^java.awt.image.BufferedImage buff-img
         "jpg"
         (java.io.File.
           "buff.jpg"))
       buff-img))
    (javafx.embed.swing.SwingFXUtils/toFXImage
      nil)))

(defn-
  girod2jfx-image
  "How to turn an SVG XML string to an image
  Uses `fxsvgimage`"
  [svg-xml-str
   width]
  (let [^org.girod.javafx.svgimage.SVGImage image ((girod2jfx-fn
                           svg-xml-str
                           width))]
    (.snapshot
      image
      "jpg"
      (java.io.File.
        "testjpg.jpg"))
    (spit
      "test.svg"
      svg-xml-str)
    (.toImage
      image)))




(defn
  xml2img
  [{:keys [fx/context
           svg
           width]}]
  {:fx/type :scroll-pane
   ;;     :fit-to-width true
   ;;     :fit-to-height true
   :content {:fx/type :image-view
             ;;             :fit-height width
             :image   (salamander2jfx-image
                        svg
                        width)
             #_       (girod2jfx-image
                        svg
                        width)}})

#_
{:fx/type  :stack-pane
 ;;   :preserve-ratio true
 :children []
 :style    {:-fx-background-repeat   "stretch" 
            :-fx-background-position "center center";
            :-fx-background-image    (let [#_#_params (LoaderParameters.)
                                           image      (->
                                                        ^String svg
                                                        (SVGLoader/load))]
                                       (.snapshot
                                         image
                                         "jpg"
                                         (java.io.File.
                                           "test.txt"))
                                       (spit "test.svg" svg)
                                       (.toImage
                                         image))}}

#_
(set! (.
        params
        applyViewportPosition)
      false)
#_#_    (set! (.
                params
                width)
              100)
(set! (.
        params
        centerImage)
      true)

#_
(def props
  (merge
    fx.shape/props
    (composite/props SVGPath
                     :content [:setter lifecycle/scalar]
                     :fill-rule [:setter lifecycle/scalar :coerce (coerce/enum FillRule)
                                 :default :non-zero])))

#_
(def lifecycle
  (lifecycle/annotate
    (composite/describe SVGPath
                        :ctor []
                        :props props)
    :svg-path))




;; I guess this is a static loader object thing?
(def jsvg-loader
  (SVGLoader.))

(defn
  jsvg-bufimg
  [svg-str]
  (let [svg-doc       (.load jsvg-loader
                             (-> svg-str
                                 .getBytes
                                 clojure.java.io/input-stream)
                             nil
                             (.documentLimits (LoaderContext/builder) (DocumentLimits. 99 99 99999)))
        doc-size      (-> svg-doc
                          .size)
        width         (-> doc-size
                          .width
                          int)
        height        (-> doc-size
                          .height
                          int)
        imgbuf        (BufferedImage. width
                                      height
                                      java.awt.image.BufferedImage/TYPE_INT_RGB)
        graphics2d    (.createGraphics imgbuf)
        output-file   (File. "test" #_file-str)
        rbspectrum    quickthing/red-blue-colors
        neutral-white (get rbspectrum
                           (/ (count rbspectrum)
                              2))]
    (.setColor graphics2d
               (java.awt.Color. (float (:r neutral-white))
                                (float (:g neutral-white))
                                (float (:b neutral-white))
                                #_Color/WHITE))
    (.fillRect graphics2d
               0
               0
               width
               height)
    (.render svg-doc
             nil
             graphics2d)
    imgbuf))

(defn
  jsvg-jxfimg
  [svg-str]
  (-> svg-str
      jsvg-bufimg
      (javafx.embed.swing.SwingFXUtils/toFXImage
        nil)))

#_
(defn
  jsvg-test
  [svg-str]
  (-> svg-str
      jsvg-bufimg
      (javax.imageio.ImageIO/write "png"
                                   (File. "jsvg-render-test.png"))))
#_
(->  "https://raw.githubusercontent.com/wiki/kxygk/imergination/krabi-root-2/indeces.svg"
     slurp
     jsvg-test)

(defn
  xml
  "Converts an SVG string to a JavaFX Group-like object
  `:svg` for the svg string
  `:width` for an optional width parameter
  NOTE: The library seems to work weird with widths
  if you specify the display width with `:width`
  then the `<svg>` tag shouldn't specify its own height/width"
  [{:keys [fx/context
           svg
           scale-x
           scale-y]}]
  {:fx/type :image-view
   :image   (jsvg-jxfimg svg)})
