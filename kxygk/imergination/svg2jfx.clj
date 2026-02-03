(ns
    kxygk.imergination.svg2jfx
  "Make a GUI node from an SVG"
  (:require clojure.java.io
            quickthing)
  (:import java.awt.image.BufferedImage
           com.github.weisj.jsvg.parser.SVGLoader
           com.github.weisj.jsvg.parser.LoaderContext
           com.github.weisj.jsvg.parser.DocumentLimits))

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
        imgbuf        (BufferedImage. width
                                      height
                                      java.awt.image.BufferedImage/TYPE_INT_RGB)
        graphics2d    (.createGraphics imgbuf)
     ;; output-file   (File. "test" #_file-str)
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
