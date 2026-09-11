(ns build
  (:require [clojure.tools.build.api :as b]
            [clojure.java.shell :refer [sh]]))

(refer-clojure :exclude '[compile])

(def lib 'kxygk/imergination)
(def version "1.0.0")
(def class-dir "jars/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def uber-file (format (str "jars/%s-"
                            #_
                            "%s-"
                            "uber.jar")
                       (name lib)
                       #_
                       version))

(defn clean [_]
  (b/delete {:path "jars"}))

(defn uber
  [opts] ;; can pass in the a git SHA to use in the app
  (let [sha (or (:sha opts)
                (System/getenv "APP_SHA")
                "TEST")]
    ;; Write a version file before compiling
    (spit "./kxygk/imergination/version.clj"
          (str "(ns kxygk.imergination.version)\n\n"
               "(def sha \"" sha "\")\n")))
  (println "Cleaning directories ... ")
  (clean nil)
  (println "Copying data resources...")
  (b/copy-dir {:src-dirs   ["."]
               :target-dir class-dir
               :include    "data/**"})
  (println "Compiling ...")
  (b/compile-clj {:basis      basis
                  :src-dirs   ["."]
                  :class-dir  class-dir
                  :ns-compile '[kxygk.imergination.guiom]
                  :jvm-opts   ["--enable-native-access=ALL-UNNAMED"]})
  #_#_
  (println "Printing Dependency Tree..")
  (let [result (clojure.java.shell/sh "clojure" "-Stree")]
    (if (zero? (:exit result))
      (spit "jars/tree.txt" (:out result))
      (println "Error:" (:err result))))
  (println "Assembling the uberjar in /jars ... ")
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis     basis
           :main      'kxygk.imergination.guiom})
  (b/copy-dir {:src-dirs ["jars"]
               :target-dir "jars/uber"
               :include "imergination-uber.jar"})
  (b/delete {:path "jars/imergination-uber.jar"}))

(def ^:private mac-iconset-entries
  [[16   "icon_16x16.png"]
   [32   "icon_16x16@2x.png"]
   [32   "icon_32x32.png"]
   [64   "icon_32x32@2x.png"]
   [128  "icon_128x128.png"]
   [256  "icon_128x128@2x.png"]
   [256  "icon_256x256.png"]
   [512  "icon_256x256@2x.png"]
   [512  "icon_512x512.png"]
   [1024 "icon_512x512@2x.png"]])

(defn- render-icon-size!
  "Nearest-neighbor upscale of the 16x16 master. All target sizes are
   exact multiples of 16, so this is lossless."
  [im src size dest]
  (let [{:keys [exit err]}
        (sh im
            src
            "-filter"
            "point"
            "-resize"
            (str size
                 "x"
                 size)
            dest)]
    (when-not (zero? exit)
      (throw (ex-info (str "Failed to render icon at "
                           size
                           "x"
                           size)
                      {:err err})))))

(defn generate-icons
  "LLM/Qwen generated function for making icons"
  [_]
  (let [os  (clojure.string/lower-case (System/getProperty "os.name"))
        src "data/icon.png"
        ;; ImageMagick 6 on Linux uses 'convert'; IM7 on Win/Mac uses 'magick'
        im  (if (clojure.string/includes? os
                                          "linux")
              "convert"
              "magick")
        out "icons"]
    (println "Generating icons for"
             os
             " Using: "
             im)
    (b/delete {:path out})
    (.mkdirs (clojure.java.io/file out))
    (cond
      (clojure.string/includes? os
                                "win") (let [sizes [16
                                                    32
                                                    48
                                                    64
                                                    128
                                                    256]
                                             pngs  (mapv #(str out
                                                               "/ico-"
                                                               %
                                                               ".png")
                                                         sizes)]
                                         (doseq [[s
                                                  p] (map vector
                                                          sizes
                                                          pngs)]
                                           (render-icon-size! im
                                                              src
                                                              s
                                                              p))
                                         (let [{:keys [exit
                                                       err]}
                                               (apply sh
                                                      im
                                                      (concat pngs
                                                              [(str out
                                                                    "/icon.ico")]))]
                                           (when-not (zero? exit)
                                             (throw (ex-info "Failed to assemble .ico"
                                                             {:err err}))))
                                         (run! #(b/delete {:path %})
                                               pngs))

      (clojure.string/includes? os
                                "mac") (let [iconset (str out
                                                          "/icon.iconset")]
                                         (.mkdirs (clojure.java.io/file iconset))
                                         (doseq [[size
                                                  name] mac-iconset-entries]
                                           (render-icon-size! im
                                                              src
                                                              size
                                                              (str iconset "/" name)))
                                         (let [{:keys [exit
                                                       err]} (sh "iconutil"
                                                                 "-c"
                                                                 "icns"
                                                                 iconset
                                                                 "-o"
                                                                 (str out
                                                                      "/icon.icns"))]
                                           (when-not (zero? exit)
                                             (throw (ex-info "iconutil failed"
                                                             {:err err})))
                                           (b/delete {:path iconset})))
      :else ;; Linux
      (render-icon-size! im
                         src
                         1024
                         (str out
                              "/icon.png")))))

(def icon-file
  (let [os (clojure.string/lower-case (System/getProperty "os.name"))]
    (cond
      (clojure.string/includes? os
                                "win") "icons/icon.ico"
      (clojure.string/includes? os
                                "mac") "icons/icon.icns"
      :else                            "icons/icon.png")))

(defn get-version [opts]
  (or (:version opts)
      (System/getenv "APP_VERSION")
      "8.8.8"))


(defn bundle [opts]
  (println "Cleaning old bundle...")
  (b/delete {:path "bundle"})
  (println "Generating Icons...")
  (generate-icons nil)
  (println "Bundling with jpackage...")
  (let [{:keys [exit
                #_outc
                err]}
        (sh "jpackage"
            "--input"
            "jars/uber"
            "--dest"
            "bundle"
            "--name"
            "Imergination"
            "--main-jar"
            "imergination-uber.jar"
            "--app-version"
            (get-version opts)
            "--icon"
            icon-file
            "--main-class"
            "kxygk.imergination.guiom"
            "--copyright"
            "Copyright 2026 George Kontsevich"
            "--vendor"
            "George Kontsevich"
            "--description"
            "Pattern extraction and index generation for IMERG data"
            "--java-options"
            "--enable-native-access=ALL-UNNAMED")]
    (if (zero? exit)
      (println "Success! Check the /bundle folder.")
      (println "Error:" err))))
