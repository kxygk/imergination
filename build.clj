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

(defn uber [_]
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
                  :ns-compile '[kxygk.imergination.guiom]})
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

(defn bundle [_]
  (println "Cleaning old bundle...")
  (b/delete {:path "bundle"})
  (println "Bundling with jpackage...")
  (let [{:keys [exit
                #_outc
                err]}
        (sh "jpackage"
            "--input" "jars/uber"
            "--dest" "bundle"
            "--name" "Imergination"
            "--main-jar" "imergination-uber.jar"
            "--main-class" "kxygk.imergination.guiom"
            "--java-options" "-Dapp.dir=$APPDIR"
            "--java-options" "--enable-native-access=ALL-UNNAMED"
            ;;#_#_
            "--type" "app-image")]
    (if (zero? exit)
      (println "Success! Check the /bundle folder.")
      (println "Error:" err))))
