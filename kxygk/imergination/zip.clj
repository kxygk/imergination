(ns kxygk.imergination.zip
  "Namespace for handling zip files.
  B/c I'm going to forget how to do this..
  So keep it isolated"
  (:require [clojure.java.io :as io]
            [babashka.fs :as fs]))

(defn unzip
  [zip-location]
  (let [temp-dir (fs/create-temp-dir {:prefix "imergination-builtin-dataset-"})
        zip-stream  (io/input-stream zip-location)]
    ;; Unzip directly to the temp directory
    (fs/unzip zip-stream
              temp-dir)
    (fs/file temp-dir)))
