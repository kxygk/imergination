(ns kxygk.imergination.datalist
  "Generates a list of the included dataset's files.
  A `datalist.edn` is required.
  Data/code is packages in the final Java JAR as a ZIP.
  `.zip` files don't have an enumeration of the items in a directory.
  So code can't query for a list of data files.
  Hence,
  the program needs a list of data files ahead of time.
  This `ns` generates this `datalist.edn`
  ;;
  Should only need to be run if the included data is modified..
  Which probably will never happen?
  Note:
  The dataset's directory path is specified in `stateom`:
  (:default-data-dirstr @stateom/*selections)"
  (:require clojure.java.io))

(let [output-dir "./data"
      data-dir (clojure.java.io/file (:default-data-dirstr @stateom/*selections))
        ;; List all files, format them as classpath paths (e.g., "data/file.json")
        files (->> (file-seq data-dir)
                   (filter #(.isFile
                              %))
                   (map #(str #_"./data/imerg-late-v06b-10yrs-2011-through-2021/"
                              (.getName %)))
                   (remove #(clojure.string/ends-with? %
                                            "datalist.edn"))
                   sort
                   vec)]
  (println (str "Writing `datalist.edn` to"
                output-dir))
  (spit (clojure.java.io/file output-dir
                              "datalist.edn")
        (pr-str files))
  (println "Manifest created with" (count files) "files."))
