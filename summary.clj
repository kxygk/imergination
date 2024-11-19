(ns
    summary
  "Generate a markdown file describing a region (for wiki)"
  (:require [clojure.java.io :as io]
            [cljfx.api       :as fx]
            [clojure.core.cache :as cache]
            [injest.path :refer [+> +>> x>> =>>]]
            state
            bisect
            svg2jfx
            matrix
            plot
            locations))

;; Manually run/trigger because not part of GUI
(-> @state/*selections
    (cljfx.api/sub-ctx state/annual-cycle
                       0)) ;; nil

;; Similar, but row-by-row instead of circle
(-> @state/*selections
    (fx/sub-ctx state/cycle-group-svg
                0))

(def
  region
  (-> @state/*selections
      (fx/sub-ctx state/region-key)
      symbol))

(mapv geoprim/as-eassou
      (-> @state/*selections
          (fx/sub-ctx state/region)
          geoprim/four-corners))

(def markdown-str
  (str "# "
       region
       "

Topography |
:-------------------------:|
"
       "![]("
       region
       "/etopo2022.jpg) |"
       "

Region Corners (Lat/Lon): "
       (mapv geoprim/as-latlon
             (-> @state/*selections
                 (fx/sub-ctx state/region)
                 geoprim/four-corners))
       "

Annual Cycle (First Year Shown) |
:-------------------------:|
"
       "![]("
       region
       "/year0.svg) |"
       "

## EOF Analysis


EOF1 | EOF2 | EOF3 | EOF4 |
:---:|:----:|:----:|:----:|
"
       "![](./"
       region
       "/sv-0.svg) |"
       "![](./"
       region
       "/sv-1.svg) |"
       "![](./"
       region
       "/sv-2.svg) |"
       "![](./"
       region
       "/sv-3.svg) |"
       "

EOF Weights|
:-------------------------:|
"
       "![]("
       region
       "/sv-weights.svg) |"
       "

EOF1 component vs EOF2 component of each data point |
:-------------------------:|
"
       "![]("
       region
       "/sv-projs.svg) |"
       "

First Pattern | Second Pattern |
:---:|:----:|
"
       "![](./"
       region
       "/first-pattern.svg) |"
       "![](./"
       region
       "/second-pattern.svg) |"
       "

Climate Indices |
:-------------------------:|
"
       "![]("
       region
       "/indeces.svg) |"
       "

## Single Pattern Analysis"
       "

EOF1 Index |
:-------------------------:|
"
       "![]("
       region
       "/eof1/index.svg) |"
       "
EOF1 vs Var |
:-------------------------:|
"
       "![]("
       region
       "/eof1/noise-1d-var-stats.svg) |"))

(spit (str "../imergination.wiki/"
           region
           ".md")
      markdown-str)

-
(clojure.java.shell/sh "pandoc"
                       (str "../imergination.wiki/"
                            region
                            ".md")
                       "-o"
                       (str "../imergination.wiki/"
                            region
                            ".html"))
