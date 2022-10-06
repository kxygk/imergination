(ns
    imergination
  "Core playground"
  (:use plot
        locations
        geoprim
        geogrid
        geogrid2svg
        geogrid4image
        geogrid4seq
        svgmaps
        [uncomplicate.neanderthal
         core
         native])
  (:require [svgmaps]
            [geojson2svg]
            [quickthing]
            (thi.ng.geom
              [core                          :as geom]
              [matrix                        :as matrix])
            #_'[thi.ng.color.core]
            [clj-curl.easy                   :as curl-easy]
            [clj-curl.opts                   :as curl-opts]
            [uncomplicate.neanderthal.linalg :as linalg]
            [thi.ng.geom.viz.core            :as viz]
            [thi.ng.geom.svg.core            :as svg]))
;; jackson is a dependency that gets pulled in at the wrong version for unclear reasons..
#_
(add-libs
  {'com.fasterxml.jackson.core/jackson-core
   {:mvn/version
    "2.10.2"}})


;; == `Inputs`

(def
  rain-filestr
  "A test rain data file"
  "/home/kxygk/Data/imerg/monthly/late/3B-MO-L.GIS.IMERG.20201201.V06B.tif"
  #_
  "./data/rain-2011-03.tif")

(def
  rain-dirstr
  "A directory will all the IMERG rain files we will use"
  "/home/kxygk/Data/imerg/monthly/late/"
  #_
  "/home/kxygk/Data/imerg/monthly/final/")

#_
(def
  shoreline-filestr
  "A shoreline file"
  "./data/shoreline-coarse.json")

(def
  elevation-filestr
  "Elevation file"
  "./data/elevation-GMTED-10S090E.tif")

(def
  output-dirstr
  "./")

;; = `Shapefiles`
;; A lot of geographic data is in the `shapefile` file format
;;
;; However the +geo+ library operates with `GeoJSON` files. These are much larger in size, but much easier to manage
;;
;; The `geo` maintainer explains in this talk how shapefiles are difficult to read: https://www.youtube.com/watch?v=d628Oggm-nU
;;
;; Maybe they can be read in through some secondary library? .. For the moment we don't have a burning need to process shapefiles

;; == `World-Shorelines`
;; For a base layer on which to build/draw, it'd be useful to simply display the world map. To do that we display the global shorlines. Shorelines of the world can be downloaded from https://www.ngdc.noaa.gov/mgg/shorelines/ Or also from https://www.soest.hawaii.edu/pwessel/gshhg/ (I think these two are actually the same)
;;
;; You should get the ESRI shapefile version. The other formats (like NetCDF) seem to be even more complex.
;;
;; The shorelines come at multiple resolutions and are split into different groupings. The README explains what the different files contain.


;; == `Conversion`
;; Now we need to convert the shapefile to a GeoJSON. Unfortunately I'm not sure this can be done within the JVM. However there is a command line tool called `ogr2org` that will do this automatically for you. It's part of GDAL
;;
;; https://gdal.org/programs/ogr2ogr.html
;;
;; The tool docs don't directly mention GeoJSON for some reason, but you can find conversion one liners on the GeoJSON page
;;
;; https://gdal.org/drivers/vector/geojson.html
;;
;; For instance, I went into the shoreline subdirectory +GSHHS_shp/c+ and ran
;; [,sh]
;; ____
;; ogr2ogr -f GeoJSON shoreline.json GSHHS_i_L1.shp
;; ____
;;
;; == `Plotting`
;; Details are found in `plot.clj` ..
;; 
#_
(spit
  "out/south-thailand.svg"
  #_
  "fujian-taiwan.svg"
  (quickthing/serialize-with-line-breaks
    (plot/shoreline-map
      locations/two-seas-region
      [locations/krabi])))

;; = `Geogrids`
;;
;; Come in different forms
;; The IMERG dataset arrived in geotiff files.
;; GeoTIFF can be treated as an image file
;; However the pixels can have negative values .. so be careful!
;;
;; There is a `geogrid` interface defined
;; And then different implementations
;; ex: `geogrid4image` and `geogrid4vec`
(def
  global-rain-grid-test-file
  "A global rain grid for some month"
  (geogrid4image/read-file
    rain-filestr
    0.1
    0.1))


#_
(spit
  "out/taiwan-rain.svg"
  #_
  "krabi-skinny-rain.svg"
  (quickthing/serialize-with-line-breaks
    (plot/grid-map
      global-rain-grid-test-file
      locations/taiwan-region
      #_
      locations/krabi-skinny-region
      []
      "臺灣"
      #_
      "กระบี่"
      )))

;; calling `subregion` over an over shouldn't change anything
#_
(spit
  "out/krabi-skinny-rain2.svg"
  (quickthing/serialize-with-line-breaks
    (plot/grid-map
      (geogrid/subregion
        (geogrid4seq/convert-to
          (geogrid/subregion 
            global-rain-grid
            locations/krabi-skinny-region))
        locations/krabi-skinny-region)
      locations/krabi-skinny-region
      [])))

;; == `Bulk`
;; We read in all the files in a given directory and read them in according to the Unicode value of each character. See Java Strings for details https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html#compareTo(java.lang.String)

(defn
  read-imerg-dir
  "Read in a directory of IMERG files.
  Optionally provide a region to immediately crop the data to
  [NOTE]
  This function is not parallelized to minimize the memory footprint.
  Each map is read and cropped sequentially (thanks to lazy evaluation)"
  ([data-dirstr
    latlon-res
    region]
   (->>
     data-dirstr
     read-imerg-dir
     (map #(geogrid/subregion
             %
             region))))
  ([data-dirstr]
   (->>
     data-dirstr
     java.io.File.
     .list
     sort
     (map
       #(geogrid4image/read-file
          (str
            data-dirstr
            %)
          0.1
          0.1)))))
#_
(read-imerg-dir
  rain-dirstr
  [0.1
   0.1]
  locations/two-seas-region)

(defn
  to-data-matrix
  "Turns a series of `geogrid` of identical size to one matrix"
  [grids]
  (let[[width-pix
        height-pix] (->
                      grids
                      first
                      geogrid/dimension-pix)
       all-data     (->>
                      grids
                      (map
                        geogrid/data)
                      (reduce
                        into
                        []))]
    (dge
      ;; rows
      (* 
        width-pix
        height-pix)
      ;; columns
      (count
        grids)
      ;; data
      all-data)))

(defn
  region-rain-matrix
  "Take directory of geotiffs `geotiff-dirstr` and cut out a region and turn that into a matrix.
  Returns a map with `:matrix` `:position` of the grid and `:resolution`"
  [geotiff-dirstr
   [eas-res
    sou-res]
   local-region]
  (let [local-rain-grids (read-imerg-dir
                           geotiff-dirstr
                           [eas-res
                            sou-res]
                           local-region)
        local-matrix     (to-data-matrix
                           local-rain-grids)]
    {:matrix     local-matrix
     :dimension  (-> local-rain-grids
                     first
                     geogrid/dimension-pix)
     :position   (-> local-rain-grids
                     first
                     geogrid/corner)
     :resolution (-> local-rain-grids
                     first
                     geogrid/eassou-res)}))


(defn
  col-to-grid
  "Given a COLUMN-OF-DATA
  as well as map with keys for the grid's
  `:dimension` `:position` and `:resolution`
  Returns a `geogrid`"
  [column-of-data
   {:keys [;;matrix
           dimension
           position
           resolution]}]
  (let [[width-pix
         height-pix] dimension
        [eas-res
         sou-res]    resolution]
    (geogrid4seq/build-grid
      width-pix
      height-pix
      eas-res
      sou-res
      position
      column-of-data)))

(defn
  matrix-col-to-grid
  "Given a matrix"
  [{:keys [matrix
           dimension
           position
           resolution]
    :as   grid-params}
   column-index]
  (col-to-grid
    (into
      []
      (col
        matrix
        column-index))
    grid-params))

#_
(defn
  ica-vectors
  "Do an ICA on our 2D data and return two vectors"
  [data]
  (let [data-as-matrix (->>
                         [(map
                            first
                            data)
                          (map
                            second
                            data)]
                         (map
                           double-array)
                         into-array)]
    (->
      (smile.projection.ICA/fit
        data-as-matrix
        2) ;; 2 dominant vectors
      .components
      #_vec)))

;; (def
;;   region-analysis
;;   [imerg-data-dir
;;    region]
;;   (let [region-rains-matrix (->>
;;                               input-region
;;                               (region-rain-matrix
;;                                 imerg-data-dir
;;                                 [0.1
;;                                  0.1]))    

(defn
  print-region-maps
  ([input-region
    [summer-idx
     winter-idx]]
   (print-region-maps
     input-region
     [summer-idx
      winter-idx]
     ""))
  ([input-region
    [summer-idx
     winter-idx]
    map-label]
   (let [region-rains-matrix (->>
                               input-region
                               (region-rain-matrix
                                 #_"/home/kxygk/Junk/alldata/"
                                 rain-dirstr
                                 [0.1
                                  0.1]))
         first-month-rain    (matrix-col-to-grid
                               region-rains-matrix
                               0)
         sixth-month-rain    (matrix-col-to-grid
                               region-rains-matrix
                               5)
         svd                 (linalg/svd
                               (:matrix
                                region-rains-matrix)
                               true
                               true)
         svs                 (assoc ;; maintains meta-data to draw the SVs as maps
                               region-rains-matrix
                               :matrix
                               (:u
                                svd))
         first-sv            (data
                               (matrix-col-to-grid
                                 svs
                                 0))
         secon-sv            (data
                               (matrix-col-to-grid
                                 svs
                                 1))
         projections         (mapv
                               vector
                               (row ;; data proj on sv1
                                 (:vt
                                  svd)
                                 0)
                               (row ;; data proj on sv1
                                 (:vt
                                  svd)
                                 1))
         winter-month        (let [[first-factor
                                    secon-factor] (get
                                                    projections
                                                    winter-idx
                                                    ;;58 ;; taiwan winter
                                                    ;;95 ;; krabi-winter
                                                    )]
                               (mapv
                                 (fn [first-element
                                      secon-element]
                                   (+
                                     (*
                                       first-factor
                                       first-element)
                                     (*
                                       secon-factor
                                       secon-element)))
                                 first-sv
                                 secon-sv))
         summer-month        (let [[first-factor
                                    secon-factor]
                                   (get
                                     projections
                                     summer-idx
                                     ;;65 ;; taiwan summer
                                     ;;91 ;; krabi summer
                                     )]
                               (mapv
                                 (fn [first-element
                                      secon-element]
                                   (+
                                     (*
                                       first-factor
                                       first-element)
                                     (*
                                       secon-factor
                                       secon-element)))
                                 first-sv
                                 secon-sv))
         month-maps          (fn
                               [months]
                               (->>
                                 months
                                 (mapv
                                   (fn
                                     [month]
                                     [month
                                      (matrix-col-to-grid
                                        region-rains-matrix
                                        month)]))
                                 (mapv
                                   (fn
                                     [[month
                                       grid]]
                                     (plot/grid-map
                                       grid
                                       input-region
                                       []
                                       (str
                                         (long
                                           (+
                                             2.0
                                             (*
                                               3.0
                                               month)))
                                         ":30"))))))]
     (spit
       "out/first-year.svg"
       (quickthing/serialize-with-line-breaks
         (quickthing/group-plots-grid
           [(->>
              (range
                0
                4)
              month-maps)
            (->>
              (range
                7
                3
                -1)
              month-maps)
            (->>
              (range
                8
                12)
              month-maps)
            (->>
              (range
                15
                11
                -1)
              month-maps)])))
     (spit
       "out/first-col.svg"
       (quickthing/serialize-with-line-breaks
         (plot/grid-map
           first-month-rain
           input-region ;; it'll crop redundantly here..
           []
           "1日")))
     (spit
       "out/sixth-col.svg"
       (quickthing/serialize-with-line-breaks
         (plot/grid-map
           sixth-month-rain
           input-region ;; it'll crop redundantly here..
           []
           "6日")))
     (spit
       "out/first-sv.svg"
       (quickthing/serialize-with-line-breaks
         (plot/grid-map
           (matrix-col-to-grid
             svs
             0)
           input-region ;; it'll crop redundantly here..
           []
           "1st SV")))
     (spit
       "out/second-sv.svg"
       (quickthing/serialize-with-line-breaks
         (plot/grid-map
           (matrix-col-to-grid
             svs
             1)
           input-region ;; it'll crop redundantly here..
           []
           "2nd SV")))
     (spit
       "out/third-sv.svg"
       (quickthing/serialize-with-line-breaks
         (plot/grid-map
           (matrix-col-to-grid
             svs
             2)
           input-region ;; it'll crop redundantly here..
           []
           "3rd SV")))
     (spit
       "out/fourth-sv.svg"
       (quickthing/serialize-with-line-breaks
         (plot/grid-map
           (matrix-col-to-grid
             svs
             3)
           input-region ;; it'll crop redundantly here..
           []
           "4th SV")))
     (spit
       "out/sv-projections.svg"
       (quickthing/serialize-with-line-breaks
         (plot/two-d-plot
           projections
           1000
           1000)))
     (spit
       "out/summer.svg"
       (quickthing/serialize-with-line-breaks
         (plot/grid-map
           (col-to-grid
             summer-month
             region-rains-matrix)
           input-region ;; it'll crop redundantly here..
           []
           "夏天")))
     (spit
       "out/winter.svg"
       (quickthing/serialize-with-line-breaks
         (plot/grid-map
           (col-to-grid
             winter-month
             region-rains-matrix)
           input-region ;; it'll crop redundantly here..
           []
           "冬天")))
     projections)))

#_
(matrix-col-to-grid
  (->>
    input-region
    (region-rain-matrix
      rain-dirstr
      [0.1
       0.1]))
  0)

#_
(print-region-maps
  locations/two-seas-region)

#_
(print-region-maps
  locations/krabi-region)

(def month-projections
  (print-region-maps
    locations/krabi-skinny-region
    [91
     95]
    "Krabi/กระบี่"))
#_
(print-region-maps
  locations/birdneck-region)


#_
(print-region-maps
  locations/minnan-region)

#_
(print-region-maps
  locations/taiwan-region
  [90
   81])

#_
(print-region-maps
  locations/north-taiwan-region
  [90
   81])

#_
(def
  krabi-matrix
  (region-rain-matrix
    rain-dirstr
    [0.1
     0.1]
    locations/krabi-skinny-region))

#_#_
(println
  projections)
(->>
  (->
    ;; create the axis
    (quickthing/standard-axis
      projections
      1000
      1000)
    ;; add data to the plot
    (assoc
      :data
      [(quickthing/dashed-line
         projections)
       (quickthing/adjustable-circles
         (map-indexed
           (fn [index
                data-point]
             (conj
               data-point
               nil ;; default radius
               {:fill
                ;;(apply
                ;;  thi.ng.color.core/rgba
                (quickthing/color-cycle
                  (rem
                    (+
                      index
                      6.0)
                    12.0)) ;;)
                ;;
                :stroke
                "#777"}))
           projections))
       (quickthing/index-text
         projections)])
    (assoc ;; turn off grid
      :grid
      nil))
  ;; turns the plot specification to svg hiccup
  (viz/svg-plot2d-cartesian)
  ;; wraps in an `<svg>` element
  (svg/svg
    {:width
     width
     ;;
     :height
     height})
  ;; turns it to XML
  (svg/serialize)
  ;; writes to file
  (spit
    "sv-projections.svg"))

#_
(two-sv-projection
  krabi-matrix
  1000
  1000)




#_#_#_
(print-region-maps
  locations/taiwan-region)


(def
  taiwan-matrix
  (region-rain-matrix
    rain-dirstr
    [0.1
     0.1]
    locations/taiwan-region))


(two-sv-projection
  taiwan-matrix
  1000
  1000)

;; = `Elevation`

;; Elevation data is similarly stored in GeoTIFF format. However here the limitation is that we typically don't get a single global file.

;; There are https://www.usgs.gov/faqs/what-types-elevation-datasets-are-available-what-formats-do-they-come-and-where-can-i-download?qt-news_science_products=0#qt-news_science_products[several different sources] for global elevation data. Differences are subtle and hard to intepret. I will be using the GMTED2010 Elevation data. It's "recent", had a reletively high resolution and the description is promising:

;; .GMTED2010
;; ====
;; The GMTED2010 product suite contains seven new raster elevation products for each of the 30-, 15-, and 7.5-arc-second spatial resolutions and incorporates the current best available global elevation data.
;; ====

;; .ASTGTMv003
;; ====
;; *Note*: [[https://lpdaac.usgs.gov/products/astgtmv003/][ASTGTMv003]] seems to be even newer and at an even higher resolution. However the resolution is maybe too high. Data comes in tiny 1\deg{} by 1\deg{} tiles - so they need to either be downloaded dynamically, or you need to have a huge local cache (with 22,912 tiles)

;; Data can be viewed [[https://search.earthdata.nasa.gov/search?q=C1711961296-LPCLOUD][here]] and retrieval could be pretty easily automated. Tiles are found at easy to understand URLs. Ex: ~https://data.lpdaac.earthdatacloud.nasa.gov/lp-prod-public/ASTGTM.003/ASTGTMV003_N02E022.1.jpg~

;; However there are some caveats.. the metadata (which can be seen on the /EarthDataCloud/ link) reads

;; #+begin_src json :eval never
;; {
;;  "westBoundingCoordinate": 21.9998611,
;;  "eastBoundingCoordinate": 23.0001389,
;;  "northBoundingCoordinate": 3.0001389,
;;  "southBoundingCoordinate": 1.9998611
;; }
;; #+end_src

;; and the top-level description mentions a one-pixel overlap: 

;; "Selected data were averaged to create final pixel values before partitioning the data into 1° by 1° tiles with a one pixel overlap"

;; Using this data would require some care..

;; The other older, more canonical dataset is the one made by the space shuttle called [[https://www.usgs.gov/centers/eros/science/usgs-eros-archive-digital-elevation-shuttle-radar-topography-mission-srtm-void?qt-science_center_objects=0#qt-science_center_objects][SRTM]]. I didn't find any easy way to download it
;; ====

;; TODO: Revisit all of this
