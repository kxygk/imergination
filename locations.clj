(ns
    locations
  "Different commonly used locations as `def`s"
  (:use geoprim
        clojure.math))

;; = `Regions-of-Interest`
;;
;; == `WORLD`

(def
  regions
  {:world                      {:region (region (point 90.0
                                                       -180.0)
                                                (point -90.0
                                                       180.0))}
   :ocean1small                {:region (region (point 90.0
                                                       -180.0)
                                                (point 88.0
                                                       -179.0))}
   :ocean1small-1pat           {:region (region (point 90.0
                                                       -180.0)
                                                (point 88.0
                                                       -179.0))}
   :ocean1large                {:region            (region (point 90.0
                                                                  -180.0)
                                                           (point 85.0
                                                                  -177.0))
                                :interesting-times [21, 41, 63, 66, 82, 97, 99, 112]}
   :ocean1largeextra           {:region            (region (point 90.0
                                                                  -180.0)
                                                           (point 85.0
                                                                  -177.0))
                                :interesting-times [21, 41, 63, 66, 82, 97, 99, 112]}
   :ocean1largeextraextra      {:region            (region (point 90.0
                                                                  -180.0)
                                                           (point 85.0
                                                                  -177.0))
                                :interesting-times [21, 41, 63, 66, 82, 97, 99, 112]}
   :ocean1largeextraextraextra {:region            (region (point 90.0
                                                                  -180.0)
                                                           (point 85.0
                                                                  -177.0))
                                :interesting-times [21, 41, 63, 66, 82, 97, 99, 112]}
   :ocean2large                {:region (region (point 90.0
                                                       -180.0)
                                                (point 85.0
                                                       -177.0))}
   :ocean1                     {:region            (region (point 1.0
                                                                  -1.0)
                                                           (point -1.5
                                                                  1.0))
                                :interesting-times [21, 41, 63, 66, 82, 97, 99, 112]}
   :ocean2                     {:region (region (point 1.0
                                                       -2.0)
                                                (point -1.0
                                                       2.0))}
   :world-region               {:region (region (point 90
                                                       -180)
                                                (point -90
                                                       180))}
   :two-seas-region            {:region (region (point 23
                                                       85)
                                                (point -5.9
                                                       119))}
   :java                       {:region (region (point -7.5
                                                       110)
                                                (point -9.0
                                                       113))}
   :java-era5                  {:region (region (point -7.5
                                                       110)
                                                (point -9.0
                                                       113))}
   :birdhead                   {:region (region (point -0.25
                                                       131.0)
                                                (point -4.5
                                                       140))}
   :krabi-region               {:region (region (point 8.5
                                                       98.0)
                                                (point 7.5
                                                       99.0))}
   :krabi-city-region          {:region (region (point 8.5
                                                       98.0)
                                                (point 7.5
                                                       99.0))}
   :krabi-root-2               {:region            (region (point (+ 6.0
                                                                     (* 5.0
                                                                        (pow 2.0
                                                                             0.5)))
                                                                  97.0)
                                                           (point 6
                                                                  101))
                                :interesting-times [38, 22, 71, 89, 65, 101, 85, 24]}
   :krabi-root-2-daily               {:region            (region (point (+ 6.0
                                                                     (* 5.0
                                                                        (pow 2.0
                                                                             0.5)))
                                                                  97.0)
                                                           (point 6
                                                                  101))
                                :interesting-times [38, 22, 71, 89, 65, 101, 85, 24]}
   :krabi-root-2-diurnal       {:region            (region (point (+ 6.0
                                                                     (* 5.0
                                                                        (pow 2.0
                                                                             0.5)))
                                                                  97.0)
                                                           (point 6
                                                                  101))
                                :interesting-times [38, 22, 71, 89, 65, 101, 85, 24]}
   :krabi-root-2-era5          {:region            (region (point (+ 6.0
                                                                     (* 5.0
                                                                        (pow 2.0
                                                                             0.5)))
                                                                  97.0)
                                                           (point 6
                                                                  101))
                                :interesting-times [38, 22, 71, 89, 65, 101, 85, 24]}
   :rift-valley-small          {:region (region (point 15.07
                                                       39.0)
                                                (point 8.0
                                                       43.0))}
   :kashmir                    {:region (region (point 37.07
                                                       73.0)
                                                (point 30.0
                                                       77.0))}
   :himalaya                   {:region (region (point 35.07
                                                       75.0)
                                                (point 28.0
                                                       79.0))}
   :eastern-korea              {:region (region (point 38.6
                                                       127.1)
                                                (point 36.1
                                                       129.3))}
   :southern-korea             {:region (region (point 36.6
                                                       126.2)
                                                (point 34.1
                                                       128.6))}
   :minnan-region              {:region (region (point 26.23
                                                       116.47)
                                                (point 21.7
                                                       125))}
   :taiwan-region              {:region (region (point 25.76
                                                       119.74)
                                                (point 21.74
                                                       122.26))}
   :north-taiwan-region        {:region (region (point 25.475
                                                       120.75)
                                                (point 23.75
                                                       122.25))}
   :taipei-region              {:region (region (point 25.475
                                                       120.75)
                                                (point 24.75
                                                       122.25))}
   :birdneck-region            {:region (region (point 0.5
                                                       133.0)
                                                (point -5.0
                                                       138.0))}
   ;; One Monsoon Regions
   :ghana-large                {:region            (region (point 10.0
                                                                  -2.5)
                                                           (point 5.0
                                                                  0.5))
                                :interesting-times [49, 52, 7, 30, 29, 39, 62, 93]}
   :ghana-small                {:region            (region (point 8.0
                                                                  -2.0)
                                                           (point 5.0
                                                                  0.0))
                                :interesting-times [49, 52, 7, 30, 29, 39, 62, 93]}
   :togo                       {:region            (region (point 9.5
                                                                  0.0)
                                                           (point 6.0
                                                                  2.0))
                                :interesting-times [49, 52, 7, 30, 29, 39, 62, 93]}
   :jos                        {:region            (region (point 10.5
                                                                  8.5)
                                                           (point 9.5
                                                                  9.5))
                                :interesting-times [49, 52, 7, 30, 29, 39, 62, 93]}
   :sichuan-wall               {:region            (region (point 32.54
                                                                  102.5)
                                                           (point 29.0
                                                                  104.5))
                                :interesting-times [97, 52, 101, 89, 54, 55, 27, 59]}
   ;; Deserts
   :marrah                     {:region            (region (point 14.5 ;;15
                                                                  23.4)
                                                           (point 12.0 ;;12
                                                                  25.4))
                                :interesting-times [55, 30, 42, 65, 54, 41, 39, 4]}
   :marrah-big                 {:region            (region (point 15.5 ;;15
                                                                  22.4)
                                                           (point 11.0 ;;12
                                                                  26.4))
                                :interesting-times [55, 30, 42, 65, 54, 41, 39, 4]}
   :udaipur                    {:region (region (point 26.0 ;;15
                                                       72.0)
                                                (point 23.0 ;;12
                                                       74.0))}
   })



(def
  poi
  {:taipei        (point 25
                         121.5)
   :krabi         (point 8.100833
                         98.984722)
   :krabi-airport (point 8.100833
                         98.984722)})




(def
  world-region
  (region
    (point
      90
      -180)
    (point
      -90
      180)))
;;
;; == `Thailand`
(def
  two-seas-region
  (region
    (point
      23
      85)
    (point
      -5.9
      119)))
(def
  krabi-region
  (region
    (point
      8.5
      98.0)
    (point
      7.5
      99.0)))

(def
  krabi-city-region
  (region
    (point
      8.5
      98.0)
    (point
      7.5
      99.0)))
#_
(def
  krabi-skinny-region
  (region
    (point
      11
      96.5)
    (point
      6
      101.5)))

(def
  krabi-root-2
  (region
    (point
      (+ 6.0
         (* 5.0
            (pow 2.0
                 0.5)))
      97.0)
    (point
      6
      101)))
#_
(+ 6.0
   (* 5.0
      (pow 2.0
           0.5)))
;; => 13.071067811865476

(def
  rift-valley-small
  (region (point 15.07
                 39.0)
          (point 8.0
                 43.0)))


(def
  kashmir
  (region (point 37.07
                 73.0)
          (point 30.0
                 77.0)))



(def
  himalaya
  (region (point 35.07
                 75.0)
          (point 28.0
                 79.0)))


;;
;; == Korea

(def
  eastern-korea
  (region (point 38.6
                 127.1)
          (point 36.1
                 129.3)))

(def
  southern-korea
  (region (point 36.6
                 126.2)
          (point 34.1
                 128.6)))

;;
;; == `Taiwan`
(def
  minnan-region
  "Taiwan/Fujian"
  (region
    (point
      26.23
      116.47)
    (point
      21.7
      125)))

(def
  taiwan-region
  "Just the island of Taiwan"
  (region
    (point
      25.76
      119.74)
    (point
      21.74
      122.26)))

(def
  north-taiwan-region
  "Northern Taiwan"
  (region
    (point
      25.475
      120.75)
    (point
      23.75
      122.25)))

(def
  taipei-region
  "Northern Taiwan"
  (region
    (point
      25.475
      120.75)
    (point
      24.75
      122.25)))

;;
;; == `Papua`
(def
  birdneck-region
  "Cenderawasih Bay"
  (region
    (point
      0.5
      133.0)
    (point
      -5.0
      138.0)))

;; = `Points-of-Interest`
(def
  taipei
  (point
    25
    121.5))

(def
  krabi
  (point
    8.100833
    98.984722))

(def
  krabi-airport
  (point
    8.100833
    98.984722))

;; (def
;;   taipei-region
;;   "Northern Taiwan"
;;   (region
;;     (point
;;       25.475
;;       120.75)
;;     (point
;;       24.75
;;       122.25)))

;; (def
;;   taipei-region
;;   "Northern Taiwan"
;;   (region
;;     (point
;;       25.4
;;       120.7)
;;     (point
;;       24.7
;;       122.2)))
