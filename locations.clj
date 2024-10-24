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
  {:world-region        (region (point 90
                                       -180)
                                (point -90
                                       180))
   :two-seas-region     (region (point 23
                                       85)
                                (point -5.9
                                       119))
   :krabi-region        (region (point 8.5
                                       98.0)
                                (point 7.5
                                       99.0))
   :krabi-city-region   (region (point 8.5
                                       98.0)
                                (point 7.5
                                       99.0))
   :krabi-root-2        (region (point (+ 6.0
                                          (* 5.0
                                             (pow 2.0
                                                  0.5)))
                                       97.0)
                                (point 6
                                       101))
   :rift-valley-small   (region (point 15.07
                                       39.0)
                                (point 8.0
                                       43.0))
   :kashmir             (region (point 37.07
                                       73.0)
                                (point 30.0
                                       77.0))
   :himalaya            (region (point 35.07
                                       75.0)
                                (point 28.0
                                       79.0))
   :eastern-korea       (region (point 38.6
                                       127.1)
                                (point 36.1
                                       129.3))
   :southern-korea      (region (point 36.6
                                       126.2)
                                (point 34.1
                                       128.6))
   :minnan-region       (region (point 26.23
                                       116.47)
                                (point 21.7
                                       125))
   :taiwan-region       (region (point 25.76
                                       119.74)
                                (point 21.74
                                       122.26))
   :north-taiwan-region (region (point 25.475
                                       120.75)
                                (point 23.75
                                       122.25))
   :taipei-region       (region (point 25.475
                                       120.75)
                                (point 24.75
                                       122.25))
   :birdneck-region     (region (point 0.5
                                       133.0)
                                (point -5.0
                                       138.0))
   :sichuan-wall        (region (point 32.54
                                       102.5)
                                (point 29.0
                                       104.5))
   ;; Deserts
   :marrah              (region (point 14.5 ;;15
                                       23.4)
                                (point 12.0 ;;12
                                       25.4))
   :udaipur             (region (point 26.0 ;;15
                                       72.0)
                                (point 23.0 ;;12
                                       74.0))
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
