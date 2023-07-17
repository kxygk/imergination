(ns
    locations
  "Different commonly used locations as `def`s"
  (:use geoprim))

;; = `Regions-of-Interest`
;;
;; == `WORLD`
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

(def
  krabi-skinny-region
  (region
    (point
      11
      96.5)
    (point
      6
      101.5)))

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
