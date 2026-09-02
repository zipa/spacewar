(ns spacewar.ui.front-view-spec
  (:require [spacewar.ui.front-view :as front]
            [speclj.core :refer [describe it should=]]))

(describe "star-size"
  (it "maps luminosity bands to sizes"
    (should= 1 (front/star-size 0))
    (should= 2 (front/star-size (/ 2 200)))
    (should= 3 (front/star-size (/ 4 200)))
    (should= 4 (front/star-size (/ 9 200)))
    (should= 5 (front/star-size (/ 19 200)))
    (should= 6 (front/star-size 1))))

(describe "star-color"
  (it "is white above the brightness threshold"
    (should= [255 255 255] (front/star-color (/ 0.5 200))))

  (it "dims below the brightness threshold"
    (should= [0 0 0] (vec (front/star-color 0)))))
