(ns spacewar.ui.front-view-hardening-spec
  (:require [spacewar.ui.front-view :as front]
            [speclj.core :refer [describe it should=]]))

(describe "star-size thresholds"
  (it "uses exclusive upper bounds for each luminosity band"
    (should= 2 (front/star-size (/ 1 200)))
    (should= 3 (front/star-size (/ 3 200)))
    (should= 4 (front/star-size (/ 5 200)))
    (should= 5 (front/star-size (/ 10 200)))
    (should= 6 (front/star-size (/ 20 200)))))
