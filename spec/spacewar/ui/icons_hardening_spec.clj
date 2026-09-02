(ns spacewar.ui.icons-hardening-spec
  (:require [spacewar.ui.icons :as icons]
            [speclj.core :refer [describe it should should-not should=]]))

(describe "transport triangles"
  (it "points dilithium up from the origin"
    (should= [0 -8 -6 6 6 6] (:dilithium icons/transport-triangle)))

  (it "points antimatter down from the origin"
    (should= [0 8 -6 -6 6 -6] (:antimatter icons/transport-triangle))))

(describe "pulse-stroke-weight"
  (it "uses the off weight at the exact midpoint of the cycle"
    (should= 2 (icons/pulse-stroke-weight true 250 5 2))
    (should= 2 (icons/pulse-stroke-weight true 750 5 2))))

(describe "pulsar-visible?"
  (it "hides a pulsar at the exact midpoint of the cycle"
    (should-not (icons/pulsar-visible? :pulsar 250))))

(describe "shield-fill-color"
  (it "uses a red-to-yellow fill with a non-flicker alpha of 100"
    (should= [255 0 0 100] (icons/shield-fill-color 0 false))))
