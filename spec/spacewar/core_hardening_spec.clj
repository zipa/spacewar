(ns spacewar.core-hardening-spec
  (:require [spacewar.core :refer [frame-timing frames-per-second]]
            [speclj.core :refer [describe it should=]]))

(describe "frames-per-second"
  (it "is 0 for an empty sample"
    (should= 0 (frames-per-second []))))

(describe "frame-timing restart detection"
  (it "uses elapsed time when the gap is 500ms"
    (should= {:ms 500 :last-update-time 100}
             (frame-timing 600 100)))

  (it "does not treat a 400ms gap as a restart even if the sum of the clocks is large"
    (should= {:ms 200 :last-update-time 200}
             (frame-timing 400 200))))
