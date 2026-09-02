(ns spacewar.core-spec
  (:require [spacewar.core :refer [add-frame-time
                                   frame-timing
                                   frames-per-second]]
            [speclj.core :refer [describe it should=]]))

(describe "frame rate"
  (it "adds to empty frame time list"
    (should= [10] (:frame-times (add-frame-time 10 {:frame-times []}))))

  (it "accumulates up to 10 times"
    (let [context (reduce #(add-frame-time %2 %1)
                          {:frame-times []}
                          [1 2 3 4 5 6 7 8 9 10 11 12])]
      (should= [3 4 5 6 7 8 9 10 11 12] (:frame-times context))))

  (it "calculates frame rates"
    (should= 0 (frames-per-second []))
    (should= 1 (frames-per-second [1000]))
    (should= 22 (int (frames-per-second [30 40 50 60])))))

(describe "frame timing"
  (it "uses the elapsed time when the clock is advancing normally"
    (should= {:ms 40 :last-update-time 100}
             (frame-timing 140 100)))

  (it "clamps a restart gap to 1ms"
    (should= {:ms 1 :last-update-time 1000}
             (frame-timing 1000 0)))

  (it "clamps non-positive elapsed time to 1ms"
    (should= {:ms 1 :last-update-time 100}
             (frame-timing 100 100))))
