(ns spacewar.game-logic.romulans-hardening-spec
  (:require [spacewar.game-logic.config :as glc]
            [spacewar.game-logic.romulans :as r]
            [speclj.core :refer [describe it should should-not should=]]))

(describe "make-romulan"
  (it "starts invisible, unarmed, and at age 0"
    (let [romulan (r/make-romulan 12 34)]
      (should= 12 (:x romulan))
      (should= 34 (:y romulan))
      (should= 0 (:age romulan))
      (should= :invisible (:state romulan))
      (should= false (:fire-weapon romulan)))))

(describe "romulan-state-transition"
  (it "does not transition when age equals the state duration"
    (with-redefs [rand (constantly 0.9)]
      (should-not (r/romulan-state-transition 10 glc/romulan-invisible-time :invisible))))

  (it "transitions when age is past duration, the millisecond remainder is within ms, and rand is high"
    (with-redefs [rand (constantly 0.9)]
      (should (r/romulan-state-transition 10 (+ glc/romulan-invisible-time 5) :invisible))))

  (it "transitions when the millisecond remainder equals ms"
    (with-redefs [rand (constantly 0.9)]
      (should (r/romulan-state-transition 10 (+ glc/romulan-invisible-time 10) :invisible))))

  (it "does not transition when the millisecond remainder is greater than ms"
    (with-redefs [rand (constantly 0.9)]
      (should-not (r/romulan-state-transition 10 (+ glc/romulan-invisible-time 20) :invisible))))

  (it "does not transition when rand is exactly 0.5"
    (with-redefs [rand (constantly 0.5)]
      (should-not (r/romulan-state-transition 10 (+ glc/romulan-invisible-time 5) :invisible))))

  (it "passes 1 to rand so the roll is on the unit interval"
    (let [seen (atom nil)]
      (with-redefs [rand (fn
                           ([] 0.9)
                           ([n] (reset! seen n) 0.9))]
        (r/romulan-state-transition 10 (+ glc/romulan-invisible-time 5) :invisible)
        (should= 1 @seen)))))
