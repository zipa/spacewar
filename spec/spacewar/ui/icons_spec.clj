(ns spacewar.ui.icons-spec
  (:require [spacewar.game-logic.config :as glc]
            [spacewar.ui.config :as uic]
            [spacewar.ui.icons :as icons]
            [speclj.core :refer [describe it should should-be-nil should-not should=]]))

(describe "klingon-state labels"
  (it "formats mission, cruise, and battle state"
    (should= "A:P-n"
             (icons/klingon-state {:mission :seek-and-destroy
                                   :cruise-state :patrol
                                   :battle-state :no-battle})))

  (it "uses defaults for unknown cruise and mission"
    (should= "-:X-K"
             (icons/klingon-state {:mission :unknown
                                   :cruise-state :unknown
                                   :battle-state :kamikazee}))))

(describe "pulse-stroke-weight"
  (it "uses the off weight when inactive"
    (should= 2 (icons/pulse-stroke-weight false 400 5 2)))

  (it "uses the off weight during the low pulse"
    (should= 2 (icons/pulse-stroke-weight true 100 5 2)))

  (it "uses the on weight during the high pulse"
    (should= 5 (icons/pulse-stroke-weight true 400 5 2))))

(describe "pulsar-visible?"
  (it "always shows non-pulsar stars"
    (should (icons/pulsar-visible? :o 400)))

  (it "shows a pulsar during the on half of the cycle"
    (should (icons/pulsar-visible? :pulsar 100)))

  (it "hides a pulsar during the off half of the cycle"
    (should-not (icons/pulsar-visible? :pulsar 400))))

(describe "transport-color"
  (it "is orange for antimatter"
    (should= uic/orange (icons/transport-color :antimatter)))

  (it "is yellow for dilithium"
    (should= uic/yellow (icons/transport-color :dilithium))))

(describe "klingon shield appearance"
  (it "is absent at full shields"
    (should-be-nil (icons/klingon-shield-appearance glc/klingon-shields)))

  (it "scales radius and percent when damaged"
    (let [appearance (icons/klingon-shield-appearance (/ glc/klingon-shields 2))]
      (should= 1/2 (:pct appearance))
      (should= 45 (:radius appearance)))))

(describe "shield-fill-color"
  (it "dims alpha when not flickering"
    (should= [255 127.5 0 100] (icons/shield-fill-color 0.5 false)))

  (it "scales alpha when flickering"
    (should= [255 127.5 0 50.0] (icons/shield-fill-color 0.5 true))))

(describe "ship heading and velocity"
  (it "defaults missing heading to 0"
    (should= 0 (icons/ship-heading {})))

  (it "uses the ship's heading"
    (should= 90 (icons/ship-heading {:heading 90})))

  (it "defaults missing velocity to zero"
    (should= [0 0] (icons/ship-velocity-vector {})))

  (it "scales the ship's velocity"
    (should= [(* 2 uic/velocity-vector-scale) 0]
             (icons/ship-velocity-vector {:velocity [2 0]}))))
