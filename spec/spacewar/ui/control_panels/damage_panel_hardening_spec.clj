(ns spacewar.ui.control-panels.damage-panel-hardening-spec
  (:require [spacewar.ui.control-panels.damage-panel :as damage]
            [speclj.core :refer [describe it should=]]))

(describe "damage-level boundaries"
  (it "is 0 only at zero damage"
    (should= 0 (damage/damage-level 0))
    (should= 1 (damage/damage-level 1)))

  (it "uses 33 as the top of level 1"
    (should= 1 (damage/damage-level 33))
    (should= 2 (damage/damage-level 34))))
