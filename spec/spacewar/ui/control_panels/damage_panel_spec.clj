(ns spacewar.ui.control-panels.damage-panel-spec
  (:require [spacewar.ui.control-panels.damage-panel :as damage]
            [speclj.core :refer [describe it should=]]))

(describe "damage-level"
  (it "is 0 when undamaged"
    (should= 0 (damage/damage-level 0)))

  (it "is 1 through one third damage"
    (should= 1 (damage/damage-level 1))
    (should= 1 (damage/damage-level 33)))

  (it "is 2 through two thirds damage"
    (should= 2 (damage/damage-level 34))
    (should= 2 (damage/damage-level 66)))

  (it "is 3 through 99"
    (should= 3 (damage/damage-level 67))
    (should= 3 (damage/damage-level 99)))

  (it "is 4 at complete damage"
    (should= 4 (damage/damage-level 100))))
