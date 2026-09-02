(ns spacewar.game-logic.ship-hardening-spec
  (:require [spacewar.game-logic.ship :as ship]
            [spacewar.game-logic.spec-mother :as mom]
            [speclj.core :refer [describe it should should-not]]))

(describe "update-destruction"
  (it "destroys the ship at exactly 100 core temperature"
    (should (:destroyed (ship/update-destruction (assoc (mom/make-ship) :core-temp 100)))))

  (it "destroys the ship at exactly 100 life support or hull damage"
    (should (:destroyed (ship/update-destruction (assoc (mom/make-ship) :life-support-damage 100))))
    (should (:destroyed (ship/update-destruction (assoc (mom/make-ship) :hull-damage 100)))))

  (it "does not destroy the ship at 99 in every fatal system"
    (should-not (:destroyed (ship/update-destruction
                              (assoc (mom/make-ship)
                                :life-support-damage 99
                                :hull-damage 99
                                :core-temp 99))))))
