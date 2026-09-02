(ns spacewar.game-logic.shots-hardening-spec
  (:require [spacewar.game-logic.shots :as shots]
            [speclj.core :refer [describe it should=]]))

(describe "shot construction"
  (it "starts unfired shots at range 0 without special flags"
    (let [shot (shots/->shot 1 2 90 :kinetic)]
      (should= 1 (:x shot))
      (should= 2 (:y shot))
      (should= 90 (:bearing shot))
      (should= :kinetic (:type shot))
      (should= 0 (:range shot))
      (should= false (:corbomite shot))
      (should= false (:kamikazee shot)))))
