(ns spacewar.game-logic.explosions-hardening-spec
  (:require [spacewar.game-logic.explosions :as explosions]
            [speclj.core :refer [describe it should should=]]))

(describe "explosion construction"
  (it "starts a new explosion at age 0"
    (should= 0 (:age (explosions/->explosion :phaser {:x 1 :y 2})))))

(describe "explosion lifetime"
  (it "removes an explosion once its age reaches the profile duration"
    (should= []
             (:explosions
               (explosions/update-explosions 10 {:explosions [{:age 990 :type :phaser}]})))))

(describe "fragment velocity"
  (it "keeps fragment speed between 80% and 100% of the given velocity"
    (let [velocity 10
          fragments (explosions/make-fragments 50 {:x 0 :y 0} velocity)]
      (should (every? #(<= (* 0.8 velocity) (:velocity %) velocity) fragments)))))
