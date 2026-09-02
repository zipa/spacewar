(ns spacewar.ui.tactical-scan-spec
  (:require
    [spacewar.spec-utils :as ut]
    [speclj.core :refer [should describe context it should= should-be-nil]]
    [spacewar.game-logic.config :as glc]
    [spacewar.game-logic.spec-mother :as mom]
    [spacewar.ui.tactical-scan :refer [alert-background-color
                                       age-color
                                       background-color
                                       explosion-radius
                                       kinetic-shot-style
                                       target-arc]]
    [spacewar.ui.config :as uic :refer [phaser-target torpedo-target]]))

(describe "explosion-radius"
  (it "returns 0 when age is 0"
    (should= 0 (explosion-radius 0 [{:velocity 100 :until 200}])))

  (it "returns velocity when age is 1"
    (should= 100 (explosion-radius 1 [{:velocity 100 :until 200}])))

  (it "returns nil when age exceeds profile duration"
    (should-be-nil (explosion-radius 201 [{:velocity 100 :until 200}])))

  (it "calculates radius with multiple profiles"
    (should= (+ (* 100 200) -20)
             (explosion-radius 201 [{:velocity 100 :until 200}
                                   {:velocity -20 :until 400}])))

  (it "calculates full expansion with multiple profiles"
    (should= (+ (* 100 200) (* 200 -20))
             (explosion-radius 400 [{:velocity 100 :until 200}
                                   {:velocity -20 :until 400}])))

  (it "returns nil when age exceeds all profiles"
    (should-be-nil (explosion-radius 401 [{:velocity 100 :until 200}
                                         {:velocity -20 :until 400}]))))

(describe "age-color"
  (it "returns start color when age is 0"
    (should= [0 0 0]
             (age-color 0 [{:until 100 :colors [[0 0 0] [100 100 100]]}])))

  (it "interpolates color at age 1"
    (should= [1 1 1]
             (age-color 1 [{:until 100 :colors [[0 0 0] [100 100 100]]}])))

  (it "uses first profile when age within first range"
    (should= [1 1 1]
             (age-color 1 [{:until 100 :colors [[0 0 0] [100 100 100]]}
                          {:until 200 :colors [[100 100 100] [255 100 20]]}])))

  (it "uses second profile when age in second range"
    (should= [101 100 99]
             (age-color 101 [{:until 100 :colors [[0 0 0] [100 100 100]]}
                            {:until 200 :colors [[100 100 100] [200 100 0]]}]))))

(describe "target-arc"
  (context "with phaser"
    (it "returns correct arc for bearing 0 spread 0"
      (let [[r start stop] (target-arc {:selected-weapon :phaser
                                       :target-bearing 0
                                       :weapon-spread-setting 0})]
        (should= phaser-target r)
        (should (ut/roughly= -3 start 0.1))
        (should (ut/roughly= 3 stop  0.1))))

    (it "returns correct arc for bearing 0 spread 10"
      (let [[r start stop] (target-arc {:selected-weapon :phaser
                                       :target-bearing 0
                                       :weapon-spread-setting 10})]
        (should= phaser-target r)
        (should (ut/roughly= -5 start 0.1))
        (should (ut/roughly= 5 stop 0.1))))

    (it "returns correct arc for bearing 90 spread 10"
      (let [[r start stop] (target-arc {:selected-weapon :phaser
                                       :target-bearing 90
                                       :weapon-spread-setting 10})]
        (should= phaser-target r)
        (should (ut/roughly= 85 start  0.1))
        (should (ut/roughly= 95 stop  0.1)))))

  (context "with torpedo"
    (it "returns correct arc for bearing 0 spread 0"
      (let [[r start stop] (target-arc {:selected-weapon :torpedo
                                       :target-bearing 0
                                       :weapon-spread-setting 0})]
        (should= torpedo-target r)
        (should (ut/roughly= -3 start  0.1))
        (should (ut/roughly= 3 stop  0.1)))))

  (context "with no weapon"
    (it "returns correct arc for bearing 0 spread 0"
      (let [[r start stop] (target-arc {:selected-weapon :none
                                       :target-bearing 0
                                       :weapon-spread-setting 0})]
        (should= 0 r)
        (should (ut/roughly= -3 start  0.1))
        (should (ut/roughly= 3 stop  0.1))))))

(describe "background color"
  (it "is black during game over"
    (should= uic/black
             (background-color {:update-time 750
                                :game-over-timer 1
                                :ship (mom/make-ship)})))

  (it "is black during the alert flash"
    (should= uic/black
             (background-color {:update-time 100
                                :game-over-timer 0
                                :ship (assoc (mom/make-ship) :hull-damage 50)})))

  (it "is dark red when the ship is damaged"
    (should= uic/dark-red
             (alert-background-color (assoc (mom/make-ship) :hull-damage 10))))

  (it "is dark red when antimatter is critical"
    (should= uic/dark-red
             (alert-background-color (assoc (mom/make-ship) :antimatter 0))))

  (it "is dark yellow when shields are weak"
    (should= uic/dark-yellow
             (alert-background-color (assoc (mom/make-ship) :shields (* 0.5 glc/ship-shields)))))

  (it "is black when the ship is healthy"
    (should= uic/black
             (alert-background-color (mom/make-ship)))))

(describe "kinetic shot style"
  (it "uses the supplied color for a normal shot"
    (should= {:color uic/white :radius 3}
             (kinetic-shot-style {:corbomite false} uic/white)))

  (it "uses red and a larger radius for a corbomite shot"
    (should= {:color uic/red :radius 5}
             (kinetic-shot-style {:corbomite true} uic/white))))

(describe "per-type shot drawing"
  (it "does not collide with the all-shots drawer"
    (let [draw-objects-in (ns-resolve 'spacewar.ui.tactical-scan 'draw-objects-in)
          draw-phaser-shots (ns-resolve 'spacewar.ui.tactical-scan 'draw-phaser-shots)
          state {:w 200 :world {:shots [{:type :phaser :x 1 :y 2}
                                        {:type :kinetic :x 3 :y 4}]}}
          seen (atom nil)]
      (with-redefs-fn {draw-objects-in (fn [_ objects _] (reset! seen objects))}
        #(draw-phaser-shots state))
      (should= [{:type :phaser :x 1 :y 2}] @seen))))