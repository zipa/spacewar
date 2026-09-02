(ns spacewar.core-spec
  (:require [spacewar.core :as core :refer [add-frame-time
                                            apply-periodic-updates
                                            crossed-interval?
                                            frame-timing
                                            frames-per-second
                                            game-over
                                            make-initial-world
                                            process-events
                                            starting-world
                                            update-world
                                            version]]
            [spacewar.game-logic.ship :as ship]
            [spacewar.game-logic.spec-mother :as mom]
            [spacewar.ui.messages :as messages]
            [speclj.core :refer [context describe it should should-not should-not-be-nil should-not= should= should> with]]))

(describe "initial world"
  (it "is created correctly"
    (should-not-be-nil (make-initial-world))
    (should (mom/valid-world? (make-initial-world)))))

(describe "view selection events"
  (it "handles strategic scan"
    (should= :strat-view
             (-> (process-events [{:event :strategic-scan}] {:ship {}})
                 :ship :selected-view)))
  (it "handles tactical scan"
    (should= :tact-view
             (-> (process-events [{:event :tactical-scan}] {:ship {}})
                 :ship :selected-view)))
  (it "handles front view"
    (should= :front-view
             (-> (process-events [{:event :front-view}] {:ship {}})
                 :ship :selected-view))))

(declare ship world)
(describe "engine panel events"
  (with ship (mom/make-ship))
  (with world (assoc (mom/make-world) :ship @ship))


  (context "engine selection"
    (it "selects warp from none"
      (let [world (assoc-in @world [:ship :selected-engine] :none)]
        (should= :warp (->> world
                            (process-events [{:event :select-warp}])
                            :ship :selected-engine)))))
  (it "selects warp from impulse"
    (let [world (assoc-in @world [:ship :selected-engine] :impulse)]
      (should= :warp
               (->> world
                    (process-events [{:event :select-warp}])
                    :ship :selected-engine))))
  (it "deselects warp"
    (let [world (assoc-in @world [:ship :selected-engine] :warp)]
      (should= :none
               (->> world
                    (process-events [{:event :select-warp}])
                    :ship :selected-engine))))

  (it "handles engine direction"
    (should= 45
             (-> (process-events [{:event :engine-direction :angle 45}] {:ship {}})
                 :ship :heading-setting)))

  (it "handles engine power"
    (should= 75
             (-> (process-events [{:event :engine-power :value 75}] {:ship {}})
                 :ship :engine-power-setting)))

  (it "handles engine engage"
    (should= 0
             (->> {:ship {:engine-power-setting 50 :selected-engine :warp}}
                  (process-events [{:event :engine-engage}])
                  :ship :engine-power-setting))))

(describe "weapons panel events"
  (with ship (mom/make-ship))
  (with world (assoc (mom/make-world) :ship @ship))

  (context "weapon selection"
    (it "selects phaser from none"
      (let [world (assoc-in @world [:ship :selected-weapon] :none)]
        (should= :phaser
                 (->> world
                      (process-events [{:event :select-phaser}])
                      :ship :selected-weapon)))))

  (it "handles weapon direction"
    (should= 90
             (->> @world
                  (process-events [{:event :weapon-direction :angle 90}])
                  :ship :target-bearing)))

  (it "handles weapon number"
    (let [new-world (process-events [{:event :weapon-number :value 2}] @world)]
      (should= 2 (:weapon-number-setting (:ship new-world)))))

  (it "handles weapon spread"
    (should= 15
             (->> @world
                  (process-events [{:event :weapon-spread :value 15}])
                  :ship :weapon-spread-setting)))

  (it "fires one phaser"
    (let [ship (assoc @ship :x 100 :y 200 :selected-weapon :phaser
                            :weapon-number-setting 1 :target-bearing 90 :weapon-spread-setting 0)
          world (assoc @world :ship ship)
          new-world (process-events [{:event :weapon-fire}] world)]
      (should= [{:x 100 :y 200 :bearing 90 :range 0 :type :phaser :corbomite false}]
               (:shots new-world)))))

(describe "updating the world"
  (with world (mom/make-world))
  (with ship (mom/make-ship))
  (it "rotates towards heading"
    (let [ship (assoc @ship :heading-setting 90 :heading 70)
          world (assoc @world :ship ship)
          world (update-world 1000 world)]
      (should= 90 (int (:heading (:ship world))))))

  (it "impulse moves ship"
    (let [ship (assoc @ship :impulse 1 :y 1000)
          world (assoc @world :ship ship)
          world (update-world 1000 world)]
      (should> (first (:velocity (:ship world))) 0)))

  (it "warp charges warp field"
    (let [ship (assoc @ship :warp 1)
          world (assoc @world :ship ship)
          world (update-world 1000 world)]
      (should= 2000 (int (:warp-charge (:ship world)))))))

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

(describe "starting world"
  (it "restores a matching saved world"
    (let [saved (assoc (mom/make-world) :version version)]
      (with-redefs [messages/send-message (fn [_])]
        (should= saved (starting-world saved)))))

  (it "starts a new world when there is no save"
    (with-redefs [messages/send-message (fn [_])]
      (should-not-be-nil (starting-world nil))))

  (it "starts a new world when the save version does not match"
    (let [saved (assoc (mom/make-world) :version "old")]
      (with-redefs [messages/send-message (fn [_])]
        (should-not= saved (starting-world saved))))))

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

(describe "crossed-interval?"
  (it "detects a new second"
    (should (crossed-interval? 1000 999 1000))
    (should-not (crossed-interval? 1500 1001 1000)))

  (it "detects a new minute"
    (should (crossed-interval? 60000 59999 60000))
    (should-not (crossed-interval? 61000 60000 60000))))

(describe "periodic world updates"
  (it "applies the per-second update on a second boundary"
    (let [world (mom/make-world)]
      (with-redefs [core/update-world-per-second (fn [w] (assoc w :second true))
                    core/update-world-per-minute (fn [w] (assoc w :minute true))]
        (let [updated (apply-periodic-updates world 1000 999)]
          (should (:second updated))
          (should-not (:minute updated))))))

  (it "applies the per-minute update on a minute boundary"
    (let [world (mom/make-world)]
      (with-redefs [core/update-world-per-minute (fn [w] (assoc w :minute true))
                    core/update-world-per-second (fn [w] (assoc w :second true))]
        (let [updated (apply-periodic-updates world 60000 59999)]
          (should (:minute updated))
          (should (:second updated)))))))

(describe "game over"
  (it "leaves an intact ship unchanged"
    (let [world (mom/make-world)]
      (should= world (game-over 10 world))))

  (it "starts destruction when the ship is first destroyed"
    (let [world (assoc (mom/make-world)
                  :ship (assoc (mom/make-ship) :destroyed true)
                  :game-over-timer 0
                  :explosions [])]
      (with-redefs [messages/send-message (fn [_])]
        (let [updated (game-over 10 world)]
          (should= 1 (:game-over-timer updated))
          (should= 1 (count (:explosions updated)))
          (should (:destroyed (:ship updated)))))))

  (it "holds the destroyed ship while explosions remain"
    (let [world (assoc (mom/make-world)
                  :ship (assoc (mom/make-ship) :destroyed true)
                  :game-over-timer 1
                  :explosions [{:type :ship}])]
      (let [updated (game-over 10 world)]
        (should= 1 (:game-over-timer updated))
        (should (:destroyed (:ship updated)))
        (should= 0 (:deaths updated)))))

  (it "reincarnates after explosions finish"
    (let [world (assoc (mom/make-world)
                  :ship (assoc (mom/make-ship) :destroyed true)
                  :game-over-timer 1
                  :explosions []
                  :deaths 2)
          reincarnated (assoc (mom/make-ship) :x 9 :y 9)]
      (with-redefs [ship/reincarnate (fn [] reincarnated)]
        (let [updated (game-over 10 world)]
          (should= 0 (:game-over-timer updated))
          (should= reincarnated (:ship updated))
          (should= 3 (:deaths updated)))))))