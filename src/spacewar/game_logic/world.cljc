(ns spacewar.game-logic.world
  (:require [clojure.spec.alpha :as s]
            [spacewar.game-logic.bases :as bases]
            [spacewar.game-logic.clouds :as clouds]
            [spacewar.game-logic.config :as glc]
            [spacewar.game-logic.explosions :as explosions]
            [spacewar.game-logic.klingons :as klingons]
            [spacewar.game-logic.notifications :as notifications]
            [spacewar.game-logic.romulans :as romulans]
            [spacewar.game-logic.ship :as ship]
            [spacewar.game-logic.shots :as shots]
            [spacewar.game-logic.stars :as stars]
            [spacewar.util :as util]))

(def version "20250417")

(s/def ::update-time number?)
(s/def ::transport-check-time number?)
(s/def ::shots :spacewar.game-logic.shots/shots)
(s/def ::ms int?)
(s/def ::game-over-timer int?)
(s/def ::minutes integer?)
(s/def ::version string?)
(s/def ::deaths int?)
(s/def ::klingons-killed int?)
(s/def ::romulans-killed int?)
(s/def ::transport-routes set?)
(s/def ::messages (s/coll-of keyword?))

(s/def ::world (s/keys :req-un [:spacewar.game-logic.explosions/explosions
                                :spacewar.game-logic.klingons/klingons
                                :spacewar.game-logic.ship/ship
                                :spacewar.game-logic.stars/stars
                                :spacewar.game-logic.bases/bases
                                :spacewar.game-logic.bases/transports
                                :spacewar.game-logic.clouds/clouds
                                :spacewar.game-logic.romulans/romulans
                                ::shots
                                ::update-time
                                ::transport-check-time
                                ::ms
                                ::game-over-timer
                                ::minutes
                                ::version
                                ::deaths
                                ::klingons-killed
                                ::romulans-killed
                                ::transport-routes]
                       :opt-un [::messages]))

(defn- add-base [world base]
  (-> world (ship/add-transport-routes-to base)
      (update :bases conj base)))

(defn make-initial-world []
  (let [ship (ship/initialize)
        stars (stars/initialize)
        bases (bases/make-random-bases stars)
        world {:stars stars
               :klingons (klingons/initialize)
               :ship ship
               :bases []
               :transports []
               :clouds []
               :romulans []
               :update-time 0
               :transport-check-time 0
               :explosions []
               :shots []
               :ms 0
               :game-over-timer 0
               :minutes 0
               :version version
               :deaths 0
               :klingons-killed 0
               :romulans-killed 0
               :transport-routes #{}
               :messages []}
        world (reduce #(add-base %1 %2) world bases)]
    (-> world
        (notifications/notify :clear-messages)
        (notifications/notify :welcome)
        (notifications/notify :save-federation))))

(defn starting-world [saved-world]
  (if (and saved-world (= version (:version saved-world)))
    (notifications/notify saved-world :old-game)
    (make-initial-world)))

(defn- debug-position-ship-handler [event world]
  (println "debug-position-ship-handler" (:pos event))
  (let [ship (:ship world)
        [x y] (:pos event)
        ship (assoc ship :x x :y y)]
    (assoc world :ship ship)))

(defn- debug-dilithium-cloud-handler [event world]
  (println event)
  (let [[x y] (:pos event)
        clouds (:clouds world)
        cloud (clouds/make-cloud x y glc/klingon-debris)
        clouds (conj clouds cloud)]
    (assoc world :clouds clouds)))

(defn- debug-resupply-ship [_ world]
  (let [ship (:ship world)
        ship (assoc ship :antimatter glc/ship-antimatter
                         :dilithium glc/ship-dilithium
                         :torpedos glc/ship-torpedos
                         :kinetics glc/ship-kinetics
                         :shields glc/ship-shields
                         :core-temp 0
                         :hull-damage 0
                         :weapons-damage 0
                         :life-support-damage 0
                         :warp-damage 0
                         :impulse-damage 0
                         :sensor-damage 0)]
    (assoc world :ship ship)))

(defn- debug-add-klingon [event world]
  (println event)
  (let [[x y] (:pos event)
        klingons (:klingons world)
        klingon (klingons/make-klingon x y)
        klingon (update klingon :antimatter / 2)
        klingons (conj klingons klingon)]
    (assoc world :klingons klingons)))

(defn- debug-add-kamikazee-klingon [event world]
  (println event)
  (let [[x y] (:pos event)
        klingons (:klingons world)
        klingon (klingons/make-klingon x y)
        klingon (update klingon :antimatter / 2)
        klingon (assoc klingon :battle-state :kamikazee)
        klingons (conj klingons klingon)]
    (assoc world :klingons klingons)))

(defn- debug-new-klingon-from-praxis [event world]
  (println event)
  (klingons/new-klingon-from-praxis world))

(defn- debug-add-romulan [event world]
  (println event)
  (let [[x y] (:pos event)
        romulans (:romulans world)
        romulan (romulans/make-romulan x y)
        romulans (conj romulans romulan)]
    (assoc world :romulans romulans)))

(defn- new-game [_event _world]
  (make-initial-world))

(defn- debug-corbomite-device-installed [_event world]
  (let [ship (:ship world)
        ship (assoc ship :corbomite-device-installed true)]
    (assoc world :ship ship)))

(defn- debug-explosion [event world]
  (let [[x y] (:pos event)
        explosion (explosions/->explosion :corbomite-device {:x x :y y})
        explosions (:explosions world)
        explosions (conj explosions explosion)]
    (assoc world :explosions explosions)))

(defn- debug-add-pulsar [event world]
  (let [[x y] (:pos event)
        pulsar (stars/make-pulsar)
        pulsar (assoc pulsar :x x :y y)
        stars (:stars world)
        stars (conj stars pulsar)]
    (assoc world :stars stars)))

(defn- debug-klingon-stats [_event world]
  (swap! glc/klingon-stats not)
  world)

(defn- process-game-events [events world]
  (let [[_ world] (->> [events world]
                       (util/handle-event :debug-position-ship debug-position-ship-handler)
                       (util/handle-event :debug-dilithium-cloud debug-dilithium-cloud-handler)
                       (util/handle-event :debug-resupply-ship debug-resupply-ship)
                       (util/handle-event :debug-add-klingon debug-add-klingon)
                       (util/handle-event :debug-add-kamikazee-klingon debug-add-kamikazee-klingon)
                       (util/handle-event :debug-add-romulan debug-add-romulan)
                       (util/handle-event :debug-new-klingon-from-praxis debug-new-klingon-from-praxis)
                       (util/handle-event :debug-corbomite-device-installed debug-corbomite-device-installed)
                       (util/handle-event :debug-explosion debug-explosion)
                       (util/handle-event :debug-add-pulsar debug-add-pulsar)
                       (util/handle-event :debug-klingon-stats debug-klingon-stats)
                       (util/handle-event :new-game new-game))]
    world))

(defn process-events [events world]
  (let [world (ship/process-events events world)
        world (process-game-events events world)
        world (shots/process-events events world)]
    world))

(defn- ship-explosion [ship]
  (explosions/->explosion :ship ship))

(defn- game-won [_ms world]
  (if (zero? (count (:klingons world)))
    (notifications/notify world :you-win)
    world))

(defn- begin-destruction [{:keys [ship explosions] :as world}]
  (-> world
      (notifications/notify :you-died)
      (assoc :explosions (conj explosions (ship-explosion ship))
             :game-over-timer 1)))

(defn- finish-destruction [world]
  (assoc world :game-over-timer 0
               :ship (ship/reincarnate)
               :deaths (inc (:deaths world))))

(defn game-over [_ms {:keys [ship game-over-timer explosions] :as world}]
  (cond (not (:destroyed ship)) world
        (zero? game-over-timer) (begin-destruction world)
        (empty? explosions) (finish-destruction world)
        :else (assoc world :game-over-timer 1)))

(defn update-world [ms world]
  (->> world
       (game-won ms)
       (game-over ms)
       (ship/update-ship ms)
       (shots/update-shots ms)
       (explosions/update-explosions ms)
       (clouds/update-clouds ms)
       (klingons/update-klingons ms)
       (bases/update-bases ms)
       (romulans/update-romulans ms)))

(defn update-world-per-second [world]
  (->> world
       (klingons/update-klingons-per-second)
       (romulans/update-romulans-per-second)))

(defn- increment-minutes [world]
  (let [minutes (get world :minutes 0)]
    (assoc world :minutes (inc minutes))))

(defn update-world-per-minute [world]
  (->> world
       (increment-minutes)
       (klingons/update-klingons-per-minute)))

(defn crossed-interval? [time last-update-time interval]
  (not= (int (/ time interval)) (int (/ last-update-time interval))))

(defn- apply-if [world pred f]
  (if pred (f world) world))

(defn apply-periodic-updates [world time last-update-time]
  (-> world
      (apply-if (crossed-interval? time last-update-time 1000) update-world-per-second)
      (apply-if (crossed-interval? time last-update-time 60000) update-world-per-minute)))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-09-02T15:09:31.525041-05:00", :module-hash "464632977", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line nil, :hash "-889253587"} {:id "def/version", :kind "def", :line 15, :end-line nil, :hash "-1656203756"} {:id "form/2/s/def", :kind "s/def", :line 17, :end-line nil, :hash "-1598849229"} {:id "form/3/s/def", :kind "s/def", :line 18, :end-line nil, :hash "-299731917"} {:id "form/4/s/def", :kind "s/def", :line 19, :end-line nil, :hash "-692760599"} {:id "form/5/s/def", :kind "s/def", :line 20, :end-line nil, :hash "240833869"} {:id "form/6/s/def", :kind "s/def", :line 21, :end-line nil, :hash "439195666"} {:id "form/7/s/def", :kind "s/def", :line 22, :end-line nil, :hash "701100934"} {:id "form/8/s/def", :kind "s/def", :line 23, :end-line nil, :hash "-1223856244"} {:id "form/9/s/def", :kind "s/def", :line 24, :end-line nil, :hash "1916353166"} {:id "form/10/s/def", :kind "s/def", :line 25, :end-line nil, :hash "2038947425"} {:id "form/11/s/def", :kind "s/def", :line 26, :end-line nil, :hash "1521916679"} {:id "form/12/s/def", :kind "s/def", :line 27, :end-line nil, :hash "1986653577"} {:id "form/13/s/def", :kind "s/def", :line 28, :end-line nil, :hash "1584752679"} {:id "form/14/s/def", :kind "s/def", :line 30, :end-line nil, :hash "-224615598"} {:id "defn-/add-base", :kind "defn-", :line 51, :end-line nil, :hash "1482379016"} {:id "defn/make-initial-world", :kind "defn", :line 55, :end-line nil, :hash "1324735204"} {:id "defn/starting-world", :kind "defn", :line 85, :end-line nil, :hash "-2020382110"} {:id "defn-/debug-position-ship-handler", :kind "defn-", :line 90, :end-line nil, :hash "-1526440406"} {:id "defn-/debug-dilithium-cloud-handler", :kind "defn-", :line 97, :end-line nil, :hash "348751863"} {:id "defn-/debug-resupply-ship", :kind "defn-", :line 105, :end-line nil, :hash "-923993429"} {:id "defn-/debug-add-klingon", :kind "defn-", :line 121, :end-line nil, :hash "-1869814293"} {:id "defn-/debug-add-kamikazee-klingon", :kind "defn-", :line 130, :end-line nil, :hash "-2060589167"} {:id "defn-/debug-new-klingon-from-praxis", :kind "defn-", :line 140, :end-line nil, :hash "890306829"} {:id "defn-/debug-add-romulan", :kind "defn-", :line 144, :end-line nil, :hash "-1397971370"} {:id "defn-/new-game", :kind "defn-", :line 152, :end-line nil, :hash "-1741216777"} {:id "defn-/debug-corbomite-device-installed", :kind "defn-", :line 155, :end-line nil, :hash "-700888416"} {:id "defn-/debug-explosion", :kind "defn-", :line 160, :end-line nil, :hash "-813886522"} {:id "defn-/debug-add-pulsar", :kind "defn-", :line 167, :end-line nil, :hash "-837702864"} {:id "defn-/debug-klingon-stats", :kind "defn-", :line 175, :end-line nil, :hash "-20498911"} {:id "defn-/process-game-events", :kind "defn-", :line 179, :end-line nil, :hash "-1781921048"} {:id "defn/process-events", :kind "defn", :line 195, :end-line nil, :hash "-38114930"} {:id "defn-/ship-explosion", :kind "defn-", :line 201, :end-line nil, :hash "-208481972"} {:id "defn-/game-won", :kind "defn-", :line 204, :end-line nil, :hash "-1734629986"} {:id "defn-/begin-destruction", :kind "defn-", :line 209, :end-line nil, :hash "2002555391"} {:id "defn-/finish-destruction", :kind "defn-", :line 215, :end-line nil, :hash "1191517208"} {:id "defn/game-over", :kind "defn", :line 220, :end-line nil, :hash "-1331723663"} {:id "defn/update-world", :kind "defn", :line 226, :end-line nil, :hash "-1447837442"} {:id "defn/update-world-per-second", :kind "defn", :line 238, :end-line nil, :hash "972965395"} {:id "defn-/increment-minutes", :kind "defn-", :line 243, :end-line nil, :hash "696082323"} {:id "defn/update-world-per-minute", :kind "defn", :line 247, :end-line nil, :hash "283019495"} {:id "defn/crossed-interval?", :kind "defn", :line 252, :end-line nil, :hash "-617945253"} {:id "defn-/apply-if", :kind "defn-", :line 255, :end-line nil, :hash "1100340720"} {:id "defn/apply-periodic-updates", :kind "defn", :line 258, :end-line nil, :hash "-612543164"}]}
;; clj-mutate-manifest-end
