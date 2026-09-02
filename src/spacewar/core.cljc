(ns spacewar.core
  (:require [quil.core :as q #?@(:cljs [:include-macros true])]
            [quil.middleware :as m]
            [spacewar.ui.complex :as main-viewer]
            [spacewar.ui.view-frame :as view-frame]
            [spacewar.ui.protocols :as p]
            [spacewar.ui.messages :as messages]
            [spacewar.game-logic.config :as glc]
            [spacewar.game-logic.ship :as ship]
            [spacewar.game-logic.stars :as stars]
            [spacewar.game-logic.klingons :as klingons]
            [spacewar.game-logic.bases :as bases]
            [spacewar.game-logic.shots :as shots]
            [spacewar.game-logic.explosions :as explosions]
            [spacewar.game-logic.clouds :as clouds]
            [spacewar.game-logic.romulans :as romulans]
            [spacewar.util :as util]
            [clojure.spec.alpha :as s]
            #?(:clj [clojure.java.io :as io])
            #?(:cljs [clojure.edn :as edn])))

#?(:cljs (enable-console-print!))

(def version "20250417")

(s/def ::update-time number?)
(s/def ::transport-check-time number?)
(s/def ::shots :spacewar.game-logic.shots/shots)
(s/def ::ms int?)
(s/def ::text string?)
(s/def ::duration int?)
(s/def ::message (s/keys :req-un [::text ::duration]))
(s/def ::game-over-timer int?)
(s/def ::minutes integer?)
(s/def ::version string?)
(s/def ::deaths int?)
(s/def ::klingons-killed int?)
(s/def ::romulans-killed int?)
(s/def ::transport-routes set?)

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
                                ::transport-routes]))

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
               :transport-routes #{}}
        world (reduce #(add-base %1 %2) world bases)]
    (view-frame/clear-messages!)
    (messages/send-message :welcome)
    (messages/send-message :save-federation)
    world))


(defn game-saved? []
  #?(:clj  (.exists (io/file "spacewar.world"))
     :cljs (and (exists? js/localStorage)
                (.getItem js/localStorage "spacewar.world"))))

(defn- read-saved-world []
  #?(:clj  (read-string (slurp "spacewar.world"))
     :cljs (edn/read-string (.getItem js/localStorage "spacewar.world"))))

(defn starting-world [saved-world]
  (if (and saved-world (= version (:version saved-world)))
    (do (messages/send-message :old-game) saved-world)
    (do (messages/send-message :new-version) (make-initial-world))))

(defn setup []
  (let [vmargin 30
        hmargin 5
        saved-world (when (game-saved?) (read-saved-world))
        world (starting-world saved-world)]
    (q/frame-rate glc/frame-rate)
    (q/color-mode :rgb)
    (q/background 200 200 200)
    (q/ellipse-mode :corner)
    (q/rect-mode :corner)
    {:state (p/setup
              (main-viewer/->complex
                {:x hmargin :y vmargin
                 :w (- (q/width) (* 2 hmargin))
                 :h (- (q/height) (* 2 vmargin))}))
     :world world
     :base-time (:update-time world)
     :fonts #?(:clj  {:lcars (q/create-font "Helvetica-Bold" 24)
                      :lcars-small (q/create-font "Arial" 18)
                      :messages (q/create-font "Bank Gothic" 30)}
               :cljs {:lcars "Helvetica-Bold"
                      :lcars-small "Helvetica"
                      :messages "Bank Gothic"})
     :frame-times []}))

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
  (klingons/new-klingon-from-praxis world)
  )

(defn- debug-add-romulan [event world]
  (println event)
  (let [[x y] (:pos event)
        romulans (:romulans world)
        romulan (romulans/make-romulan x y)
        romulans (conj romulans romulan)]
    (assoc world :romulans romulans)))

(defn- new-game [event world]
  (make-initial-world))

(defn- debug-corbomite-device-installed [event world]
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
    (assoc world :stars stars))
  )

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
                       (util/handle-event :new-game new-game)
                       )]
    world))

(defn process-events [events world]
  (let [world (ship/process-events events world)
        world (process-game-events events world)
        world (shots/process-events events world)]
    world))

(defn- ship-explosion [ship]
  (explosions/->explosion :ship ship))

(defn- game-won [_ms world]
  (when (zero? (count (:klingons world)))
    (messages/send-message :you-win))
  world)

(defn- begin-destruction [{:keys [ship explosions] :as world}]
  (messages/send-message :you-died)
  (assoc world :explosions (conj explosions (ship-explosion ship))
               :game-over-timer 1))

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
       (romulans/update-romulans ms)
       (view-frame/update-messages ms)
       ))

(defn update-world-per-second [world]
  (messages/add-messages! world)
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

(defn add-frame-time [frame-time context]
  (let [frame-times (->
                      context
                      :frame-times
                      (conj frame-time))
        frame-times (if (> (count frame-times) 10)
                      (vec (rest frame-times))
                      frame-times)]
    (assoc context :frame-times frame-times))
  )

(defn frames-per-second [frame-times]
  (if (empty? frame-times)
    0
    (let [sum (reduce + frame-times)
          mean (/ sum (count frame-times))
          fps (/ 1000 mean)]
      fps)))


(defn frame-timing [time last-update-time]
  (let [raw (- time last-update-time)
        last (if (> raw 500) time last-update-time)]
    {:ms (max 1 (- time last))
     :last-update-time last}))

(defn crossed-interval? [time last-update-time interval]
  (not= (int (/ time interval)) (int (/ last-update-time interval))))

(defn- apply-if [world pred f]
  (if pred (f world) world))

(defn apply-periodic-updates [world time last-update-time]
  (-> world
      (apply-if (crossed-interval? time last-update-time 1000) update-world-per-second)
      (apply-if (crossed-interval? time last-update-time 60000) update-world-per-minute)))

(defn- save-world [world]
  #?(:clj  (spit "spacewar.world" world)
     :cljs (when (exists? js/localStorage)
             (.setItem js/localStorage "spacewar.world" world))))

(defn update-state [context]
  (let [{:keys [world base-time]} context
        time (+ base-time (q/millis))
        last-update-time (:update-time world)
        timing (frame-timing time last-update-time)
        ms (:ms timing)
        last-update-time (:last-update-time timing)
        context (add-frame-time ms context)
        fps (frames-per-second (:frame-times context))
        complex (:state context)
        world (assoc world :update-time time :ms ms :fps fps)
        [complex events] (p/update-state complex world)
        world (process-events (flatten events) world)
        world (update-world ms world)
        world (apply-periodic-updates world time last-update-time)]
    (when (crossed-interval? time last-update-time 5000)
      (save-world world))
    (assoc context :state complex :world world)))

(defn draw-state [{:keys [state]}]
  (q/fill 200 200 200)
  (q/rect-mode :corner)
  (q/no-stroke)
  (q/rect 0 0 (q/width) (q/height))
  (p/draw state)
  )

#?(:clj
   (defn ^:export -main [& args]
     (q/defsketch space-war
                  :title "Space War"
                  :size [(- (q/screen-width) 10) (- (q/screen-height) 40)]
                  :setup setup
                  :update update-state
                  :draw draw-state
                  :middleware [m/fun-mode]
                  )
     args)

   :cljs
   (defn ^:export -main []
     (let [size [(max (- (.-scrollWidth (.-body js/document)) 20) 900)
                 (max (- (.-innerHeight js/window) 25) 700)]]
       (q/defsketch space-war
                    :title "Space War"
                    :size size
                    :setup setup
                    :update update-state
                    :draw draw-state
                    :middleware [m/fun-mode]
                    :host "space-war"
                    )

       )
     )
   )