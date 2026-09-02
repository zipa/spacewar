(ns spacewar.ui.messages
  (:require [spacewar.game-logic.config :as glc]
            [spacewar.ui.view-frame :as view-frame]))

(def last-message (atom nil))

(defn- msg
  ([text]
   (view-frame/add-message! text 4000))
  ([text duration]
   (view-frame/add-message! text duration)))

(defn- percent [value max]
  (int (* 100 (/ value max))))

(defn- percent-text [prefix value max suffix]
  (str prefix (percent value max) suffix))

(def status-message-text
  {:life-damage (constantly "Life Support Damage.")
   :life-severe (constantly "Life Support Damage Severe!")
   :life-critical (constantly "Life Support Critical!")
   :hull-damage (constantly "Hull Damage.")
   :hull-severe (constantly "Hull Damage Severe!")
   :hull-critical (constantly "Hull Critical!")
   :sensor-damage (constantly "Sensor Damage.")
   :sensor-severe (constantly "Sensor Damage Severe!")
   :sensor-critical (constantly "Sensors Critical!")
   :impulse-damage (constantly "Impulse Damage.")
   :impulse-severe (constantly "Impulse Damage Severe!")
   :impulse-critical (constantly "Impulse Critical!")
   :warp-damage (constantly "Warp Damage.")
   :warp-severe (constantly "Warp Damage Severe!")
   :warp-critical (constantly "Warp Critical!")
   :weapons-damage (constantly "Weapons Damage.")
   :weapons-severe (constantly "Weapons Damage Severe!")
   :weapons-critical (constantly "Weapons Critical!")
   :shields-charged (constantly "Shields Fully Charged.")
   :shields-ready (constantly "Shields Battle Ready.")
   :shields-damaged (fn [value max] (percent-text "Shields Holding. " value max "%."))
   :shields-severe (fn [value max] (percent-text "Shields Weak." value max "%!"))
   :shields-critical (constantly "Shields Critical!")
   :antimatter-full (constantly "Antimatter Full.")
   :antimatter-topped-off (constantly "Antimatter Topped Off.")
   :antimatter-high (fn [value max] (percent-text "Antimatter High. " value max "%."))
   :antimatter-low (fn [value max] (percent-text "Antimatter Low. " value max "%!"))
   :antimatter-critical (constantly "Antimatter Critical!")
   :dilithium-full (constantly "Dilithium Full.")
   :dilithium-topped-off (constantly "Dilithium Topped Off.")
   :dilithium-high (fn [value max] (percent-text "Dilithium High. " value max "%."))
   :dilithium-low (fn [value max] (percent-text "Dilithium Low. " value max "%!"))
   :dilithium-critical (constantly "Dilithium Critical!")
   :temp-normal (constantly "Temperature Normal.")
   :temp-high (fn [value max] (percent-text "Temperature High. " value max "%."))
   :temp-critical (constantly "Temperature Critical!")
   :temp-severe (fn [value max] (percent-text "Temperature Severe. " value max "%!"))
   :torpedos-critical (constantly "Torpedos Critical!")
   :torpedos-low (fn [value _] (str "Torpedos Low. " value "!"))
   :torpedos-full (constantly "Torpedos Full.")
   :torpedos-normal (fn [value _] (str "Torpedos Nominal. " value "."))
   :torpedos-high (constantly "Torpedos High.")
   :kinetics-critical (constantly "Kinetics Critical!")
   :kinetics-low (fn [value _] (str "Kinetics Low. " value "!"))
   :kinetics-full (constantly "Kinetics Full.")
   :kinetics-normal (fn [value _] (str "Kinetics Nominal. " value "."))
   :kinetics-high (fn [value _] (str "Kinetics High. " value "!"))})

(def event-messages
  {:you-win {:text "The Federation is safe!  You win!" :duration 1000000}
   :you-died {:text "You died!" :duration 5000}
   :welcome {:text "Welcome to Spacewar!" :duration 5000}
   :save-federation {:text "Save the Federation!" :duration 10000}
   :old-game {:text "Saved game loaded." :duration 10000}
   :new-version {:text "Saved game ignored.  New version." :duration 10000}
   :no-star {:text "No star nearby." :duration 5000}
   :already-deployed {:text "Base already deployed." :duration 5000}
   :insufficient-resources {:text "Insufficient resources." :duration 5000}
   :romulan-appearing {:text "*******\nRomulan ship appearing!\n*******" :duration 10000}
   :corbomite-device {:text "*******\nCorbomite device ready!\n*******" :duration 10000}})

(defn send-message
  ([message-key value max]
   (when-let [text-fn (status-message-text message-key)]
     (msg (text-fn value max))))
  ([message-key]
   (when-let [{:keys [text duration]} (event-messages message-key)]
     (msg text duration))))

(defn item-message [item key max thresholds]
  (if (nil? (key @last-message))
    (swap! last-message assoc key (key item))
    (let [value (key item)
          old-value (key @last-message)
          pct-value (/ value max)
          pct-old-value (/ old-value max)
          pct-diff (abs (- pct-value pct-old-value))]
      (when (> pct-diff 0.1)
        (swap! last-message assoc key value)
        (loop [previous 0
               thresholds thresholds]
          (if (empty? thresholds)
            nil
            (let [[threshold message] (first thresholds)]
              (if (and (<= previous pct-value)
                       (< pct-value threshold)
                       (some? message))
                (send-message message value max)
                (recur threshold (rest thresholds))))))))))

(defn- damage-thresholds [mild severe critical] [[0.1 nil] [0.3 mild] [0.8 severe] [1 critical]])

(def ship-damage-messages
  [[:life-support-damage :life-damage :life-severe :life-critical]
   [:hull-damage :hull-damage :hull-severe :hull-critical]
   [:sensor-damage :sensor-damage :sensor-severe :sensor-critical]
   [:impulse-damage :impulse-damage :impulse-severe :impulse-critical]
   [:warp-damage :warp-damage :warp-severe :warp-critical]
   [:weapons-damage :weapons-damage :weapons-severe :weapons-critical]])

(defn- ship-messages [ship]
  (doseq [[key mild severe critical] ship-damage-messages]
    (item-message ship key 100 (damage-thresholds mild severe critical)))
  (item-message ship :shields glc/ship-shields
                [[0.2 :shields-critical]
                 [0.5 :shields-severe]
                 [0.8 :shields-damaged]
                 [0.95 :shields-ready]
                 [1 :shields-charged]])
  (item-message ship :antimatter glc/ship-antimatter
                [[0.2 :antimatter-critical]
                 [0.5 :antimatter-low]
                 [0.8 :antimatter-high]
                 [0.95 :antimatter-full]
                 [1 :antimatter-topped-off]])
  (item-message ship :dilithium glc/ship-dilithium
                [[0.2 :dilithium-critical]
                 [0.5 :dilithium-low]
                 [0.8 :dilithium-high]
                 [0.95 :dilithium-full]
                 [1 :dilithium-topped-off]])
  (item-message ship :core-temp 100
                [[0.1 nil]
                 [0.3 :temp-normal]
                 [0.5 :temp-high]
                 [0.8 :temp-severe]
                 [1 :temp-critical]])
  (item-message ship :torpedos glc/ship-torpedos
                [[0.1 :torpedos-critical]
                 [0.3 :torpedos-low]
                 [0.5 :torpedos-normal]
                 [0.8 :torpedos-high]
                 [1 :torpedos-full]])
  (item-message ship :kinetics glc/ship-kinetics
                [[0.1 :kinetics-critical]
                 [0.3 :kinetics-low]
                 [0.5 :kinetics-normal]
                 [0.8 :kinetics-high]
                 [1 :kinetics-full]]))

(defn add-messages! [world]
  (ship-messages (:ship world)))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-09-02T15:16:30.476591-05:00", :module-hash "-1890885733", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line nil, :hash "-1425829718"} {:id "def/last-message", :kind "def", :line 5, :end-line nil, :hash "-2137700098"} {:id "defn-/msg", :kind "defn-", :line 7, :end-line nil, :hash "807405242"} {:id "defn-/percent", :kind "defn-", :line 13, :end-line nil, :hash "1595700280"} {:id "defn-/percent-text", :kind "defn-", :line 16, :end-line nil, :hash "125715173"} {:id "def/status-message-text", :kind "def", :line 19, :end-line nil, :hash "-1704263089"} {:id "def/event-messages", :kind "def", :line 68, :end-line nil, :hash "-198287565"} {:id "defn/send-message", :kind "defn", :line 81, :end-line nil, :hash "-1420568741"} {:id "defn/item-message", :kind "defn", :line 89, :end-line nil, :hash "-385701440"} {:id "defn-/damage-thresholds", :kind "defn-", :line 110, :end-line nil, :hash "348009455"} {:id "def/ship-damage-messages", :kind "def", :line 112, :end-line nil, :hash "701265884"} {:id "defn-/ship-messages", :kind "defn-", :line 120, :end-line nil, :hash "-1714732556"} {:id "defn/add-messages!", :kind "defn", :line 160, :end-line nil, :hash "1018780107"}]}
;; clj-mutate-manifest-end
