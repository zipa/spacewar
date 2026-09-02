(ns spacewar.game-logic.explosions
  (:require
    [clojure.spec.alpha :as s]))

(def physics
  {:phaser {:duration 1000 :fragments 20 :fragment-velocity 0.5}
   :torpedo {:duration 2000 :fragments 50 :fragment-velocity 0.2}
   :kinetic {:duration 800 :fragments 10 :fragment-velocity 0.1}
   :klingon {:duration 4000 :fragments 100 :fragment-velocity 0.2}
   :romulan {:duration 4000 :fragments 100 :fragment-velocity 0.2}
   :klingon-kinetic {:duration 800 :fragments 20 :fragment-velocity 0.2}
   :klingon-phaser {:duration 1000 :fragments 20 :fragment-velocity 0.5}
   :klingon-torpedo {:duration 2000 :fragments 50 :fragment-velocity 0.2}
   :romulan-blast {:duration 4000 :fragments 100 :fragment-velocity 0.2}
   :ship {:duration 8000 :fragments 300 :fragment-velocity 0.2}
   :corbomite-device {:duration 4000 :fragments 200 :fragment-velocity 0.2}})

(s/def ::x number?)
(s/def ::y number?)
(s/def ::type #{:phaser :torpedo :kinetic
                :klingon :klingon-kinetic
                :klingon-phaser :ship :corbomite-device})
(s/def ::age number?)
(s/def ::velocity number?)
(s/def ::direction number?)
(s/def ::fragments (s/coll-of (s/keys :req-un [::x ::y ::velocity ::direction])))
(s/def ::explosion (s/keys :req-un [::x ::y ::type ::age ::fragments]))
(s/def ::explosions (s/coll-of ::explosion))

(defn make-fragments [n explosion velocity]
  (let [{:keys [x y]} explosion]
    (repeatedly n
                #(identity {:x x :y y
                            :velocity (* (+ 0.8 (rand 0.2)) velocity)
                            :direction (rand 360)}))))

(defn- active-explosion [explosion]
  (let [{:keys [age type]} explosion
        duration (:duration (type physics))]
    (> duration age)))

(defn update-explosions [ms world]
  (let [explosions (:explosions world)
        explosions (map #(update % :age + ms) explosions)
        explosions (filter active-explosion explosions)]
    (assoc world :explosions (doall explosions))))

(defn ->explosion [explosion-type {:keys [x y] :as object}]
  (let [profile (explosion-type physics) initial-age 0]
    {:age initial-age :x x :y y
     :type explosion-type
     :fragments (make-fragments (:fragments profile) object (:fragment-velocity profile))}))

(defn shot->explosion [shot]
  (->explosion (:type shot) shot))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-09-02T15:07:09.437011-05:00", :module-hash "-1650434506", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line nil, :hash "1839733741"} {:id "def/physics", :kind "def", :line 5, :end-line nil, :hash "-196133181"} {:id "form/2/s/def", :kind "s/def", :line 18, :end-line nil, :hash "-327762176"} {:id "form/3/s/def", :kind "s/def", :line 19, :end-line nil, :hash "-1573557823"} {:id "form/4/s/def", :kind "s/def", :line 20, :end-line nil, :hash "1513829972"} {:id "form/5/s/def", :kind "s/def", :line 23, :end-line nil, :hash "193938100"} {:id "form/6/s/def", :kind "s/def", :line 24, :end-line nil, :hash "-426286654"} {:id "form/7/s/def", :kind "s/def", :line 25, :end-line nil, :hash "-881033533"} {:id "form/8/s/def", :kind "s/def", :line 26, :end-line nil, :hash "7619213"} {:id "form/9/s/def", :kind "s/def", :line 27, :end-line nil, :hash "1223459631"} {:id "form/10/s/def", :kind "s/def", :line 28, :end-line nil, :hash "-766919353"} {:id "defn/make-fragments", :kind "defn", :line 30, :end-line nil, :hash "-418308731"} {:id "defn-/active-explosion", :kind "defn-", :line 37, :end-line nil, :hash "1727961697"} {:id "defn/update-explosions", :kind "defn", :line 42, :end-line nil, :hash "1456186818"} {:id "defn/->explosion", :kind "defn", :line 48, :end-line nil, :hash "-594928943"} {:id "defn/shot->explosion", :kind "defn", :line 54, :end-line nil, :hash "-942678280"}]}
;; clj-mutate-manifest-end
