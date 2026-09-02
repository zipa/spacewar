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
  (let [profile (explosion-type physics)]
    {:x x :y y
     :age 0 :type explosion-type
     :fragments (make-fragments (:fragments profile) object (:fragment-velocity profile))}))

(defn shot->explosion [shot]
  (->explosion (:type shot) shot))