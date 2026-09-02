(ns spacewar.game-logic.klingons
  (:require [clojure.spec.alpha :as s]
            [spacewar.game-logic.clouds :as clouds]
            [spacewar.game-logic.config :as glc]
            [spacewar.game-logic.explosions :as explosions]
            [spacewar.game-logic.shots :as shots]
            [spacewar.geometry :as geo]
            [spacewar.util :as util]
            [spacewar.vector :as vector]))

;The Klingon state machine has two super-states: {:battle :cruise}.
;In the :battle super-state a klingon will use the :battle-state FSM.
;In the :cruise super-state a klingon will use the :cruise-state FSM.

(s/def ::x number?)
(s/def ::y number?)
(s/def ::shields number?)
(s/def ::antimatter number?)

(s/def ::kinetics number?)
(s/def ::torpedos number?)
(s/def ::weapon-charge number?)
(s/def ::velocity (s/tuple number? number?))
(s/def ::thrust (s/tuple number? number?))
(s/def ::battle-state-age number?)
(s/def ::battle-state #{:no-battle :flank-right :flank-left :retreating :advancing :kamikazee})
(s/def ::cruise-state #{:patrol :guard :refuel :mission})
(s/def ::mission #{:blockade :seek-and-destroy :escape-corbomite})

(s/def ::klingon (s/keys :req-un [::x ::y ::shields ::antimatter
                                  ::kinetics ::torpedos ::weapon-charge
                                  ::velocity ::thrust
                                  ::battle-state-age ::battle-state
                                  ::cruise-state
                                  ::mission]
                         :opt-un [:spacewar.game-logic.hit/hit]))
(s/def ::klingons (s/coll-of ::klingon))

(def cruise-fsm {:patrol {:low-antimatter :refuel
                          :low-torpedo :patrol
                          :capable :guard
                          :well-supplied :mission}
                 :refuel {:low-antimatter :refuel
                          :low-torpedo :patrol
                          :capable :guard
                          :well-supplied :mission}
                 :guard {:low-antimatter :refuel
                         :low-torpedo :guard
                         :capable :guard
                         :well-supplied :mission}
                 :mission {:low-antimatter :refuel
                           :low-torpedo :guard
                           :capable :mission
                           :well-supplied :mission}
                 })

(defn super-state [klingon ship]
  (let [distance (geo/distance [(:x ship) (:y ship)]
                               [(:x klingon) (:y klingon)])]
    (if (< distance glc/klingon-tactical-range)
      :battle
      :cruise)))

(defn- random-mission []
  (if (< 0.5 (rand))
    :seek-and-destroy
    :blockade))

(defn make-random-klingon []
  (let [thrust glc/klingon-cruise-thrust]
    {:x (int (rand glc/known-space-x))
     :y (int (rand glc/known-space-y))
     :shields glc/klingon-shields
     :antimatter (rand glc/klingon-antimatter)
     :kinetics (rand glc/klingon-kinetics)
     :torpedos (rand glc/klingon-torpedos)
     :weapon-charge 0
     :velocity [(- 20 (rand 40)) (- 20 (rand 40))]
     :thrust [(rand-nth [thrust (- thrust)])
              (rand-nth [thrust (- thrust)])]
     :battle-state-age 0
     :battle-state :no-battle
     :cruise-state :patrol
     :mission (random-mission)}))

(defn make-klingon [x y]
  {:x x
   :y y
   :shields glc/klingon-shields
   :antimatter glc/klingon-antimatter
   :kinetics glc/klingon-kinetics
   :torpedos glc/klingon-torpedos
   :weapon-charge 0
   :velocity [0 0]
   :thrust [0 0]
   :battle-state-age 0
   :battle-state :no-battle
   :cruise-state :patrol
   :mission (random-mission)})

(defn new-klingon-from-praxis [world]
  (let [klingons (:klingons world)
        klingon (make-klingon (rand-int glc/known-space-x) 0)
        klingon (assoc klingon :mission :seek-and-destroy
                               :cruise-state :mission
                               :thrust [0 0]
                               :velocity [0 -1]
                               :antimatter glc/klingon-antimatter
                               :torpedos glc/klingon-torpedos
                               :kinetics glc/klingon-kinetics)]
    (assoc world :klingons (conj klingons klingon))))

(defn initialize []
  (repeatedly glc/number-of-klingons make-random-klingon))

(defn damage-by-phasers [hit]
  (let [ranges (:damage hit)]
    (reduce +
            (map #(* glc/phaser-damage (- 1 (/ % glc/phaser-range)))
                 ranges))))

(defn- hit-damage [hit]
  (condp = (:weapon hit)
    :torpedo (:damage hit)
    :kinetic (:damage hit)
    :phaser (damage-by-phasers hit)))

(defn hit-klingon [klingon]
  (let [{:keys [shields hit battle-state-age]} klingon
        klingon (dissoc klingon :hit)
        shields (if (some? hit) (- shields (hit-damage hit)) shields)
        battle-state-age (if (some? hit)
                           (inc glc/klingon-battle-state-transition-age)
                           battle-state-age)]
    (assoc klingon :shields shields :battle-state-age battle-state-age)))

(defn- klingon-destruction [klingons]
  (if (empty? klingons)
    []
    (map #(explosions/->explosion :klingon %) klingons)))

(defn recharge-shield [ms klingon]
  (let [{:keys [antimatter shields]} klingon
        shield-deficit (- glc/klingon-shields shields)
        max-charge (* ms glc/klingon-shield-recharge-rate)
        real-charge (min shield-deficit
                         max-charge
                         (/ antimatter glc/klingon-shield-recharge-cost))
        antimatter (- antimatter (* glc/klingon-shield-recharge-cost real-charge))
        shields (+ shields real-charge)]
    (assoc klingon :antimatter antimatter
                   :shields shields)))

(defn- recharge-shields [ms klingons]
  (map #(recharge-shield ms %) klingons))

(defn- klingon-debris-cloud [klingon]
  (clouds/make-cloud (:x klingon) (:y klingon) (* (rand 1) glc/klingon-debris)))

(defn- klingon-debris-clouds [dead-klingons]
  (map klingon-debris-cloud dead-klingons))

(defn update-kamikazee [ms klingon]
  (if (= :kamikazee (:battle-state klingon))
    (update klingon :shields - (* ms glc/klingon-kamikazee-shield-depletion))
    klingon))

(defn destroyed-klingon? [klingon]
  (neg? (:shields klingon)))

(defn update-klingon-defense [ms {:keys [klingons klingons-killed explosions clouds] :as world}]
  (let [klingons (map hit-klingon klingons)
        klingons (map #(update-kamikazee ms %) klingons)
        dead (filter destroyed-klingon? klingons)
        alive (remove destroyed-klingon? klingons)
        klingons-killed (+ klingons-killed (count dead))
        klingons (recharge-shields ms alive)]
    (assoc world :klingons klingons :klingons-killed klingons-killed
                 :explosions (concat explosions (klingon-destruction dead))
                 :clouds (concat clouds (klingon-debris-clouds dead)))))

(defn- charge-weapons [ms klingons ship]
  (for [klingon klingons]
    (if (> glc/klingon-kinetic-firing-distance
           (geo/distance [(:x ship) (:y ship)]
                         [(:x klingon) (:y klingon)]))
      (let [efficiency (/ (:shields klingon) glc/klingon-shields)
            charge-increment (* ms efficiency)]
        (update klingon :weapon-charge + charge-increment))
      klingon)))

; Firing solution from
;http://danikgames.com/blog/how-to-intersect-a-moving-target-in-2d/
(defn- firing-solution [klingon ship shot-velocity]
  (let [ax (:x klingon)
        ay (:y klingon)
        bx (:x ship)
        by (:y ship)
        [ux uy] (:velocity ship)
        vmag shot-velocity
        abx (- bx ax)
        aby (- by ay)
        abmag (Math/sqrt (+ (* abx abx) (* aby aby)))
        abx (/ abx abmag)
        aby (/ aby abmag)
        udotab (+ (* abx ux) (* aby uy))
        ujx (* udotab abx)
        ujy (* udotab aby)
        uix (- ux ujx)
        uiy (- uy ujy)
        vix uix
        viy uiy
        vimag (Math/sqrt (+ (* vix vix) (* viy viy)))
        vjmag (Math/sqrt (+ (* vmag vmag) (* vimag vimag)))
        vjx (* abx vjmag)
        vjy (* aby vjmag)
        vx (+ vjx vix)
        vy (+ vjy viy)]
    (geo/angle-degrees [0 0]
                       [vx vy])))

(defn- ready-to-fire-kinetic? [klingon]
  (and
    (> (klingon :antimatter) glc/klingon-kinetic-power)
    (> (:kinetics klingon) 0)
    (<= glc/klingon-kinetic-threshold
        (:weapon-charge klingon))))

(defn- ready-to-fire-phaser? [klingon ship]
  (let [ship-pos [(:x ship) (:y ship)]
        klingon-pos [(:x klingon) (:y klingon)]
        dist (geo/distance ship-pos klingon-pos)]
    (and
      (< dist glc/klingon-phaser-firing-distance)
      (> (:antimatter klingon) glc/klingon-phaser-power)
      (<= glc/klingon-phaser-threshold
          (:weapon-charge klingon)))))

(defn- turning? [ship]
  (let [heading (:heading ship)
        heading-setting (:heading-setting ship)
        diff (abs (- heading heading-setting))]
    (> diff 0.5)))

(defn- warping? [ship]
  (> (:warp ship) 0))

(defn- ready-to-fire-torpedo? [klingon ship]
  (let [ship-pos [(:x ship) (:y ship)]
        klingon-pos [(:x klingon) (:y klingon)]
        dist (geo/distance ship-pos klingon-pos)]
    (and
      (not (warping? ship))
      (not (turning? ship))
      (> (int (:torpedos klingon)) 0)
      (< dist glc/klingon-torpedo-firing-distance)
      (> (:antimatter klingon) glc/klingon-torpedo-power)
      (<= glc/klingon-torpedo-threshold
          (:weapon-charge klingon)))))

(defprotocol klingon-weapon
  (weapon-velocity [this])
  (weapon-threshold [this])
  (weapon-power [this])
  (weapon-inventory [this])
  (weapon-type [this]))

(deftype klingon-kinetic []
  klingon-weapon
  (weapon-velocity [_] glc/klingon-kinetic-velocity)
  (weapon-threshold [_] glc/klingon-kinetic-threshold)
  (weapon-power [_] glc/klingon-kinetic-power)
  (weapon-inventory [_] :kinetics)
  (weapon-type [_] :klingon-kinetic)
  )

(deftype klingon-phaser []
  klingon-weapon
  (weapon-velocity [_] glc/klingon-phaser-velocity)
  (weapon-threshold [_] glc/klingon-phaser-threshold)
  (weapon-power [_] glc/klingon-phaser-power)
  (weapon-inventory [_] nil)
  (weapon-type [_] :klingon-phaser)
  )

(deftype klingon-torpedo []
  klingon-weapon
  (weapon-velocity [_] glc/klingon-torpedo-velocity)
  (weapon-threshold [_] glc/klingon-torpedo-threshold)
  (weapon-power [_] glc/klingon-torpedo-power)
  (weapon-inventory [_] :torpedos)
  (weapon-type [_] :klingon-torpedo)
  )

(defn- fire-shot [klingon ship weapon]
  (let [shot-velocity (weapon-velocity weapon)
        kamikazee? (= :kamikazee (:battle-state klingon))
        shot-velocity (if kamikazee?
                        (* glc/kamikazee-shot-velocity-factor shot-velocity)
                        shot-velocity)
        shot (shots/->shot
               (:x klingon) (:y klingon)
               (firing-solution klingon ship shot-velocity)
               (weapon-type weapon))]
    (assoc shot :kamikazee kamikazee?)))

(defn- apply-weapon-costs [klingon weapon]
  (let [klingon (-> klingon
                    (update :weapon-charge - (weapon-threshold weapon))
                    (update :antimatter - (weapon-power weapon)))
        inventory (weapon-inventory weapon)]
    (if inventory
      (update klingon inventory dec)
      klingon)))

(defn- fire-charged-weapons [klingons ship]
  (loop [klingons klingons shots [] fired-klingons []]
    (if (empty? klingons)
      [fired-klingons shots]
      (let [klingon (first klingons)
            weapon (cond
                     (ready-to-fire-torpedo? klingon ship) (->klingon-torpedo)
                     (ready-to-fire-phaser? klingon ship) (->klingon-phaser)
                     (ready-to-fire-kinetic? klingon) (->klingon-kinetic)
                     :else nil)]
        (if weapon
          (recur (rest klingons)
                 (conj shots (fire-shot klingon ship weapon))
                 (conj fired-klingons (apply-weapon-costs klingon weapon)))
          (recur (rest klingons) shots (conj fired-klingons klingon)))))))

(defn delay-shooting? []
  (> 95 (rand 100)))

(defn update-klingon-offense [ms world]
  (if (pos? (:game-over-timer world))
    world
    (let [{:keys [klingons ship shots]} world
          klingons (charge-weapons ms klingons ship)
          [klingons new-shots] (if (delay-shooting?)
                                 [klingons []]
                                 (fire-charged-weapons klingons ship))]
      (assoc world :klingons klingons
                   :shots (concat shots new-shots)))))

(defn- battle? [ship klingon]
  (let [ship-pos (util/pos ship)
        klingon-pos (util/pos klingon)
        dist (geo/distance ship-pos klingon-pos)]
    (> glc/klingon-tactical-range dist)))

(defn- thrust-if-battle [ship klingon]
  (if (battle? ship klingon)
    (let [battle-state (:battle-state klingon)
          ship-pos (util/pos ship)
          klingon-pos (util/pos klingon)
          degrees (geo/angle-degrees klingon-pos ship-pos)
          degrees (+ degrees (battle-state glc/klingon-evasion-trajectories))
          radians (geo/->radians degrees)
          efficiency (/ (:shields klingon) glc/klingon-shields)
          efficiency (+ (/ 2 3) (/ efficiency 3))
          effective-thrust (min (klingon :antimatter)
                                (* glc/klingon-tactical-thrust efficiency))
          thrust (vector/from-angular effective-thrust radians)
          thrust (if (= :kamikazee (:battle-state klingon))
                   (vector/scale glc/klingon-kamikazee-thrust-factor thrust)
                   thrust)]
      (assoc klingon :thrust thrust))
    klingon))

(defn- accelerate-klingon [ms klingon]
  (let [{:keys [thrust velocity antimatter]} klingon]
    (if (= thrust [0 0])
      klingon
      (let [available-antimatter (min glc/klingon-antimatter antimatter)
            full-thrust-antimatter (* ms glc/klingon-thrust-antimatter)
            thrusting-antimatter (min available-antimatter full-thrust-antimatter)
            efficiency (/ thrusting-antimatter full-thrust-antimatter)
            thrust (vector/scale efficiency thrust)
            acc (vector/scale ms thrust)
            velocity (vector/add acc velocity)
            antimatter (- antimatter thrusting-antimatter)]
        (assoc klingon :velocity velocity :antimatter antimatter)))))

(defn- reverse-thrust-if-out-of-bounds [thrust pos]
  (cond
    (and (neg? (first pos)) (neg? (first thrust)))
    [(- (first thrust)) (second thrust)]

    (and (> (first pos) glc/known-space-x) (pos? (first thrust)))
    [(- (first thrust)) (second thrust)]

    (and (> (second pos) glc/known-space-y) (pos? (second thrust)))
    [(first thrust) (- (second thrust))]

    :else thrust
    ))

(defn move-klingon [ms klingon]
  (let [{:keys [x y velocity thrust]} klingon
        delta-v (vector/scale ms velocity)
        pos (vector/add delta-v [x y])
        thrust (reverse-thrust-if-out-of-bounds thrust pos)]
    (assoc klingon :x (first pos) :y (second pos) :thrust thrust))
  )

(defn calc-drag [ms]
  (Math/pow glc/klingon-drag ms))

(defn- drag-klingon [ms klingon]
  (let [drag-factor (calc-drag ms)]
    (update klingon :velocity #(vector/scale drag-factor %))))

(defn- full-retreat [klingon]
  (assoc klingon :thrust [0 (- glc/klingon-cruise-thrust)]))

(defn update-klingon-motion [ms world]
  (let [ship (:ship world)
        klingons (:klingons world)
        klingons (map #(thrust-if-battle ship %) klingons)
        klingons (map #(accelerate-klingon ms %) klingons)
        klingons (map #(move-klingon ms %) klingons)
        klingons (map #(drag-klingon ms %) klingons)]
    (assoc world :klingons klingons)))

(defn- find-thefts [klingons bases]
  (for [klingon klingons
        base bases
        :when (< (geo/distance (util/pos klingon) (util/pos base))
                 glc/klingon-docking-distance)]
    [klingon base]))

(defn- klingon-steals-antimatter [[thief victim]]
  (let [guarding? (or
                    (#{:guard :refuel} (:cruise-state thief))
                    (and (= :mission (:cruise-state thief))
                         (= :blockade (:mission thief))))
        amount-needed (- glc/klingon-antimatter (:antimatter thief))
        amount-available (:antimatter victim)
        amount-stolen (min amount-needed amount-available)
        thief (update thief :antimatter + amount-stolen)
        victim (update victim :antimatter - amount-stolen)
        thief (if guarding?
                (assoc thief :velocity [0 0] :thrust [0 0])
                thief)]
    [thief victim]))

(defn- steal-antimatter [thefts]
  (let [thieves (util/pos-map (map first thefts))
        victims (util/pos-map (map second thefts))]
    (loop [thefts thefts
           thieves thieves
           victims victims]
      (if (empty? thefts)
        [(vals thieves) (vals victims)]
        (let [theft (first thefts)
              thief (get thieves (util/pos (first theft)))
              victim (get victims (util/pos (second theft)))
              [thief victim] (klingon-steals-antimatter [thief victim])]
          (recur (rest thefts)
                 (assoc thieves (util/pos thief) thief)
                 (assoc victims (util/pos victim) victim)))))))

(defn klingons-steal-antimatter [world]
  (let [{:keys [klingons bases]} world
        thefts (find-thefts klingons bases)
        thieves (set (map first thefts))
        victims (set (map second thefts))
        innocents (vec (clojure.set/difference (set klingons) thieves))
        unmolested (vec (clojure.set/difference (set bases) victims))
        [thieves victims] (steal-antimatter thefts)
        klingons (concat thieves innocents)
        bases (concat victims unmolested)]
    (assoc world :klingons klingons :bases bases)))

(defn random-battle-state []
  (let [selected-index (rand-int (count glc/klingon-battle-states))]
    (nth glc/klingon-battle-states selected-index)))

(defn change-expired-battle-state [klingon]
  (let [{:keys [battle-state-age battle-state]} klingon]
    (if (>= battle-state-age glc/klingon-battle-state-transition-age)
      (random-battle-state)
      battle-state)))

(defn determine-battle-state [klingon ship]
  (let [dist (geo/distance (util/pos klingon) (util/pos ship))
        antimatter (:antimatter klingon)
        new-battle-state (condp <= dist
                           glc/klingon-tactical-range :no-battle
                           glc/klingon-evasion-limit :advancing
                           (change-expired-battle-state klingon))
        should-retreat? (and
                          (<= antimatter glc/klingon-antimatter-runaway-threshold)
                          (<= dist glc/klingon-tactical-range))
        should-kamikazee? (and (> glc/klingon-kamikazee-probability (rand))
                               (< antimatter glc/klingon-antimatter-kamikazee-threshold))
        new-battle-state (if should-retreat?
                           (if should-kamikazee?
                             :kamikazee
                             :retreating)
                           new-battle-state)]
    new-battle-state))

(defn- update-klingon-state [ms ship klingon]
  (if (= :kamikazee (:battle-state klingon))
    klingon
    (let [{:keys [battle-state-age]} klingon
          new-battle-state (determine-battle-state klingon ship)
          age (if (>= battle-state-age glc/klingon-battle-state-transition-age)
                0
                (+ battle-state-age ms))]
      (assoc klingon :battle-state new-battle-state
                     :battle-state-age age))))

(defn update-klingons-state [ms world]
  (let [{:keys [ship klingons]} world
        klingons (map #(update-klingon-state ms ship %) klingons)]
    (assoc world :klingons klingons)))

(defn update-torpedo-and-kinetic-production [ms {:keys [antimatter torpedos kinetics] :as klingon}]
  (let [deficit (- glc/klingon-torpedos torpedos)
        max-production (* ms glc/klingon-torpedo-production-rate)
        can-make-torpedos? (< antimatter glc/klingon-torpedo-antimatter-threshold)
        new-torpedos (if can-make-torpedos?
                       0
                       (min deficit max-production))
        efficiency (/ new-torpedos max-production)
        antimatter-cost (* ms glc/klingon-torpedo-antimatter-cost efficiency)
        torpedos (+ torpedos new-torpedos)
        antimatter (- antimatter antimatter-cost)
        kinetics (min glc/klingon-kinetics (+ kinetics (* ms glc/klingon-kinetic-production-rate)))]
    (assoc klingon :antimatter antimatter :torpedos torpedos :kinetics kinetics)))

(defn update-klingon-torpedo-production [ms world]
  (let [klingons (:klingons world)
        klingons (map #(update-torpedo-and-kinetic-production ms %) klingons)]
    (assoc world :klingons klingons))
  )

(defn remove-klingons-out-of-range [_ms world]
  (let [klingons (:klingons world)
        klingons (remove #(< (:y %) (- glc/klingon-tactical-range)) klingons)]
    (assoc world :klingons klingons))
  )

(defn update-klingons [ms world]
  (->> world
       (update-klingons-state ms)
       (update-klingon-defense ms)
       (update-klingon-offense ms)
       (update-klingon-motion ms)
       (update-klingon-torpedo-production ms)
       (remove-klingons-out-of-range ms)
       ))

(defn- thrust-to-nearest-base [klingon bases]
  (if (= (:battle-state klingon) :no-battle)
    (if (empty? bases)
      (assoc klingon :cruise-state :patrol)
      (let [distance-map (apply hash-map
                                (flatten
                                  (map #(list (geo/distance (util/pos klingon) (util/pos %)) %) bases)))
            nearest-base (distance-map (apply min (keys distance-map)))
            angle-to-base (geo/angle-degrees (util/pos klingon) (util/pos nearest-base))
            thrust (vector/from-angular glc/klingon-cruise-thrust (geo/->radians angle-to-base))]
        (assoc klingon :thrust thrust)))
    klingon))

(defn- thrust-towards-ship [klingon ship]
  (if (= (:battle-state klingon) :no-battle)
    (let [angle-to-ship (geo/angle-degrees (util/pos klingon) (util/pos ship))
          thrust (vector/from-angular glc/klingon-cruise-thrust (geo/->radians angle-to-ship))]
      (assoc klingon :thrust thrust))
    klingon))

(defn- thrust-toward-mission [klingon ship bases]
  (condp = (:mission klingon)
    :seek-and-destroy (thrust-towards-ship klingon ship)
    :blockade (thrust-to-nearest-base klingon bases)
    :escape-corbomite (full-retreat klingon)
    klingon)
  )

(defn- thrust-to-nearest-antimatter-source [klingon stars bases]
  (if (= (:battle-state klingon) :no-battle)
    (let [fraction-fuel-remaining (/ (:antimatter klingon) glc/klingon-antimatter)
          target-classes (condp <= fraction-fuel-remaining
                           0.3 #{:o :b}
                           0.1 #{:o :b :a :f}
                           #{:o :b :a :f :g :k :m})
          antimatter-stars (filter #(target-classes (:class %)) stars)
          antimatter-bases (filter #(= :antimatter-factory (:type %)) bases)
          base-distance-map (apply hash-map
                                   (flatten
                                     (map #(list (geo/distance (util/pos klingon) (util/pos %)) %)
                                          antimatter-bases)))
          star-distance-map (apply hash-map
                                   (flatten
                                     (map #(list (geo/distance (util/pos klingon) (util/pos %)) %)
                                          antimatter-stars)))
          distance-to-nearest-antimatter-star (apply min (keys star-distance-map))
          distance-to-nearest-antimatter-base (if (empty? antimatter-bases)
                                                1000000000  ;very far away.
                                                (apply min (keys base-distance-map)))
          nearest-antimatter-star (star-distance-map distance-to-nearest-antimatter-star)
          nearest-antimatter-base (base-distance-map distance-to-nearest-antimatter-base)
          angle-to-target (if (< (* 0.7 distance-to-nearest-antimatter-base)
                                 distance-to-nearest-antimatter-star)
                            (geo/angle-degrees (util/pos klingon) (util/pos nearest-antimatter-base))
                            (geo/angle-degrees (util/pos klingon) (util/pos nearest-antimatter-star)))
          thrust (vector/from-angular glc/klingon-cruise-thrust (geo/->radians angle-to-target))]
      (assoc klingon :thrust thrust))
    klingon))

(defn- thrust-in-random-direction [klingon]
  (if (= (:battle-state klingon) :no-battle)
    (let [angle (rand (* 2 Math/PI))
          tx (* glc/klingon-cruise-thrust (Math/cos angle))
          ty (* glc/klingon-cruise-thrust (Math/sin angle))]
      (assoc klingon :thrust [tx ty]))
    klingon)
  )

(defn- occupying? [base klingon]
  (let [distance (geo/distance (util/pos klingon) (util/pos base))]
    (<= distance glc/klingon-docking-distance)))

(defn- unoccupied-base? [base klingons]
  (let [occupiers (filter #(occupying? base %) klingons)]
    (<= (count occupiers) 3)))

(defn find-unoccupied-bases [bases klingons]
  (filter #(unoccupied-base? % klingons) bases)
  )

(defn cruise-klingons [{:keys [ship klingons bases stars] :as world}]
  (let [cruise-states (group-by :cruise-state klingons)
        on-mission-klingons (:mission cruise-states)
        patrolling-klingons (:patrol cruise-states)
        guarding-klingons (:guard cruise-states)
        refuelling-klingons (:refuel cruise-states)
        unoccupied-bases (find-unoccupied-bases bases klingons)
        on-mission-klingons (map #(thrust-toward-mission % ship unoccupied-bases) on-mission-klingons)
        guarding-klingons (map #(thrust-to-nearest-base % unoccupied-bases) guarding-klingons)
        refuelling-klingons (map #(thrust-to-nearest-antimatter-source % stars bases) refuelling-klingons)
        klingons (concat on-mission-klingons
                         patrolling-klingons
                         guarding-klingons
                         refuelling-klingons)
        ]
    (assoc world :klingons klingons))
  )

(defn cruise-transition [{:keys [antimatter torpedos]}]
  (let [antimatter (/ antimatter glc/klingon-antimatter 0.01)
        torpedos (/ torpedos glc/klingon-torpedos 0.01)]
    (cond
      (<= antimatter 40) :low-antimatter
      (<= torpedos 40) :low-torpedo
      (and (> antimatter 60) (> torpedos 80)) :well-supplied
      :else :capable
      ))
  )

(defn stay-refueling? [{:keys [antimatter cruise-state]}]
  (and (= :refuel cruise-state)
       (< antimatter (* glc/klingon-pct-refueling-target glc/klingon-antimatter))))

(defn- change-cruise-state [klingon]
  (assoc klingon :cruise-state
         (if (stay-refueling? klingon)
           :refuel
           (get-in cruise-fsm [(:cruise-state klingon) (cruise-transition klingon)]))))

(defn- change-all-cruise-states [{:keys [klingons] :as world}]
  (assoc world :klingons (map change-cruise-state klingons)))

(defn produce-antimatter [ms klingon stars]
  (let [antimatter (:antimatter klingon)
        thrust (:thrust klingon)
        velocity (:velocity klingon)
        refueling? (= :refuel (:cruise-state klingon))
        deficit (- glc/klingon-antimatter antimatter)
        distance-map (apply hash-map
                            (flatten
                              (map #(list (geo/distance (util/pos klingon) (util/pos %)) %) stars)))
        distance-to-nearest-star (apply min (keys distance-map))
        in-range? (< distance-to-nearest-star glc/klingon-range-for-antimatter-production)
        nearest-star (distance-map distance-to-nearest-star)
        production (if in-range?
                     (* ms (glc/klingon-antimatter-production-rate (:class nearest-star)))
                     0)
        thrust (if (and in-range? refueling?)
                 [0 0]
                 thrust)
        velocity (if (and in-range? refueling?)
                   [0 0]
                   velocity)]
    (assoc klingon :antimatter (+ antimatter (min deficit production))
                   :thrust thrust
                   :velocity velocity))
  )

(defn klingons-produce-antimatter [{:keys [klingons stars] :as world}]
  (let [klingons (map #(produce-antimatter 1000 % stars) klingons)]
    (assoc world :klingons klingons)))

(defn update-klingons-per-second [world]
  (-> world
      (cruise-klingons)
      (klingons-steal-antimatter)
      (klingons-produce-antimatter)))

(defn change-patrol-direction [{:keys [klingons] :as world}]
  (let [cruise-states (group-by :cruise-state klingons)
        on-mission-klingons (:mission cruise-states)
        patrolling-klingons (:patrol cruise-states)
        guarding-klingons (:guard cruise-states)
        refuelling-klingons (:refuel cruise-states)
        patrolling-klingons (map thrust-in-random-direction patrolling-klingons)
        klingons (concat on-mission-klingons
                         patrolling-klingons
                         guarding-klingons
                         refuelling-klingons)
        ]
    (assoc world :klingons klingons)))

(defn praxis-invasion-imminent? [{:keys [minutes klingons ship]}]
  (and (< (rand) (/ (or minutes 0) glc/minutes-till-full-klingon-invasion))
       (<= (count klingons) (* 1.5 glc/number-of-klingons))
       (not (:corbomite-device-installed ship))))

(defn- add-klingons-from-praxis [world]
  (if (praxis-invasion-imminent? world)
    (new-klingon-from-praxis world)
    world))

(def next-mission
  {:blockade :seek-and-destroy
   :seek-and-destroy :blockade
   :escape-corbomite :escape-corbomite})

(defn- try-change-mission [klingon]
  (if (< (rand) glc/klingon-odds-to-change-mission)
    (assoc klingon :mission (next-mission (:mission klingon)))
    klingon))

(defn try-change-missions [{:keys [klingons] :as world}]
  (let [klingons (map try-change-mission klingons)]
    (assoc world :klingons klingons))
  )

(defn- change-mission-to-escape [klingon]
  (assoc klingon :mission :escape-corbomite :cruise-state :mission))

(defn- change-escapees-to-attackers [klingon]
  (if (= :escape-corbomite (:mission klingon))
    (assoc klingon :mission :seek-and-destroy :cruise-state :mission)
    klingon))

(defn- check-corbomite [{:keys [klingons ship] :as world}]
  (let [corbomite (:corbomite-device-installed ship)
        klingons (if corbomite
                   (map change-mission-to-escape klingons)
                   (map change-escapees-to-attackers klingons))]
    (assoc world :klingons klingons)))

(defn update-klingons-per-minute [world]
  (-> world
      (change-patrol-direction)
      (change-all-cruise-states)
      (try-change-missions)
      (check-corbomite)
      (add-klingons-from-praxis)))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-09-02T15:23:12.212692-05:00", :module-hash "-1860358115", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line nil, :hash "2041401792"} {:id "form/1/s/def", :kind "s/def", :line 15, :end-line nil, :hash "-1601268936"} {:id "form/2/s/def", :kind "s/def", :line 16, :end-line nil, :hash "839037243"} {:id "form/3/s/def", :kind "s/def", :line 17, :end-line nil, :hash "-766491683"} {:id "form/4/s/def", :kind "s/def", :line 18, :end-line nil, :hash "-296380760"} {:id "form/5/s/def", :kind "s/def", :line 20, :end-line nil, :hash "-33483593"} {:id "form/6/s/def", :kind "s/def", :line 21, :end-line nil, :hash "-1918353882"} {:id "form/7/s/def", :kind "s/def", :line 22, :end-line nil, :hash "-863941446"} {:id "form/8/s/def", :kind "s/def", :line 23, :end-line nil, :hash "-80697882"} {:id "form/9/s/def", :kind "s/def", :line 24, :end-line nil, :hash "-331096263"} {:id "form/10/s/def", :kind "s/def", :line 25, :end-line nil, :hash "2105018837"} {:id "form/11/s/def", :kind "s/def", :line 26, :end-line nil, :hash "490729248"} {:id "form/12/s/def", :kind "s/def", :line 27, :end-line nil, :hash "674980840"} {:id "form/13/s/def", :kind "s/def", :line 28, :end-line nil, :hash "-283824123"} {:id "form/14/s/def", :kind "s/def", :line 30, :end-line nil, :hash "4346432"} {:id "form/15/s/def", :kind "s/def", :line 37, :end-line nil, :hash "1999371337"} {:id "def/cruise-fsm", :kind "def", :line 39, :end-line nil, :hash "-733856447"} {:id "defn/super-state", :kind "defn", :line 57, :end-line nil, :hash "1078729123"} {:id "defn-/random-mission", :kind "defn-", :line 64, :end-line nil, :hash "-325788070"} {:id "defn/make-random-klingon", :kind "defn", :line 69, :end-line nil, :hash "1873430646"} {:id "defn/make-klingon", :kind "defn", :line 86, :end-line nil, :hash "1513481362"} {:id "defn/new-klingon-from-praxis", :kind "defn", :line 101, :end-line nil, :hash "1393749895"} {:id "defn/initialize", :kind "defn", :line 113, :end-line nil, :hash "1948490979"} {:id "defn/damage-by-phasers", :kind "defn", :line 116, :end-line nil, :hash "1978821102"} {:id "defn-/hit-damage", :kind "defn-", :line 122, :end-line nil, :hash "1438935883"} {:id "defn/hit-klingon", :kind "defn", :line 128, :end-line nil, :hash "-1108745792"} {:id "defn-/klingon-destruction", :kind "defn-", :line 137, :end-line nil, :hash "562602567"} {:id "defn/recharge-shield", :kind "defn", :line 142, :end-line nil, :hash "529509371"} {:id "defn-/recharge-shields", :kind "defn-", :line 154, :end-line nil, :hash "1993227426"} {:id "defn-/klingon-debris-cloud", :kind "defn-", :line 157, :end-line nil, :hash "-1201906958"} {:id "defn-/klingon-debris-clouds", :kind "defn-", :line 160, :end-line nil, :hash "810953516"} {:id "defn/update-kamikazee", :kind "defn", :line 163, :end-line nil, :hash "1348088718"} {:id "defn/destroyed-klingon?", :kind "defn", :line 168, :end-line nil, :hash "1958312476"} {:id "defn/update-klingon-defense", :kind "defn", :line 171, :end-line nil, :hash "1810278570"} {:id "defn-/charge-weapons", :kind "defn-", :line 182, :end-line nil, :hash "-996752256"} {:id "defn-/firing-solution", :kind "defn-", :line 194, :end-line nil, :hash "1360645866"} {:id "defn-/ready-to-fire-kinetic?", :kind "defn-", :line 222, :end-line nil, :hash "286767401"} {:id "defn-/ready-to-fire-phaser?", :kind "defn-", :line 229, :end-line nil, :hash "-1298114391"} {:id "defn-/turning?", :kind "defn-", :line 239, :end-line nil, :hash "-658383278"} {:id "defn-/warping?", :kind "defn-", :line 245, :end-line nil, :hash "-1959457956"} {:id "defn-/ready-to-fire-torpedo?", :kind "defn-", :line 248, :end-line nil, :hash "419465046"} {:id "form/41/defprotocol", :kind "defprotocol", :line 261, :end-line nil, :hash "-915546039"} {:id "form/42/deftype", :kind "deftype", :line 268, :end-line nil, :hash "368613505"} {:id "form/43/deftype", :kind "deftype", :line 277, :end-line nil, :hash "-1687897791"} {:id "form/44/deftype", :kind "deftype", :line 286, :end-line nil, :hash "830693143"} {:id "defn-/fire-shot", :kind "defn-", :line 295, :end-line nil, :hash "-2067672320"} {:id "defn-/apply-weapon-costs", :kind "defn-", :line 307, :end-line nil, :hash "1946001455"} {:id "defn-/fire-charged-weapons", :kind "defn-", :line 316, :end-line nil, :hash "292485026"} {:id "defn/delay-shooting?", :kind "defn", :line 332, :end-line nil, :hash "802153836"} {:id "defn/update-klingon-offense", :kind "defn", :line 335, :end-line nil, :hash "249717939"} {:id "defn-/battle?", :kind "defn-", :line 346, :end-line nil, :hash "656049882"} {:id "defn-/thrust-if-battle", :kind "defn-", :line 352, :end-line nil, :hash "-635101692"} {:id "defn-/accelerate-klingon", :kind "defn-", :line 371, :end-line nil, :hash "-57756220"} {:id "defn-/reverse-thrust-if-out-of-bounds", :kind "defn-", :line 385, :end-line nil, :hash "-846164268"} {:id "defn/move-klingon", :kind "defn", :line 399, :end-line nil, :hash "-1029410724"} {:id "defn/calc-drag", :kind "defn", :line 407, :end-line nil, :hash "1110340394"} {:id "defn-/drag-klingon", :kind "defn-", :line 410, :end-line nil, :hash "1835911996"} {:id "defn-/full-retreat", :kind "defn-", :line 414, :end-line nil, :hash "1349816527"} {:id "defn/update-klingon-motion", :kind "defn", :line 417, :end-line nil, :hash "1514040876"} {:id "defn-/find-thefts", :kind "defn-", :line 426, :end-line nil, :hash "917787853"} {:id "defn-/klingon-steals-antimatter", :kind "defn-", :line 433, :end-line nil, :hash "-1424523753"} {:id "defn-/steal-antimatter", :kind "defn-", :line 448, :end-line nil, :hash "1988935735"} {:id "defn/klingons-steal-antimatter", :kind "defn", :line 464, :end-line nil, :hash "1895805521"} {:id "defn/random-battle-state", :kind "defn", :line 476, :end-line nil, :hash "1761132822"} {:id "defn/change-expired-battle-state", :kind "defn", :line 480, :end-line nil, :hash "-821668677"} {:id "defn/determine-battle-state", :kind "defn", :line 486, :end-line nil, :hash "1413392273"} {:id "defn-/update-klingon-state", :kind "defn-", :line 505, :end-line nil, :hash "-1225611710"} {:id "defn/update-klingons-state", :kind "defn", :line 516, :end-line nil, :hash "44506540"} {:id "defn/update-torpedo-and-kinetic-production", :kind "defn", :line 521, :end-line nil, :hash "-565068343"} {:id "defn/update-klingon-torpedo-production", :kind "defn", :line 535, :end-line nil, :hash "-1122268588"} {:id "defn/remove-klingons-out-of-range", :kind "defn", :line 541, :end-line nil, :hash "1917287916"} {:id "defn/update-klingons", :kind "defn", :line 547, :end-line nil, :hash "1730320043"} {:id "defn-/thrust-to-nearest-base", :kind "defn-", :line 557, :end-line nil, :hash "1471159286"} {:id "defn-/thrust-towards-ship", :kind "defn-", :line 570, :end-line nil, :hash "-414957590"} {:id "defn-/thrust-toward-mission", :kind "defn-", :line 577, :end-line nil, :hash "-1699844973"} {:id "defn-/thrust-to-nearest-antimatter-source", :kind "defn-", :line 585, :end-line nil, :hash "1185655050"} {:id "defn-/thrust-in-random-direction", :kind "defn-", :line 616, :end-line nil, :hash "111972354"} {:id "defn-/occupying?", :kind "defn-", :line 625, :end-line nil, :hash "-1358796243"} {:id "defn-/unoccupied-base?", :kind "defn-", :line 629, :end-line nil, :hash "1308817218"} {:id "defn/find-unoccupied-bases", :kind "defn", :line 633, :end-line nil, :hash "-273422775"} {:id "defn/cruise-klingons", :kind "defn", :line 637, :end-line nil, :hash "591909959"} {:id "defn/cruise-transition", :kind "defn", :line 655, :end-line nil, :hash "-1371865176"} {:id "defn/stay-refueling?", :kind "defn", :line 666, :end-line nil, :hash "303376836"} {:id "defn-/change-cruise-state", :kind "defn-", :line 670, :end-line nil, :hash "2067339093"} {:id "defn-/change-all-cruise-states", :kind "defn-", :line 676, :end-line nil, :hash "-438118864"} {:id "defn/produce-antimatter", :kind "defn", :line 679, :end-line nil, :hash "1300621933"} {:id "defn/klingons-produce-antimatter", :kind "defn", :line 705, :end-line nil, :hash "-1128825207"} {:id "defn/update-klingons-per-second", :kind "defn", :line 709, :end-line nil, :hash "2137249835"} {:id "defn/change-patrol-direction", :kind "defn", :line 715, :end-line nil, :hash "-550092087"} {:id "defn/praxis-invasion-imminent?", :kind "defn", :line 729, :end-line nil, :hash "119729262"} {:id "defn-/add-klingons-from-praxis", :kind "defn-", :line 734, :end-line nil, :hash "1448751541"} {:id "def/next-mission", :kind "def", :line 739, :end-line nil, :hash "-1321426209"} {:id "defn-/try-change-mission", :kind "defn-", :line 744, :end-line nil, :hash "-2138728075"} {:id "defn/try-change-missions", :kind "defn", :line 749, :end-line nil, :hash "-625873248"} {:id "defn-/change-mission-to-escape", :kind "defn-", :line 754, :end-line nil, :hash "-995377561"} {:id "defn-/change-escapees-to-attackers", :kind "defn-", :line 757, :end-line nil, :hash "-851181139"} {:id "defn-/check-corbomite", :kind "defn-", :line 762, :end-line nil, :hash "769178620"} {:id "defn/update-klingons-per-minute", :kind "defn", :line 769, :end-line nil, :hash "-712646527"}]}
;; clj-mutate-manifest-end
