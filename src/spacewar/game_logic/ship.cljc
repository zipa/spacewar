(ns spacewar.game-logic.ship
  (:require
    [clojure.set :as set]
    [clojure.spec.alpha :as s]
    [spacewar.game-logic.bases :as bases]
    [spacewar.game-logic.config :as glc]
    [spacewar.geometry :as geo]
    [spacewar.game-logic.notifications :as notifications]
    [spacewar.util :as util :refer [handle-event]]
    [spacewar.vector :as vector]))

(s/def ::x number?)
(s/def ::y number?)
(s/def ::warp number?)
(s/def ::warp-charge number?)
(s/def ::impulse number?)
(s/def ::heading (s/and number? #(<= 0 % 360)))
(s/def ::velocity (s/tuple number? number?))
(s/def ::selected-view #{:front-view :strat-view :tact-view})
(s/def ::selected-weapon #{:phaser :kinetic :torpedo :none})
(s/def ::selected-engine #{:warp :impulse :none})
(s/def ::target-bearing (s/and number? #(<= 0 % 360)))
(s/def ::engine-power-setting number?)
(s/def ::weapon-number-setting number?)
(s/def ::weapon-spread-setting number?)
(s/def ::saved-weapon-settings (s/keys :req-un [::phaser ::kinetic ::torpedo]))
(s/def ::heading-setting (s/and number? #(<= 0 % 360)))
(s/def ::antimatter number?)
(s/def ::core-temp number?)
(s/def ::dilithium number?)
(s/def ::shields number?)
(s/def ::kinetics number?)
(s/def ::torpedos number?)
(s/def ::percent (s/and number? #(<= 0 % 100)))
(s/def ::life-support-damage ::percent)
(s/def ::hull-damage ::percent)
(s/def ::sensor-damage ::percent)
(s/def ::impulse-damage ::percent)
(s/def ::warp-damage ::percent)
(s/def ::weapons-damage ::percent)
(s/def ::destroyed boolean?)
(s/def ::corbomite-device-installed boolean?)

(s/def ::strat-scale (s/and number? #(<= 1 % 10)))


(s/def ::ship (s/keys :req-un [::x ::y ::warp ::warp-charge
                               ::impulse ::heading ::velocity
                               ::selected-view ::selected-weapon
                               ::selected-engine ::target-bearing
                               ::engine-power-setting
                               ::weapon-number-setting
                               ::weapon-spread-setting
                               ::saved-weapon-settings
                               ::heading-setting
                               ::antimatter ::core-temp
                               ::dilithium ::shields
                               ::kinetics ::torpedos
                               ::life-support-damage ::hull-damage
                               ::sensor-damage ::impulse-damage
                               ::warp-damage ::weapons-damage
                               ::strat-scale
                               ::destroyed
                               ::corbomite-device-installed]))

(defn initialize []
  {:x (int (rand glc/known-space-x))
   :y (int (rand glc/known-space-y))
   :warp 0
   :warp-charge 0
   :impulse 0
   :heading 0
   :velocity [0 0]
   :selected-view :front-view
   :selected-weapon :none
   :selected-engine :none
   :target-bearing 0
   :engine-power-setting 0
   :weapon-number-setting 1
   :weapon-spread-setting 1
   :saved-weapon-settings {:phaser {:number-setting 1
                                    :spread-setting glc/min-spread}
                           :torpedo {:number-setting 1
                                     :spread-setting glc/min-spread}
                           :kinetic {:number-setting 1
                                     :spread-setting glc/min-spread}}
   :heading-setting 0
   :antimatter glc/ship-antimatter
   :core-temp 0
   :dilithium glc/ship-dilithium
   :shields glc/ship-shields
   :kinetics glc/ship-kinetics
   :torpedos glc/ship-torpedos
   :life-support-damage 0
   :hull-damage 0
   :sensor-damage 0
   :warp-damage 0
   :impulse-damage 0
   :weapons-damage 0
   :strat-scale 1
   :destroyed false
   :corbomite-device-installed false}
  )

(defn heat-core [antimatter ship]
  (let [heat (* antimatter glc/antimatter-to-heat)]
    (update ship :core-temp + heat)))

(defn dissipate-core-heat [ms ship]
  (let [dilithium (:dilithium ship)
        efficiency (Math/sqrt (/ dilithium glc/ship-dilithium))
        dissipation-factor (- 1 (max glc/passive-heat-dissipation (* efficiency glc/dilithium-heat-dissipation)))]
    (update ship :core-temp * (Math/pow dissipation-factor ms))))

(defn drag [[x y :as v]]
  (if (and (zero? x) (zero? y))
    [0 0]
    (let [mag (vector/magnitude v)
          mag-sqr (* mag mag)
          uv (vector/unit v)]
      (vector/scale (* -1 mag-sqr glc/drag-factor) uv))))

(defn apply-drag [drag velocity]
  (let [new-velocity (vector/add velocity drag)]
    (if (= (geo/sign (first new-velocity))
           (geo/sign (first velocity)))
      new-velocity
      [0 0])))

(defn apply-impulse [ms velocity heading impulse]
  (let [delta-v (* ms glc/impulse-thrust impulse)
        radians (geo/->radians heading)
        dv (vector/from-angular delta-v radians)
        new-velocity (vector/add velocity dv)]
    new-velocity))


(defn rotation-direction [current-heading desired-heading]
  (let [diff (mod (- desired-heading current-heading) 360)]
    (cond (> diff 180) (- diff 360)
          :else diff)))

(defn- warp-factor [warp]
  (* warp warp 0.5))

(defn- calc-warp-charge [warp]
  (Math/pow warp 1.3))

(defn calc-dilithium-consumed [warp ms]
  (* warp ms glc/ship-dilithium-consumption))

(defn- consume-dilithium [dilithium warp ms]
  (if (zero? warp)
    dilithium
    (max 0 (- dilithium (calc-dilithium-consumed warp ms)))))

(defn- warp-ship [ms ship]
  (if (zero? (:warp ship))
    ship
    (let [{:keys [x y warp warp-charge
                  heading antimatter
                  dilithium]} ship
          dilithium (consume-dilithium dilithium warp ms)
          power-required (* ms (warp-factor warp) glc/warp-power)
          power-used (min power-required antimatter)
          antimatter (- antimatter power-used)
          actual-warp (* warp (/ power-used power-required))
          warp-charge-increment (* ms (calc-warp-charge actual-warp) glc/warp-charge-rate)
          warp-charge-increment (if (:corbomite-device-installed ship)
                                  (* warp-charge-increment glc/corbomite-warp-factor-boost)
                                  warp-charge-increment)
          warp-efficiency (/ (- 100 (:warp-damage ship)) 100)
          warp-charge-increment (* warp-charge-increment warp-efficiency)
          warp-charge (+ warp-charge-increment warp-charge)
          warp-trigger (> warp-charge glc/warp-threshold)
          warp-charge (if warp-trigger 0 warp-charge)
          radians (geo/->radians heading)
          warp-vector (vector/from-angular glc/warp-leap radians)
          [wx wy] (if warp-trigger
                    (vector/add [x y] warp-vector)
                    [x y])
          ship (heat-core power-used ship)]
      (assoc ship :x wx :y wy :warp-charge warp-charge
                  :antimatter antimatter
                  :dilithium dilithium))))

(defn- impulse-ship [ms ship]
  (let [{:keys [antimatter velocity heading impulse x y]} ship
        power-required (* ms glc/impulse-power impulse)
        power-used (min power-required antimatter)
        actual-impulse (if (zero? power-required)
                         0
                         (* impulse (/ power-used power-required)))
        impulse-efficiency (/ (- 100 (:impulse-damage ship)) 100)
        actual-impulse (* actual-impulse impulse-efficiency)
        antimatter (- antimatter power-used)
        drag (drag velocity)
        accelerated-v (apply-impulse ms velocity heading actual-impulse)
        velocity (apply-drag drag accelerated-v)
        [px py] (vector/add [x y] (vector/scale ms velocity))
        ship (heat-core power-used ship)]
    (assoc ship :x px :y py
                :velocity velocity
                :antimatter antimatter)))

(defn rotate-ship [ms ship]
  (let [{:keys [heading heading-setting]} ship
        total-rotation (rotation-direction heading heading-setting)
        rotation-step (* glc/rotation-rate ms (geo/sign total-rotation))
        rotation-step (if (< (abs total-rotation) (abs rotation-step))
                        total-rotation
                        rotation-step)
        new-heading (mod (+ heading rotation-step) 360)]
    (assoc ship :heading new-heading)))

(defn charge-shields [ms ship]
  (let [corbomite (:corbomite-device-installed ship)
        antimatter (:antimatter ship)
        shields (:shields ship)
        difference (- glc/ship-shields shields)
        charge (min difference antimatter (* ms
                                             glc/ship-shield-recharge-rate
                                             (if corbomite 3 1)))
        ship (update ship :shields + charge)
        antimatter (* charge glc/ship-shield-recharge-cost (if corbomite 0.5 1))
        ship (update ship :antimatter - antimatter)
        ship (heat-core antimatter ship)]
    ship))

(defn repair-capacity [ms ship]
  (let [{:keys [life-support-damage]} ship
        repair-factor (if (:corbomite-device-installed ship) 0.03 0.01)]
    (* ms glc/ship-repair-capacity (- 100 life-support-damage) repair-factor)))

(defn repair-ship [ms ship]
  (loop [systems [:life-support-damage
                  :hull-damage
                  :warp-damage
                  :sensor-damage
                  :weapons-damage
                  :impulse-damage
                  ]
         capacity (repair-capacity ms ship)
         ship ship]
    (if (or (zero? capacity)
            (empty? systems))
      ship
      (let [system (first systems)
            repair (min capacity (system ship))
            ship (update ship system - repair)]
        (recur (rest systems) (- capacity repair) ship)))))

(defn update-destruction [ship]
  (let [{:keys [life-support-damage
                hull-damage
                core-temp]} ship
        destroyed (or (>= life-support-damage 100)
                      (>= hull-damage 100)
                      (>= core-temp 100))]
    (if destroyed
      (assoc ship :destroyed true)
      ship)))

(defn constrain-ship [{:keys [x y velocity] :as ship}]
  (let [nx (min (max x 0) glc/known-space-x)
        ny (min (max y 0) glc/known-space-y)
        nv (if (and (= x nx) (= y ny)) velocity [0.0 0.0])]
    (assoc ship :x nx :y ny :velocity nv)))

(defn update-ship [ms world]
  (let [ship (:ship world)
        ship (update-destruction ship)
        ship (if (:destroyed ship)
               ship
               (->> ship
                    (warp-ship ms)
                    (impulse-ship ms)
                    (constrain-ship)
                    (rotate-ship ms)
                    (charge-shields ms)
                    (repair-ship ms)
                    (dissipate-core-heat ms)))]
    (assoc world :ship ship)))

(defn- set-heading-handler [{:keys [angle]} ship]
  (assoc ship :heading-setting angle))

(defn- set-target-bearing-handler [{:keys [angle]} ship]
  (assoc ship :target-bearing angle))

(defn- set-engine-power-handler [{:keys [value]} ship]
  (assoc ship :engine-power-setting value))

(defn- set-weapon-number-handler [{:keys [value]} ship]
  (let [selected-weapon (:selected-weapon ship)
        ship (assoc ship :weapon-number-setting value)
        ship (assoc-in ship [:saved-weapon-settings selected-weapon :number-setting] value)
        ship (if (> value 1)
               ship
               (do
                 (assoc-in ship [:saved-weapon-settings selected-weapon :spread-setting] glc/min-spread)
                 (assoc ship :weapon-spread-setting glc/min-spread)))]
    ship))

(defn- set-weapon-spread-handler [{:keys [value]} ship]
  (let [selected-weapon (:selected-weapon ship)
        ship (assoc ship :weapon-spread-setting value)
        ship (assoc-in ship [:saved-weapon-settings selected-weapon :spread-setting] value)]
    ship))

(defn- engage-engine-handler [_ ship]
  (let [{:keys [selected-engine engine-power-setting]} ship]
    (if (= selected-engine :none)
      ship
      (assoc ship selected-engine engine-power-setting
                  :engine-power-setting 0))))

(defn- select-engine [engine ship]
  (assoc ship :selected-engine
         (if (= (:selected-engine ship) engine) :none engine)))

(defn- select-impulse [_ ship]
  (select-engine :impulse ship))

(defn- select-warp [_ ship]
  (select-engine :warp ship))

(defn- select-front-view [_ ship]
  (assoc ship :selected-view :front-view))

(defn- select-strat-view [_ ship]
  (assoc ship :selected-view :strat-view))

(defn- select-tact-view [_ ship]
  (assoc ship :selected-view :tact-view))

(defn- select-weapon [weapon {:keys [selected-weapon] :as ship}]
  (assoc ship
    :weapon-number-setting (get-in ship [:saved-weapon-settings weapon :number-setting] 1)
    :weapon-spread-setting (get-in ship [:saved-weapon-settings weapon :spread-setting] glc/min-spread)
    :selected-weapon (if (= selected-weapon weapon) :none weapon)))

(defn- select-phaser [_ ship]
  (select-weapon :phaser ship))

(defn- select-torpedo [_ ship]
  (select-weapon :torpedo ship))

(defn- select-kinetic [_ ship]
  (select-weapon :kinetic ship))

(defn- set-strat-scale [{:keys [value]} ship]
  (assoc ship :strat-scale value))

(defn- in-range-of-base [ship base]
  (< (geo/distance [(:x base) (:y base)]
                   [(:x ship) (:y ship)])
     glc/ship-docking-distance))

(defn- resupply-ship [ship base commodity maximum]
  (if (= :corbomite-device (:type base))
    [ship base]
    (let [need (- maximum (ship commodity))
          supplied (int (min need (base commodity)))
          ship (update ship commodity + supplied)
          base (update base commodity - supplied)]
      [ship base])))

(defn- resupply-ship-from-bases [ship bases]
  (let [base-map (util/pos-map bases)]
    (loop [ship ship
           bases (keys base-map)
           base-map base-map]
      (if (empty? bases)
        [ship (vals base-map)]
        (if (= :corbomite-device (:type (get base-map (first bases))))
          (recur (assoc ship :corbomite-device-installed true)
                 (rest bases)
                 (assoc base-map (first bases) nil))
          (let [base (get base-map (first bases))
                [ship base] (resupply-ship ship base :antimatter glc/ship-antimatter)
                [ship base] (resupply-ship ship base :dilithium glc/ship-dilithium)
                [ship base] (resupply-ship ship base :torpedos glc/ship-torpedos)
                [ship base] (resupply-ship ship base :kinetics glc/ship-kinetics)]
            (recur ship (rest bases) (assoc base-map (first bases) base))))))))

(defn dock-ship [_ world]
  (let [ship (:ship world)
        bases (:bases world)
        grouped-bases (group-by #(in-range-of-base ship %) bases)
        docked-bases (grouped-bases true)
        distant-bases (grouped-bases false)
        [ship docked-bases] (resupply-ship-from-bases ship docked-bases)
        docked-bases (filter some? docked-bases)
        ship (assoc ship
               :velocity [0 0]
               :warp 0
               :impulse 0)
        world (assoc world :ship ship :bases (concat distant-bases docked-bases))]
    world))

(defn deployment-classes [factory]
  (condp = factory
    :antimatter-factory #{:o :b :a}
    :dilithium-factory #{:k :m}
    :weapon-factory #{:f :g}
    :corbomite-factory #{:pulsar}))

(defn- find-deployable-star [type ship stars]
  (let [stars-in-range (filter #(< (geo/distance (util/pos ship) (util/pos %))
                                   glc/ship-deploy-distance)
                               stars)
        deployable-classes (deployment-classes type)]
    (first (filter #(deployable-classes (:class %)) stars-in-range))))

(defn- base-already-deployed? [star bases]
  (let [bases-in-range (filter #(< (geo/distance (util/pos star) (util/pos %))
                                   glc/ship-deploy-distance)
                               bases)]
    (not (empty? bases-in-range))))

(defn- sufficient-resources-for-deployment? [ship]
  (let [{:keys [antimatter dilithium]} ship]
    (and (> antimatter glc/base-deployment-antimatter)
         (> dilithium glc/base-deployment-dilithium))))

(defn add-transport-routes-to [{:keys [bases] :as world}
                               new-base]
  (let [base-pairs (map #(list % new-base) bases)
        base-distances (map #(list (geo/distance
                                     [(:x (first %)) (:y (first %))]
                                     [(:x (second %)) (:y (second %))])
                                   %)
                            base-pairs)
        valid-routes (filter #(<= (first %) glc/transport-range) base-distances)
        sorted-routes (map second (sort-by first valid-routes))
        transport-routes (map (fn [[b1 b2]]
                                #{[(:x b1) (:y b1)]
                                  [(:x b2) (:y b2)]}) (take glc/base-routes-limit sorted-routes))]
    (update world :transport-routes set/union (set transport-routes))))

(defn deploy-base [type world]
  (let [{:keys [ship stars bases]} world
        deployable-star (find-deployable-star type ship stars)]
    (cond (not deployable-star)
          (notifications/notify world :no-star)

          (base-already-deployed? deployable-star bases)
          (notifications/notify world :already-deployed)

          (not (sufficient-resources-for-deployment? ship))
          (notifications/notify world :insufficient-resources)

          :else
          (let [{:keys [x y]} ship
                bases (:bases world)
                base (bases/make-base [x y] type)
                bases (conj bases base)
                ship (-> ship
                         (update :antimatter - glc/base-deployment-antimatter)
                         (update :dilithium - glc/base-deployment-dilithium))
                world (-> world
                          (add-transport-routes-to base)
                          (assoc :bases bases :ship ship))]
            world))))

(defn- deploy-antimatter-factory [_ world]
  (deploy-base :antimatter-factory world))

(defn- deploy-dilithium-factory [_ world]
  (deploy-base :dilithium-factory world))

(defn- deploy-weapon-factory [_ world]
  (deploy-base :weapon-factory world))

(defn- deploy-corbomite-factory [_ world]
  (deploy-base :corbomite-factory world))

(defn process-events [events world]
  (let [ship (:ship world)
        [_ ship] (->> [events ship]
                      (handle-event :front-view select-front-view)
                      (handle-event :strategic-scan select-strat-view)
                      (handle-event :tactical-scan select-tact-view)
                      (handle-event :engine-direction set-heading-handler)
                      (handle-event :engine-power set-engine-power-handler)
                      (handle-event :weapon-direction set-target-bearing-handler)
                      (handle-event :weapon-number set-weapon-number-handler)
                      (handle-event :weapon-spread set-weapon-spread-handler)
                      (handle-event :engine-engage engage-engine-handler)
                      (handle-event :select-impulse select-impulse)
                      (handle-event :select-warp select-warp)
                      (handle-event :select-phaser select-phaser)
                      (handle-event :select-torpedo select-torpedo)
                      (handle-event :select-kinetic select-kinetic)
                      (handle-event :strat-scale set-strat-scale)
                      )
        world (assoc world :ship ship)
        [_ world] (->> [events world]
                       (handle-event :select-dock dock-ship)
                       (handle-event :antimatter-factory deploy-antimatter-factory)
                       (handle-event :dilithium-factory deploy-dilithium-factory)
                       (handle-event :weapon-factory deploy-weapon-factory)
                       (handle-event :corbomite-factory deploy-corbomite-factory))]
    world))

(defn dockable? [ship bases]
  (if (empty? bases)
    false
    (let [distances (map #(geo/distance [(:x ship) (:y ship)]
                                        [(:x %) (:y %)]) bases)
          closest (apply min distances)]
      (< closest glc/ship-docking-distance))))

(defn deployable? [factory ship stars]
  (let [{:keys [x y]} ship
        deployment-classes-set (deployment-classes factory)
        deployable-stars (filter #(deployment-classes-set (:class %)) stars)
        distances (map #(geo/distance [x y] [(:x %) (:y %)]) deployable-stars)
        closest (if (empty? distances)
                  nil
                  (apply min distances))]
    (if closest
      (< closest glc/ship-deploy-distance)
      false)))

(defn reincarnate []
  {:x (int (rand glc/known-space-x))
   :y (int (rand glc/known-space-y))
   :warp 0
   :warp-charge 0
   :impulse 0
   :heading 0
   :velocity [0 0]
   :selected-view :front-view
   :selected-weapon :none
   :selected-engine :none
   :target-bearing 0
   :engine-power-setting 0
   :weapon-number-setting 1
   :weapon-spread-setting 1
   :heading-setting 0
   :antimatter (/ glc/ship-antimatter 2)
   :core-temp 0
   :dilithium (/ glc/ship-dilithium 2)
   :shields glc/ship-shields
   :kinetics (/ glc/ship-kinetics 2)
   :torpedos (/ glc/ship-torpedos 2)
   :life-support-damage 0
   :hull-damage 0
   :sensor-damage 0
   :warp-damage 0
   :impulse-damage 0
   :weapons-damage 0
   :strat-scale 1
   :destroyed false
   :corbomite-device-installed false})

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-09-02T15:27:21.190737-05:00", :module-hash "1441560178", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line nil, :hash "-515122568"} {:id "form/1/s/def", :kind "s/def", :line 12, :end-line nil, :hash "-766576684"} {:id "form/2/s/def", :kind "s/def", :line 13, :end-line nil, :hash "-658860312"} {:id "form/3/s/def", :kind "s/def", :line 14, :end-line nil, :hash "1718079922"} {:id "form/4/s/def", :kind "s/def", :line 15, :end-line nil, :hash "1044972775"} {:id "form/5/s/def", :kind "s/def", :line 16, :end-line nil, :hash "601616094"} {:id "form/6/s/def", :kind "s/def", :line 17, :end-line nil, :hash "661647838"} {:id "form/7/s/def", :kind "s/def", :line 18, :end-line nil, :hash "-1541167390"} {:id "form/8/s/def", :kind "s/def", :line 19, :end-line nil, :hash "345180962"} {:id "form/9/s/def", :kind "s/def", :line 20, :end-line nil, :hash "-778352455"} {:id "form/10/s/def", :kind "s/def", :line 21, :end-line nil, :hash "2143133304"} {:id "form/11/s/def", :kind "s/def", :line 22, :end-line nil, :hash "-1029738257"} {:id "form/12/s/def", :kind "s/def", :line 23, :end-line nil, :hash "-746325366"} {:id "form/13/s/def", :kind "s/def", :line 24, :end-line nil, :hash "1454124517"} {:id "form/14/s/def", :kind "s/def", :line 25, :end-line nil, :hash "1771698920"} {:id "form/15/s/def", :kind "s/def", :line 26, :end-line nil, :hash "1653556948"} {:id "form/16/s/def", :kind "s/def", :line 27, :end-line nil, :hash "1005528172"} {:id "form/17/s/def", :kind "s/def", :line 28, :end-line nil, :hash "-1014299089"} {:id "form/18/s/def", :kind "s/def", :line 29, :end-line nil, :hash "594356513"} {:id "form/19/s/def", :kind "s/def", :line 30, :end-line nil, :hash "-1618395538"} {:id "form/20/s/def", :kind "s/def", :line 31, :end-line nil, :hash "1336204944"} {:id "form/21/s/def", :kind "s/def", :line 32, :end-line nil, :hash "1858768506"} {:id "form/22/s/def", :kind "s/def", :line 33, :end-line nil, :hash "183410875"} {:id "form/23/s/def", :kind "s/def", :line 34, :end-line nil, :hash "757850503"} {:id "form/24/s/def", :kind "s/def", :line 35, :end-line nil, :hash "-469685196"} {:id "form/25/s/def", :kind "s/def", :line 36, :end-line nil, :hash "1997129297"} {:id "form/26/s/def", :kind "s/def", :line 37, :end-line nil, :hash "1696734714"} {:id "form/27/s/def", :kind "s/def", :line 38, :end-line nil, :hash "1077213775"} {:id "form/28/s/def", :kind "s/def", :line 39, :end-line nil, :hash "903415767"} {:id "form/29/s/def", :kind "s/def", :line 40, :end-line nil, :hash "-143927217"} {:id "form/30/s/def", :kind "s/def", :line 41, :end-line nil, :hash "-1791608106"} {:id "form/31/s/def", :kind "s/def", :line 42, :end-line nil, :hash "883719179"} {:id "form/32/s/def", :kind "s/def", :line 44, :end-line nil, :hash "1084927409"} {:id "form/33/s/def", :kind "s/def", :line 47, :end-line nil, :hash "508445874"} {:id "defn/initialize", :kind "defn", :line 66, :end-line nil, :hash "-1214786313"} {:id "defn/heat-core", :kind "defn", :line 105, :end-line nil, :hash "-382413100"} {:id "defn/dissipate-core-heat", :kind "defn", :line 109, :end-line nil, :hash "28139199"} {:id "defn/drag", :kind "defn", :line 115, :end-line nil, :hash "150150660"} {:id "defn/apply-drag", :kind "defn", :line 123, :end-line nil, :hash "-1287505458"} {:id "defn/apply-impulse", :kind "defn", :line 130, :end-line nil, :hash "1655280603"} {:id "defn/rotation-direction", :kind "defn", :line 138, :end-line nil, :hash "1892912780"} {:id "defn-/warp-factor", :kind "defn-", :line 143, :end-line nil, :hash "-1213953667"} {:id "defn-/calc-warp-charge", :kind "defn-", :line 146, :end-line nil, :hash "-742248653"} {:id "defn/calc-dilithium-consumed", :kind "defn", :line 149, :end-line nil, :hash "-513829479"} {:id "defn-/consume-dilithium", :kind "defn-", :line 152, :end-line nil, :hash "1388960244"} {:id "defn-/warp-ship", :kind "defn-", :line 157, :end-line nil, :hash "-276158224"} {:id "defn-/impulse-ship", :kind "defn-", :line 187, :end-line nil, :hash "614941535"} {:id "defn/rotate-ship", :kind "defn", :line 206, :end-line nil, :hash "-1925884874"} {:id "defn/charge-shields", :kind "defn", :line 216, :end-line nil, :hash "252122130"} {:id "defn/repair-capacity", :kind "defn", :line 230, :end-line nil, :hash "810164079"} {:id "defn/repair-ship", :kind "defn", :line 235, :end-line nil, :hash "-797069544"} {:id "defn/update-destruction", :kind "defn", :line 253, :end-line nil, :hash "-691913651"} {:id "defn/constrain-ship", :kind "defn", :line 264, :end-line nil, :hash "-996981431"} {:id "defn/update-ship", :kind "defn", :line 270, :end-line nil, :hash "1089467633"} {:id "defn-/set-heading-handler", :kind "defn-", :line 285, :end-line nil, :hash "1444217055"} {:id "defn-/set-target-bearing-handler", :kind "defn-", :line 288, :end-line nil, :hash "-427250870"} {:id "defn-/set-engine-power-handler", :kind "defn-", :line 291, :end-line nil, :hash "665177956"} {:id "defn-/set-weapon-number-handler", :kind "defn-", :line 294, :end-line nil, :hash "-861340237"} {:id "defn-/set-weapon-spread-handler", :kind "defn-", :line 305, :end-line nil, :hash "282433183"} {:id "defn-/engage-engine-handler", :kind "defn-", :line 311, :end-line nil, :hash "-1654434315"} {:id "defn-/select-engine", :kind "defn-", :line 318, :end-line nil, :hash "1320877743"} {:id "defn-/select-impulse", :kind "defn-", :line 322, :end-line nil, :hash "-1015968027"} {:id "defn-/select-warp", :kind "defn-", :line 325, :end-line nil, :hash "1557997775"} {:id "defn-/select-front-view", :kind "defn-", :line 328, :end-line nil, :hash "1486103928"} {:id "defn-/select-strat-view", :kind "defn-", :line 331, :end-line nil, :hash "-1966312104"} {:id "defn-/select-tact-view", :kind "defn-", :line 334, :end-line nil, :hash "482436695"} {:id "defn-/select-weapon", :kind "defn-", :line 337, :end-line nil, :hash "-1504549164"} {:id "defn-/select-phaser", :kind "defn-", :line 343, :end-line nil, :hash "1372339022"} {:id "defn-/select-torpedo", :kind "defn-", :line 346, :end-line nil, :hash "592663459"} {:id "defn-/select-kinetic", :kind "defn-", :line 349, :end-line nil, :hash "265010969"} {:id "defn-/set-strat-scale", :kind "defn-", :line 352, :end-line nil, :hash "-374431909"} {:id "defn-/in-range-of-base", :kind "defn-", :line 355, :end-line nil, :hash "-1533933004"} {:id "defn-/resupply-ship", :kind "defn-", :line 360, :end-line nil, :hash "-1067330063"} {:id "defn-/resupply-ship-from-bases", :kind "defn-", :line 369, :end-line nil, :hash "-644879361"} {:id "defn/dock-ship", :kind "defn", :line 387, :end-line nil, :hash "-730536737"} {:id "defn/deployment-classes", :kind "defn", :line 402, :end-line nil, :hash "-2036110947"} {:id "defn-/find-deployable-star", :kind "defn-", :line 409, :end-line nil, :hash "559287736"} {:id "defn-/base-already-deployed?", :kind "defn-", :line 416, :end-line nil, :hash "128775193"} {:id "defn-/sufficient-resources-for-deployment?", :kind "defn-", :line 422, :end-line nil, :hash "1859225786"} {:id "defn/add-transport-routes-to", :kind "defn", :line 427, :end-line nil, :hash "667112512"} {:id "defn/deploy-base", :kind "defn", :line 442, :end-line nil, :hash "2091840552"} {:id "defn-/deploy-antimatter-factory", :kind "defn-", :line 467, :end-line nil, :hash "159736022"} {:id "defn-/deploy-dilithium-factory", :kind "defn-", :line 470, :end-line nil, :hash "-990739614"} {:id "defn-/deploy-weapon-factory", :kind "defn-", :line 473, :end-line nil, :hash "950400757"} {:id "defn-/deploy-corbomite-factory", :kind "defn-", :line 476, :end-line nil, :hash "2142896687"} {:id "defn/process-events", :kind "defn", :line 479, :end-line nil, :hash "-1587831849"} {:id "defn/dockable?", :kind "defn", :line 507, :end-line nil, :hash "-1356247118"} {:id "defn/deployable?", :kind "defn", :line 515, :end-line nil, :hash "-1715895728"} {:id "defn/reincarnate", :kind "defn", :line 527, :end-line nil, :hash "-186612791"}]}
;; clj-mutate-manifest-end
