(ns spacewar.ui.tactical-scan
  (:require [quil.core :as q #?@(:cljs [:include-macros true])]
            [spacewar.util :as util]
            [spacewar.ui.config :as uic]
            [spacewar.ui.icons :as icons]
            [spacewar.game-logic.config :as glc]
            [spacewar.ui.protocols :as p]
            [spacewar.geometry :as geo]
            [spacewar.vector :as vector]))

(defn alert-background-color [{:keys [shields antimatter
                                      life-support-damage
                                      hull-damage warp-damage
                                      impulse-damage sensor-damage
                                      weapons-damage]}]
  (let [max-damage (max life-support-damage
                        hull-damage
                        warp-damage
                        impulse-damage
                        sensor-damage
                        weapons-damage)]
    (cond (> max-damage 0) uic/dark-red
          (< (/ antimatter glc/ship-antimatter) 0.1) uic/dark-red
          (< (/ shields glc/ship-shields) 0.6) uic/dark-yellow
          :else uic/black)))

(defn background-color [{:keys [update-time ship game-over-timer]}]
  (if (or (pos? game-over-timer)
          (< (mod update-time 1000) 500))
    uic/black
    (alert-background-color ship)))

(defn- draw-background [state]
  (let [{:keys [w h]} state]
    (apply q/fill (background-color (:world state)))
    (q/rect-mode :corner)
    (q/rect 0 0 w h)))

(defn- in-range [x y ship]
  (< (geo/distance [x y] [(:x ship) (:y ship)]) (/ glc/tactical-range 2)))

(defn- click->pos [tactical-scan click]
  (let [{:keys [x y w h world]} tactical-scan
        ship (:ship world)
        center (vector/add [(/ w 2) (/ h 2)] [x y])
        scale (/ glc/tactical-range w)
        click-delta (vector/subtract click center)
        tactical-click-delta (vector/scale scale click-delta)]
    (vector/add tactical-click-delta [(:x ship) (:y ship)])))

(defn- click->bearing [tactical-scan click]
  (let [tactical-loc (click->pos tactical-scan click)
        ship (-> tactical-scan :world :ship)
        ship-loc [(:x ship) (:y ship)]
        bearing (geo/angle-degrees ship-loc tactical-loc)
        ] bearing))

(defn- present-objects [state objects]
  (if (empty? objects)
    []
    (let [{:keys [w world]} state
          ship (:ship world)
          scale (/ w glc/tactical-range)
          presentables (->> objects
                            (filter #(in-range (:x %) (:y %) ship))
                            (map #(assoc % :x (- (:x %) (:x ship))
                                           :y (- (:y %) (:y ship))))
                            (map #(assoc % :x (* (:x %) scale)
                                           :y (* (:y %) scale))))]
      presentables)))

(defn- draw-objects-in [state objects draw]
  (let [{:keys [w h]} state
        presentable-objects (present-objects state objects)]
    (doseq [{:keys [x y] :as object} presentable-objects]
      (q/with-translation
        [(+ x (/ w 2)) (+ y (/ h 2))]
        (draw object)))))

(defn- draw-objects [state key draw]
  (draw-objects-in state (-> state :world key) draw))

(defn- draw-bases [state]
  (draw-objects state :bases icons/draw-base-icon))

(defn- draw-transports [state]
  (draw-objects state :transports icons/draw-transport-icon))

(defn- draw-stars [state]
  (q/no-stroke)
  (q/ellipse-mode :center)
  (draw-objects state :stars icons/draw-star-icon))

(defn- draw-klingon-and-shield [klingon]
  (icons/draw-klingon-shields (:shields klingon))
  (icons/draw-klingon-icon klingon)
  (icons/draw-klingon-counts klingon))

(defn- draw-klingons [state]
  (draw-objects state :klingons draw-klingon-and-shield))

(defn- draw-romulans [state]
  (draw-objects state :romulans icons/draw-romulan))

(defn target-arc [ship]
  (let [{:keys [selected-weapon
                target-bearing
                weapon-spread-setting]} ship
        range (condp = selected-weapon
                :phaser uic/phaser-target
                :torpedo uic/torpedo-target
                :kinetic uic/kinetic-target
                0)
        half-spread (max 3 (/ weapon-spread-setting 2))]
    [range
     (- target-bearing half-spread)
     (+ target-bearing half-spread)]))

(defn- draw-target-arc [tgt-radius start stop]
  (q/no-stroke)
  (q/fill 255 255 255 50)
  (q/ellipse-mode :center)
  (q/arc 0 0 tgt-radius tgt-radius
         (geo/->radians start)
         (geo/->radians stop)
         #?(:clj :pie)))

(defn- draw-ship [state]
  (let [{:keys [w h]} state
        ship (->> state :world :ship)
        [vx vy] (icons/ship-velocity-vector ship)
        radians (geo/->radians (icons/ship-heading ship))
        [tgt-radius start stop] (target-arc ship)]
    (q/with-translation
      [(/ w 2) (/ h 2)]
      (when (not= :none (:selected-weapon ship))
        (draw-target-arc tgt-radius start stop))
      (icons/draw-ship-icon [vx vy] radians ship))))

(defn- draw-torpedo-segment []
  (let [angle (rand 360)
        color (repeatedly 3 #(+ 128 (rand 127)))
        length (+ 5 (rand 5))
        radians (geo/->radians angle)
        [tx ty] (vector/from-angular length radians)]
    (apply q/stroke color)
    (q/line 0 0 tx ty)))

(defn- draw-torpedo [color shot]
  (q/stroke-weight 1)
  (doseq [_ (range 3)]
    (draw-torpedo-segment))
  (apply q/fill (if (:corbomite shot) uic/red color))
  (q/ellipse-mode :center)
  (q/ellipse 0 0 4 4))

(defn- shots-of-type [state type]
  (filter #(= type (:type %)) (:shots (:world state))))

(defn- draw-shots-of-type [state type draw]
  (draw-objects-in state (shots-of-type state type) draw))

(defn- draw-torpedo-shots [state]
  (draw-shots-of-type state :torpedo (partial draw-torpedo uic/white)))

(defn- draw-klingon-torpedo-shots [state]
  (draw-shots-of-type state :klingon-torpedo (partial draw-torpedo uic/green)))

(defn- draw-romulan-blast-shots [state]
  (draw-shots-of-type state :romulan-blast (partial icons/draw-romulan-shot (/ (:w state) glc/tactical-range))))

(defn kinetic-shot-style [shot color]
  (if (:corbomite shot)
    {:color uic/red :radius 5}
    {:color color :radius 3}))

(defn- draw-kinetic-shot [color shot]
  (let [{:keys [color radius]} (kinetic-shot-style shot color)]
    (q/ellipse-mode :center)
    (q/no-stroke)
    (apply q/fill color)
    (q/ellipse 0 0 radius radius)))

(defn- draw-kinetic-shots [state]
  (draw-shots-of-type state :kinetic (partial draw-kinetic-shot uic/kinetic-color)))

(defn- draw-klingon-kinetic-shots [state]
  (draw-shots-of-type state :klingon-kinetic (partial draw-kinetic-shot uic/klingon-kinetic-color)))

(defn- phaser-intensity [range]
  (let [intensity (* 255 (- 1 (/ range glc/phaser-range)))]
    [intensity intensity intensity]))

(defn- phaser-color [shot]
  (phaser-intensity (:range shot)))

(defn- klingon-phaser-color [_]
  uic/green)

(defn- draw-phaser-shot [color-function shot]
  (let [{:keys [bearing]} shot
        radians (geo/->radians bearing)
        [sx sy] (vector/from-angular uic/phaser-length radians)
        beam-color (if (:corbomite shot) uic/red (color-function shot))]
    (apply q/stroke beam-color)
    (q/stroke-weight 6)
    (q/line 0 0 sx sy)))

(defn- draw-phaser-shots [state]
  (draw-shots-of-type state :phaser (partial draw-phaser-shot phaser-color)))

(defn- draw-klingon-phaser-shots [state]
  (draw-shots-of-type state :klingon-phaser (partial draw-phaser-shot klingon-phaser-color)))

(defn explosion-radius [age profile]
  (loop [profile profile radius 0 last-time 0]
    (let [{:keys [velocity until]} (first profile)]
      (cond (empty? profile)
            nil

            (> age until)
            (recur (rest profile)
                   (+ radius (* (- until last-time) velocity))
                   until)

            :else
            (+ radius (* velocity (- age last-time)))))))

(defn age-color [age profile]
  (loop [profile profile last-age 0 last-color [0 0 0]]
    (if (empty? profile)
      last-color
      (if (<= age (:until (first profile)))
        (let [profile-entry (first profile)
              {:keys [until colors]} profile-entry
              [c1 c2] colors
              diff (util/color-diff c2 c1)
              span (- until last-age)
              increment (util/color-scale diff (/ (- age last-age) span))]
          (util/color-add increment c1))
        (let [profile-entry (first profile)
              {:keys [until colors]} profile-entry]
          (recur (rest profile) until (last colors)))))))

(defn draw-fragment [fragment age fragment-color]
  (let [{:keys [velocity direction]} fragment
        radians (geo/->radians direction)
        velocity-vector (vector/from-angular velocity radians)
        [hx hy] (vector/scale age velocity-vector)
        [tx ty] (vector/scale (* age 0.9) velocity-vector)
        ]
    (q/stroke-weight 1)
    (apply q/stroke fragment-color)
    (q/line hx hy tx ty)))

(defn draw-explosion [state explosion]
  (let [{:keys [age type]} explosion
        {:keys [explosion-profile
                explosion-color-profile
                fragment-color-profile]} (type uic/explosion-profiles)]
    (let [fragments (present-objects state (:fragments explosion))
          radius (explosion-radius age explosion-profile)
          explosion-color (age-color age explosion-color-profile)
          fragment-color (age-color age fragment-color-profile)
          ex (- (rand 6) 3)
          ey (- (rand 6) 3)]
      (apply q/fill explosion-color)
      (q/ellipse-mode :center)
      (q/no-stroke)
      (q/ellipse ex ey radius radius)
      (doseq [fragment fragments]
        (draw-fragment fragment age fragment-color)))))

(defn- draw-explosions [state]
  (draw-objects state :explosions (partial draw-explosion state)))

(defn- draw-clouds [state]
  (draw-objects state :clouds icons/draw-cloud-icon))

(defn- draw-shots [state]
  (draw-phaser-shots state)
  (draw-torpedo-shots state)
  (draw-kinetic-shots state)
  (draw-klingon-kinetic-shots state)
  (draw-klingon-phaser-shots state)
  (draw-klingon-torpedo-shots state)
  (draw-romulan-blast-shots state))

(deftype tactical-scan [state]
  p/Drawable
  (draw [_]
    (let [{:keys [x y]} state]
      (q/with-translation
        [x y]
        (draw-background state)
        (draw-stars state)
        (draw-shots state)
        (draw-klingons state)
        (when (zero? (-> state :world :game-over-timer))
          (draw-ship state))
        (draw-bases state)
        (draw-transports state)
        (draw-explosions state)
        (draw-clouds state)
        (draw-romulans state)
        )))

  (setup [_]
    (tactical-scan. state))

  (update-state [_ world]
    (let [{:keys [x y w h]} state
          last-left-down (:left-down state)
          mx (q/mouse-x)
          my (q/mouse-y)
          mouse-in (geo/inside-rect [x y w h] [mx my])
          left-down (and mouse-in (q/mouse-pressed?) (= :left (q/mouse-button)))
          state (assoc state :mouse-in mouse-in :left-down left-down)
          left-up (and (not left-down) last-left-down mouse-in)
          pressed? (q/key-pressed?)
          the-key (q/key-as-keyword)
          key (and pressed? the-key)
          event (if left-up
                  (condp = key
                    :p {:event :debug-position-ship :pos (click->pos state [mx my])}
                    :c {:event :debug-dilithium-cloud :pos (click->pos state [mx my])}
                    :e {:event :debug-explosion :pos (click->pos state [mx my])}
                    :r {:event :debug-resupply-ship}
                    :k {:event :debug-add-klingon :pos (click->pos state [mx my])}
                    :K {:event :debug-add-kamikazee-klingon :pos (click->pos state [mx my])}
                    :R {:event :debug-add-romulan :pos (click->pos state [mx my])}
                    :P {:event :debug-add-pulsar :pos (click->pos state [mx my])}
                    :f {:event :debug-klingon-stats}
                    {:event :weapon-direction :angle (click->bearing state [mx my])})
                  nil)]
      (p/pack-update (tactical-scan. (assoc state :world world)) event)))

  )

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-09-02T15:19:29.764416-05:00", :module-hash "1757748227", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line nil, :hash "-1897451860"} {:id "defn/alert-background-color", :kind "defn", :line 11, :end-line nil, :hash "1730895951"} {:id "defn/background-color", :kind "defn", :line 27, :end-line nil, :hash "-480110998"} {:id "defn-/draw-background", :kind "defn-", :line 33, :end-line nil, :hash "825942674"} {:id "defn-/in-range", :kind "defn-", :line 39, :end-line nil, :hash "1208892990"} {:id "defn-/click->pos", :kind "defn-", :line 42, :end-line nil, :hash "-382790201"} {:id "defn-/click->bearing", :kind "defn-", :line 51, :end-line nil, :hash "123162258"} {:id "defn-/present-objects", :kind "defn-", :line 58, :end-line nil, :hash "216427940"} {:id "defn-/draw-objects-in", :kind "defn-", :line 72, :end-line nil, :hash "10105005"} {:id "defn-/draw-objects", :kind "defn-", :line 80, :end-line nil, :hash "662916671"} {:id "defn-/draw-bases", :kind "defn-", :line 83, :end-line nil, :hash "-677265022"} {:id "defn-/draw-transports", :kind "defn-", :line 86, :end-line nil, :hash "1243913959"} {:id "defn-/draw-stars", :kind "defn-", :line 89, :end-line nil, :hash "2065887959"} {:id "defn-/draw-klingon-and-shield", :kind "defn-", :line 94, :end-line nil, :hash "-189458959"} {:id "defn-/draw-klingons", :kind "defn-", :line 99, :end-line nil, :hash "-1075201940"} {:id "defn-/draw-romulans", :kind "defn-", :line 102, :end-line nil, :hash "-1345276942"} {:id "defn/target-arc", :kind "defn", :line 105, :end-line nil, :hash "331131591"} {:id "defn-/draw-target-arc", :kind "defn-", :line 119, :end-line nil, :hash "223166239"} {:id "defn-/draw-ship", :kind "defn-", :line 128, :end-line nil, :hash "1901537573"} {:id "defn-/draw-torpedo-segment", :kind "defn-", :line 140, :end-line nil, :hash "1252130796"} {:id "defn-/draw-torpedo", :kind "defn-", :line 149, :end-line nil, :hash "-1574191242"} {:id "defn-/shots-of-type", :kind "defn-", :line 157, :end-line nil, :hash "2036833819"} {:id "defn-/draw-shots", :kind "defn-", :line 160, :end-line nil, :hash "705483539"} {:id "defn-/draw-torpedo-shots", :kind "defn-", :line 163, :end-line nil, :hash "1121835101"} {:id "defn-/draw-klingon-torpedo-shots", :kind "defn-", :line 166, :end-line nil, :hash "-1287935316"} {:id "defn-/draw-romulan-blast-shots", :kind "defn-", :line 169, :end-line nil, :hash "1336458119"} {:id "defn/kinetic-shot-style", :kind "defn", :line 172, :end-line nil, :hash "1087892353"} {:id "defn-/draw-kinetic-shot", :kind "defn-", :line 177, :end-line nil, :hash "-1419753648"} {:id "defn-/draw-kinetic-shots", :kind "defn-", :line 184, :end-line nil, :hash "2113763034"} {:id "defn-/draw-klingon-kinetic-shots", :kind "defn-", :line 187, :end-line nil, :hash "1974523538"} {:id "defn-/phaser-intensity", :kind "defn-", :line 190, :end-line nil, :hash "-1292853832"} {:id "defn-/phaser-color", :kind "defn-", :line 194, :end-line nil, :hash "1563479252"} {:id "defn-/klingon-phaser-color", :kind "defn-", :line 197, :end-line nil, :hash "-1271569186"} {:id "defn-/draw-phaser-shot", :kind "defn-", :line 200, :end-line nil, :hash "-1446566353"} {:id "defn-/draw-phaser-shots", :kind "defn-", :line 209, :end-line nil, :hash "1489613281"} {:id "defn-/draw-klingon-phaser-shots", :kind "defn-", :line 212, :end-line nil, :hash "1448815833"} {:id "defn/explosion-radius", :kind "defn", :line 215, :end-line nil, :hash "-1457699058"} {:id "defn/age-color", :kind "defn", :line 229, :end-line nil, :hash "1598005774"} {:id "defn/draw-fragment", :kind "defn", :line 245, :end-line nil, :hash "783676924"} {:id "defn/draw-explosion", :kind "defn", :line 256, :end-line nil, :hash "-1162631426"} {:id "defn-/draw-explosions", :kind "defn-", :line 274, :end-line nil, :hash "193979323"} {:id "defn-/draw-clouds", :kind "defn-", :line 277, :end-line nil, :hash "-534272277"} {:id "defn-/draw-shots", :kind "defn-", :line 280, :end-line nil, :hash "1474085514"} {:id "form/43/deftype", :kind "deftype", :line 289, :end-line nil, :hash "-1810698516"}]}
;; clj-mutate-manifest-end
