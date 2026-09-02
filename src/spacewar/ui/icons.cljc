(ns spacewar.ui.icons
  (:require [quil.core :as q #?@(:cljs [:include-macros true])]
            [spacewar.ui.config :as uic]
            [spacewar.game-logic.config :as glc]
            [spacewar.geometry :as geo]
            [spacewar.vector :as vector]
            [spacewar.util :as util]))

(def transport-colors
  {:antimatter uic/orange
   :dilithium uic/yellow})

(defn transport-color [commodity]
  (transport-colors commodity))

(def transport-triangle
  {:dilithium [0 -8 -6 6 6 6]
   :antimatter [0 8 -6 -6 6 -6]})

(defn- draw-transport-shape [commodity]
  (apply q/triangle (transport-triangle commodity)))

(defn draw-transport-icon [transport]
  (let [commodity (:commodity transport)]
    (q/ellipse-mode :center)
    (q/no-stroke)
    (apply q/fill (transport-color commodity))
    (draw-transport-shape commodity)))

(defn ship-heading [ship]
  (or (:heading ship) 0))

(defn ship-velocity-vector [ship]
  (vector/scale uic/velocity-vector-scale (or (:velocity ship) [0 0])))

(defn pulse-stroke-weight [active? millis on-weight off-weight]
  (if (and active? (> (mod millis 500) 250))
    on-weight
    off-weight))

(defn- age-angle [age]
  (let [maturity (min 1 (/ age glc/base-maturity-age))]
    (* (- 1 maturity) 2 Math/PI)))

(defn- draw-base-age [age]
  (q/fill 0 0 0 150)
  (q/no-stroke)
  (q/ellipse-mode :center)
  (q/arc 0 0 30 30 0 (age-angle age) #?(:clj :pie)))

(defn- draw-inventory-circle [commodity maximum color radius]
  (let [angle (* 2 Math/PI (/ commodity maximum))]
    (q/stroke-weight 3)
    (q/no-fill)
    (when (> angle 0.01)
      (apply q/stroke color)
      (q/arc 0 0 radius radius 0 angle))))

(defn- draw-base-contents [antimatter dilithium corbomite]
  (draw-inventory-circle dilithium glc/base-dilithium-maximum uic/yellow 30)
  (draw-inventory-circle antimatter glc/base-antimatter-maximum uic/orange 38)
  (draw-inventory-circle corbomite glc/corbomite-maximum uic/green 46))

(defn- draw-base-counts [base]
  (apply q/fill uic/white)
  (apply q/stroke uic/white)
  (q/stroke-weight 1)

  (q/text-align :right :center)
  (q/text-font (:lcars-small (q/state :fonts)) 12)
  (q/text (str "T-" (int (:torpedos base))) -30 0)
  (q/text-align :left :center)
  (q/text (str "K-" (int (:kinetics base))) 30 0)
  )

(defn- draw-base-adornments [base]
  (draw-base-age (:age base))
  (draw-base-contents (:antimatter base) (:dilithium base) (:corbomite base))
  (when (= (:type base) :weapon-factory)
    (draw-base-counts base)))

(defn draw-romulan-icon []
  (q/stroke-weight 2)
  (q/line -3 -6 3 -6)
  (q/line 3 -6 6 -3)
  (q/line 6 -3 6 3)
  (q/line 6 3 3 9)
  (q/line 3 9 -3 9)
  (q/line -3 9 -6 3)
  (q/line -6 3 -6 -3)
  (q/line -6 -3 -3 -6)
  (q/line 6 -3 12 -9)
  (q/line 12 -9 12 -6)
  (q/line 12 -6 6 3)
  (q/line -6 -3 -12 -9)
  (q/line -12 -9 -12 -6)
  (q/line -12 -6 -6 3)
  (q/line -12 -12 -12 3)
  (q/line 12 -12 12 3)

  )

(defmulti draw-romulan :state)

(defmethod draw-romulan :invisible [romulan]
  (q/no-stroke)
  (q/fill 255 255 255 (min 15 (* 15 (/ (:age romulan) glc/romulan-invisible-time))))
  (q/ellipse 0 0 30 30)
  )

(defmethod draw-romulan :appearing [romulan]
  (apply q/stroke (conj uic/white (min 255 (* 255 (/ (:age romulan) glc/romulan-appearing-time)))))
  (draw-romulan-icon)
  )

(defmethod draw-romulan :visible [romulan]
  (apply q/stroke (util/color-shift uic/white uic/orange (min 1 (/ (:age romulan) glc/romulan-visible-time))))
  (draw-romulan-icon)
  )

(defmethod draw-romulan :firing [romulan]
  (apply q/stroke (util/color-shift uic/orange uic/red (min 1 (/ (:age romulan) glc/romulan-firing-time))))
  (doseq [_ (range 10)]
    (q/line 0 0 (- 30 (rand 60)) (- 30 (rand 60))))
  (draw-romulan-icon)
  )

(defmethod draw-romulan :fading [romulan]
  (apply q/stroke (util/color-shift uic/red uic/black (min 1 (/ (:age romulan) glc/romulan-fading-time))))
  (draw-romulan-icon)
  )

(defmethod draw-romulan :disappeared [_]
  )

(defn draw-strategic-romulan []
  (apply q/stroke uic/orange)
  (draw-romulan-icon))

(defmulti draw-base-icon :type)

(defmethod draw-base-icon :weapon-factory [base]
  (q/no-fill)
  (apply q/stroke uic/weapon-factory-color)
  (q/stroke-weight 2)
  (q/ellipse-mode :center)
  (q/ellipse 0 0 12 12)
  (q/ellipse 0 0 20 20)
  (q/line 0 -6 0 6)
  (q/line -6 0 6 0)
  (draw-base-adornments base))

(defmethod draw-base-icon :antimatter-factory [base]
  (q/no-fill)
  (apply q/stroke uic/antimatter-factory-color)
  (q/stroke-weight 2)
  (q/ellipse-mode :center)
  (q/ellipse 0 0 12 12)
  (q/line 0 -6 0 6)
  (q/line -6 0 6 0)
  (q/ellipse 0 -8 5 5)
  (q/ellipse 0 8 5 5)
  (q/ellipse -8 0 5 5)
  (q/ellipse 8 0 5 5)
  (draw-base-adornments base))


(defmethod draw-base-icon :dilithium-factory [base]
  (q/no-fill)
  (apply q/stroke uic/dilithium-factory-color)
  (q/stroke-weight 2)
  (q/ellipse-mode :center)
  (q/quad 0 6 6 0 0 -6 -6 0)
  (q/quad 0 10 10 0 0 -10 -10 0)
  (q/line 0 -6 0 6)
  (q/line -6 0 6 0)
  (q/line 3 10 -3 10)
  (q/line 3 -10 -3 -10)
  (q/line 10 3 10 -3)
  (q/line -10 3 -10 -3)
  (draw-base-adornments base))

(defmethod draw-base-icon :corbomite-factory [base]
  (q/no-fill)
  (apply q/stroke uic/corbomite-factory-color)
  (q/stroke-weight 2)
  (q/line -10 10 10 -10)
  (q/line 10 10 -10 -10)
  (q/line -10 10 10 10)
  (q/line -10 -10 10 -10)
  (apply q/fill (if (< 200 (mod (q/millis) 400))
                  uic/corbomite-factory-color
                  uic/black))
  (q/ellipse 0 0 10 10)
  (draw-base-adornments base))

(defmethod draw-base-icon :corbomite-device [_base]
  (q/no-fill)
  (apply q/stroke uic/corbomite-factory-color)
  (q/stroke-weight 2)
  (q/line -10 10 10 -10)
  (q/line 10 10 -10 -10)
  (q/line -10 10 10 10)
  (q/line -10 -10 10 -10)
  (apply q/stroke uic/red)
  (q/stroke-weight 4)
  (q/line -10 0 10 0)
  (apply q/fill (if (< 200 (mod (q/millis) 400))
                  uic/red
                  uic/black))
  (q/ellipse 0 0 10 10))

(def cruise-state-label
  {:patrol "P" :refuel "R" :guard "G" :mission "M"})

(def mission-label
  {:blockade "B" :seek-and-destroy "A" :escape-corbomite "E"})

(def battle-state-label
  {:no-battle "n"
   :flank-right "fr"
   :flank-left "fl"
   :retreating "r"
   :advancing "a"
   :kamikazee "K"})

(defn klingon-state [{:keys [cruise-state battle-state mission]}]
  (str (get mission-label mission "-")
       ":"
       (get cruise-state-label cruise-state "X")
       "-"
       (battle-state-label battle-state)))

(defn draw-klingon-counts [klingon]
  (let [shields (int (:shields klingon))]
    (when @glc/klingon-stats
      (apply q/fill uic/white)
      (apply q/stroke uic/white)
      (q/stroke-weight 1)
      (q/text-align :right :center)
      (q/text-font (:lcars-small (q/state :fonts)) 12)
      (q/text (str "T-" (int (:torpedos klingon))) -30 0)
      (q/text-align :left :center)
      (q/text (str "A-" (int (/ (:antimatter klingon) glc/klingon-antimatter 0.01)) "%") 30 0)
      (q/text-align :center :bottom)
      (q/text (str (klingon-state klingon) "[" shields "]") 0 -30)
      (q/text-align :center :top)
      (q/text (str "K-" (int (:kinetics klingon))) 0 30)
      )))

(defn draw-klingon-icon [klingon]
  (apply q/fill uic/black)
  (apply q/stroke uic/klingon-color)
  (q/stroke-weight (pulse-stroke-weight (= :kamikazee (:battle-state klingon))
                                        (q/millis) 5 2))
  (q/ellipse-mode :center)
  (q/line 0 0 10 -6)
  (q/line 10 -6 14 -3)
  (q/line 0 0 -10 -6)
  (q/line -10 -6 -14 -3)
  (q/ellipse 0 0 6 6))

(defn klingon-shield-appearance [shields]
  (when (< shields glc/klingon-shields)
    (let [pct (/ shields glc/klingon-shields)]
      {:pct pct
       :radius (+ 35 (* pct 20))})))

(defn shield-fill-color [pct flicker]
  [255 (* pct 255) 0 (if flicker (* pct 100) 100)])

(defn draw-klingon-shields [shields]
  (when-let [{:keys [pct radius]} (klingon-shield-appearance shields)]
    (apply q/fill (shield-fill-color pct (< (rand 3) pct)))
    (q/ellipse-mode :center)
    (q/no-stroke)
    (q/ellipse 0 0 radius radius)))

(defn draw-ship-icon [[vx vy] radians ship]
  (apply q/stroke uic/enterprise-vector-color)
  (q/stroke-weight 2)
  (q/line 0 0 vx vy)
  (q/with-rotation
    [radians]
    (apply q/stroke uic/enterprise-color)
    (q/stroke-weight (pulse-stroke-weight (:corbomite-device-installed ship)
                                          (q/millis) 4 2))
    (q/ellipse-mode :center)
    (apply q/fill uic/black)
    (q/line -9 -9 0 0)
    (q/line -9 9 0 0)
    (q/ellipse 0 0 9 9)
    (q/line -5 9 -15 9)
    (q/line -5 -9 -15 -9)))

(defn pulsar-visible? [star-class millis]
  (or (not= star-class :pulsar)
      (< (mod millis 500) 250)))

(defn draw-star-icon [star]
  (let [class (:class star)]
    (apply q/fill (class uic/star-colors))
    (when (pulsar-visible? class (q/millis))
      (q/ellipse 0 0 (class uic/star-sizes) (class uic/star-sizes)))))

(defn- draw-blob [jitter half-jitter diameter]
  (q/ellipse (- half-jitter (rand jitter))
             (- half-jitter (rand jitter))
             diameter diameter)
  )

(defn- draw-spark [x y]
  (apply q/fill uic/yellow)
  (q/ellipse x y 1 1))

(defn- rand-sign []
  (if (< 0.5 (rand 1)) 1 -1))

(defn draw-cloud-icon [cloud]
  (let [diameter (* 0.3 (:concentration cloud))
        jitter (/ diameter 5)
        half-jitter (/ jitter 2)]
    (apply q/fill (conj uic/yellow 10))
    (q/no-stroke)
    (q/ellipse-mode :center)
    (doseq [_ (range 10)]
      (draw-blob jitter half-jitter (* diameter (- 0.5 (rand 1)))))
    (doseq [_ (range 20)]
      (let [r (rand (/ diameter 4))
            x (+ 2 (rand r))
            x-sqr (* x x)
            y (Math/sqrt (- (* r r) x-sqr))
            x (* (rand-sign) x)
            y (* (rand-sign) y)]
        (draw-spark x y)))))

(defn- romulan-blast-visual-intensity [range]
  (let [factor (* 255 (- 1.0 (/ range glc/romulan-blast-range)))]
    factor))

(defn- romulan-blast-weight []
  (- 10 (rand 8)))

(defn- romulan-blast-color []
  [255 (rand 255) (rand 50)])

(defn draw-romulan-shot [scale shot]
  (let [{:keys [range bearing]} shot
        scaled-range (* range scale)
        radians (geo/->radians bearing)
        radians-to-origin (+ radians Math/PI)
        shot-center (vector/from-angular scaled-range radians-to-origin)
        shot-x (first shot-center)
        shot-y (second shot-center)
        half-pi (/ Math/PI 2)]
    (apply q/stroke (conj (romulan-blast-color) (romulan-blast-visual-intensity range)))
    (q/stroke-weight (romulan-blast-weight))
    (q/ellipse-mode :radius)
    (q/no-fill)
    (q/arc shot-x shot-y scaled-range scaled-range (- radians half-pi) (+ radians half-pi)))
  )

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-09-02T15:27:13.491838-05:00", :module-hash "260163330", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line nil, :hash "600165237"} {:id "def/transport-colors", :kind "def", :line 9, :end-line nil, :hash "1330093860"} {:id "defn/transport-color", :kind "defn", :line 13, :end-line nil, :hash "1757374122"} {:id "def/transport-triangle", :kind "def", :line 16, :end-line nil, :hash "244320460"} {:id "defn-/draw-transport-shape", :kind "defn-", :line 20, :end-line nil, :hash "717833840"} {:id "defn/draw-transport-icon", :kind "defn", :line 23, :end-line nil, :hash "1408402312"} {:id "defn/ship-heading", :kind "defn", :line 30, :end-line nil, :hash "-493524199"} {:id "defn/ship-velocity-vector", :kind "defn", :line 33, :end-line nil, :hash "-1115421989"} {:id "defn/pulse-stroke-weight", :kind "defn", :line 36, :end-line nil, :hash "1449677673"} {:id "defn-/age-angle", :kind "defn-", :line 41, :end-line nil, :hash "-1213702589"} {:id "defn-/draw-base-age", :kind "defn-", :line 45, :end-line nil, :hash "-934628582"} {:id "defn-/draw-inventory-circle", :kind "defn-", :line 51, :end-line nil, :hash "394245389"} {:id "defn-/draw-base-contents", :kind "defn-", :line 59, :end-line nil, :hash "-959496878"} {:id "defn-/draw-base-counts", :kind "defn-", :line 64, :end-line nil, :hash "-1812298811"} {:id "defn-/draw-base-adornments", :kind "defn-", :line 76, :end-line nil, :hash "877578654"} {:id "defn/draw-romulan-icon", :kind "defn", :line 82, :end-line nil, :hash "-622202833"} {:id "defmulti/draw-romulan", :kind "defmulti", :line 103, :end-line nil, :hash "1723152823"} {:id "defmethod/draw-romulan/:invisible", :kind "defmethod", :line 105, :end-line nil, :hash "1685113575"} {:id "defmethod/draw-romulan/:appearing", :kind "defmethod", :line 111, :end-line nil, :hash "-835465715"} {:id "defmethod/draw-romulan/:visible", :kind "defmethod", :line 116, :end-line nil, :hash "178240182"} {:id "defmethod/draw-romulan/:firing", :kind "defmethod", :line 121, :end-line nil, :hash "-337225742"} {:id "defmethod/draw-romulan/:fading", :kind "defmethod", :line 128, :end-line nil, :hash "2012318282"} {:id "defmethod/draw-romulan/:disappeared", :kind "defmethod", :line 133, :end-line nil, :hash "-923173760"} {:id "defn/draw-strategic-romulan", :kind "defn", :line 136, :end-line nil, :hash "-1422029552"} {:id "defmulti/draw-base-icon", :kind "defmulti", :line 140, :end-line nil, :hash "292678971"} {:id "defmethod/draw-base-icon/:weapon-factory", :kind "defmethod", :line 142, :end-line nil, :hash "-514895037"} {:id "defmethod/draw-base-icon/:antimatter-factory", :kind "defmethod", :line 153, :end-line nil, :hash "1342257635"} {:id "defmethod/draw-base-icon/:dilithium-factory", :kind "defmethod", :line 168, :end-line nil, :hash "-1978398845"} {:id "defmethod/draw-base-icon/:corbomite-factory", :kind "defmethod", :line 183, :end-line nil, :hash "1321563952"} {:id "defmethod/draw-base-icon/:corbomite-device", :kind "defmethod", :line 197, :end-line nil, :hash "-960189443"} {:id "def/cruise-state-label", :kind "def", :line 213, :end-line nil, :hash "-1469247729"} {:id "def/mission-label", :kind "def", :line 216, :end-line nil, :hash "254971457"} {:id "def/battle-state-label", :kind "def", :line 219, :end-line nil, :hash "599221409"} {:id "defn/klingon-state", :kind "defn", :line 227, :end-line nil, :hash "1757240205"} {:id "defn/draw-klingon-counts", :kind "defn", :line 234, :end-line nil, :hash "-699052308"} {:id "defn/draw-klingon-icon", :kind "defn", :line 251, :end-line nil, :hash "876542638"} {:id "defn/klingon-shield-appearance", :kind "defn", :line 263, :end-line nil, :hash "1354760667"} {:id "defn/shield-fill-color", :kind "defn", :line 269, :end-line nil, :hash "-2126496365"} {:id "defn/draw-klingon-shields", :kind "defn", :line 272, :end-line nil, :hash "1363637682"} {:id "defn/draw-ship-icon", :kind "defn", :line 279, :end-line nil, :hash "-439385141"} {:id "defn/pulsar-visible?", :kind "defn", :line 296, :end-line nil, :hash "1758776766"} {:id "defn/draw-star-icon", :kind "defn", :line 300, :end-line nil, :hash "-1293846891"} {:id "defn-/draw-blob", :kind "defn-", :line 306, :end-line nil, :hash "-915192409"} {:id "defn-/draw-spark", :kind "defn-", :line 312, :end-line nil, :hash "-118580277"} {:id "defn-/rand-sign", :kind "defn-", :line 316, :end-line nil, :hash "-1976281536"} {:id "defn/draw-cloud-icon", :kind "defn", :line 319, :end-line nil, :hash "103834135"} {:id "defn-/romulan-blast-visual-intensity", :kind "defn-", :line 337, :end-line nil, :hash "-1078501130"} {:id "defn-/romulan-blast-weight", :kind "defn-", :line 341, :end-line nil, :hash "-617709196"} {:id "defn-/romulan-blast-color", :kind "defn-", :line 344, :end-line nil, :hash "-751959723"} {:id "defn/draw-romulan-shot", :kind "defn", :line 347, :end-line nil, :hash "-2013190537"}]}
;; clj-mutate-manifest-end
