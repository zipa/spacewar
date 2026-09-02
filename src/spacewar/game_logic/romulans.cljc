(ns spacewar.game-logic.romulans
  (:require [clojure.spec.alpha :as s]
            [spacewar.game-logic.config :as glc]
            [spacewar.game-logic.explosions :as explosions]
            [spacewar.game-logic.shots :as shots]
            [spacewar.game-logic.notifications :as notifications]
            [spacewar.geometry :as geo]
            [spacewar.vector :as vector]
            [spacewar.util :as util]))

(s/def ::x number?)
(s/def ::y number?)
(s/def ::age number?)
(s/def ::state #{:invisible :appearing :visible :firing :fading :disappeared})
(s/def ::fire-weapon boolean?)

(s/def ::romulan (s/keys :reg-un [::x ::y ::age ::state ::fire-weapon]))
(s/def ::romulans (s/coll-of ::romulan))

(defn make-romulan [x y] {:x x :y y :age 0 :state :invisible :fire-weapon false})

(defn update-romulans-age [ms world]
  (let [romulans (:romulans world)
        romulans (map #(update % :age + ms) romulans)]
    (assoc world :romulans romulans)))

(def romulan-state-duration
  {:invisible glc/romulan-invisible-time
   :appearing glc/romulan-appearing-time
   :visible glc/romulan-visible-time
   :firing glc/romulan-firing-time
   :fading glc/romulan-fading-time})

(defn romulan-state-transition [ms age state]
  (and (> age (romulan-state-duration state))
       (<= (rem age 1000) ms)
       (< 0.5 (rand 1))))

(defn update-romulan-state [ms romulan]
  (if (romulan-state-transition ms (:age romulan) (:state romulan))
    (let [next-state (condp = (:state romulan)
                       :invisible :appearing
                       :appearing :visible
                       :visible :firing
                       :firing :fading
                       :fading :disappeared)]
      (assoc romulan :state next-state :age 0 :fire-weapon (= next-state :fading)
                     :appeared? (= next-state :appearing)))
    romulan))

(defn update-romulans-state [ms world]
  (let [romulans (map #(update-romulan-state ms %) (:romulans world))
        appeared (filter :appeared? romulans)
        romulans (map #(dissoc % :appeared?) romulans)
        world (assoc world :romulans romulans)]
    (reduce (fn [w _] (notifications/notify w :romulan-appearing))
            world
            appeared)))

(defn remove-disappeared-romulans [world]
  (let [romulans (:romulans world)
        romulans (doall (remove #(= :disappeared (:state %)) romulans))]
    (assoc world :romulans romulans)))

(defn- explode-romulan [romulan]
  (explosions/->explosion :romulan romulan)
  )

(defn destroy-hit-romulans [{:keys [romulans explosions romulans-killed] :as world}]
  (let [hit-romulans (filter :hit romulans)
        explosions (concat explosions (map explode-romulan hit-romulans))
        romulans (remove :hit romulans)
        romulans-killed (+ romulans-killed (count hit-romulans))]
    (assoc world :romulans romulans
                 :explosions explosions
                 :romulans-killed romulans-killed)))

(defn- romulan-shots [ship romulan]
  (if (:fire-weapon romulan)
    (let [bearing (geo/angle-degrees (util/pos romulan) (util/pos ship))
          shot (shots/->shot (:x romulan) (:y romulan) bearing :romulan-blast)]
      shot)
    nil))

(defn fire-romulan-weapons [world]
  (let [{:keys [shots romulans ship]} world
        new-shots (filter some? (map #(romulan-shots ship %) romulans))
        romulans (map #(assoc % :fire-weapon false) romulans)]
    (assoc world :romulans romulans :shots (concat shots new-shots)))
  )

(defn update-romulans [ms world]
  (->> world
       (update-romulans-age ms)
       (update-romulans-state ms)
       (fire-romulan-weapons)
       (remove-disappeared-romulans)
       (destroy-hit-romulans)))

(defn add-romulan [{:keys [ship romulans] :as world}]
  (if (zero? (:warp ship))
    (let [{:keys [x y]} ship
          dist (- glc/romulan-appear-distance (* glc/romulan-appear-distance (rand 0.5)))
          angle (rand 360)
          pos (vector/from-angular dist (geo/->radians angle))
          [rx ry] (vector/add [x y] pos)
          romulans (conj romulans (make-romulan rx ry))]
      (assoc world :romulans romulans))
    world)
  )

(defn add-occasional-romulan [world]
  (if (< (rand 1) glc/romulan-appear-odds-per-second)
    (add-romulan world)
    world))


(defn update-romulans-per-second [world]
  (-> world
      (add-occasional-romulan)))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-09-02T15:16:25.972046-05:00", :module-hash "-1347853681", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line nil, :hash "1419516039"} {:id "form/1/s/def", :kind "s/def", :line 11, :end-line nil, :hash "-1821849317"} {:id "form/2/s/def", :kind "s/def", :line 12, :end-line nil, :hash "-590949776"} {:id "form/3/s/def", :kind "s/def", :line 13, :end-line nil, :hash "-262737073"} {:id "form/4/s/def", :kind "s/def", :line 14, :end-line nil, :hash "791653934"} {:id "form/5/s/def", :kind "s/def", :line 15, :end-line nil, :hash "-911318250"} {:id "form/6/s/def", :kind "s/def", :line 17, :end-line nil, :hash "-472298019"} {:id "form/7/s/def", :kind "s/def", :line 18, :end-line nil, :hash "-1399541548"} {:id "defn/make-romulan", :kind "defn", :line 20, :end-line nil, :hash "-1499860732"} {:id "defn/update-romulans-age", :kind "defn", :line 22, :end-line nil, :hash "897340577"} {:id "def/romulan-state-duration", :kind "def", :line 27, :end-line nil, :hash "92879147"} {:id "defn/romulan-state-transition", :kind "defn", :line 34, :end-line nil, :hash "-587681179"} {:id "defn/update-romulan-state", :kind "defn", :line 39, :end-line nil, :hash "641101397"} {:id "defn/update-romulans-state", :kind "defn", :line 51, :end-line nil, :hash "1226790010"} {:id "defn/remove-disappeared-romulans", :kind "defn", :line 60, :end-line nil, :hash "-1844102003"} {:id "defn-/explode-romulan", :kind "defn-", :line 65, :end-line nil, :hash "1832693499"} {:id "defn/destroy-hit-romulans", :kind "defn", :line 69, :end-line nil, :hash "1819180135"} {:id "defn-/romulan-shots", :kind "defn-", :line 78, :end-line nil, :hash "-834018807"} {:id "defn/fire-romulan-weapons", :kind "defn", :line 85, :end-line nil, :hash "409026408"} {:id "defn/update-romulans", :kind "defn", :line 92, :end-line nil, :hash "1991701697"} {:id "defn/add-romulan", :kind "defn", :line 100, :end-line nil, :hash "369710481"} {:id "defn/add-occasional-romulan", :kind "defn", :line 112, :end-line nil, :hash "-647004437"} {:id "defn/update-romulans-per-second", :kind "defn", :line 118, :end-line nil, :hash "1620581680"}]}
;; clj-mutate-manifest-end
