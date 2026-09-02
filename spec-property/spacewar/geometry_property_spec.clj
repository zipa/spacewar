(ns spacewar.geometry-property-spec
  (:require [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [spacewar.geometry :as geo]
            [speclj.core :refer [describe it should]]))

(defn- holds? [property]
  (let [result (tc/quick-check 100 property)]
    (should (:pass? result))))

(defn- angle-diff [a b]
  (let [delta (mod (- a b) 360.0)
        delta (if (> delta 180.0) (- delta 360.0) delta)]
    (Math/abs delta)))

(def finite (gen/double* {:min -720.0 :max 720.0 :NaN? false :infinite? false}))
(def coord (gen/choose -1000 1000))
(def point (gen/tuple coord coord))

(describe "geometry properties"
  (it "converts degrees to radians and back modulo 360"
    (holds?
      (prop/for-all [d finite]
        (let [round-trip (geo/->degrees (geo/->radians d))]
          (< (angle-diff round-trip d) 1.0e-6)))))

  (it "has a symmetric non-negative distance"
    (holds?
      (prop/for-all [a point b point]
        (let [ab (geo/distance a b)
              ba (geo/distance b a)]
          (and (>= ab 0)
               (< (Math/abs (- ab ba)) 1.0e-6))))))

  (it "treats a point as inside a rectangle only when within half-open bounds"
    (holds?
      (prop/for-all [x (gen/choose -100 100)
                     y (gen/choose -100 100)
                     w (gen/choose 1 50)
                     h (gen/choose 1 50)
                     px (gen/choose -100 100)
                     py (gen/choose -100 100)]
        (= (geo/inside-rect [x y w h] [px py])
           (and (>= px x)
                (>= py y)
                (< px (+ x w))
                (< py (+ y h))))))))
