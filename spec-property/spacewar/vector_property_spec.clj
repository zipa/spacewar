(ns spacewar.vector-property-spec
  (:require [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [spacewar.vector :as v]
            [speclj.core :refer [describe it should]]))

(defn- holds? [property]
  (let [result (tc/quick-check 100 property)]
    (should (:pass? result))))

(def scalar (gen/choose -100 100))
(def vec2 (gen/tuple scalar scalar))
(def nonzero-vec (gen/such-that (fn [[x y]] (or (not (zero? x)) (not (zero? y))))
                                vec2
                                30))

(describe "vector properties"
  (it "adds commutatively"
    (holds?
      (prop/for-all [a vec2 b vec2]
        (= (v/add a b) (v/add b a)))))

  (it "treats subtraction as the inverse of addition"
    (holds?
      (prop/for-all [a vec2 b vec2]
        (= a (v/subtract (v/add a b) b)))))

  (it "leaves a vector unchanged when scaled by 1"
    (holds?
      (prop/for-all [a vec2]
        (= a (v/scale 1 a)))))

  (it "gives unit vectors magnitude 1"
    (holds?
      (prop/for-all [a nonzero-vec]
        (let [u (v/unit a)]
          (< (Math/abs (- (v/magnitude u) 1.0)) 1.0e-9))))))
