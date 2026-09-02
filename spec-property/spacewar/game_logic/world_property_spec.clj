(ns spacewar.game-logic.world-property-spec
  (:require [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [spacewar.game-logic.explosions :as explosions]
            [spacewar.game-logic.notifications :as notifications]
            [spacewar.game-logic.spec-mother :as mom]
            [spacewar.game-logic.world :as world]
            [speclj.core :refer [describe it should]]))

(defn- holds? [trials property]
  (let [result (tc/quick-check trials property)]
    (should (:pass? result))))

(describe "world properties"
  (it "creates a spec-valid initial world"
    (holds?
      20
      (prop/for-all [_ (gen/choose 0 100)]
        (true? (mom/valid-world? (world/make-initial-world))))))

  (it "appends notification keys in order"
    (holds?
      100
      (prop/for-all [keys (gen/vector gen/keyword 0 20)]
        (= keys (:messages (reduce notifications/notify {:messages []} keys))))))

  (it "never increases the number of explosions when aging them"
    (holds?
      100
      (prop/for-all [age (gen/choose 0 20000)
                     ms (gen/choose 1 500)]
        (let [before {:explosions [{:age age :type :phaser}]}
              after (explosions/update-explosions ms before)]
          (<= (count (:explosions after)) (count (:explosions before))))))))
