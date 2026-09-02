(ns spacewar.ui.messages-hardening-spec
  (:require [spacewar.game-logic.spec-mother :as mom]
            [spacewar.ui.messages :refer [add-messages! item-message last-message send-message]]
            [speclj.core :refer [before describe it should-have-invoked should-not-have-invoked stub with with-stubs]]))

(describe "item-message change detection"
  (with-stubs)
  (before (reset! last-message {:key 0}))
  (with thresholds [[0.2 :first]
                    [0.5 :second]
                    [0.6 nil]
                    [1 :third]])

  (it "treats an unchanged low-scale reading as no change"
    (reset! last-message {:key 11})
    (with-redefs [send-message (stub :send-message)]
      (item-message {:key 11} :key 100 @thresholds)
      (should-not-have-invoked :send-message)))

  (it "does not send at exactly a 10% change"
    (with-redefs [send-message (stub :send-message)]
      (item-message {:key 10} :key 100 @thresholds)
      (should-not-have-invoked :send-message)))

  (it "sends the first bucket at exactly 0%"
    (reset! last-message {:key 50})
    (with-redefs [send-message (stub :send-message)]
      (item-message {:key 0} :key 100 @thresholds)
      (should-have-invoked :send-message {:with [:first 0 100]})))

  (it "sends the first bucket just below the 0.2 threshold"
    (reset! last-message {:key 50})
    (with-redefs [send-message (stub :send-message)]
      (item-message {:key 19} :key 100 @thresholds)
      (should-have-invoked :send-message {:with [:first 19 100]})))

  (it "sends the second bucket at exactly the 0.2 threshold"
    (reset! last-message {:key 0})
    (with-redefs [send-message (stub :send-message)]
      (item-message {:key 20} :key 100 @thresholds)
      (should-have-invoked :send-message {:with [:second 20 100]}))))

(describe "ship damage thresholds"
  (with-stubs)
  (it "reports hull critical near total destruction"
    (reset! last-message (mom/make-ship))
    (with-redefs [send-message (stub :send-message)]
      (add-messages! {:ship (assoc (mom/make-ship) :hull-damage 90)})
      (should-have-invoked :send-message {:with [:hull-critical 90 100]}))))
