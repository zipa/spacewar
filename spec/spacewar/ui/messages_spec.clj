(ns spacewar.ui.messages-spec
  (:require
    [spacewar.ui.messages :refer :all]
    [spacewar.ui.view-frame :as view-frame]
    [speclj.core :refer :all]))

(declare thresholds)

(describe "Messages"
  (with-stubs)
  (before (reset! last-message {:key 0}))
  (with thresholds [[0.2 :first]
                    [0.5 :second]
                    [0.6 nil]
                    [1 :third]])

  (it "sends no message when there's nothing in last-message"
    (reset! last-message {})
    (with-redefs [send-message (stub :send-message)]
      (item-message {:key 0} :key 100 @thresholds)
      (should-not-have-invoked :send-message)))

  (it "sends no message when there has been less than a 10% change"
    (with-redefs [send-message (stub :send-message)]
      (item-message {:key 9} :key 100 @thresholds)
      (should-not-have-invoked :send-message)))

  (it "sends :first message after >10% change"
    (with-redefs [send-message (stub :send-message)]
      (item-message {:key 11} :key 100 @thresholds)
      (should-have-invoked :send-message {:with [:first 11 100]})
      (should= 11 (:key @last-message))))

  (it "sends :second message after >10% change"
    (with-redefs [send-message (stub :send-message)]
      (item-message {:key 49} :key 100 @thresholds)
      (should-have-invoked :send-message {:with [:second 49 100]})))

  (it "sends :third message after >10% change"
    (with-redefs [send-message (stub :send-message)]
      (item-message {:key 99} :key 100 @thresholds)
      (should-have-invoked :send-message {:with [:third 99 100]})))

  (it "sends no message if beyond threshold after >10% change"
    (with-redefs [send-message (stub :send-message)]
      (item-message {:key 101} :key 100 @thresholds)
      (should-not-have-invoked :send-message)
      (should= 101 (:key @last-message))))

  (it "sends no message if message is nil"
    (with-redefs [send-message (stub :send-message)]
      (item-message {:key 59} :key 100 @thresholds)
      (should-not-have-invoked :send-message)
      (should= 59 (:key @last-message))))
  )

(describe "send-message"
  (with-stubs)
  (it "sends a status message with default duration"
    (with-redefs [view-frame/add-message! (stub :add-message!)]
      (send-message :hull-damage 0 100)
      (should-have-invoked :add-message! {:with ["Hull Damage." 4000]})))

  (it "formats a percentage status message"
    (with-redefs [view-frame/add-message! (stub :add-message!)]
      (send-message :shields-damaged 50 100)
      (should-have-invoked :add-message! {:with ["Shields Holding. 50%." 4000]})))

  (it "formats a low-count inventory message"
    (with-redefs [view-frame/add-message! (stub :add-message!)]
      (send-message :torpedos-low 3 100)
      (should-have-invoked :add-message! {:with ["Torpedos Low. 3!" 4000]})))

  (it "sends an event message with its duration"
    (with-redefs [view-frame/add-message! (stub :add-message!)]
      (send-message :you-died)
      (should-have-invoked :add-message! {:with ["You died!" 5000]})))

  (it "ignores unknown status keys"
    (with-redefs [view-frame/add-message! (stub :add-message!)]
      (send-message :not-a-message 1 1)
      (should-not-have-invoked :add-message!)))

  (it "ignores unknown event keys"
    (with-redefs [view-frame/add-message! (stub :add-message!)]
      (send-message :not-a-message)
      (should-not-have-invoked :add-message!))))