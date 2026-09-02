(ns spacewar.game-logic.notifications-spec
  (:require [spacewar.game-logic.notifications :as notifications]
            [speclj.core :refer [describe it should=]]))

(describe "notifications"
  (it "appends a message key to an empty world"
    (should= {:messages [:welcome]}
             (notifications/notify {} :welcome)))

  (it "preserves existing messages in order"
    (should= {:messages [:welcome :you-win]}
             (notifications/notify {:messages [:welcome]} :you-win)))

  (it "treats a missing messages collection as empty"
    (should= {:ship {} :messages [:no-star]}
             (notifications/notify {:ship {}} :no-star))))
