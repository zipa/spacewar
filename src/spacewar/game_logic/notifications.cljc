(ns spacewar.game-logic.notifications)

(defn notify [world message-key]
  (update world :messages (fnil conj []) message-key))
