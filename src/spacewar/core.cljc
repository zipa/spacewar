(ns spacewar.core
  (:require [quil.core :as q #?@(:cljs [:include-macros true])]
            [quil.middleware :as m]
            [spacewar.ui.complex :as main-viewer]
            [spacewar.ui.view-frame :as view-frame]
            [spacewar.ui.protocols :as p]
            [spacewar.ui.messages :as messages]
            [spacewar.game-logic.config :as glc]
            [spacewar.game-logic.world :as world]
            #?(:clj [clojure.java.io :as io])
            #?(:cljs [clojure.edn :as edn])))

#?(:cljs (enable-console-print!))

(defn game-saved? []
  #?(:clj  (.exists (io/file "spacewar.world"))
     :cljs (and (exists? js/localStorage)
                (.getItem js/localStorage "spacewar.world"))))

(defn- read-saved-world []
  #?(:clj  (read-string (slurp "spacewar.world"))
     :cljs (edn/read-string (.getItem js/localStorage "spacewar.world"))))

(defn- present-messages [game-world]
  (doseq [message-key (:messages game-world)]
    (if (= :clear-messages message-key)
      (view-frame/clear-messages!)
      (messages/send-message message-key)))
  (assoc game-world :messages []))

(defn setup []
  (let [vmargin 30
        hmargin 5
        saved-world (when (game-saved?) (read-saved-world))
        game-world (present-messages (world/starting-world saved-world))]
    (q/frame-rate glc/frame-rate)
    (q/color-mode :rgb)
    (q/background 200 200 200)
    (q/ellipse-mode :corner)
    (q/rect-mode :corner)
    {:state (p/setup
              (main-viewer/->complex
                {:x hmargin :y vmargin
                 :w (- (q/width) (* 2 hmargin))
                 :h (- (q/height) (* 2 vmargin))}))
     :world game-world
     :base-time (:update-time game-world)
     :fonts #?(:clj  {:lcars (q/create-font "Helvetica-Bold" 24)
                      :lcars-small (q/create-font "Arial" 18)
                      :messages (q/create-font "Bank Gothic" 30)}
               :cljs {:lcars "Helvetica-Bold"
                      :lcars-small "Helvetica"
                      :messages "Bank Gothic"})
     :frame-times []}))

(defn add-frame-time [frame-time context]
  (let [frame-times (->
                      context
                      :frame-times
                      (conj frame-time))
        frame-times (if (> (count frame-times) 10)
                      (vec (rest frame-times))
                      frame-times)]
    (assoc context :frame-times frame-times)))

(defn frames-per-second [frame-times]
  (if (empty? frame-times)
    0
    (let [sum (reduce + frame-times)
          mean (/ sum (count frame-times))
          fps (/ 1000 mean)]
      fps)))

(defn frame-timing [time last-update-time]
  (let [raw (- time last-update-time)
        last (if (> raw 500) time last-update-time)]
    {:ms (max 1 (- time last))
     :last-update-time last}))

(defn- save-world [game-world]
  #?(:clj  (spit "spacewar.world" game-world)
     :cljs (when (exists? js/localStorage)
             (.setItem js/localStorage "spacewar.world" game-world))))

(defn update-state [context]
  (let [{:keys [world base-time]} context
        time (+ base-time (q/millis))
        last-update-time (:update-time world)
        timing (frame-timing time last-update-time)
        ms (:ms timing)
        last-update-time (:last-update-time timing)
        context (add-frame-time ms context)
        fps (frames-per-second (:frame-times context))
        complex (:state context)
        game-world (assoc world :update-time time :ms ms :fps fps)
        [complex events] (p/update-state complex game-world)
        game-world (world/process-events (flatten events) game-world)
        game-world (world/update-world ms game-world)
        _ (when (world/crossed-interval? time last-update-time 1000)
            (messages/add-messages! game-world))
        game-world (world/apply-periodic-updates game-world time last-update-time)
        game-world (present-messages game-world)]
    (view-frame/update-messages ms game-world)
    (when (world/crossed-interval? time last-update-time 5000)
      (save-world game-world))
    (assoc context :state complex :world game-world)))

(defn draw-state [{:keys [state]}]
  (q/fill 200 200 200)
  (q/rect-mode :corner)
  (q/no-stroke)
  (q/rect 0 0 (q/width) (q/height))
  (p/draw state))

#?(:clj
   (defn ^:export -main [& args]
     (q/defsketch space-war
                  :title "Space War"
                  :size [(- (q/screen-width) 10) (- (q/screen-height) 40)]
                  :setup setup
                  :update update-state
                  :draw draw-state
                  :middleware [m/fun-mode])
     args)

   :cljs
   (defn ^:export -main []
     (let [size [(max (- (.-scrollWidth (.-body js/document)) 20) 900)
                 (max (- (.-innerHeight js/window) 25) 700)]]
       (q/defsketch space-war
                    :title "Space War"
                    :size size
                    :setup setup
                    :update update-state
                    :draw draw-state
                    :middleware [m/fun-mode]
                    :host "space-war"))))
