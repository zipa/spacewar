(ns spacewar.ui.front-view
  (:require [quil.core :as q #?@(:cljs [:include-macros true])]
            [spacewar.geometry :as geo]
            [spacewar.ui.strategic-scan :refer [->strategic-scan]]
            [spacewar.ui.protocols :as p]
            [spacewar.vector :as vector]))

; Stars
;                                 * star
;                                 |
;                                 | v-distance
;                                 |
;       ---> Direction of ship    |
;----------------------------------
;        h-distance
;
; luminosity is the absolute brightness of the star.
; Angle (in radians) is the angle of the star from the center of the
; screen.  (right is zero, left is pi, etc.)
;
; All units are arbitrary except the angle.
; The constants you see scattered around are all tweaks
; done by hand.  Fiddle with them to see how they work.


(def star-count 200)
(def f-lum 200)                                             ; luminosity factor

(defn- move-star [star]
  (let [h-distance (dec (:h-distance star))]
    (if (pos? h-distance)
      (assoc star :h-distance h-distance)
      nil)))

(defn- move-stars [stars]
  (filter some? (map move-star stars)))

(defn- star-in-frame [state sx sy]
  (let [{:keys [x y w h]} state
        margin 10
        xmin (+ x margin)
        ymin (+ y margin)
        xmax (- (+ x w) margin)
        ymax (- (+ y h) margin)]
    (and (< sx xmax)
         (< sy ymax)
         (> sx xmin)
         (> sy ymin))))

(defn star-size [m]
  (let [mm (* f-lum m)]
    (cond
      (< mm 1) 1
      (< mm 3) 2
      (< mm 5) 3
      (< mm 10) 4
      (< mm 20) 5
      :else 6)))

(defn star-color [m]
  (let [mm (* f-lum m)]
    (if (>= mm 0.5)
      [255 255 255]
      (repeat 3 (* 2 mm 256)))))

; magic numbers are tweaks that affect the star pattern.
(defn- make-random-star []
  (let [luminosity (+ 1 (rand 5))
        h-distance (rand (* luminosity 200))]
    {:h-distance h-distance
     :v-distance (+ -20 (rand 200) (/ h-distance 20))
     :angle (geo/->radians (rand 360))
     :luminosity luminosity}))

(defn- make-stars [n]
  (repeatedly n make-random-star))

(defn- add-stars [state]
  (let [stars (:stars state)]
    (if (< (count stars) star-count)
      (assoc state :stars (conj stars (make-random-star)))
      state)))

(deftype front-view [state]
  p/Drawable
  (draw [_]
    (let [{:keys [x y h w stars]} state]
      (q/no-stroke)
      (doseq [star stars]
        (let [{:keys [h-distance v-distance luminosity angle]} star
              ; rd is radial distance of star from center of screen
              rd (* h (/ v-distance h-distance))
              [rx ry] (vector/from-angular rd angle)
              sx (+ rx x (/ w 2))
              sy (+ ry y (/ h 2))
              ; m is relative brightness, inversely proportional to distance.
              m (/ luminosity
                   (Math/sqrt (+ (* h-distance h-distance) (* v-distance v-distance))))
              sz (star-size m)]
          (when (star-in-frame state sx sy)
            (do
              (apply q/fill (star-color m))
              (q/ellipse-mode :corner)
              (q/ellipse sx sy sz sz)))))))

  (setup [_] (front-view. (assoc state :stars (make-stars star-count))))

  (update-state [_ _]
    (p/pack-update
      (front-view. (add-stars (update state :stars move-stars))))))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-09-02T15:27:05.14158-05:00", :module-hash "1541074923", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line nil, :hash "1904651839"} {:id "def/star-count", :kind "def", :line 26, :end-line nil, :hash "152856414"} {:id "def/f-lum", :kind "def", :line 27, :end-line nil, :hash "-625376035"} {:id "defn-/move-star", :kind "defn-", :line 29, :end-line nil, :hash "-1580297727"} {:id "defn-/move-stars", :kind "defn-", :line 35, :end-line nil, :hash "881781089"} {:id "defn-/star-in-frame", :kind "defn-", :line 38, :end-line nil, :hash "502584766"} {:id "defn/star-size", :kind "defn", :line 50, :end-line nil, :hash "1308209851"} {:id "defn/star-color", :kind "defn", :line 60, :end-line nil, :hash "-1518652543"} {:id "defn-/make-random-star", :kind "defn-", :line 67, :end-line nil, :hash "1968287013"} {:id "defn-/make-stars", :kind "defn-", :line 75, :end-line nil, :hash "-1953400823"} {:id "defn-/add-stars", :kind "defn-", :line 78, :end-line nil, :hash "-17736195"} {:id "form/11/deftype", :kind "deftype", :line 84, :end-line nil, :hash "-247493245"}]}
;; clj-mutate-manifest-end
