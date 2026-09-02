(ns spacewar.geometry)

(defn sign [x]
  (cond (zero? x) 0 (pos? x) 1
        :else -1))

(defn round [x]
  (long (Math/round (double x))))

(defn ->degrees [radians]
  (mod (* 360 (/ radians (* 2 Math/PI))) 360))

(defn ->radians [degrees]
  (mod (* 2 Math/PI (/ degrees 360)) (* 2 Math/PI)))

(defn- square [x] (* x x))

(defn distance [[x1 y1] [x2 y2]]
  (Math/sqrt
    (+ (square (- x1 x2))
       (square (- y1 y2)))))


(defn inside-rect [[rx ry rw rh] [px py]]
  (and (>= px rx)
       (>= py ry)
       (< px (+ rx rw))
       (< py (+ ry rh))))

(defn inside-circle [[cx cy radius] [px py]]
  (< (distance [cx cy] [px py]) radius))

(defn angle-degrees [[x1 y1] [x2 y2]]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (if (and (zero? dx) (zero? dy)) 0
      (mod (->degrees (Math/atan2 dy dx)) 360))))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-09-02T15:08:50.111687-05:00", :module-hash "326548399", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line nil, :hash "-736077597"} {:id "defn/sign", :kind "defn", :line 3, :end-line nil, :hash "983064319"} {:id "defn/round", :kind "defn", :line 7, :end-line nil, :hash "-466601113"} {:id "defn/->degrees", :kind "defn", :line 10, :end-line nil, :hash "765363962"} {:id "defn/->radians", :kind "defn", :line 13, :end-line nil, :hash "1352371928"} {:id "defn-/square", :kind "defn-", :line 16, :end-line nil, :hash "-1977614569"} {:id "defn/distance", :kind "defn", :line 18, :end-line nil, :hash "1289450817"} {:id "defn/inside-rect", :kind "defn", :line 24, :end-line nil, :hash "1995485539"} {:id "defn/inside-circle", :kind "defn", :line 30, :end-line nil, :hash "-1068094812"} {:id "defn/angle-degrees", :kind "defn", :line 33, :end-line nil, :hash "187545035"}]}
;; clj-mutate-manifest-end
