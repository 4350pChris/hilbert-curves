(ns hilbert-curves.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn calculate-params [order]
  ; Calculate number of points and total number of points.
  (let [N (Math/pow 2 order)
        total (* N N)]
    {:order order
     :N N
     :total total}))

(defn hilbert-start-point [i]
  (let [masked (bit-and i 3)]
    (condp = masked
      0 [0 0]
      1 [0 1]
      2 [1 1]
      3 [1 0])))

(defn hilbert-rotate [significant point order]
  (let [rx (first point)
        ry (second point)
        quadrant (bit-and significant 3)
        len (Math/pow 2 order)]
    (condp = quadrant
      0 [ry rx]
      1 [rx (+ ry len)]
      2 [(+ rx len) (+ ry len)]
      3 [(- (* 2 len) 1 ry) (- len 1 rx)])))

(defn hilbert [order i]
  (let [point (hilbert-start-point i)]
    (loop [n 1
           point point
           significant (bit-shift-right i 2)]
      (if (< n order)
        (recur (inc n)
               (hilbert-rotate significant point n)
               (bit-shift-right significant 2))
        point))))

(defn normalize-point [N point]
  (let [x (first point)
        y (second point)
        len (/ (q/width) N)]
    [(+ (* x len) (/ len 2))
     (+ (* y len) (/ len 2))]))

(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 1)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  (calculate-params 3))

(defn update-state [state]
  (calculate-params (:order state)))

(defn line-from-points [point1 point2]
  (let [x1 (first point1)
        y1 (second point1)
        x2 (first point2)
        y2 (second point2)]
    (q/stroke-weight 1)
    (q/line x1 y1 x2 y2)))

(defn show-point [point]
  (let [x (first point)
        y (second point)]
    (q/stroke-weight 5)
    (q/ellipse x y 5 5)
    (q/text (str x "i bims junge ," y) (+ x 5) y)))

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 0)
  (q/no-fill)
  ; Draw rectangle with hilbert curve passing through it
  (q/stroke 255)
  (q/stroke-weight 1)
  ; Draw hilbert curve for every point in the rectangle
  (let [N (:N state)
        total (:total state)
        order (:order state)
        normalize (partial normalize-point N)]
    (dotimes [i (dec total)]
      (line-from-points (normalize (hilbert order (inc i)))
                        (normalize (hilbert order i)))
      (show-point (normalize (hilbert order i))))))

(q/defsketch hilbert-curves
  :title "Hilbert Curves"
  :size [512 512]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])

(defn -main [& args])
