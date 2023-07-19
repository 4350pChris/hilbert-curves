(ns hilbert-curves.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [hilbert-curves.lib :as lib]))


(defn setup []
  ; Set frame rate to 30 frames per second.
  (q/frame-rate 5)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  (lib/calculate-params 3 0))

(defn update-state [state]
  (lib/calculate-params (:order state) (:counter state)))

(defn line-from-points [point1 point2]
  (let [x1 (first point1)
        y1 (second point1)
        x2 (first point2)
        y2 (second point2)]
    (q/stroke-weight 1)
    (q/stroke 255)
    (q/line x1 y1 x2 y2)))

(defn show-point [point]
  (let [x (first point)
        y (second point)]
    (q/stroke 255)
    (q/stroke-weight 5)
    (q/ellipse x y 5 5)
    (q/text (str x "i bims junge ," y) (+ x 5) y)))

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 0)
  (q/no-fill)
  ; Draw hilbert curve for every point in the rectangle
  (let [quadrants (:quadrants state)
        counter (:counter state)
        order (:order state)
        normalize (partial lib/normalize-point quadrants q/width)]
    (dotimes [i (dec counter)]
      (line-from-points (normalize (lib/hilbert order (inc i)))
                        (normalize (lib/hilbert order i)))
      ;; (show-point (normalize (lib/hilbert order i))))))
      )))

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
