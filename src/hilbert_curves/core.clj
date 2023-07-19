(ns hilbert-curves.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [hilbert-curves.lib :as lib]))

(def order 3)

(defn setup []
  ; Set frame rate to 5 frames per second.
  (q/frame-rate 5)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  (lib/calculate-params order (q/height) (q/width)))

(defn update-state [state]
  (lib/calculate-params (:order state) (q/height) (q/width) (:counter state)))

(defn line-from-points [point1 point2]
  (let [x1 (first point1)
        y1 (second point1)
        x2 (first point2)
        y2 (second point2)]
    (q/stroke-weight 1)
    (q/stroke 255)
    (q/line x1 y1 x2 y2)))

(defn draw-state [state]
  ; Clear the sketch by filling it with light-grey color.
  (q/background 40)
  (q/no-fill)
  ; Draw hilbert curve for every point in the rectangle
  (let [points (take (:counter state) (:points state))]
    (doseq [[point1 point2] (partition 2 1 points)]
      (line-from-points point1 point2))))

(q/defsketch hilbert-curves
  :title "Hilbert Curves"
  :size [512 512]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top :resizable]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])

(defn -main [& args])
