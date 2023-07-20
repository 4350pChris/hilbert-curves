(ns hilbert-curves.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [hilbert-curves.lib :as lib]
            [hilbert-curves.controls :refer [key-press show-controls]]))

(def order (atom 1))
(def controls-height 100)

(defn draw-height []
  (- (q/height) controls-height))

(defn setup []
  ; Set frame rate to 5 frames per second.
  (q/frame-rate 5)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb)
  (lib/calculate-params @order (draw-height) (q/width)))

(defn update-state [state]
  (lib/calculate-params (:order state) (draw-height) (q/width) (:counter state) (:counter-increments state)))

(defn draw-state [state]
  (q/background 20)
  (q/fill 240)
  (q/stroke-weight 1)
  (q/stroke 255)
  (show-controls state)
  ; Clear the sketch by filling it with light-grey color.
  ; Make room for controls at top of the sketch.
  (q/translate 0 controls-height)
  ;; (q/no-fill)
  ; Draw hilbert curve for every point in the rectangle
  (let [points (take (:counter state) (:points state))]
    (doseq [[point1 point2] (partition 2 1 points)]
      (q/line point1 point2))))

(q/defsketch hilbert-curves
  :title "Hilbert Curves"
  :size [512 (+ 512 controls-height)]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  :features [:keep-on-top :resizable]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode]
  :key-typed key-press)

(defn -main [& args])
