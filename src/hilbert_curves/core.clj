(ns hilbert-curves.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [hilbert-curves.lib :as lib]
            [hilbert-curves.controls :refer [key-press show-controls]]))

(def initial-order 1)
(def controls-height 100)

(defn draw-height []
  (- (q/height) controls-height))

(defn setup []
  (q/frame-rate 30)
  ; Set color mode to HSB (HSV) instead of default RGB.
  (q/color-mode :hsb 360 255 255)
  (lib/calculate-params initial-order (draw-height) (q/width)))

(defn update-state [state]
  (merge state
         (lib/calculate-params (:order state) (draw-height) (q/width) (:counter state) (:counter-increments state))))

(defn draw-basic [points]
  (doseq [[point1 point2] (partition 2 1 points)]
    (q/line point1 point2)))

(defn draw-dithered
  [image points]
  (->> points
       (partition 2 1)
       (reduce
        (fn [brightness [point1 point2]]
          (let [brightness-result (lib/hilbert-dither-points image point2 point1 brightness)]
            (q/stroke (- 255 (int brightness-result)))
            (q/line point1 point2)
            brightness-result))
        (- 255 (q/brightness (q/get-pixel image 0 0))))))

(defn draw-grid
  [quadrants height width]
  (let [w-len (/ width quadrants)
        h-len (/ height quadrants)]
    (doseq [x (range 0 width w-len)]
      (q/line x 0 x height))
    (doseq [y (range 0 height h-len)]
      (q/line 0 y width y))))

(defn draw-state [state]
  (q/background 20)
  (q/fill 240)
  (q/stroke-weight 1)
  (q/stroke 255)
  (show-controls state)
  ; Clear the sketch by filling it with light-grey color.
  ; Make room for controls at top of the sketch.
  (q/translate 0 controls-height)
  (when (:show-grid state)
    (draw-grid (:quadrants state) (draw-height) (q/width)))
  ; Draw hilbert curve for every point in the rectangle
  (let [points (take (:counter state) (:points state))]
    (if-let [image (:image state)]
      (when (q/loaded? image)
        (q/resize image (q/width) (draw-height))
        (draw-dithered image points))
      (draw-basic points))))

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
