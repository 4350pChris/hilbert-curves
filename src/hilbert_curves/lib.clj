(ns hilbert-curves.lib
  (:require [quil.core :as q]))

(defn quadrants-from-order [order] (Math/pow 2 order))

(defn total-from-quadrants [quadrants] (* quadrants quadrants))

(defn hilbert-start-point [i]
  (let [masked (bit-and i 3)]
    (condp = masked
      0 [0 0]
      1 [0 1]
      2 [1 1]
      3 [1 0])))

(defn hilbert-rotate [significant [x y] order]
  (let [quadrant (bit-and significant 3)
        len (Math/pow 2 order)]
    (condp = quadrant
      0 [y x]
      1 [x (+ y len)]
      2 [(+ x len) (+ y len)]
      3 [(- (* 2 len) 1 y) (- len 1 x)])))

(defn hilbert
  ([order n point significant]
   (if (< n order)
     (recur order
            (inc n)
            (hilbert-rotate significant point n)
            (bit-shift-right significant 2))
     point))
  ([order i] (hilbert order 1 (hilbert-start-point i) (bit-shift-right i 2))))

(defn normalize-point
  "Multiply the point by the dimensions of the rectangle and add half the width or height as offset."
  [quadrants height width [x y]]
  (let [w-len (/ width quadrants)
        h-len (/ height quadrants)]
    [(+ (* x w-len) (/ w-len 2))
     (+ (* y h-len) (/ h-len 2))]))

(defn hilbert-dither-points
  [image [x1 y1] [x2 y2] brightness]
  (let [pixel-from-image #(q/get-pixel image (int %1) (int %2))
        px1 (pixel-from-image x1 y1)
        px2 (pixel-from-image x2 y2)
        b-diff (- (q/brightness px2) (q/brightness px1))]
    (q/constrain (+ b-diff brightness) 0 255)))

(defn make-points [quadrants height width order total]
  (map (partial normalize-point quadrants height width)
       (map (partial hilbert order) (range total))))

(defn calculate-params
  ([order height width] (calculate-params order height width 0 50))
  ([order height width counter counter-increments]
   (let [q (quadrants-from-order order)
         total (total-from-quadrants q)
         points (make-points q height width order total)
         do-not-loop? (= counter-increments 0)]
     ; disable loop if counter-increments is 0, reactivate if it's stopped and counter-increments is > 0
     (cond
       do-not-loop? (q/no-loop)
       (not (q/looping?)) (q/start-loop))
     {:order order
      :quadrants q
      :total total
      :counter-increments counter-increments
      :counter (cond
                 do-not-loop? total
                 (>= counter total) 0
                 :else (+ counter-increments counter))
      :points points})))
