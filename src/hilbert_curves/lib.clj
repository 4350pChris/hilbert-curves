(ns hilbert-curves.lib)

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

(defn make-points [quadrants height width order total]
  (map (partial normalize-point quadrants height width)
       (map (partial hilbert order) (range total))))

(defn calculate-params
  ([order height width] (calculate-params order height width 0 50))
  ([order height width counter counter-increments]
   (let [q (quadrants-from-order order)
         total (total-from-quadrants q)]
     {:order order
      :quadrants q
      :total total
      :counter-increments counter-increments
      :counter (if (>= counter total) 0 (+ counter-increments counter))
      :points (make-points q height width order total)})))
