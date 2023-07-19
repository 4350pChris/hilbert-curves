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

(defn normalize-point
  "Multiply the point by the dimensions of the rectangle and add half the width or height as offset."
  [quadrants height width point]
  (let [x (first point)
        y (second point)
        w-len (/ width quadrants)
        h-len (/ height quadrants)]
    [(+ (* x w-len) (/ w-len 2))
     (+ (* y h-len) (/ h-len 2))]))

(defn make-points [quadrants height width order total]
  (map (partial normalize-point quadrants height width)
       (map (partial hilbert order) (range total))))

(defn calculate-params
  ([order height width] (calculate-params order height width 0))
  ([order height width counter]
   (let [q (quadrants-from-order order)
         total (total-from-quadrants q)]
     {:order order
      :quadrants q
      :total total
      :counter (if (>= counter total) 0 (+ 50 counter))
      :points (make-points q height width order total)})))
