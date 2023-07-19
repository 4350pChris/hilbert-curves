(ns hilbert-curves.lib)

(defn calculate-params [order counter]
  ; Calculate number of points and total number of points.
  (let [q (Math/pow 2 order)
        total (* q q)]
    {:order order
     :quadrants q
     :total total
     :counter (if (>= counter total) 0 (+ 50 counter))}))

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

(defn normalize-point [quadrant width point]
  (let [x (first point)
        y (second point)
        len (/ (width) quadrant)]
    [(+ (* x len) (/ len 2))
     (+ (* y len) (/ len 2))]))
