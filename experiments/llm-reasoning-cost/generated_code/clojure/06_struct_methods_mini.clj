(defrecord Point [x y])

(defn squared-mag [point]
  (+ (* (:x point) (:x point)) (* (:y point) (:y point))))

(defn filter-and-sum [points max-mag]
  (reduce
    (fn [total point]
      (if (<= (squared-mag point) max-mag)
        (+ total (squared-mag point))
        total))
    0
    points))
