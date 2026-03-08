(defrecord Point [^int x ^int y]
  Object
  (squared_mag [this]
    (let [^int x (:x this)
          ^int y (:y this)]
      (int (+ (* x x) (* y y))))))

(defn filter_and_sum
  [points ^int max_mag]
  (reduce
   (fn [^int total ^Point p]
     (let [^int mag (.squared_mag p)]
       (if (<= mag max_mag)
         (int (+ total mag))
         total)))
   0
   points))