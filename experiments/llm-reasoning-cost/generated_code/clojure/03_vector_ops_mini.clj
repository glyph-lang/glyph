(defn sum_positive [vec]
  (let [positive-vec (atom [])]
    (doseq [num vec]
      (when (> num 0)
        (swap! positive-vec conj num)))
    (reduce + @positive-vec)))
