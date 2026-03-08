(defn sum_positive
  [nums]
  (let [positives (reduce (fn [v n]
                            (if (> n 0)
                              (conj v n)
                              v))
                          []
                          nums)]
    (reduce + 0 positives)))