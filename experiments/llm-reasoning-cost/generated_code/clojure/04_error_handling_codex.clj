(defn safe-divide
  [^int numerator ^int denominator]
  (if (zero? denominator)
    {:err "division by zero"}
    {:ok (quot numerator denominator)}))