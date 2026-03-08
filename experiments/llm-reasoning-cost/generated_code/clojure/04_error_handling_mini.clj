(defn safe-divide [numerator denominator]
  (if (zero? denominator)
    (let [error-message "division by zero"]
      (println error-message)
      {:error error-message})
    {:ok (quot numerator denominator)}))
