(defn count-words [text]
  (let [words (clojure.string/split (clojure.string/lower-case text) #"\s+")]
    (reduce (fn [acc word]
              (update acc word (fnil inc 0)))
            {}
            words)))

(defn print-top [word-counts n]
  (->> word-counts
       (sort-by (fn [[_ count]] [- count (key _)]))
       (take n)
       (doseq [[word count]] (println (str "  " count " " word)))))

(defn -main []
  (let [text "the cat sat on the mat the cat sat"
        word-counts (count-words text)]
    (print-top word-counts 5)))

(-main)
