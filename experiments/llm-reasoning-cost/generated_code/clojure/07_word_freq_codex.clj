(ns word-frequency-counter
  (:gen-class)
  (:require [clojure.string :as str]))

(defn count_words
  [text]
  (reduce
   (fn [m w]
     (update m w (fnil inc 0)))
   {}
   (str/split (str/lower-case text) #"\s+")))

(defn print_top
  [word-count n]
  (doseq [[word count]
          (take n
                (sort-by (fn [[w c]] [(- c) w]) word-count))]
    (printf "  %d %s%n" count word)))

(defn -main
  [& _args]
  (let [text "the cat sat on the mat the cat sat"
        counts (count_words text)]
    (print_top counts 5)))