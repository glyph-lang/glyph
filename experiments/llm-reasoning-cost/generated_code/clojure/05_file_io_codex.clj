(ns file-io.core
  (:require [clojure.java.io :as io]))

(defn write-greeting
  [filename]
  (try
    (with-open [w (io/writer filename)]
      (.write w "Hello from the program!"))
    {:ok true}
    (catch Exception e
      {:ok false :error e})))