(ns file-io-example
  (:require [clojure.java.io :as io]))

(defn write-greeting [filename]
  (try
    (with-open [writer (io/writer filename)]
      (.write writer "Hello from the program!"))
    {:status :success}
    (catch Exception e
      {:status :error, :message (.getMessage e)})))
