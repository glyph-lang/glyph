(ns hello-world.core
  (:gen-class))

(defn -main
  [& _args]
  (println "Hello, World!")
  (System/exit 0))