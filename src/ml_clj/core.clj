(ns ml-clj.core
  (:gen-class)
  (:use [ml-clj.knn]))

(defn -main [& args]
  (let [[groups labels] (create-dataset)]
    (time (kNN [0.0 0.0] groups labels 3))))
