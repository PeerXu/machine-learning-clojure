(ns ml-clj.core
  (:use [ml-clj.knn :only [kNN create-dataset]]))

(defn -main [& args]
  (let [[groups labels] (create-dataset)]
    (time (kNN [0.0 0.0] groups labels 3))))
