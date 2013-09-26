(ns ml-clj.knn)

(defn create-dataset []
  '(((0.1 0.1)
     (0.0 0.0)
     (0.2 0.2)
     (1.1 1.1)
     (1.0 1.0))
    (:a :a :a :b :b)))

(defn distance [xs ys]
  (. java.lang.Math sqrt (apply + (map #(* % %) (map #(- %1 %2) xs ys)))))

(defn counter [xs] (reduce #(assoc %1 %2 (+ 1 (get %1 %2 0))) {} xs))

(defn kNN [xs data-set labels k]
  (let* [label-groups (map #(concat [%1] %2) labels data-set)
         label-groups-sort-by-distance (sort-by
                                        #(distance xs (rest %1)) label-groups)
         k-labels (take k (map first label-groups-sort-by-distance))
         labels-group-by-count (sort-by first (counter k-labels))]
        (-> labels-group-by-count
            reverse
            first
            first)))
