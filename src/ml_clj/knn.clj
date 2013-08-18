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

(defn count-list-element [xs]
  (reduce
   (fn [cnts x]
     (let [n (x cnts)]
       (if (= n nil)
         (assoc cnts x 1)
         (assoc cnts x (+ n 1)))))
   {}
   xs))

(defn kNN [inX dataSet labels k]
  (let* [label-groups (map #(concat [%1] %2) labels dataSet)
         label-groups-sort-by-distance (sort-by
                                        #(distance inX (rest %1)) label-groups)
         k-labels (take k (map first label-groups-sort-by-distance))
         labels-group-by-count (sort-by first (count-list-element k-labels))]
        (-> labels-group-by-count
            reverse
            first
            first)))
