(ns ml-clj.trees
  (:use ml-clj.utils))

(defn create-dataset []
  [[1, 1, "yes"]
   [1, 1, "yes"]
   [1, 0, "no"]
   [0, 1, "no"]
   [0, 1, "no"]])

(defn create-feature []
  ["no surfacing", "flippers"])

(def ds (create-dataset))
(def ls (create-feature))

(defn split-dataset [dataset axis value]
  (mapv #(vec (concat (take axis %) (take-last (- (count %) (+ axis 1)) %)))
        (filter #(= (get % axis) value) dataset)))

(defn calc-info-gain [dataset base-entropy axis]
  (let [unique-feat-list (set (map #(nth % axis) dataset))
        prob-entropy (fn [dataset axis value]
                       (let [sub-dataset (split-dataset dataset axis value)
                             prob (/ (count sub-dataset) (count dataset))
                             sub-dataset-class-list (map last sub-dataset)
                             sub-shann-ent (shannon-entropy sub-dataset-class-list)]
                         (* prob sub-shann-ent)))]
    (- base-entropy (apply + (map #(prob-entropy dataset axis %) unique-feat-list)))))

(defn choose-best-feture-to-split [dataset]
  (let [base-shnn-ent (shannon-entropy (map last dataset))
        num-features (- (count (first dataset)) 1)]
    (second (apply (partial max-key first)
                   (map (fn [i] (list (calc-info-gain dataset base-shnn-ent i) i))
                        (range num-features))))))

(defn majority-count [class-list]
   (apply
    (partial max-key first)
    (mapv (fn [[k v]] (vector v k)) (counter class-list))))

(defn create-tree [dataset labels]
  (let [class-list (map last dataset)]
    (cond
     (= (set class-list) 1) (first class-list)
     (= (count (first dataset)) 1) (second (majority-count class-list))
     :else (let [best-feat (choose-best-feture-to-split dataset)
                 best-feat-label (get labels best-feat)
                 sub-labels (vec (drop-nth best-feat labels))
                 unique-feat-values (set (map #(nth % best-feat) dataset))]
             {best-feat-label (reduce (fn [sub-tree feat]
                                        (assoc sub-tree feat (create-tree (split-dataset dataset best-feat feat) sub-labels)))
                                      {}
                                      unique-feat-values)}))))

(defn classify [input-tree, feat-labels, test-vec]
  (let [top-feat (first (keys input-tree))
        child-tree (get input-tree top-feat)
        feat-index (.indexOf feat-labels top-feat)
        key (get test-vec feat-index)
        value-of-feat (get child-tree key)]
    (if (instance? clojure.lang.PersistentArrayMap value-of-feat)
      (classify value-of-feat feat-labels test-vec)
      value-of-feat)))
