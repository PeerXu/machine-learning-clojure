(ns ml-clj.utils)

(defn distance [xs ys]
  (Math/sqrt (apply + (map #(* % %) (map #(- %1 %2) xs ys)))))

(defn counter [xs] (reduce #(assoc %1 %2 (+ 1 (get %1 %2 0))) {} xs))

(defn log
  ([x] (Math/log x))
  ([x base] (/ (Math/log x)
               (Math/log base))))

(defn shannon-entropy [xs]
  (let [xlen (float (count xs))
        cnts (counter xs)]
    (reduce (fn [shn-ent [_k v]]
              (let [prob (/ v xlen)]
                (- shn-ent (* prob (log prob 2)))))
            0.0
            cnts)))

(defn drop-nth [n xs]
  (concat (take n xs) (drop (inc n) xs)))
