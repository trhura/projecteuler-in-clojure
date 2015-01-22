(require '[clojure.string :as string])

(defn digit-vec [number]
  (vec (map #(- (int %) 48) (str number))))

(defn to-digit [digit-vec]
  (int (apply + (map-indexed (fn [pos val]
                          (* val (Math/pow 10 pos)))
                        (reverse digit-vec)))))

(defn rotate [coll]
  (conj (-> coll rest vec) (first coll)))

(defn digit-rotations [number]
    (loop [d (rotate (digit-vec number))
           rotations nil]
      (if (= d (digit-vec number))
        (cons (to-digit d) rotations)
        (recur (rotate d) (conj rotations (to-digit d))))))

(defn is-prime? [num]
  (cond (< num 2) false
        (= num 2) true
        (even? num) false
        :else (loop [n 3]
                (cond (> (* n n) num) true
                      (zero? (rem num n)) false
                      :else (recur (+ 2 n))))))

(defn prob35[]
  (count (for [n (range 1000000)
               :when (every? is-prime? (digit-rotations n))]
           n)))
