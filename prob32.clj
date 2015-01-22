
(defn has? [coll val]
  (loop [coll coll]
    (cond (empty? coll) false
          (= (first coll) val) true
          :else (recur (rest coll)))))

(defn pandigital? [coll n]
  (and (= (count coll) n)
       (distinct? coll)
       (every? #(has? coll %) (range 1 (inc n)))))

(defn digit-vec [number]
  (vec (map #(- (int %) 48) (str number))))

(defn digit-vecs [& numbers]
  (apply concat (map digit-vec numbers)))

(defn prob32 []
  (apply + (distinct
            (concat (for [a (range 10 100)
                          b (range 100 1000)
                          :when (pandigital? (digit-vecs a b (* a b)) 9)]
                      (* a b))
                    (for [a (range 2 10)
                          b (range 1000 5000)
                          :when (pandigital? (digit-vecs a b (* a b)) 9)]
                      (* a b))))))
