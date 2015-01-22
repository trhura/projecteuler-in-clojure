(defn is-prime? [num]
  (cond (< num 2) false
        (= num 2) true
        (even? num) false
        :else (loop [n 3]
                (cond (> (* n n) num) true
                      (zero? (rem num n)) false
                      :else (recur (+ 2 n))))))

(defn quadratics-is-prime [n a b]
  (is-prime? (+ (* n n) (* a n) b)))

(defn find-consecutive-prime [[a b]]
  [(count (for [n (iterate inc 0) :while (quadratics-is-prime n a b)] [a b]))
   (* a b)])

(defn prob27 []
  (apply max-key first
         (map find-consecutive-prime
              (for [x (range -999 1000)
                    y (range -999 1000)]
                [x y]))))
