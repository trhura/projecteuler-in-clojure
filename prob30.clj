
(defn sum-of-pow-of-digits [number p]
  (loop [num number
         sum 0]
    (if (< num 1)
      (int sum)
      (recur (int (/ num 10))
             (+ sum (Math/pow (int (rem num 10)) p))))))

(defn prob30 []
  (reduce + (for [x (range 2 1000000) :when (= x (sum-of-pow-of-digits x 5))] x)))
