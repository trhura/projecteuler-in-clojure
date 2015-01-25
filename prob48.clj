
(defn expt [x n]
  ;; expt implementation which only care about the last ten digit
  (let [multiply (fn [a b] (rem (*' a b) 10000000000))
        square (fn [n] (multiply n n))])

  (cond (= n 0)         1
        (= n 1)         x
        (even? n)       (expt (square x) (/ n 2))
        (odd?  n)       (*' x (expt (square x) (/ (- n 1) 2)))))

(defn prob48 []
  (rem (apply + (for [i (range 1 1001)]
             (expt i i)))
       10000000000))
