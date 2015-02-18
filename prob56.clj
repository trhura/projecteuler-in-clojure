;; A googol (10100) is a massive number: one followed by one-hundred
;; zeros; 100100 is almost unimaginably large: one followed by
;; two-hundred zeros. Despite their size, the sum of the digits in
;; each number is only 1.

;; Considering natural numbers of the form, ab, where a, b < 100, what
;; is the maximum digital sum?

(defn digit-vector [number]
  ;; return a vector of digits in number
  (vec (map #(- (int %) 48) (str (bigint number)))))

(defn expt [x n]
  ;; expt implementation which only care about the last ten digit
  (let [multiply (fn [a b] (*' a b))
        square (fn [n] (multiply n n))]

    (cond (= n 0)         1
          (= n 1)         x
          (even? n)       (expt (square x) (/ n 2))
          (odd?  n)       (*' x (expt (square x) (/ (- n 1) 2))))))

(defn prob56 []
  (apply max (for [a (range 90 100)
                   b (range 90 100)]
               (apply + (digit-vector (expt a b))))))
