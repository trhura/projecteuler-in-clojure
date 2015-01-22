;; If we list all the natural numbers below 10 that are multiples of 3 or 5, we
;; get 3, 5, 6 and 9. The sum of these multiples is 23.
;;
;; Find the sum of all the multiples of 3 or 5 below 1000.

(defn divisible? [num div]
  (zero? (rem num div)))

(defn prob1 []
  (apply + (for [n (range 1 1000)
                 :when (or (divisible? n 3)
                           (divisible? n 5))]
             n)))
