;; The first known prime found to exceed one million digits was
;; discovered in 1999, and is a Mersenne prime of the form 26972593−1; it
;; contains exactly 2,098,960 digits. Subsequently other Mersenne primes,
;; of the form 2p−1, have been found which contain more digits.

;; However, in 2004 there was found a massive non-Mersenne prime which
;; contains 2,357,207 digits: 28433×27830457+1.

;; Find the last ten digits of this prime num

(defn multiply [a b]
  ;; mult implementation which only care about the last ten digit
  (rem (*' a b) 10000000000))

(defn expt [x n]
  ;; expt implementation
  (let [square (fn [n] (multiply n n))]
    (cond (= n 0)         1
          (= n 1)         x
          (even? n)       (expt (square x) (/ n 2))
          (odd?  n)       (*' x (expt (square x) (/ (- n 1) 2))))))

(defn prob97 []
  (apply str (take-last 10 (str (inc (* 28433 (expt 2 7830457)))))))
