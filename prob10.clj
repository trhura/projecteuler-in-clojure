;; The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
;; Find the sum of all the primes below two million.

(defn is-prime? [num]
  ;; primality test by trail-division
  (cond (< num 2) false
        (= num 2) true
        (even? num) false
        :else (loop [n 3]
                (cond (> (* n n) num) true
                      (zero? (rem num n)) false
                      :else (recur (+ 2 n))))))

(defn prime-sequence []
  ;; generate lazy seq of primes
  (let [next-primes (fn lazy-primes [num]
                      ;; return lazy prime primes starting from num
                      (if (is-prime? num)
                        (cons num (lazy-seq (lazy-primes (inc num))))
                        (lazy-seq (lazy-primes (inc num)))))]
    (next-primes 2)))

(defn prob10 []
  (apply + (for [p (prime-sequence)
                 :while (< p 2000000)]
             p)))

(defn prob10-eratosthenes []
  ;; more elegant, but not efficient
  ;; stackoverflow :(
  (loop [seq (range 2 2000000)
         sum 0]
    (if (empty? seq)
      sum
      (let [p (first seq)
            marked-seq (remove #(zero? (rem % p)) (rest seq))]
        (recur marked-seq (+' sum p))))))
