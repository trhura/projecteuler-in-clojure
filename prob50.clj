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

(defn prob50 []
  (let [sub-primes  (fn [sum largest]
                      ;; try to subtract initial primes from sum
                      ;; until it reaches a prime or smaller than
                      ;; previously detected largest prime
                      (loop [primes (prime-sequence)
                             sum sum
                             largest-prime largest]
                        (let [prime (first primes)
                              primesum (- sum prime)]
                          (if (< primesum largest-prime)
                            largest-prime
                            (if (is-prime? primesum)
                              primesum
                              (recur (next primes) primesum largest-prime))))))

        add-primes (fn [limit]
                     ;; add 2 + 3 + ... prime until it is greater than limit
                     ;; then if the sum not is prime, try to subtract prime
                     ;; sequence from the begining to see if it is prime
                     (loop [primes (prime-sequence)
                            primesum 0
                            largest-prime 0]
                       (let [prime (first primes)
                             sum  (+ prime primesum)]
                         (if (> sum limit)
                           (subtract-primes primesum largest-prime)
                           (if (is-prime? sum)
                             (recur (next primes) sum sum)
                             (recur (next primes) sum largest-prime))))))]
    (add-primes 1000000)))
