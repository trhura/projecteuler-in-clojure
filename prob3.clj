;; The prime factors of 13195 are 5, 7, 13 and 29.
;; What is the largest prime factor of the number 600851475143 ?

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


(defn prime-factors [number]
  ;; prime-factors by trial divison
  (loop [num number
         prime-seq (prime-sequence)
         factors nil]

    (let [prime-num (first prime-seq)]
      (cond (> (* prime-num prime-num) num) (cons num factors)
            (zero? (rem num prime-num)) (recur (/ num prime-num )
                                               (prime-sequence)
                                               (cons prime-num factors))
            :else (recur num (next prime-seq) factors)))))

(defn prob3 []
  (apply max (prime-factors 600851475143)))
