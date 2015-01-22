;; 2520 is the smallest number that can be divided by each of the
;; numbers from 1 to 10 without any remainder.

;; What is the smallest positive number that is evenly divisible by
;; all of the numbers from 1 to 20?

;; (defn prob5 []
;;   (let [numbers (range 11 21)]
;;     (loop [n 20]
;;       (if (every? #(zero? (rem n %)) numbers)
;;         n
;;         (recur (+ 2 n))))))

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

(defn count-item [coll item]
  (count (filter #(= item %) coll)))

(defn prime-factors-by-power [num]
  ;; return a map with prime as keys
  ;; and count (power) as values
  (let [factors (prime-factors num)]
    (into {} (for [itm (distinct factors)
                   :let [cnt (count-item factors itm)]]
               {itm cnt}))))

(defn prob5 []
  ;; find least common multiplier by prime factorization
  ;; See http://en.wikipedia.org/wiki/Least_common_multiple
  (let [primes-with-pow (for [n (range 1 21)]
                          (prime-factors-by-power n))
        primes-with-highest-pow (apply merge-with max primes-with-pow)]
    (apply * (map (fn [[n p]] (int (Math/pow n p))) primes-with-highest-pow))))
