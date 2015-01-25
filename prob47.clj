;; The first two consecutive numbers to have two distinct prime
;; factors are:

;; 14 = 2 × 7 15 = 3 × 5

;; The first three consecutive numbers to have three distinct prime
;; factors are:

;; 644 = 2² × 7 × 23 645 = 3 × 5 × 43 646 = 2 × 17 × 19.

;; Find the first four consecutive integers to have four distinct
;; prime factors. What is the first of these numbers?

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

(def prime-factors-by-power
  (memoize (fn [num]
             ;; return a map with prime as keys
             ;; and count (power) as values
             (let [factors (prime-factors num)]
               (into {} (for [itm (distinct factors)
                              :let [cnt (count-item factors itm)]]
                          {itm cnt}))))))

(defn prime-factors-pow-multiplied [num]
  (map #(apply * %) (seq (prime-factors-by-power num))))

(defn prob47 []
  (loop [i 500]
    (let [pi0 (prime-factors-by-power i)
          pi1 (prime-factors-by-power (+ i 1))
          pi2 (prime-factors-by-power (+ i 2))
          pi3 (prime-factors-by-power (+ i 3))]
      (if (and (= 4 (count pi0) (count pi1) (count pi2) (count pi3))
               (apply distinct? (concat pi0 pi1 pi2 pi3)))
        i
        (recur (inc i))))))
