;; It was proposed by Christian Goldbach that every odd composite
;; number can be written as the sum of a prime and twice a square.

;; 9 = 7 + 2×12 15 = 7 + 2×22 21 = 3 + 2×32 25 = 7 + 2×32 27 = 19 +
;; 2×22 33 = 31 + 2×12

;; It turns out that the conjecture was false.

;; What is the smallest odd composite that cannot be written as the
;; sum of a prime and twice a square?


;; Miller-Rabin implementation is copied from the link below
;; http://programmingpraxis.com/2009/05/01/primality-checking/

(defn factor-2s
   "return argument N as vector [s,d] where N = (2^S)*d"
   [n]
   (loop [n' n  e 0]
      (if (even? n')
         (recur (/ n' 2) (inc e))
         [e n'])))

(defn square [x]  (* x x))

(defn pow
   "compute (a^n) (mod m)
       (efficient algorithm, doing modulo arithmetic after each step)"
   [a n m]
   (cond
      (zero? n)  1
      (odd? n)   (mod (* a (pow a (dec n) m)) m)
      (even? n)  (mod (square (pow a (/ n 2) m)) m)))

(defn random-gen
   "Lazily computed list of random numbers"
   [n]
   (map #(long (rand %1)) (cycle [n])))

(defn probably-prime
   "Miller-Rabin: given an integer n, determine if it is prime with
    probability 1 - (4^-k), where k is the number of tests performed."
   [accuracy n]
   (let [[s d] (factor-2s (dec n))
        composite-witness?  (fn [a]
           (and
              (let [t (pow a d n)]  (and (not= t 1) (not= t (dec n))))
              (every? (fn [r] (not= (pow a (* r d) n) (dec n)))
                      (take (dec s) (iterate #(+ %1 %1) 2)))))]
      (cond
         (< n 2)   false
         (= n 2)   true
         (even? n) false
         true
            (not-any? composite-witness?
                  (take accuracy (map (partial + 2) (random-gen (min (- n 4) 0x7FFFFFFFFFFFFFFF))))))))

(def prime-accuracy 10)
(def is-prime?  (partial probably-prime prime-accuracy))

(defn primes-below [n]
  (cons 2 (for [i (range 3 n)
                :when (is-prime? i)]
            i)))

(defn prove-goldbach [num]
  (loop [primes (primes-below num)]
    (if (empty? primes)
      false
      (let [p (first primes)
            n (Math/sqrt (* 1/2 (- num p)))]
        (if (== (bigint n) n)
          true
          (recur (next primes)))))))

(defn prob46 []
  (let [ocn (fn [upto] (filter #(and (odd? %)
                                     (not (is-prime? %)))
                               (range 3 upto)))]
    (loop [nums (ocn 10000)]
      (when (not (empty? nums))
        (if (not (prove-goldbach (first nums)))
          (first nums)
          (recur (next nums)))))))
