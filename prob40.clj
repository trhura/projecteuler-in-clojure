;; An irrational decimal fraction is created by concatenating the
;; positive integers:

;; 0.123456789101112131415161718192021...

;; It can be seen that the 12th digit of the fractional part is 1.

;; If dn represents the nth digit of the fractional part, find the value
;; of the following expression.

;; d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000

(defn irrational-decimal-sequence []
  ;; generate lazy seq of primes
  (let [irrational-seq (fn lazy-irrational [num]
                         (concat (str num) (lazy-seq (lazy-irrational (inc num)))))]
    (irrational-seq 1)))

(defn nth-digit [n]
  (Character/getNumericValue (nth (irrational-decimal-sequence) (dec n))))

(defn prob40[]
  (* (nth-digit 1)
     (nth-digit 10)
     (nth-digit 100)
     (nth-digit 1000)
     (nth-digit 10000)
     (nth-digit 100000)
     (nth-digit 1000000)))
