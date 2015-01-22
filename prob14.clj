;; The following iterative sequence is defined for the set of positive
;; integers:

;; n → n/2 (n is even) n → 3n + 1 (n is odd)

;; Using the rule above and starting with 13, we generate the
;; following sequence: 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1

;; It can be seen that this sequence (starting at 13 and finishing at
;; 1) contains 10 terms. Although it has not been proved yet (Collatz
;; Problem), it is thought that all starting numbers finish at 1.

;; Which starting number, under one million, produces the longest
;; chain?

;; NOTE: Once the chain starts the terms are allowed to go above one
;; million.

(def limit 1000000)

(defn next-number [n]
  ;; return the next collaz number after n
  (if (even? n)
    (/ n 2)
    (+ (* 3 n) 1)))

(def count-collaz-sequence
  (fn [number]
    (cond
     ;; http://www.ericr.nl/wondrous/techpage.html
     ;; http://math.stackexchange.com/questions/60573/reducing-the-time-to-calculate-collatz-sequences
     (even? number) 0
     (zero? (rem (- number 2) 3)) 0
     (zero? (rem (- number 5) 8)) 0
     (zero? (rem (- number 2) 9)) 0
     (zero? (rem (- number 4) 9)) 0
     (zero? (rem (- number 5) 9)) 0
     (zero? (rem (- number 8) 9)) 0

     :else (loop [num number
                  cnt 0]
             (if (= num 1) (inc cnt)
                 (recur (next-number num) (inc cnt)))))))

(require '[clojure.core.reducers :as r])

(defn prob14 []
  (apply max-key second
         (map (fn [i] [i (count-collaz-sequence i)])
               (range (/ limit 2) limit))))
