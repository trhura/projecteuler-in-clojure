;; It is possible to write five as a sum in exactly six different
;; ways:

;; 4 + 1
;; 3 + 2
;; 3 + 1 + 1
;; 2 + 2 + 1
;; 2 + 1 + 1 + 1
;; 1 + 1 + 1 + 1 + 1

;; How many different ways can one hundred be written as a sum of at
;; least two positive integers?

(def number-of-partitions
  ;; http://en.wikipedia.org/wiki/Partition_%28number_theory%29#Recurrence_formula
  (fn [n] (cond
           (< n 0) 0
           (= n 0) 1
           :else   (apply + (for [k (interleave (range 1 1000)
                                                (range -1 -1000 -1))
                                  :let [gk (* 1/2 k (- (* 3 k) 1))]
                                  :while (>= (- n gk) 0)]
                              (* (int (Math/pow (- 1) (dec k)))
                                 (number-of-partitions (- n gk))))))))
;; user> (time (number-of-partitions 30))
;; "Elapsed time: 134978.672483 msecs"
;; 5604

(def memoized-number-of-partitions
  (memoize
   (fn [n] (cond
           (< n 0) 0
           (= n 0) 1
           :else   (apply + (for [k (interleave (range 1 1000)
                                                (range -1 -1000 -1))
                                  :let [gk (* 1/2 k (- (* 3 k) 1))]
                                  :while (>= (- n gk) 0)]
                              (* (int (Math/pow (- 1) (dec k)))
                                 (memoized-number-of-partitions (- n gk)))))))))


;; user> (time (memoized-number-of-partitions 30))
;; "Elapsed time: 2.075863 msecs"
;; 5604

(defn prob75 []
  (dec (memoized-number-of-partitions 100)))
