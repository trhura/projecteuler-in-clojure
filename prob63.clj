;; The 5-digit number, 16807=75, is also a fifth power. Similarly, the
;; 9-digit number, 134217728=89, is a ninth power.

;; How many n-digit positive integers exist which are also an nth power?

(defn expt [x n]
  ;; expt implementation
  (let [multiply (fn [a b] (*' a b))
        square (fn [n] (multiply n n))]

    (cond (= n 0)         1
          (= n 1)         x
          (even? n)       (expt (square x) (/ n 2))
          (odd?  n)       (*' x (expt (square x) (/ (- n 1) 2))))))

(defn perfect-pows [pow]
  ;; a lazy sequence of perfect powers
  (let [generate (fn this [i]
                   (cons (expt i pow) (lazy-seq (this (inc i)))))]
    (generate 1)))

(defn digit-count [number]
  ;; return the number of digits in number
  (count (str number)))

(defn n-digits-seq [seq n]
  ;; return the items in lazy seq with exactly n digit-counts
  ;; assuming the seq is sorted in increasing order
  ;; can't use filter for lazy-seq
  (let [less-than-n-digit (fn [num] (<= (digit-count num) n))]
    (filter #(= n (digit-count %)) (take-while less-than-n-digit seq))))

(defn prob63 []
  (apply + (for [i (range 1 1000)
                 :while (not (empty? (n-digits-seq (perfect-pows i) i)))]
             (count (n-digits-seq (perfect-pows i) i)))))
