;; Euler's Totient function, φ(n) [sometimes called the phi function],
;; is used to determine the number of numbers less than n which are
;; relatively prime to n. For example, as 1, 2, 4, 5, 7, and 8, are
;; all less than nine and relatively prime to nine, φ(9)=6.

;; n Relatively Prime φ(n) n/φ(n)
;; 2 1 1 2
;; 3 1,2 2 1.5
;; 4 1,3 2 2
;; 5 1,2,3,4 4 1.25
;; 6 1,5 2 3
;; 7 1,2,3,4,5,6 6 1.1666...
;; 8 1,3,5,7 4 2
;; 9 1,2,4,5,7,8 6 1.5
;; 10 1,3,7,9 4 2.5

;; It can be seen that n=6 produces a maximum n/φ(n) for n ≤ 10.
;; Find the value of n ≤ 1,000,000 for which n/φ(n) is a maximum.

(defn generate-prime-sieve [size]
  (let [next-prime (fn [p sieve]
                     ;; the next non-false val in sieve
                     ;; greater than p
                     (loop [index (inc p)]
                       (cond
                        (>= index size)     size
                        (true? (get sieve index)) index
                        :else (recur (inc index)))))

        prime-vec (vec (concat [false false true]
                               (take (- size 3)
                                     (repeat true))))]
    (loop [p 2
           sieve (transient prime-vec)]
      (let [nextp (next-prime p sieve)]
        (if (< p size)
          (recur nextp (reduce (fn [t i] (assoc! t i false))
                               sieve
                               (range (* p p) size p)))
          (persistent! sieve))))))

(def prime-sieve (generate-prime-sieve 1000000))

(defn prime-sequence []
  (filter (comp not nil?)
          (map-indexed (fn [idx itm]
                         (if (get prime-sieve idx)
                           itm
                           nil))
                       (range 1000000))))

(defn prime-factors [number]
  ;; prime-factors by trial divison
  (loop [num number
         prime-seq (prime-sequence)
         factors nil]

    (let [prime-num (first prime-seq)]
      (if (> (* prime-num prime-num) num)
        (cons num factors)
        (if (zero? (rem num prime-num))
          (recur (/ num prime-num)
                 (prime-sequence)
                 (cons prime-num factors))
          (recur num (next prime-seq) factors))))))

(defn phi-function [number]
  (apply * number (for [p (distinct (prime-factors number))]
                    (- 1 (/ 1 p)))))

(defn prob69 []
  (apply max-key second (for [num (range 2 1000000)]
                          (do (when (zero? (rem num 10000))
                                (println num))
                              [num (/ num (phi-function num))]))))
