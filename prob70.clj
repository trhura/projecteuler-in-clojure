;; Euler's Totient function, φ(n) [sometimes called the phi function],
;; is used to determine the number of positive numbers less than or
;; equal to n which are relatively prime to n. For example, as 1, 2,
;; 4, 5, 7, and 8, are all less than nine and relatively prime to
;; nine, φ(9)=6.  The number 1 is considered to be relatively prime to
;; every positive number, so φ(1)=1.

;; Interestingly, φ(87109)=79180, and it can be seen that 87109 is a
;; permutation of 79180.

;; Find the value of n, 1 < n < 107, for which φ(n) is a permutation
;; of n and the ratio n/φ(n) produces a minimum.

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

(def prime-sieve (generate-prime-sieve 10000000))

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

(defn digit-vector [num]
  ;; convert a number string to a vector of its digits
  (map #(Character/getNumericValue %) (str num)))

(defn is-permutation? [x y]
  ;; return true if x is a permutation of y
  (= (sort (digit-vector x))
     (sort (digit-vector y))))

(defn prob70 []
  ;; SLOW
  (loop [i 2
         mini 10E10]

    (when (= (rem i 10000) 0)
      (println i mini))

    (if (> i 10000000)
      mini
      (let [phi (phi-function i)]
        (if (is-permutation? i phi)
          (recur (inc i) (min mini (/ i phi)))
          (recur (inc i) mini))))))
