;; Starting with 1 and spiralling anticlockwise in the following way, a
;; square spiral with side length 7 is formed.

;; 37 36 35 34 33 32 31
;; 38 17 16 15 14 13 30
;; 39 18  5  4  3 12 29
;; 40 19  6  1  2 11 28
;; 41 20  7  8  9 10 27
;; 42 21 22 23 24 25 26
;; 43 44 45 46 47 48 49

;; It is interesting to note that the odd squares lie along the bottom
;; right diagonal, but what is more interesting is that 8 out of the 13
;; numbers lying along both diagonals are prime; that is, a ratio of 8/13
;; ≈ 62%.

;; If one complete new layer is wrapped around the spiral above, a square
;; spiral with side length 9 will be formed. If this process is
;; continued, what is the side length of the square spiral for which the
;; ratio of primes along both diagonals first falls below 10%?

(defn get-spiral-corners [spiral-length]
  ;; return the fourmost corners of spiral
  (let [step (- spiral-length 1)
        pend (* (- spiral-length 2) (- spiral-length 2)) ;; previous end
        start (+ step pend)]
    (map #(+ start (* step %)) (range 4))))

(defn is-prime? [num]
  ;; primality test by trail-division
  (cond (< num 2) false
        (= num 2) true
        (even? num) false
        :else (loop [n 3]
                (cond (> (* n n) num) true
                      (zero? (rem num n)) false
                      :else (recur (+ 2 n))))))

(defn prime-ratio [coll]
  (/ (count (filter is-prime? coll)) (count coll)))

(defn prob58 []
  (loop [spiral-length 3
         primes-count  0  ;; primes in [1]
         total-count   1] ;; [1]
    (let [corners (get-spiral-corners spiral-length)
          primes-count (+ primes-count
                          (count (filter is-prime? corners)))
          total-count  (+ total-count 4)
          ratio (/ primes-count total-count)]
      (if (< ratio 0.10)
        spiral-length
        (recur (+ 2 spiral-length) primes-count total-count)))))
