;; Triangle, pentagonal, and hexagonal numbers are generated by the
;; following formulae: Triangle Tn=n(n+1)/2 1, 3, 6, 10, 15, ...
;; Pentagonal Pn=n(3n−1)/2 1, 5, 12, 22, 35, ...  Hexagonal Hn=n(2n−1)
;; 1, 6, 15, 28, 45, ...

;; It can be verified that T285 = P165 = H143 = 40755.

;; Find the next triangle number that is also pentagonal and
;; hexagonal.

(defn pentagonals []
  ;; lazy sequence of infinite pentagonals
  (let [pentagonal-seq (fn lazy-pentagonals [i]
                         (cons (* 1/2 i (- (* 3 i) 1))
                               (lazy-seq (lazy-pentagonals (inc i)))))]
    (pentagonal-seq 1)))

(defn is-hexagonal? [x]
  ;; wikipedia/wiki/Hexagonal_number#Test_for_hexagonal_numbers
  (let [n (/ (+ (Math/sqrt (+ (* 8 x) 1)) 1) 4)]
    (== (bigint n) n)))

(defn prob45 []
  ;; no need to check triangle, as every hexagonal is triangle
  (loop [pentagons (pentagonals)]
    (if (and (is-hexagonal? (first pentagons))
             (not-any? #{(first pentagons)} [1 40755]))
      (first pentagons)
      (recur (next pentagons)))))