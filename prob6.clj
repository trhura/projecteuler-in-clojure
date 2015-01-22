;; The sum of the squares of the first ten natural numbers is, 12 + 22
;; + ... + 102 = 385

;; The square of the sum of the first ten natural numbers is, (1 + 2 +
;; ... + 10)2 = 552 = 3025

;; Hence the difference between the sum of the squares of the first
;; ten natural numbers and the square of the sum is 3025 − 385 = 2640.

;; Find the difference between the sum of the squares of the first one
;; hundred natural numbers and the square of the sum.

(defn prob6 []
  (let [natural-numbers (range 101)]
    (int (-
          ;; square of sum
          (Math/pow (apply + natural-numbers) 2)
          ;; sum of square
          (apply +  (map #(Math/pow % 2) natural-numbers))))))
