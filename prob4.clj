
;; A palindromic number reads the same both ways. The largest
;; palindrome made from the product of two 2-digit numbers is 9009 =
;; 91 × 99.

;; Find the largest palindrome made from the product of two 3-digit
;; numbers.

(defn digit-vector [number]
  ;; convert number to a vector of its digits
  (loop [n number
         v (vector)]
    (let [digit (rem n 10)
          remaining (int (/ n 10))]
      (if (< remaining 1)
        (cons digit v)
        (recur remaining (cons digit v))))))

(defn is-palindrome? [coll]
  ;; check whether a collection is palindrome or not
  (loop [left 0
         right (dec (count coll))]
    (if (> left right)
      true
      (if (not= (nth coll left)
                (nth coll right))
        false
        (recur (inc left) (dec right))))))

(defn prob4 []
  (apply max (for [x (range 100 999)
                   y (range 100 999)
                   :when (is-palindrome? (digit-vector (* x y)))]
               (* x y))))
