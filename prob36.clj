;; The decimal number, 585 = 10010010012 (binary), is palindromic in
;; both bases.

;; Find the sum of all numbers, less than one million, which are
;; palindromic in base 10 and base 2.

;; (Please note that the palindromic number, in either base, may not
;; include leading zeros.)

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

(defn prob36 []
  (apply + (for [i (range 1000000)
                 :when (and (is-palindrome? (str i))
                            (is-palindrome? (Integer/toBinaryString i)))]
             i)))
