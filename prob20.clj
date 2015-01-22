;; n! means n × (n − 1) × ... × 3 × 2 × 1

;; For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800, and the sum
;; of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

;; Find the sum of the digits in the number 100!

(defn digit-vector [number]
  ;; return a vector of digits in number
  (vec (map #(- (int %) 48) (str number))))

(defn factorial [number]
  (loop [num number
         res 1]
    (if (<= num 1) res
        (recur (dec num) (*' res num)))))

(defn prob20 []
  (apply + (digit-vector (factorial 100))))
