;; It can be seen that the number, 125874, and its double, 251748,
;; contain exactly the same digits, but in a different order.

;; Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and
;; 6x, contain the same digits.

(defn digit-vector-set [string]
  ;; convert a number string to a set of its digits
  (set (map #(- (int %) 48) (str string))))

(defn prob52 []
  (loop [num 1]
    (if (apply = (map #(-> %
                           (* num)
                           digit-vector-set)
                      (range 1 7)))
      num
      (recur (inc num)))))
