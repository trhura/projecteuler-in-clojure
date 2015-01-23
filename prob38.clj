;; Take the number 192 and multiply it by each of 1, 2, and 3:

;;     192 × 1 = 192 192 × 2 = 384 192 × 3 = 576

;; By concatenating each product we get the 1 to 9 pandigital,
;; 192384576. We will call 192384576 the concatenated product of 192 and
;; (1,2,3)

;; The same can be achieved by starting with 9 and multiplying by 1, 2,
;; 3, 4, and 5, giving the pandigital, 918273645, which is the
;; concatenated product of 9 and (1,2,3,4,5).

;; What is the largest 1 to 9 pandigital 9-digit number that can be
;; formed as the concatenated product of an integer with (1,2, ... , n)
;; where n > 1?

(require '[clojure.string :refer [join]])

(defn pandigital? [number n]
  ;; return the given number is 1 to n pandigital
  ;; not really correct but should do
  (and (= n (count (set (str number))))
       (not (some #{\0} (str number)))))

(defn pandigital-product [num]
  (loop [n 1
         c ""]
    (cond
     (> (count c) 9)    nil
     (= (count c) 9)    (if (pandigital? c 9)
                          c
                          nil)
     :else              (recur (inc n) (join (concat c (str (* num n))))))))

(defn prob38 []
  (apply max (map #(Integer/parseInt %) (for [i (range 10000)
                                              :when (not (nil? (pandigital-product i)))]
                                          (pandigital-product i)))))
