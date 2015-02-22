;; It is possible to show that the square root of two can be expressed
;; as an infinite continued fraction.

;; √ 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...

;; By expanding this for the first four iterations, we get:

;; 1 + 1/2 = 3/2 = 1.5
;; 1 + 1/(2 + 1/2) = 7/5 = 1.4
;; 1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
;; 1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...

;; The next three expansions are 99/70, 239/169, and 577/408, but the
;; eighth expansion, 1393/985, is the first example where the number
;; of digits in the numerator exceeds the number of digits in the
;; denominator.

;; In the first one-thousand expansions, how many fractions contain a
;; numerator with more digits than denominator?

;; (defn expand [n]
;;   ;; un-mathemathic
;;   ;; expand square root of two n time as an infinite
;;   ;; continued fraction.
;;   (loop [iteration n
;;          result    2]
;;     (if (== iteration 1)
;;       (+ 1 (/ 1 result))
;;       (recur (dec iteration) (+ 2 (/ 1 result))))))

(defn digit-count [number]
  ;; return the number of digits in number
  (count (str number)))

(defn root-two-fractions []
  ;; http://en.wikipedia.org/wiki/Square_root_of_2#Continued_fraction_representation
  ;; generate lazy seq of fractions
  (let [next-fraction (fn lazy-fraction [num]
                        (let [p (numerator num)
                              q (denominator num)]
                          (cons num (lazy-seq (lazy-fraction (/ (+ p (* 2 q)) (+ p q)))))))]
    (next-fraction 3/2)))

(defn prob57 []
  (count (filter
          (fn [num] (let [n (numerator num)
                          d (denominator num)]
                      (> (digit-count n) (digit-count d))))
          (take 1000 (root-two-fractions)))))
