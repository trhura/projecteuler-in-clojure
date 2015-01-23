;; If p is the perimeter of a right angle triangle with integral
;; length sides, {a,b,c}, there are exactly three solutions for p =
;; 120.

;; {20,48,52}, {24,45,51}, {30,40,50}

;; For which value of p ≤ 1000, is the number of solutions maximised?


(defn prob39 []
  ;; http://en.wikipedia.org/wiki/Formulas_for_generating_Pythagorean_triples
  (apply max-key
         second
         (frequencies (map #(apply + %) (set (for [m (range 100)
                                                   n (range 1 m)
                                                   k (range 1 100)
                                                   :let [a (- (* m m) (* n n))
                                                         b (* 2 m n)
                                                         c (+ (* m m) (* n n))]
                                                   :when (<= (* k (+ a b c)) 1000)]
                                               [(* k a) (* k b) (* k c)]))))))

;; (defn stifel-sequence []
;;   (apply concat (for [i (range)
;;                       :let [num (inc i)
;;                             div (inc (* 2 num))
;;                             frt (+ num (/ num div))
;;                             b (numerator frt)
;;                             a (denominator frt)
;;                             c (int (Math/sqrt (+ (* a a) (* b b))))]
;;                       :while (<= (+ a b c) 1000)]
;;                   (for [k (range 1 100)
;;                         :while (< (* k (+ a b c)) 1000)]
;;                     (map int [(* k a) (* k b) (* k c)])
;;                     ))))

;; (defn ozanam-sequence []
;;   (apply concat (for [i (range)
;;                       :let [num (inc i)
;;                             frt (+ num (/ (+ (* 4 num) 3)
;;                                           (+ (* 4 num) 4)))
;;                             b (numerator frt)
;;                             a (denominator frt)
;;                             c (int (Math/sqrt (+ (* a a) (* b b))))]
;;                       :while (<= (+ a b c) 1000)]
;;                   (for [k (range 1 100)
;;                         :while (< (* k (+ a b c)) 1000)]
;;                     (map int [(* k a) (* k b) (* k c)])
;;                     ))))

;; (def get-factor-pairs
;;   (memoize (fn [num]
;;              (set (for [i (range 1 (/ num 2))
;;                         :when (zero? (rem num i))]
;;                     (sort [(/ num i) i]))))))

;; (defn get-pythagorean-triplets [b]
;;   ;; http://en.wikipedia.org/wiki/Formulas_for_generating_Pythagorean_
;;   ;; triples#Generating_triples_when_one_side_is_known
;;   (if (even? b)
;;     (let [mn (/ b 2)]
;;       (for [[n m] (get-factor-pairs mn)
;;             :let [a (- (* m m) (* n n))
;;                   c (+ (* m m) (* n n))]]
;;         [a b c]))
;;     (let [mn b]
;;       (for [[n m] (get-factor-pairs mn)
;;             :let [a (/ (- (* m m) (* n n)) 2)
;;                   c (/ (+ (* m m) (* n n)) 2)]]
;;         [a b c]))))

;; (defn prob39 []
;;   (let [;; all possible triplets when 1 < b < 500
;;         all-triplets (apply concat (for [b (range 1 500)]
;;                                      (get-pythagorean-triplets b)))

;;         ;; take only triplets where a + b + c <= 1000
;;         valid-triplets (filter (fn [[a b c]] (<= (+ a b c) 1000)) all-triplets)

;;         ;;
;;         perimeters (map #(apply + %) valid-triplets)
;;         ]
;;     valid-triplets

;;     ))
