;; A Pythagorean triplet is a set of three natural numbers, a < b < c,
;; for which, a2 + b2 = c2

;; For example, 32 + 42 = 9 + 16 = 25 = 52.

;; There exists exactly one Pythagorean triplet for which a + b + c =
;; 1000.  Find the product abc.

(defn prob9-brute []
  ;; brute-force
  (apply * (first (for [c (range 500 0 -1)
               a (range 1 c)
               :let [b (- 1000 (+ a c))]
               :when (= (+ (* a a) (* b b))
                        (* c c))]
           [a b c]))))

(defn prob9 []
  ;; http://en.wikipedia.org/wiki/Formulas_for_generating_Pythagorean_triples
  (apply * (first (for [m (range 0 1000)
                        n (range 1 m)
                        k (range 1 5)
                        :let [a (* k (- (* m m) (* n n)))
                              b (* k 2 m n)
                              c (* k (+ (* m m) (* n n)))]
                        :when (= (+ a b c) 1000)]
                    [a b c]))))
