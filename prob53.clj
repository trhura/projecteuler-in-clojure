(defn _factorial [number]
  (loop [num number
         res 1]
    (if (<= num 1) res
        (recur (dec num) (*' res num)))))

(def factorial (memoize _factorial))

(defn n-combinations [n r]
  (/ (factorial n) (*' (factorial r) (factorial (- n r)))))

(defn prob53 []
  (count (for [n (range 23 101)
                 r (range 1 n)
                 :when (> (n-combinations n r) 1000000)]
             1)))
