(defn count-digits [num]
  (count (str num)))

(defn -fib [a b]
  (cons a (lazy-seq (-fib b (+' b a)))))

(defn fib []
  (-fib 1 1))

(defn prob25 []
  (loop [fib (fib)
         n 1]
    (if (>= (count-digits (first fib)) 1000)
      n
      (recur (rest fib) (inc n)))))
