;; We shall say that an n-digit number is pandigital if it makes use of
;; all the digits 1 to n exactly once. For example, 2143 is a 4-digit
;; pandigital and is also prime.

;; What is the largest n-digit pandigital prime that exists?

(defn is-prime? [num]
  ;; primality test by trail-division
  (cond (< num 2) false
        (= num 2) true
        (even? num) false
        :else (loop [n 3]
                (cond (> (* n n) num) true
                      (zero? (rem num n)) false
                      :else (recur (+ 2 n))))))

(defn lexicographic-permutation [coll]
  ;; reverse lexicographic order
  ;; http://en.wikipedia.org/wiki/Permutation#Generation_in_lexicographic_order
  (let [;; get the last index len - 1
        lastidx (fn [v] (dec (count v)))

        ;; swap i1 and i2 in v
        swap  (fn  [v i1 i2]
                (assert (vector? v))
                (assoc v i2 (v i1) i1 (v i2)))

        get-k (fn [v]
                (assert (vector? v))
                ;; rightmost index k such that a[k] < a[k + 1]
                (loop [curidx (lastidx v)]
                  (if (<= curidx 0) -1
                      (if (< (get v curidx) (get v (dec curidx)))
                        (dec curidx)
                        (recur (dec curidx))))))

        get-l (fn [v k]
                (assert (vector? v))
                (loop [curidx (lastidx v)]
                  (assert (> curidx 0))
                  (if (< (get v curidx) (get v k))
                    curidx
                    (recur (dec curidx)))))

        next-permutation (fn [v]
                           (assert (vector? v))
                           (if (= (get-k v) -1) nil
                               (let [k (get-k v)
                                     l (get-l v k)
                                     k+ (inc k)
                                     swapv (swap v k (get-l v k))]
                                 (apply vector
                                        (concat (subvec swapv 0 k+)
                                                (reverse (subvec swapv k+)))))))

        all-permutations (fn lazy-permutations [curperm]
                           (if (nil? curperm)
                             nil
                             (cons curperm (lazy-seq (lazy-permutations (next-permutation curperm))))))]

    (all-permutations (vec (reverse (sort coll))))))

(defn pandigitals []
  (apply concat (for [i (range 9 0 -1)]
                  (lexicographic-permutation (range i 0 -1)))))

(defn to-number [digit-vector]
  ;; convert a digit vector to an actual number
  (apply + (map (fn [[p d]] (int (* d (Math/pow 10 p))))
                (map-indexed vector (reverse digit-vector)))))

(defn prob41 []
  (loop [p (pandigitals)]
    (if (is-prime? (to-number (first p)))
      (to-number (first p))
      (recur (next p)))))
