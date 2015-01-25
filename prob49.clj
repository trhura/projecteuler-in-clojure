;; The arithmetic sequence, 1487, 4817, 8147, in which each of the terms
;; increases by 3330, is unusual in two ways: (i) each of the three terms
;; are prime, and, (ii) each of the 4-digit numbers are permutations of
;; one another.

;; There are no arithmetic sequences made up of three 1-, 2-, or 3-digit
;; primes, exhibiting this property, but there is one other 4-digit
;; increasing sequence.

;; What 12-digit number do you form by concatenating the three terms in
;; this sequence?

(defn lexicographic-permutation [coll]
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
                      (if (> (get v curidx) (get v (dec curidx)))
                        (dec curidx)
                        (recur (dec curidx))))))

        get-l (fn [v k]
                (assert (vector? v))
                (loop [curidx (lastidx v)]
                  (assert (> curidx 0))
                  (if (> (get v curidx) (get v k))
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

        all-permutations (fn get-all [cv all]
                           (if (nil? cv) all
                             (recur (next-permutation cv) (conj all cv))))]

    (all-permutations (apply vector (sort coll)) [])))

(defn is-prime? [num]
  ;; primality test by trail-division
  (cond (< num 2) false
        (= num 2) true
        (even? num) false
        :else (loop [n 3]
                (cond (> (* n n) num) true
                      (zero? (rem num n)) false
                      :else (recur (+ 2 n))))))

(defn prime-sequence [start end]
  ;; generate lazy seq of primes
  (let [next-primes (fn lazy-primes [num]
                      ;; return lazy prime primes starting from num
                      (if (> num end)
                        nil
                        (if (is-prime? num)
                          (cons num (lazy-seq (lazy-primes (inc num))))
                          (lazy-seq (lazy-primes (inc num))))))]
    (next-primes start)))

(defn digit-vector [number]
  ;; return a vector of digits in number
  (vec (map #(- (int %) 48) (str number))))

(defn to-number [digit-vector]
  ;; convert a digit vector to an actual number
  (apply + (map (fn [[p d]] (bigint (*' d (Math/pow 10 p))))
                (map-indexed vector (reverse digit-vector)))))

(defn double-exists [coll]
  ;; there exists a double of i in any element i of coll
  (some #(some #{(* 2 %)} coll) coll))

(defn prob49 []
  (map #(apply str (map str %))
       (for [i (prime-sequence 1487 5000)
             :let [permutations (->> i
                                     digit-vector
                                     lexicographic-permutation
                                     (map to-number)
                                     (remove #{i}))
                   prime-permutations (filter is-prime? permutations)
                   differnce-with-i   (map #(- % i) prime-permutations)
                   double-difference  (double-exists differnce-with-i)]
             :when (not (nil? double-difference))]
         [i (+ i (/ double-difference 2)) (+ i double-difference)])))
