;; The number, 1406357289, is a 0 to 9 pandigital number because it is
;; made up of each of the digits 0 to 9 in some order, but it also has
;; a rather interesting sub-string divisibility property.

;; Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this
;; way, we note the following:

;;     d2d3d4=406 is divisible by 2
;;     d3d4d5=063 is divisible by 3
;;     d4d5d6=635 is divisible by 5
;;     d5d6d7=357 is divisible by 7
;;     d6d7d8=572 is divisible by 11
;;     d7d8d9=728 is divisible by 13
;;     d8d9d10=289 is divisible by 17

;; Find the sum of all 0 to 9 pandigital numbers with this property.


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

        all-permutations (fn lazy-permutations [curperm]
                           (if (nil? curperm)
                             nil
                             (cons curperm (lazy-seq (lazy-permutations (next-permutation curperm))))))]

    (all-permutations (vec (sort coll)))))

(defn to-number [digit-vector]
  ;; convert a digit vector to an actual number
  (apply + (map (fn [[p d]] (bigint (*' d (Math/pow 10 p))))
                (map-indexed vector (reverse digit-vector)))))

(defn pandigitals []
  (lexicographic-permutation (range 0 10)))

(defn substring-divisible? [digitvec]
  (and (zero? (rem (to-number (subvec digitvec 1 4)) 2))
       (zero? (rem (to-number (subvec digitvec 2 5)) 3))
       (zero? (rem (to-number (subvec digitvec 3 6)) 5))
       (zero? (rem (to-number (subvec digitvec 4 7)) 7))
       (zero? (rem (to-number (subvec digitvec 5 8)) 11))
       (zero? (rem (to-number (subvec digitvec 6 9)) 13))
       (zero? (rem (to-number (subvec digitvec 7 10)) 17))))

(defn prob44 []
  (apply + (->> (pandigitals)
                (filter substring-divisible?)
                (map to-number))))
