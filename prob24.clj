;; A permutation is an ordered arrangement of objects. For example,
;; 3124 is one possible permutation of the digits 1, 2, 3 and 4. If
;; all of the permutations are listed numerically or alphabetically,
;; we call it lexicographic order. The lexicographic permutations of
;; 0, 1 and 2 are:

;; 012 021 102 120 201 210

;; What is the millionth lexicographic permutation of the digits 0, 1,
;; 2, 3, 4, 5, 6, 7, 8 and 9?

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

(defn prob24 []
  (nth (lexicographic-permutation (range 10)) 999999))
