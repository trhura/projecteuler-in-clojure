(defn prob24 []
  (nth (lexicographic-permutation (range 10)) 999999))

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
