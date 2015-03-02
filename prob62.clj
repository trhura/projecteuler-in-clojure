;; The cube, 41063625 (3453), can be permuted to produce two other cubes:
;; 56623104 (3843) and 66430125 (4053). In fact, 41063625 is the smallest
;; cube which has exactly three permutations of its digits which are also
;; cube.

;; Find the smallest cube for which exactly five permutations of its
;; digits are cube.

(defn perfect-cubes []
  ;; a lazy sequence of perfect cubes, aka 1, 8 ...
  (let [generate (fn this [i]
                   (cons (* i i i) (lazy-seq (this (inc i)))))]
    (generate 1)))

(defn digit-vector [num]
  ;; convert a number string to a vector of its digits
  (vec (map #(- (int %) 48) (str num))))

(defn is-permutation? [x y]
  ;; return true if x is a permutation of y
  (= (sort (digit-vector x))
     (sort (digit-vector y))))

(defn digit-count [number]
  ;; return the number of digits in number
  (count (str number)))

(defn n-digits-seq [seq n]
  ;; return the items in lazy seq with exactly n digit-counts
  ;; assuming the seq is sorted in increasing order
  ;; can't use filter for lazy-seq
  (let [n-digit (fn [num] (= (digit-count num) n))
        not-n-digit (comp not n-digit)]
    (take-while n-digit
                (drop-while not-n-digit seq))))

(defn prob62 []
  (loop [cubes (perfect-cubes)]
    (let [c (first cubes)
          n-digit-cubes (n-digits-seq (perfect-cubes)
                                      (digit-count c))
          permutations  (filter (partial is-permutation? c)
                                n-digit-cubes)]
      (if (= 5 (count permutations))
        permutations
        (recur (next cubes))))))
