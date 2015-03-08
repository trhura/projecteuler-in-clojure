;; Triangle, square, pentagonal, hexagonal, heptagonal, and octagonal
;; numbers are all figurate (polygonal) numbers and are generated by
;; the following formulae:

;; Triangle   P3,n=n(n+1)/2   1, 3, 6, 10, 15, ...
;; Square   P4,n=n2   1, 4, 9, 16, 25, ...
;; Pentagonal   P5,n=n(3n−1)/2   1, 5, 12, 22, 35, ...
;; Hexagonal   P6,n=n(2n−1)   1, 6, 15, 28, 45, ...
;; Heptagonal   P7,n=n(5n−3)/2   1, 7, 18, 34, 55, ...
;; Octagonal   P8,n=n(3n−2)   1, 8, 21, 40, 65, ...

;; The ordered set of three 4-digit numbers: 8128, 2882, 8281, has
;; three interesting properties.

;;     The set is cyclic, in that the last two digits of each number
;;     is the first two digits of the next number (including the last
;;     number with the first).  Each polygonal type: triangle
;;     (P3,127=8128), square (P4,91=8281), and pentagonal
;;     (P5,44=2882), is represented by a different number in the set.
;;     This is the only set of 4-digit numbers with this property.

;; Find the sum of the only ordered set of six cyclic 4-digit numbers
;; for which each polygonal type: triangle, square, pentagonal,
;; hexagonal, heptagonal, and octagonal, is represented by a different
;; number in the set.


(defn digit-count [number]
  ;; return the number of digits in number
  (count (str number)))

(defn triangle-numbers []
  ;; lazy-seq of triangle numbers
  (let [generate (fn this [i]
                       (cons (/ (* i (inc i)) 2) (lazy-seq (this (inc i)))))]
    (generate 1)))

(defn square-numbers []
  ;; lazy-seq of square numbers
  (let [generate (fn this [i]
                       (cons (int (Math/pow i 2)) (lazy-seq (this (inc i)))))]
    (generate 1)))

(defn pentagonal-numbers []
  ;; lazy-seq of pentagonal numbers
  (let [generate (fn this [i]
                       (cons (/ (* i (dec (* 3 i))) 2) (lazy-seq (this (inc i)))))]
    (generate 1)))

(defn hexagonal-numbers []
  ;; lazy-seq of hexagonal numbers
  (let [generate (fn this [i]
                   (cons (* i (dec (* 2 i))) (lazy-seq (this (inc i)))))]
    (generate 1)))

(defn heptagonal-numbers []
  ;; lazy-seq of heptagonal numbers
  (let [generate (fn this [i]
                       (cons  (/ (* i (- (* 5 i) 3)) 2) (lazy-seq (this (inc i)))))]
    (generate 1)))

(defn octagonal-numbers []
  ;; lazy-seq of octagonal numbers
  (let [generate (fn this [i]
                       (cons (* i (- (* 3 i) 2)) (lazy-seq (this (inc i)))))]
    (generate 1)))

(defn n-digits-seq [seq n]
  ;; return the items in lazy seq with exactly n digit-counts
  ;; assuming the seq is sorted in increasing order
  ;; can't use filter for lazy-seq
  (let [n-digit (fn [num] (= (digit-count num) n))
        not-n-digit (comp not n-digit)]
    (take-while n-digit
                (drop-while not-n-digit seq))))

(defn remove-nils [seq]
  (if (coll? seq)
    (map remove-nils (filter (comp not nil?) seq))
    seq))

(defn filter-empty [seq]
  (if (coll? seq)
    (filter (comp not empty?) (map filter-empty seq))
    seq))

(defn smart-flatten [seq]
  (if (coll? seq)
    (if (not-any? coll? seq)
      [seq]
      (mapcat smart-flatten seq))))

(defn find-cyclic-sequences [seq num max-length]
  ;; find the cyclic number sequence in seq with length
  ;; less than max-length
  ;; assuming num is also in seq
  (let [str-seq  (map str seq)
        start-num  (str num)

        next-cyclic-numbers (fn [num]
                              (filter #(.startsWith % (subs num 2))
                                      str-seq))

        find-cyclics  (fn this [num cyclic-seq]
                        (let [next-nums (next-cyclic-numbers num)]
                          (if (or (empty? next-nums)
                                  (>= (count cyclic-seq) max-length))
                            nil
                            (map (fn [n]
                                   (if (= start-num n)
                                     (conj cyclic-seq num)
                                     (this n (conj cyclic-seq num))))
                                 next-nums))))]

    (-> (find-cyclics start-num [])
        remove-nils
        filter-empty
        smart-flatten)))

(defn find-n-cyclic-numbers [seq n]
  ;; find the set of n cyclic 4-digit numbers in seq
  (let [convert-strings-to-int (fn [seq]
                                 (map (fn [coll] (map (partial read-string)
                                                      coll))
                                      seq))]
    (convert-strings-to-int
     (filter #(= n (count %))
             (-> (map #(do (println "find cyclic for" %)
                           (find-cyclic-sequences seq % n)) seq)
                 remove-nils
                 filter-empty
                 smart-flatten)))))

(defn everyx-in-eachofy [xs ys]
  (if (not (apply distinct? xs))
    false
    (loop [xs (set xs)
           ys (map set ys)]
      (if (empty? xs)
        true
        (let [cys (first ys)
              x-in-y (some #(cys %) xs)]
          (if (nil? x-in-y)
            false
            (recur (remove #{x-in-y} xs)
                   (remove #(= cys %) ys))))))))

(defn prob61 []
  (let [triangles (n-digits-seq (triangle-numbers) 4)
        squares (n-digits-seq (square-numbers) 4)
        pentagonals (n-digits-seq (pentagonal-numbers) 4)
        hexagonals (n-digits-seq (hexagonal-numbers) 4)
        heptagonals (n-digits-seq (heptagonal-numbers) 4)
        octagonals (n-digits-seq (octagonal-numbers) 4)
        alls (set (concat triangles squares pentagonals
                          hexagonals heptagonals octagonals))]
    (filter #(everyx-in-eachofy % [octagonals heptagonals hexagonals
                                    pentagonals squares triangles])
            (find-n-cyclic-numbers alls 6))))