;; It is possible to write five as a sum in exactly six different
;; ways:

;; 4 + 1
;; 3 + 2
;; 3 + 1 + 1
;; 2 + 2 + 1
;; 2 + 1 + 1 + 1
;; 1 + 1 + 1 + 1 + 1

;; How many different ways can one hundred be written as a sum of at
;; least two positive integers?

(def number-of-partitions
  ;; http://en.wikipedia.org/wiki/Partition_%28number_theory%29#Recurrence_formula
  ;; http://en.wikipedia.org/wiki/Pentagonal_number_theorem#Relation_with_partitions
  (memoize
   (fn [n]
     (cond
      (< n 0)    0
      (= n 0)    1
      :else      (apply + (for [k (interleave (range 1 1000) (range -1 -1000 -1))
                                :let [pk (pentagonal-number k)]
                                :while (>= (- n pk) 0)]
                            (* (int (Math/pow (- 1) (dec k)))
                               (number-of-partitions (- n pk)))))))))

(defn prob76 []
  (dec (number-of-partitions 100)))
