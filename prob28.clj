;; Starting with the number 1 and moving to the right in a clockwise
;; direction a 5 by 5 spiral is formed as follows:

;; 21 22 23 24 25
;; 20  7  8  9 10
;; 19  6  1  2 11
;; 18  5  4  3 12
;; 17 16 15 14 13

;; It can be verified that the sum of the numbers on the diagonals is
;; 101.

;; What is the sum of the numbers on the diagonals in a 1001 by 1001
;; spiral formed in the same way?

(defn get-diagonals-in-spiral [n]
  ;; n odd number >= 1
  (loop [start 1
         step  2
         diag  nil]
    (if (> step n) (concat diag [start])
        (recur (+ start (* step 4))
               (+ 2 step)
               (concat diag (for [x (range 4)]
                              (+ start (* x step))))))))

(defn prob28 []
  (reduce + (get-diagonals-in-spiral 1001)))
