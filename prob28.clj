
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
