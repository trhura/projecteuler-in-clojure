(require '[clojure.string :as string])

(defn recurring? [fractions]
  (loop [frcts fractions]
    (let [len (count frcts)
          v (apply vector frcts)]
      (cond (not (even? len)) (recur (next v))
            (> (- (count fractions) (count frcts)) 10) false ;; needed to stop looking after 10th char
            (< len 8) false ;; check at least more than 4 char are repeated
            (= (subvec v 0 (/ len 2)) (subvec v (/ len 2))) true
            :else (recur (next v))))))

(defn divide-one-by [num]
  (loop [leftover 1 frcts []]
    (let [q (int (/ (* leftover 10) num)) ;; quotient
          r (rem (* leftover 10) num)] ;; remainder
      (cond (zero? leftover) frcts
            ;;(> (count frcts) 20) frcts
            (recurring? (conj frcts q)) (conj frcts q)
            :else (recur r (conj frcts q))))))

(defn prob26 []
  (apply max-key second
         (for [i (range 1 1000)]
           (do (println i)
               [i (count (divide-one-by i))]))))
