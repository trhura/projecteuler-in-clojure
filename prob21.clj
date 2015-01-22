(defn get-proper-divisors [num]
  (for [x (range 1 (inc (/ num 2))) :when (zero? (rem num x))] x))

(defn spd [num]
  ;; sum of proper divisors
  (apply + (get-proper-divisors num)))

(defn prob21 []
  (apply + (for [x (range 2 10000) :when (let [spdx (spd x)]
                                           (and (= (spd spdx) x) (not (= x spdx))))]
             x)))
