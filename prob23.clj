(defn get-proper-divisors [num]
  (for [x (range 1 (inc (/ num 2))) :when (zero? (rem num x))] x))

(defn spd [num]
  ;; sum of proper divisors
  (apply + (get-proper-divisors num)))

(defn is-abundant? [num]
  (> (spd num) num))

(defn prob23 []
  (let [abundant-numbers (apply hash-set (for [x (range 12 28123) :when (is-abundant? x)] x))]
    (apply + (for [x (range 1 28124) :when (every? #(not (contains? abundant-numbers (- x %))) abundant-numbers)] x))))
