;; By replacing the 1st digit of the 2-digit number *3, it turns out that
;; six of the nine possible values: 13, 23, 43, 53, 73, and 83, are all
;; prime.

;; By replacing the 3rd and 4th digits of 56**3 with the same digit, this
;; 5-digit number is the first example having seven primes among the ten
;; generated numbers, yielding the family: 56003, 56113, 56333, 56443,
;; 56663, 56773, and 56993. Consequently 56003, being the first member of
;; this family, is the smallest prime with this property.

;; Find the smallest prime which, by replacing part of the number (not
;; necessarily adjacent digits) with the same digit, is part of an eight
;; prime value family.

;; placeholder
(def _ -1)

(defn digit-vector [string]
  ;; convert a number string to a set of its digits
  (map #(- (int %) 48) (str string)))

(defn to-number [digit-vector]
  ;; convert a digit vector to an actual number
  (apply + (map (fn [[p d]] (int (* d (Math/pow 10 p))))
                (map-indexed vector (reverse digit-vector)))))

(defn is-prime? [num]
  ;; primality test by trail-division
  (cond (< num 2) false
        (= num 2) true
        (even? num) false
        :else (loop [n 3]
                (cond (> (* n n) num) true
                      (zero? (rem num n)) false
                      :else (recur (+ 2 n))))))

(defn n-prime-value-family? [dv n]
  ;; replace placeholders with 0-9 digits
  ;; return true if at least n of them are prime
  (>= (count (filter is-prime?
                     (map #(to-number
                            (replace {_ %} dv))
                          ;; skip zero if dv starts with _
                          (if (= (first dv) _)
                            (range 1 10)
                            (range 10)))))
      n))

(defn duplicate-digits [dv]
  ;; return the digits in dv with multiple occurences
  (sort (keys (filter (fn [[itm cnt]] (> cnt 1))
                      (frequencies dv)))))

(defn get-possible-families [dv]
  ;; replace a set of dv in which digits with multiple
  ;; appearances with replaced with _
  (for [d (duplicate-digits dv)
        l (range 1 10 2)]
    (conj (vec (replace {d _} dv)) l)))

(defn prob51 []
  (loop [lst (mapcat (fn [x] (-> x
                                 digit-vector
                                 get-possible-families))
                     ;; make sure range is big enough
                     (range 5700 100000))]
      (let [dv (first lst)]
        (if (n-prime-value-family? dv 8)
          dv
          (recur (next lst))))))
