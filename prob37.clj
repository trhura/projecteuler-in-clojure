;; The number 3797 has an interesting property. Being prime itself, it
;; is possible to continuously remove digits from left to right, and
;; remain prime at each stage: 3797, 797, 97, and 7. Similarly we can
;; work from right to left: 3797, 379, 37, and 3.

;; Find the sum of the only eleven primes that are both truncatable
;; from left to right and right to left.

;; NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

(def is-prime? (memoize (fn [num]
                          ;; primality test by trail-division
                          (cond (< num 2) false
                                (= num 2) true
                                (even? num) false
                                :else (loop [n 3]
                                        (cond (> (* n n) num) true
                                              (zero? (rem num n)) false
                                              :else (recur (+ 2 n))))))))

(defn digit-vector [number]
  ;; convert number to a vector of its digits
  (loop [n number
         v (vector)]
    (let [digit (rem n 10)
          remaining (int (/ n 10))]
      (if (< remaining 1)
        (cons digit v)
        (recur remaining (cons digit v))))))

(defn to-number [digit-vector]
  ;; convert a digit vector to an actual number
  (apply + (map (fn [[p d]] (int (* d (Math/pow 10 p))))
                (map-indexed vector (reverse digit-vector)))))

(defn remove-left [number]
  ;; return a list of numbers got by removing digits from left
  (loop [vec (digit-vector number)
         lst nil]
    (if (empty? vec)
      (map to-number lst)
      (recur (rest vec) (conj lst vec)))))

(defn remove-right [number]
  ;; return a list of numbers got by removing digits from right
  (loop [vec (digit-vector number)
         lst nil]
    (if (empty? vec)
      (map to-number lst)
      (recur (butlast vec) (conj lst vec)))))

(defn truncatable-prime? [number]
  (let [digitvector (digit-vector number)
        truncations (sort (set (concat (remove-left number)
                                       (remove-right number))))]
    (not-any? #(not (is-prime? %)) truncations)))

(defn prob37 []
  (loop [n 10
         primes nil]
    (cond
     (= (count primes) 11) (apply + primes)
     (truncatable-prime? n) (recur (inc n) (conj primes n))
     :else (recur (inc n) primes))))
