;; A number chain is created by continuously adding the square of the
;; digits in a number to form a new number until it has been seen
;; before.

;; For example,

;; 44 → 32 → 13 → 10 → 1 → 1 85 → 89 → 145 → 42 → 20 → 4 → 16 → 37 →
;; 58 → 89

;; Therefore any chain that arrives at 1 or 89 will become stuck in an
;; endless loop. What is most amazing is that EVERY starting number
;; will eventually arrive at 1 or 89.

;; How many starting numbers below ten million will arrive at 89?

(defn next-chain-number [num]
  (apply + (map #(let [n (- (int %) 48)]
                   (* n n))
                (str num))))

;; workaround for recursive memoization
(def memoized-follow-chain nil)

(defn follow-chain [num]
  (if (or (= num 1)
          (= num 89))
    num
    (memoized-follow-chain (next-chain-number num))))

(def memoized-follow-chain (memoize follow-chain))

(defn prob92 []
  (count (filter #(= 89 (memoized-follow-chain %)) (range 1 10000000))))
