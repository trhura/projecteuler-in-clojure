(defn digit-vec [number]
  (vec (map #(- (int %) 48) (str number))))

(defn _factorial [number]
  (loop [num number
         res 1]
    (if (<= num 1) res
        (recur (dec num) (*' res num)))))

(def factorial (memoize _factorial))

(def upper-bound (apply + (map factorial (range 10))))

(defn curious? [number]
  (loop [digits (digit-vec number)
         sum 0]
    (cond (> sum upper-bound) false
          (empty? digits) (= sum number)
          :else (recur (rest digits) (+ sum (factorial (first digits)))))))

(defn prob34 []
  (apply + (for [num (range 3 upper-bound) :when (curious? num)]
             num)))
