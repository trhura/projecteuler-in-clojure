(defn digit-vector [number]
  ;; return a vector of digits in number
  (vec (map #(- (int %) 48) (str number))))

(defn increasing-number? [num]
  (let [snum (digit-vector num)]
    (every? true? (map <= snum (next snum)))))

(defn decreasing-number? [num]
  (let [snum (digit-vector num)]
    (every? true? (map >= snum (next snum)))))

(defn bouncy-number? [num]
  (and (not (increasing-number? num))
       (not (decreasing-number? num))))

(defn prob112 []
  (loop [i 1
         bouncy-cnt 0]
    (let [bouncy-cnt (if (bouncy-number? i)
                       (inc bouncy-cnt)
                       bouncy-cnt)]
    (if (== (/ bouncy-cnt i) 0.99)
      i
      (recur (inc i) bouncy-cnt)))))
