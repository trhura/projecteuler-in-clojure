
(defn curious-fraction? [n d]
  (let [n1 (int (/ n 10))
        n2 (rem n 10)
        d1 (int (/ d 10))
        d2 (rem d 10)]
    (if (and (= n2 d1)
             (not (zero? d2))
             (= (/ n1 d2) (/ n d)))
      true
      false)))

(defn prob33 []
  (apply * (map #(apply / %)
                (for [d (range 11 100)
                      n (range 10 d)
                      :when (curious-fraction? n d)]
                  [n d]))))
