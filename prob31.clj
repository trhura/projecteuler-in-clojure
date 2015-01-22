(defn make-amount-from-coins [amount coins]
  ;; much much faster when the coins are sorted in decreasing order
  (let [current-coin (first coins)
        remaining-coins (rest coins)
        possible-left-amounts (for [count (range (/ amount current-coin))]
                                       (- amount (* count current-coin)))
        can-get-amount-from-curcoin (if (zero? (rem amount current-coin)) 1 0)]

    (if (empty? remaining-coins)
      can-get-amount-from-curcoin
      (apply + can-get-amount-from-curcoin
             (map #(make-amount-from-coins % remaining-coins)
                                   possible-left-amounts)))))

(defn prob31 []
  (make-amount-from-coins 200 (reverse [1 2 5 10 20 50 100 200])))
