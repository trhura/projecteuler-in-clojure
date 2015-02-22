(require '[clojure.string :as string])

(def all-values [\1 \2 \3 \4 \5 \6 \7 \8 \9 \T \J \Q \K \A])
(def value first)
(def suit second)

(defn compare-cards [card1 card2]
  ;; comparator to compare card1 and card2
  (let [val1 (value card1)
        val2 (value card2)]
    (compare (.indexOf all-values val1)
             (.indexOf all-values val2))))

(defn royal-flush? [cards]
  ;; Ten, Jack, Queen, King, Ace, in same suit.
  (let [cards (sort compare-cards cards)]
    (= [\T \J \Q \K \A] (map value cards))))

(defn four-of-a-kind? [cards]
  ;; Four cards of the same value.
  (let [cards (sort compare-cards cards)]
    (not (nil? (some #{4} (vals (frequencies (map value cards))))))))

(defn flush? [cards]
  ;; Flush: All cards of the same suit.
  (let [cards (sort compare-cards cards)]
    (apply = (map suit cards))))

(defn straight? [cards]
  ;; Straight: All cards are consecutive values.
  (let [cards (sort compare-cards cards)]
    (not= -1 (java.util.Collections/indexOfSubList all-values
                                                   (map value cards)))))
(defn straight-flush? [cards]
  ;; All cards are consecutive values of same suit.
  (let [cards (sort compare-cards cards)]
    (and (straight? cards) (flush? cards))))

(defn three-of-a-kind? [cards]
  ;; Three of a Kind: Three cards of the same value.
  (let [cards (sort compare-cards cards)]
    (not (nil? (some #{3} (vals (frequencies (map value cards))))))))

(defn pairs? [cards]
  ;; One Pair: Two cards of the same value.
  (let [cards (sort compare-cards cards)]
    (not (nil? (some #{2} (vals (frequencies (map value cards))))))))

(defn full-house? [cards]
  ;; Full House: Three of a kind and a pair.
  (let [cards (sort compare-cards cards)]
    (and (three-of-a-kind? cards) (pairs? cards))))

(defn count-times [coll n]
  ;; count how many times n appear in coll
  (get (frequencies coll) n 0))

(defn two-pairs? [cards]
  ;; Two Pairs: Two different pairs.
  (let [cards (sort compare-cards cards)]
    (= 2 (count-times (vals (frequencies (map value cards))) 2))))

(defn sort-by-pairs [cards]
  ;; sort cards by decreasing order, also taking
  ;; consideration of pairs
  (let [cards (sort compare-cards cards)
        freq  (frequencies (map value cards))]
    ;;(println cards freq (map #(get freq (first %)) cards ))
    (reverse (sort-by #(get freq (first %)) cards))))

(defn first-has-higher-cards? [cards1 cards2]
  ;; true if cards1 is higher than cards2
  (loop [cards1 (sort-by-pairs cards1)
         cards2 (sort-by-pairs cards2)]
    (if (empty? cards1)
      (throw (Exception. "No tie please"))
      (cond
       (= 1  (compare-cards (first cards1) (first cards2))) true
       (= -1 (compare-cards (first cards1) (first cards2))) false
       :else (recur (next cards1) (next cards2))))))

(defn first-win? [cards1 cards2]
  ;; return true if cards1 win
  ;; otherwise false
  (cond

   (royal-flush? cards1)        (if (royal-flush? cards2)
                                  (throw (Exception. "No tie please"))
                                  true)
   (royal-flush? cards2)        false

   (straight-flush? cards1)     (if (straight-flush? cards2)
                                  (first-has-higher-cards? cards1 cards2)
                                  true)
   (straight-flush? cards2)     false

   (four-of-a-kind? cards1)     (if (four-of-a-kind? cards2)
                                  (first-has-higher-cards? cards1 cards2)
                                  true)
   (four-of-a-kind? cards2)     false

   (full-house? cards1)     (if (full-house? cards2)
                              (first-has-higher-cards? cards1 cards2)
                              true)
   (full-house? cards2)     false

   (flush? cards1)     (if (flush? cards2)
                         (first-has-higher-cards? cards1 cards2)
                         true)
   (flush? cards2)     false

   (straight? cards1)     (if (straight? cards2)
                            (first-has-higher-cards? cards1 cards2)
                            true)
   (straight? cards2)     false

   (three-of-a-kind? cards1)     (if (three-of-a-kind? cards2)
                                   (first-has-higher-cards? cards1 cards2)
                                   true)
   (three-of-a-kind? cards2)     false

   (two-pairs? cards1)     (if (two-pairs? cards2)
                             (first-has-higher-cards? cards1 cards2)
                             true)
   (two-pairs? cards2)     false

   (pairs? cards1)     (if (pairs? cards2)
                         (first-has-higher-cards? cards1 cards2)
                         true)
   (pairs? cards2)     false

   :else        (first-has-higher-cards? cards1 cards2)))

(defn prob54 []
  (let [parse-cards (fn [hand]
                      (let [cards (vec (string/split hand #" "))]
                        ;;(println cards)
                        [(subvec cards 0 5)
                         (subvec cards 5)]))

        hands (string/split (slurp "p054_poker.txt") #"\n")]
    (count (filter true? (map #(apply first-win? %) (map parse-cards hands))))))
