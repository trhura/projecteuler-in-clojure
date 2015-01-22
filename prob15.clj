;; Starting in the top left corner of a 2×2 grid, and only being able
;; to move to the right and down, there are exactly 6 routes to the
;; bottom right corner.

;; How many such routes are there through a 20×20 grid?

(defn sum-previous-items [coll]
  ;; set each element of coll to summation of all previous elments
  ;; for example, [1 4 10 20] => [(1) (1+4) (1+4+10) (1+4+10+20)]
  (loop [pos 0
         sum 0
         vct []]
    (if (not (< pos (count coll)))
      vct
      (let [nitm (nth coll pos)
            nsum (+' sum nitm)]
        (recur (inc pos) nsum (conj vct nsum))))))

(defn count-lattice-paths [size]
  ;; step through each diagonal using pascal triangle
  ;; http://www.robertdickau.com/lattices.html
  (loop [ndiagonal 1
         previous-paths [1]]

    (let [total-paths (*' 2 (apply +' previous-paths))]
      (if (= ndiagonal size)
        total-paths
        (recur (inc ndiagonal) (conj (sum-previous-items previous-paths)
                                     total-paths))))))

(defn factorial [number]
  (loop [num number
         res 1]
    (if (<= num 1) res
        (recur (dec num) (*' res num)))))

(defn count-lattice-paths2 [size]
  ;; implementation based on the explanation below
  ;; http://copingwithcomputers.com/2013/07/06/lattice-paths/
  (let [n (* size 2)
        k (/ n 2)]
  (/ (factorial n) (* (factorial k) (factorial (- n k))))))
