;; In the 5 by 5 matrix below, the minimal path sum from the top left to
;; the bottom right, by only moving to the right and down, is indicated
;; in bold red and is equal to 2427.

;; [131 673 234 103 18
;;  201 96 342 965 150
;;  630 803 746 422 111
;;  537 699 497 121 956
;;  805 732 524 37 331]

;; Find the minimal path sum, in matrix.txt (right click and "Save
;; Link/Target As..."), a 31K text file containing a 80 by 80 matrix,
;; from the top left to the bottom right by only moving right and down.

(require '[clojure.string :as string])

(def test [131 673 234 103 18
        201 96 342 965 150
        630 803 746 422 111
        537 699 497 121 956
        805 732 524 37 331])

(defn size [matrix]
  ;; size of the matrix
  (int (Math/sqrt (count matrix))))

(defn get-row [matrix i]
  ;; get row of index i
  (int (/ i (size matrix))))

(defn get-col [matrix i]
  ;; get col of index i
  (rem i (size matrix)))

(defn get-right [matrix i]
  ;; get index of right item
  (let [col (get-col matrix i)]
    (if (< (inc col) (size matrix))
      (+ (inc col) (* (get-row matrix i) (size matrix)))
      -1)))

(defn get-left [matrix i]
  ;; get index of right item
  (let [col (get-col matrix i)]
    (if (> col 0)
      (+ (dec col) (* (get-row matrix i) (size matrix)))
      -1)))

(defn get-down [matrix i]
   ;; get index of item below
  (let [row (get-row matrix i)]
    (if (< (inc row) (size matrix))
      (+ (get-col matrix i) (* (inc row) (size matrix)))
      -1)))

(defn add-minimal-sums [matrix]
  (let [add-right (fn [tv i]
                    (let [right-idx (get-right tv i)
                          right-sum-itm (nth tv right-idx 0)
                          right-itm (nth matrix right-idx 0)]

                      (if (not= right-idx -1)
                        (if (or (< (+ (nth tv i) right-itm)
                                   right-sum-itm)
                                (= (get-row tv i) 0))
                          (assoc! tv
                                  right-idx
                                  (+ (nth tv i)
                                     right-itm))))))
        add-down  (fn [tv i]
                    (let [down-idx (get-down tv i)
                          down-itm (nth tv down-idx 0)]
                      (if (not= down-idx -1)
                        (assoc! tv
                                down-idx
                                (+ down-itm (nth tv i))))))]

    (loop [trm (transient matrix)
           i 0]
      (if (< i (count matrix))
        (do
          (add-right trm i)
          (add-down trm i)
          (recur trm (inc i)))
        (persistent! trm)))))

(defn prob81 []
  (let [matrix (mapcat (fn [line]
                         (map (fn [num] (Integer/parseInt num))
                              (string/split line #",")))
                       (string/split-lines (slurp "p081_matrix.txt")))]
    (last (add-minimal-sums (vec matrix)))))
