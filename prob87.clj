
(defn generate-prime-sieve [size]
  (let [next-prime (fn [p sieve]
                     ;; the next non-false val in sieve
                     ;; greater than p
                     (loop [index (inc p)]
                       (cond
                        (>= index size)     size
                        (true? (get sieve index)) index
                        :else (recur (inc index)))))

        prime-vec (vec (concat [false false true]
                               (take (- size 3)
                                     (repeat true))))]
    (loop [p 2
           sieve (transient prime-vec)]
      (let [nextp (next-prime p sieve)]
        (if (< p size)
          (recur nextp (reduce (fn [t i] (assoc! t i false))
                               sieve
                               (range (* p p) size p)))
          (persistent! sieve))))))

(def upper-bound 50000000)
(def prime-sieve (generate-prime-sieve (Math/pow upper-bound 1/2)))

(defn prime-sequence []
  (filter (comp not nil?)
          (map-indexed (fn [idx itm]
                         (if (get prime-sieve idx)
                           itm
                           nil))
                       (range (Math/pow upper-bound 1/2)))))

(def prime-square-powers (filter #(< % upper-bound)
                                 (map (fn [x] (bigint (Math/pow x 2)))
                                      (prime-sequence))))

(def prime-cube-powers (filter #(< % upper-bound)
                                 (map (fn [x] (bigint (Math/pow x 3)))
                                      (prime-sequence))))

(def prime-4th-powers (filter #(< % upper-bound)
                              (map (fn [x] (bigint (Math/pow x 4)))
                                   (prime-sequence))))

(defn get-possible-summants [num seq]
  ;; return numbers in seq which can be less than five million
  ;; when added with num
  (filter #(< (+ % num) upper-bound)
          seq))

(defn prob87 []
  (-> (mapcat (fn [a]
                (let [bs (get-possible-summants a prime-cube-powers)]
                  (mapcat (fn [b]
                            (map #(+ a b %) (get-possible-summants (+ b a) prime-4th-powers)))
                          bs)))
              prime-square-powers)
      sort
      distinct
      count))
