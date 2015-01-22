(require '[clojure.string :as string])

(defn alphabetical-value [name]
  (apply + (map #(-> % int (- 64)) name)))

(defn read-names-from-file [filename]
  (map #(subs % 1 (dec (count %))) (string/split (slurp filename) #",")))

(defn prob22 []
  (apply + (->> "p022_names.txt"
                read-names-from-file
                sort
                (map alphabetical-value)
                (map-indexed vector)
                (map (fn [[pos value]] (* (inc pos) value))))))
