;; By counting carefully it can be seen that a rectangular grid
;;measuring 3 by 2 contains eighteen rectangles:

;;Although there exists no rectangular grid that contains exactly two
;;million rectangles,find the area of the grid with the nearest
;;solution.

(defn count-rectangles [m n]
  ;; how many rectangles are there in a grid of mxn
  (* m (inc m) n (inc n) 1/4))

(defn diff-to-num [n num]
  ;; calculate how far n is from num
  ;; for example, 5 is 2 far away from 7 and so is 9
  (let [abs (fn [n] (max n (- n)))]
    (abs (- num n))))

(defn prob85 []
  (let [[_ [w h]] (apply min-key
                         first
                         (for [w (range 1 100)
                               h (range 1 w)
                               :let [rc (count-rectangles w h)]]
                           [(diff-to-num rc 2000000)
                            [w h]]))]
    (* w h)))
