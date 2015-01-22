;; If the numbers 1 to 5 are written out in words: one, two, three,
;; four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in
;; total.

;; If all the numbers from 1 to 1000 (one thousand) inclusive were
;; written out in words, how many letters would be used?

;; NOTE: Do not count spaces or hyphens. For example, 342 (three
;; hundred and forty-two) contains 23 letters and 115 (one hundred and
;; fifteen) contains 20 letters. The use of "and" when writing out
;; numbers is in compliance with British usage.

(require '[clojure.string :refer [join replace]])

(def ^:private numap
  {0 "",1 "one",2 "two",3 "three",4 "four",5 "five",
   6 "six",7 "seven",8 "eight",9 "nine",10 "ten",
   11 "eleven",12 "twelve",13 "thirteen",14 "fourteen",
   15 "fifteen",16 "sixteen",17 "seventeen",18 "eighteen",
   19 "nineteen",20 "twenty",30 "thirty",40 "forty",
   50 "fifty",60 "sixty",70 "seventy",80 "eighty",90 "ninety"})

(defn numberword [num]
  "Takes a number and return a full written string form. For example,
   23237897 will be written as \"twenty-three million two hundred and
   thirty-seven thousand eight hundred and ninety-seven\".  "

  (if (zero? num)
    ;; FIXME:
    ""

  (let [digitcnt (int (Math/log10 num))
        divisible? (fn [num div] (zero? (rem num div)))
        n-digit (fn [num n] (Character/getNumericValue (.charAt (str num) n)))]

    (cond
     ;; handle million part
     (>= digitcnt 6)    (join " " [(numberword (int (/ num 1000000)))
                                  "million"
                                  (numberword (rem num 1000000))])

     ;; handle thousand part
     (>= digitcnt 3)    (join " " [(numberword (int (/ num 1000)))
                                   "thousand"
                                   (numberword (rem num 1000))])

     ;; handle hundred part
     (>= digitcnt 2)    (if (divisible? num 100)
                          (join " " [(numap (int (/ num 100)))
                                     "hundred"])
                          (join " " [(numap (int (/ num 100)))
                                     "hundred"
                                     "and"
                                     (numberword (rem num 100))]))

     ;; handle the last two digits
     (< num 20)                 (numap num)
     (divisible? num 10)        (numap num)
     :else                      (join "-" [(numap (* 10 (n-digit num 0)))
                                           (numap (n-digit num 1))])))))


(defn prob17 []
  (apply + (for [i (range 1 1001)]
             (count (replace (numberword i) #"[-\s]" "")))))
