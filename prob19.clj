;; You are given the following information, but you may prefer to do
;; some research for yourself.

;;     1 Jan 1900 was a Monday.  Thirty days has September, April,
;;     June and November.  All the rest have thirty-one, Saving
;;     February alone, Which has twenty-eight, rain or shine.  And on
;;     leap years, twenty-nine.  A leap year occurs on any year evenly
;;     divisible by 4, but not on a century unless it is divisible by
;;     400.

;; How many Sundays fell on the first of the month during the
;; twentieth century (1 Jan 1901 to 31 Dec 2000)?

(defn prob19 []
  (loop [startday 1
         ttlcount 0
         year 1901]
    (if (> year 2000)
      ttlcount
      (recur (find-starting-weekday-next-year year startday)
             (+ ttlcount (count-sundays-in-first-of-months year startday))
             (inc year)))))

(defn count-sundays-in-first-of-months [year startday]
  (loop [count 0
         month 1
         startday startday]
    (if (> month 12)
      count
      (if (= startday 6)
        (recur (inc count) (inc month) (find-starting-weekday-next-month year month startday))
        (recur count (inc month) (find-starting-weekday-next-month year month startday))))))

(defn days-in-month [year month]
  (let [days-by-month {
                       1 31,2 28,3 31,4 30,
                       5 31,6 30,7 31,8 31,
                       9 30,10 31,11 30,12 31,
                       }]
    (if (and (= month 2) (is-leap-year? year))
      29
      (days-by-month month))))

(defn days-in-year [year]
  (if (is-leap-year? year)
    366
    365))

(defn find-starting-weekday-next-year [year weekday]
  (rem (+ weekday (rem (days-in-year year) 7)) 7))

(defn find-starting-weekday-next-month [year month weekday]
  (rem (+ weekday (rem (days-in-month year month) 7)) 7))

(defn is-leap-year? [year]
  (if (zero? (rem year 4))
    (if (and (zero? (rem year 100)) (not (zero? (rem year 400))))
      false
      true)
    false))
