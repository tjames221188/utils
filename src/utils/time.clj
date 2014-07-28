(ns utils.time
  (:import (org.joda.time DateTime DateTimeConstants))
  (:require [clj-time.core :as time]
            [clj-time.coerce :as c]
            [clj-time.periodic :as p]))

(defn ->DateTime
  [^java.util.Date d]
  (c/from-date d))
(defn ->Date
  [^DateTime d]
  (c/to-date d))

(defn plus-days
  [^DateTime d n]
  (time/plus d (time/days n)))

(defn time-range
  [start end step]
  "Returns a lazy sequence of DateTimes from start to end,
  incremented by 'step' units of time"
  (let [inf-range (p/periodic-seq start step)
        below-end? (fn [t] (time/within? (time/interval start end) t))]
    (take-while #(not (time/after?  % end)) inf-range)))

(defn weekends-between
  [^DateTime start ^DateTime end]
  (let [first-saturday (.withDayOfWeek start DateTimeConstants/SATURDAY)
        first-sunday (.withDayOfWeek start DateTimeConstants/SUNDAY)
        last-saturday (plus-days (.withDayOfWeek end DateTimeConstants/SATURDAY) -7)
        last-sunday (plus-days (.withDayOfWeek end DateTimeConstants/SUNDAY) -7)
        saturdays (time-range first-saturday last-saturday (time/days 7))
        sundays (time-range first-sunday last-sunday (time/days 7))]
    (interleave saturdays sundays)))

