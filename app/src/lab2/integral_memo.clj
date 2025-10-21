(ns lab2.integral-memo
  (:gen-class)
  (:require test-utils.test-utils))

(def h 1/10)

(defn trapezoid-area [f x0 x1]
  (double (* (- x1 x0) (/ (+ (f x0) (f x1)) 2))))

(defn recursive-integral-from-end [f x0 x1 self]
  (if (>= x0 x1)
    0
    (+ (self f x0 (- x1 h) self) (trapezoid-area f (- x1 h) x1))))

(defn recursive-integral-from-start [f x0 x1 self]
  (if (>= x0 x1)
    0
    (+ (self f (+ x0 h) x1 self) (trapezoid-area f x0 (+ x0 h)))))

(def memoized-recursive-integral-from-end (memoize recursive-integral-from-end))
(def memoized-recursive-integral-from-start (memoize recursive-integral-from-start))

(defn mem-integral [f]
  (fn [x]
    (if (neg? x)
      (memoized-recursive-integral-from-start f x 0 memoized-recursive-integral-from-start)
      (memoized-recursive-integral-from-end f 0 x memoized-recursive-integral-from-end))))

((mem-integral (fn [x] x)) 5)

(defn test-memoization-saves-time-on-bigger-close-argument []
  (let [integral (mem-integral (fn [x] x))
        time1 (test-utils.test-utils/elapsed-ms #(integral 5))
        time2 (test-utils.test-utils/elapsed-ms #(integral 6))]
    (if (> time1 time2)
      (println "yes")
      (println "no"))))

(test-memoization-saves-time-on-bigger-close-argument)