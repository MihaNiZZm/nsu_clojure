(ns lab2.integral-memo-test
  (:require [clojure.test :refer [deftest is]]
            [lab2.integral-memo :as memo]
            [test-utils.test-utils :refer [approx= elapsed-ms]]))

(def f-x (fn [x] x))
(def f-x2 (fn [x] (* x x)))
(def f-sin (fn [x] (Math/sin x)))

(deftest test-integrate-linear
  (let [actual ((memo/mem-integral f-x) 1)
        expected 0.5]
    (is (approx= actual expected 1e-2))))

(deftest test-integrate-linear-negative
  (let [actual ((memo/mem-integral f-x) -1)
        expected -0.5]
    (is (approx= actual expected 1e-2))))

(deftest test-integrate-quadratic
  (let [actual ((memo/mem-integral f-x2) 3)
        expected 9.0]
    (is (approx= actual expected 1e-2))))

(deftest test-integrate-sin
  (let [actual ((memo/mem-integral f-sin) Math/PI)
        expected 2.0]
    (is (approx= actual expected 1e-2))))

(deftest test-memoization-saves-time-on-bigger-close-argument
  (let [integral (memo/mem-integral (fn [x] x))
        time1 (elapsed-ms #(integral 5))
        time2 (elapsed-ms #(integral 6))]
    (is (> time1 time2))))

(deftest test-memoization-saves-time-on-the-same-argument
  (let [integral (memo/mem-integral (fn [x] x))
        time1 (elapsed-ms #(integral 7))
        time2 (elapsed-ms #(integral 7))]
    (is (> time1 time2))))

(deftest test-memoization-saves-time-on-smaller-close-argument
  (let [integral (memo/mem-integral (fn [x] x))
        time1 (elapsed-ms #(integral 9))
        time2 (elapsed-ms #(integral 8))]
    (is (> time1 time2))))
