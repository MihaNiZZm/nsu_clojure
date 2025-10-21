(ns lab2.integral-test
  (:require [clojure.test :refer [deftest is]]
            [lab2.integral :as simple]
            [test-utils.test-utils :refer [approx=]]))

(def f-x (fn [x] x))
(def f-x2 (fn [x] (* x x)))
(def f-sin (fn [x] (Math/sin x)))

(deftest test-integrate-linear
  (let [actual ((simple/integral f-x) 1)
        expected 0.5]
    (is (approx= actual expected))))

(deftest test-integrate-linear-negative
  (let [actual ((simple/integral f-x) -1)
        expected -0.5]
    (is (approx= actual expected))))

(deftest test-integrate-quadratic
  (let [actual ((simple/integral f-x2) 3)
        expected 9.0]
    (is (approx= actual expected))))

(deftest test-integrate-sin
  (let [actual ((simple/integral f-sin) Math/PI)
        expected 2.0]
    (is (approx= actual expected))))
