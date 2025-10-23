(ns lab2.lazy-seq-integral-test
  (:require [clojure.test :refer [deftest is]]
            [lab2.lazy-seq-integral :as lazy-seq]
            [test-utils.test-utils :refer [approx=]]))

(def f-x (fn [x] x))
(def f-x2 (fn [x] (* x x)))
(def f-sin (fn [x] (Math/sin x)))

(deftest test-integrate-linear
  (let [actual ((lazy-seq/integral f-x) 1)
        expected 0.5]
    (is (approx= actual expected))))

(deftest test-integrate-linear-negative
  (let [actual ((lazy-seq/integral f-x) -1)
        expected -0.5]
    (is (approx= actual expected))))

(deftest test-integrate-quadratic
  (let [actual ((lazy-seq/integral f-x2) 3)
        expected 9.0]
    (is (approx= actual expected))))

(deftest test-integrate-sin
  (let [actual ((lazy-seq/integral f-sin) Math/PI)
        expected 2.0]
    (is (approx= actual expected))))

(deftest test-integrate-linear-on-a-big-argument
  (let [actual ((lazy-seq/integral f-x) 10000)
        expected 50000000.0]
    (is (approx= actual expected))))
