(ns lab4.dnf-test
  (:require [clojure.test :refer [deftest is]])
  (:use [lab4.exprs]
        [lab4.expr-examples]
        [lab4.dnf]
        [test-utils.test-utils]))

(defn print-info
  [num expr actual-dnf expected-dnf]
  (println "Test" num
           "\nExpression:" (pretty-bool expr)
           "\nActual DNF:" (pretty-bool actual-dnf)
           "\nExpected DNF:" (pretty-bool expected-dnf)
           "\n"))

(defn print-subst-info
  [num expr subst-expr actual-dnf expected-dnf]
  (println "Test" num
           "\nExpression:" (pretty-bool expr)
           "\nExpression after substitution:" (pretty-bool subst-expr)
           "\nActual DNF:" (pretty-bool actual-dnf)
           "\nExpected DNF:" (pretty-bool expected-dnf)
           "\n"))

; Standard DNF convertion tests set
(deftest dnf-1
  (let [expression exp-1
        actual-dnf (convert-to-dnf expression)
        expected-dnf (b-var :x)]
    (print-info 1 expression actual-dnf expected-dnf)
    (is (=  actual-dnf expected-dnf))))

(deftest dnf-2
  (let [expression exp-2
        actual-dnf (convert-to-dnf expression)
        expected-dnf (b-and 
                      (b-var :x) 
                      (b-var :y))]
    (print-info 2 expression actual-dnf expected-dnf)
    (is (=  actual-dnf expected-dnf))))

(deftest dnf-3
  (let [expression exp-3
        actual-dnf (convert-to-dnf expression)
        expected-dnf (b-or
                      (b-var :x)
                      (b-var :y))]
    (print-info 3 expression actual-dnf expected-dnf)
    (is (=  actual-dnf expected-dnf))))

(deftest dnf-4
  (let [expression exp-4
        actual-dnf (convert-to-dnf expression)
        expected-dnf (b-or (b-and (b-var :a)
                                  (b-var :b))
                           (b-and (b-var :a)
                                  (b-var :c)))]
    (print-info 4 expression actual-dnf expected-dnf)
    (is (=  actual-dnf expected-dnf))))

(deftest dnf-5
  (let [expression exp-5
        actual-dnf (convert-to-dnf expression)
        expected-dnf (b-or (b-not (b-var :p))
                           (b-and (b-var :q)
                                  (b-not (b-var :r))))]
    (print-info 5 expression actual-dnf expected-dnf)
    (is (=  actual-dnf expected-dnf))))

(deftest dnf-6
  (let [expression exp-6
        actual-dnf (convert-to-dnf expression)
        expected-dnf (b-or (b-not (b-var :x))
                           (b-and (b-var :y)
                                  (b-var :z)))]
    (print-info 6 expression actual-dnf expected-dnf)
    (is (=  actual-dnf expected-dnf))))

(deftest dnf-7
  (let [expression exp-7
        actual-dnf (convert-to-dnf expression)
        expected-dnf (b-or
                      (b-and (b-var :x) (b-not (b-var :z)))
                      (b-and (b-var :y) (b-not (b-var :z))))]
    (print-info 7 expression actual-dnf expected-dnf)
    (is (= actual-dnf expected-dnf))))

(deftest dnf-8
  (let [expression exp-8
        actual-dnf (convert-to-dnf expression)
        expected-dnf (b-const false)]
    (print-info 8 expression actual-dnf expected-dnf)
    (is (= actual-dnf expected-dnf))))

(deftest dnf-9
  (let [expression exp-9
        actual-dnf (convert-to-dnf expression)
        expected-dnf (b-const true)]
    (print-info 9 expression actual-dnf expected-dnf)
    (is (= actual-dnf expected-dnf))))

(deftest dnf-10
  (let [expression exp-10
        actual-dnf (convert-to-dnf expression)
        expected-dnf (b-or
                      (b-and (b-var :x) (b-var :y))
                      (b-and (b-not (b-var :x)) (b-var :z)))]
    (print-info 10 expression actual-dnf expected-dnf)
    (is (= actual-dnf expected-dnf))))

(deftest dnf-11
  (let [expression exp-11
        actual-dnf (convert-to-dnf expression)
        expected-dnf (b-var :x)]
    (print-info 11 expression actual-dnf expected-dnf)
    (is (= actual-dnf expected-dnf))))

(deftest dnf-12
  (let [expression exp-12
        actual-dnf (convert-to-dnf expression)
        expected-dnf (b-var :y)]
    (print-info 12 expression actual-dnf expected-dnf)
    (is (= actual-dnf expected-dnf))))

(deftest dnf-13
  (let [expression exp-13
        actual-dnf (convert-to-dnf expression)
        expected-dnf (b-const false)]
    (print-info 13 expression actual-dnf expected-dnf)
    (is (= actual-dnf expected-dnf))))

(deftest dnf-14
  (let [expression exp-14
        actual-dnf (convert-to-dnf expression)
        expected-dnf (b-const true)]
    (print-info 14 expression actual-dnf expected-dnf)
    (is (= actual-dnf expected-dnf))))

(deftest dnf-15
  (let [expression exp-15
        actual-dnf (convert-to-dnf expression)
        expected-dnf (b-const true)]
    (print-info 15 expression actual-dnf expected-dnf)
    (is (= actual-dnf expected-dnf))))

(deftest dnf-16
  (let [expression exp-16
        actual-dnf (convert-to-dnf expression)
        expected-dnf (b-const false)]
    (print-info 16 expression actual-dnf expected-dnf)
    (is (= actual-dnf expected-dnf))))

; Substitution and DNF convertion tests set
(deftest single-substitution-1
  (let [expression exp-4
        subst-expr (subst (b-var :a) (b-const true) expression)
        actual-dnf (convert-to-dnf subst-expr)
        expected-dnf (b-or (b-var :b) 
                           (b-var :c))]
    (print-subst-info 17 expression subst-expr actual-dnf expected-dnf)
    (is (= actual-dnf expected-dnf))))

(deftest single-substitution-2
  (let [expression exp-4
        subst-expr (subst (b-var :a) (b-const false) expression)
        actual-dnf (convert-to-dnf subst-expr)
        expected-dnf (b-const false)]
    (print-subst-info 18 expression subst-expr actual-dnf expected-dnf)
    (is (= actual-dnf expected-dnf))))

(deftest single-substitution-3
  (let [expression exp-4
        subst-expr (subst (b-var :b) (b-const false) expression)
        actual-dnf (convert-to-dnf subst-expr)
        expected-dnf (b-and (b-var :a) 
                           (b-var :c))]
    (print-subst-info 19 expression subst-expr actual-dnf expected-dnf)
    (is (= actual-dnf expected-dnf))))

(deftest single-substitution-4
  (let [expression exp-4
        subst-expr (subst (b-var :b) (b-const true) expression)
        actual-dnf (convert-to-dnf subst-expr)
        expected-dnf (b-var :a)]
    (print-subst-info 20 expression subst-expr actual-dnf expected-dnf)
    (is (= actual-dnf expected-dnf))))

(deftest double-substitution-1
  (let [expression exp-4
        subst-expr (->> expression
                        (subst (b-var :a) (b-const true))
                        (subst (b-var :b) (b-const false)))
        actual-dnf (convert-to-dnf subst-expr)
        expected-dnf (b-var :c)]
    (print-subst-info 21 expression subst-expr actual-dnf expected-dnf)
    (is (= actual-dnf expected-dnf))))

(deftest double-substitution-2
  (let [expression exp-4
        subst-expr (->> expression
                        (subst (b-var :a) (b-const true))
                        (subst (b-var :b) (b-const true)))
        actual-dnf (convert-to-dnf subst-expr)
        expected-dnf (b-const true)]
    (print-subst-info 22 expression subst-expr actual-dnf expected-dnf)
    (is (= actual-dnf expected-dnf))))

(deftest double-substitution-3
  (let [expression exp-4
        subst-expr (->> expression
                        (subst (b-var :a) (b-const false))
                        (subst (b-var :b) (b-const true)))
        actual-dnf (convert-to-dnf subst-expr)
        expected-dnf (b-const false)]
    (print-subst-info 23 expression subst-expr actual-dnf expected-dnf)
    (is (= actual-dnf expected-dnf))))

(deftest double-substitution-4
  (let [expression exp-4
        subst-expr (->> expression 
                        (subst (b-var :b) (b-const true))
                        (subst (b-var :c) (b-const false)))
        actual-dnf (convert-to-dnf subst-expr)
        expected-dnf (b-var :a)]
    (print-subst-info 24 expression subst-expr actual-dnf expected-dnf)
    (is (= actual-dnf expected-dnf))))

(deftest double-substitution-5
  (let [expression exp-4
        subst-expr (->> expression
                        (subst (b-var :b) (b-const false))
                        (subst (b-var :c) (b-const false)))
        actual-dnf (convert-to-dnf subst-expr)
        expected-dnf (b-const false)]
    (print-subst-info 25 expression subst-expr actual-dnf expected-dnf)
    (is (= actual-dnf expected-dnf))))

(deftest triple-substitution-1
  (let [expression exp-5
        subst-expr (->> expression
                        (subst (b-var :p) (b-const false))
                        (subst (b-var :q) (b-const false))
                        (subst (b-var :r) (b-const false)))
        actual-dnf (convert-to-dnf subst-expr)
        expected-dnf (b-const true)]
    (print-subst-info 26 expression subst-expr actual-dnf expected-dnf)
    (is (= actual-dnf expected-dnf))))

(deftest triple-substitution-2
  (let [expression exp-5
        subst-expr (->> expression
                        (subst (b-var :p) (b-const true))
                        (subst (b-var :q) (b-const false))
                        (subst (b-var :r) (b-const false)))
        actual-dnf (convert-to-dnf subst-expr)
        expected-dnf (b-const false)]
    (print-subst-info 27 expression subst-expr actual-dnf expected-dnf)
    (is (= actual-dnf expected-dnf))))

(deftest triple-substitution-3
  (let [expression exp-5
        subst-expr (->> expression
                        (subst (b-var :p) (b-const true))
                        (subst (b-var :q) (b-const true))
                        (subst (b-var :r) (b-const false)))
        actual-dnf (convert-to-dnf subst-expr)
        expected-dnf (b-const true)]
    (print-subst-info 28 expression subst-expr actual-dnf expected-dnf)
    (is (= actual-dnf expected-dnf))))

(deftest triple-substitution-4
  (let [expression exp-5
        subst-expr (->> expression
                        (subst (b-var :p) (b-const true))
                        (subst (b-var :q) (b-const true))
                        (subst (b-var :r) (b-const true)))
        actual-dnf (convert-to-dnf subst-expr)
        expected-dnf (b-const false)]
    (print-subst-info 29 expression subst-expr actual-dnf expected-dnf)
    (is (= actual-dnf expected-dnf))))

; Hard tests set
(deftest large-test-1
  (let [expression large-expression
        actual-dnf (convert-to-dnf expression)
        expected-dnf (b-or (b-and (b-not (b-var :a))
                                  (b-not (b-var :c))
                                  (b-not (b-var :d))
                                  (b-var :e))
                           (b-and (b-var :b)
                                  (b-not (b-var :c))
                                  (b-var :a)
                                  (b-var :d))
                           (b-and (b-var :b)
                                  (b-not (b-var :c))
                                  (b-not (b-var :d))
                                  (b-var :e)))]
    (print-info 30 expression actual-dnf expected-dnf)
    (is (= actual-dnf expected-dnf))))

(deftest large-test-2
  (let [expression large-expression
        subst-expr (->> expression
                        (subst (b-var :a) (b-const true))
                        (subst (b-var :b) (b-const true))
                        (subst (b-var :c) (b-const false))
                        (subst (b-var :d) (b-const true)))
        actual-dnf (convert-to-dnf subst-expr)
        expected-dnf (b-const true)]
    (print-subst-info 31 expression subst-expr actual-dnf expected-dnf)
    (is (= actual-dnf expected-dnf))))

(deftest large-test-3
  (let [expression super-expression
        subst-expr (->> expression
                        (subst (b-var :a) (b-const true))
                        (subst (b-var :b) (b-const false))
                        (subst (b-var :c) (b-const false))
                        (subst (b-var :d) (b-const true))
                        (subst (b-var :e) (b-const false))
                        (subst (b-var :f) (b-const true)))
        actual-dnf (convert-to-dnf subst-expr)
        expected-dnf (b-const true)]
    (print-subst-info 32 expression subst-expr actual-dnf expected-dnf)
    (is (= actual-dnf expected-dnf))))
