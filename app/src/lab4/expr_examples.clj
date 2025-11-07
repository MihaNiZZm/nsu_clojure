(ns lab4.expr-examples
  (:use [lab4.exprs]))

(def exp-1
  (b-var :x))

(def exp-2
  (b-and (b-var :x) (b-var :y)))

(def exp-3
  (b-or (b-var :x) (b-var :y)))

(def exp-4
  (b-and (b-var :a) (b-or (b-var :b) (b-var :c))))

(def exp-5
  (b-or (b-not (b-var :p)) (b-and (b-var :q) (b-not (b-var :r)))))

(def exp-6
  (b-implies (b-var :x) (b-and (b-var :y) (b-var :z))))

(def exp-7
  (b-not (b-implies (b-or (b-var :x) (b-var :y)) (b-var :z))))

(def exp-8
  (b-and (b-var :x) (b-not (b-var :x))))

(def exp-9
  (b-or (b-not (b-var :x))
        (b-or (b-not (b-var :y)) (b-var :x))))

(def exp-10
  (b-or
   (b-and (b-var :x) (b-var :y))
   (b-and (b-not (b-var :x)) (b-var :z))))

(def exp-11
  (b-and (b-var :x) (b-const true)))

(def exp-12
  (b-or (b-var :y) (b-const false)))

(def exp-13
  (b-and (b-var :y) (b-const false)))

(def exp-14
  (b-or (b-var :y) (b-const true)))

(def exp-15
  (b-or exp-3 (b-const false) (b-not exp-3)))

(def exp-16
  (b-and exp-3 (b-const true) (b-not exp-3)))

(def large-expression
  (b-and
   (b-implies (b-var :a) (b-or (b-var :b) (b-var :c)))
   (b-not (b-var :c))
   (b-or (b-and (b-var :a) (b-var :d)) (b-and (b-not (b-var :d)) (b-var :e)))
   (b-const true)))

(def super-expression
  (b-or
   (b-and
    (b-implies (b-var :a) (b-or (b-var :b) (b-not (b-var :c))))
    (b-or (b-var :d) (b-and (b-var :e) (b-var :f)))
    (b-not (b-and (b-var :f) (b-var :g)))
    (b-const true))
   (b-and
    (b-var :a)
    (b-not (b-or (b-var :b) (b-var :d)))
    (b-or (b-var :c) (b-not (b-implies (b-var :e) (b-var :f))))
    (b-const false))
   (b-implies
    (b-and (b-var :f) (b-not (b-var :a)))
    (b-or (b-var :g) (b-not (b-var :d))))
   (b-not
    (b-or
     (b-var :b)
     (b-implies (b-var :e) (b-not (b-var :f)))))
   (b-and
    (b-var :g)
    (b-or (b-not (b-var :c)) (b-var :e))
    (b-const true))))
