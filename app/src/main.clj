(ns main
  (:require [lab4.exprs :refer [pretty-bool]]
            [lab4.dnf :refer [convert-to-dnf]])
  (:use [lab4.expr-examples]))

(defn print-expr-and-its-dnf
  "Приводит выражение к его ДНФ и выводит начальное и конечное состояния выражения."
  [expr]
  (println "Expression:" (pretty-bool expr) "\nDNF:" (pretty-bool (convert-to-dnf expr))))

(defn -main [] 
  (print-expr-and-its-dnf exp-1)
  (print-expr-and-its-dnf exp-2)
  (print-expr-and-its-dnf exp-3)
  (print-expr-and-its-dnf exp-4)
  (print-expr-and-its-dnf exp-5)
  (print-expr-and-its-dnf exp-6)
  (print-expr-and-its-dnf exp-7)
  (print-expr-and-its-dnf exp-8)
  (print-expr-and-its-dnf exp-9)
  (print-expr-and-its-dnf exp-10)
  (print-expr-and-its-dnf exp-11)
  (print-expr-and-its-dnf exp-12)
  (print-expr-and-its-dnf exp-13)
  (print-expr-and-its-dnf exp-14)
  (print-expr-and-its-dnf exp-15)
  (print-expr-and-its-dnf exp-16)
  (print-expr-and-its-dnf large-expression)
  (print-expr-and-its-dnf super-expression))

(-main)