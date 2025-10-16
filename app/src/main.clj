(ns main
  (:gen-class)
  (:require [lab1.first]
            [lab1.second]
            [lab1.fourth]
            [lab2.integral]))

(defn -main
  []
  (lab2.integral/check-result))
