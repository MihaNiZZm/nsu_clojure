(ns lab4.bool-const
  (:gen-class))

(defn bool-const
  "Создать булеву константу (true или false)."
  [value]
  {:pre [(or (= value true) (= value false))]}
  (list ::bool-const value))

(defn bool-const?
  "Проверить, является ли выражение булевой константой."
  [expr]
  (= (first expr) ::bool-const))

(defn bool-const-value
  "Получить значение булевой константы."
  [expr]
  {:pre [(bool-const? expr)]}
  (second expr))
