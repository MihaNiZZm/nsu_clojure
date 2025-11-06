(ns lab4.bool-var
  (:gen-class))

(defn bool-var
  "Создать переменную (например, :p)"
  [name]
  {:pre [(keyword? name)]}
  (list ::bool-var name))

(defn bool-var? 
  "Проверить, является ли выражение булевой переменной."
  [expr]
  (= (first expr) ::bool-var))

(defn bool-var-name 
  "Получить имя булевой переменной."
  [expr]
  {:pre [(bool-var? expr)]}
  (second expr))

(defn same-bool-vars? 
  "Проверить, являются ли две булевы переменные одной и той же переменной."
  [v1 v2]
  {:pre [(bool-var? v1) (bool-var? v2)]}
  (and (bool-var? v1) 
       (bool-var? v2)
       (= (bool-var-name v1) 
          (bool-var-name v2))))
