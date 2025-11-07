(ns lab4.exprs
  (:require [clojure.string :refer [join]]))

; Boolean constants API
(defn b-const
  "Создать булеву константу (true или false)."
  [value]
  {:pre [(or (= value true) (= value false))]}
  (list ::bool-const value))

(defn b-const?
  "Проверить, является ли выражение булевой константой."
  [expr]
  (= (first expr) ::bool-const))

(defn b-const-val
  "Получить значение булевой константы."
  [expr]
  {:pre [(b-const? expr)]}
  (second expr))

; Boolean variables API
(defn b-var
  "Создать переменную (например, :p)."
  [name]
  {:pre [(keyword? name)]}
  (list ::bool-var name))

(defn b-var?
  "Проверить, является ли выражение булевой переменной."
  [expr]
  (= (first expr) ::bool-var))

(defn b-var-name
  "Получить имя булевой переменной."
  [expr]
  {:pre [(b-var? expr)]}
  (second expr))

(defn b-same-vars?
  "Проверить, являются ли две булевы переменные одной и той же переменной."
  [v1 v2]
  {:pre [(b-var? v1) (b-var? v2)]}
  (and (b-var? v1)
       (b-var? v2)
       (= (b-var-name v1)
          (b-var-name v2))))

; Boolean operations API
(defn b-and
  "Конъюнкция (and) для произвольного числа аргументов."
  [& args]
  (list* ::and args))

(defn b-and?
  "Проверить, является ли выражение конъюнкцией."
  [expr]
  (= (first expr) ::and))

(defn b-or
  "Дизъюнкция (or) для произвольного числа аргументов."
  [& args]
  (list* ::or args))

(defn b-or?
  "Проверить, является ли выражение дизъюнкцией."
  [expr]
  (= (first expr) ::or))

(defn b-not
  "Отрицание (not)."
  [arg]
  (list ::not arg))

(defn b-not?
  "Проверить, является ли выражение отрицанием."
  [expr]
  (= (first expr) ::not))

(defn b-implies
  "Импликация: a -> b"
  [a b]
  (list ::implies a b))

(defn b-implies?
  "Проверить, является ли выражение импликацией."
  [expr]
  (= (first expr) ::implies))

; Utility functions API
(defn subst
  "Выполняет подстановку значения переменной var-name в символьное булево выражение expr.
   Возвращает новое выражение, где (bool-var var-name) заменены на (bool-const value)."
  [var-name value expr]
  {:pre [(b-var? var-name)
         (b-const? value)]}
  (cond
    (b-const? expr) expr

    (b-var? expr)
    (if (= expr var-name) 
      value
      expr)

    (b-not? expr)
    (b-not (subst var-name value (second expr)))

    (or (b-and? expr) (b-or? expr))
    (let [op (first expr)
          args (rest expr)]
      (cons op (map #(subst var-name value %) args)))

    (b-implies? expr)
    (let [[_ a b] expr]
      (b-implies (subst var-name value a)
                 (subst var-name value b)))

    :else
    (throw (Exception. (str "Unknown operation/type: " (first expr))))))

(defn b-args [expr]
  (rest expr))

(defn pretty-bool
  "Преобразует символьное булево выражение в строку с красивым форматированием."
  [expr]
  (cond
    (b-const? expr)
    (if (b-const-val expr) "⊤" "⊥")

    (b-var? expr)
    (name (b-var-name expr))

    (b-not? expr)
    (str "¬" (let [x (second expr)]
               (if (or (b-and? x) (b-or? x) (b-implies? x))
                 (let [inner-expr (b-args x)]
                   (if (some #(or (b-and? %) (b-or? %) (b-implies? %)) inner-expr)
                     (str "(" (pretty-bool x) ")")
                     (pretty-bool x)))
                 (pretty-bool x))))

    (b-and? expr)
    (str "(" (join " ∧ " (map pretty-bool (b-args expr))) ")")

    (b-or? expr)
    (str "(" (join " ∨ " (map pretty-bool (b-args expr))) ")")

    (b-implies? expr)
    (let [[_ l r] expr]
      (str "(" (pretty-bool l) " → " (pretty-bool r) ")"))

    :else
    (str expr)))
