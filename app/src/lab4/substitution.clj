(ns lab4.substitution
  (:gen-class)
  (:require [lab4.bool-const :refer [bool-const? bool-const]]
            [lab4.bool-var :refer [bool-var? bool-var-name]]
            [lab4.operations :refer [b-not? b-not b-and? b-or? b-implies? b-implies]]))

(defn subst
  "Выполняет подстановку значения переменной var-name в символьное булево выражение expr.
   Возвращает новое выражение, где (bool-var var-name) заменены на (bool-const value)."
  [expr var-name value]
  {:pre [(bool-var? var-name)
         (bool-const? value)]}
  (cond
    (bool-const? expr) expr ; если выражение константа, то возвращаем его как есть
    (bool-var? expr) ; если выражение переменная, то проверяем, является ли она переменной var-name
    (if (= (bool-var-name expr) var-name) ; если переменная var-name, то заменяем её на bool-const value
      (bool-const value)
      expr) ; если нет, то возвращаем выражение как есть

    (b-not? expr) ; если выражение отрицание, то заменяем аргумент на результат подстановки
    (b-not (subst (second expr) var-name value))

    (or (b-and? expr) (b-or? expr)) ; если выражение конъюнкция или дизъюнкция, то заменяем аргументы на результаты подстановки
    (let [op (first expr)
          args (rest expr)]
      (cons op (map #(subst % var-name value) args)))

    (b-implies? expr) ; если выражение импликация, то заменяем аргументы на результаты подстановки
    (let [[_ a b] expr]
      (b-implies (subst a var-name value)
                 (subst b var-name value)))

    :else ; если выражение неизвестно, то выбрасываем ошибку
    (throw (ex-info (str "Неизвестный тип выражения: " expr) {:expr expr}))))
