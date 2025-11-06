(ns lab4.operations
  (:gen-class))

  (defn b-and
    "Конъюнкция (and) для произвольного числа аргументов."
    [& args]
    (list* ::and args))

  (defn b-and?
    "Проверить, является ли выражение конъюнкцией."
    [expr]
    {:pre [(list? expr)]}
    (= (first expr) ::and))

  (defn b-or
    "Дизъюнкция (or) для произвольного числа аргументов."
    [& args]
    (list* ::or args))

  (defn b-or?
    "Проверить, является ли выражение дизъюнкцией."
    [expr]
    {:pre [(list? expr)]}
    (= (first expr) ::or))

  (defn b-not
    "Отрицание (not)."
    [arg]
    (list ::not arg))

  (defn b-not?
    "Проверить, является ли выражение отрицанием."
    [expr]
    {:pre [(list? expr)]}
    (= (first expr) ::not))

  (defn b-implies
    "Импликация: a => b"
    [a b]
    (list ::implies a b))

  (defn b-implies?
    "Проверить, является ли выражение импликацией."
    [expr]
    {:pre [(list? expr)]}
    (= (first expr) ::implies))
  