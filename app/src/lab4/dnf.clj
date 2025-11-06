(ns lab4.dnf
  (:gen-class)
  (:require [lab4.bool-const :refer [bool-const? bool-const bool-const-value]]
            [lab4.bool-var :refer [bool-var?]]
            [lab4.operations :refer [b-not? b-not b-and? b-and b-or? b-or b-implies?]]
            [lab4.substitution :refer [subst]]))

(defn b-args [expr]
  {:pre [(list? expr)]}
  (rest expr))

(defn push-negations
  "Применяет законы де Моргана рекурсивно, чтобы отрицания были только у переменных/констант."
  [expr]
  (cond
    (bool-const? expr) expr
    (bool-var? expr) expr

    (b-not? expr)
    (let [inside (second expr)]
      (cond
        (bool-const? inside)
        (bool-const (not (bool-const-value inside)))

        (bool-var? inside) expr

        (b-not? inside)   ; not not X = X
        (push-negations (second inside))

        (b-and? inside)   ; not (a and b) = (not a) or (not b)
        (apply b-or (map #(push-negations (b-not %)) (b-args inside)))

        (b-or? inside)    ; not (a or b) = (not a) and (not b)
        (apply b-and (map #(push-negations (b-not %)) (b-args inside)))

        (b-implies? inside) ; not (a => b) = a and not b
        (let [[_ a b] inside]
          (push-negations (b-and a (b-not b))))

        :else (throw (Exception. (str "push-negations: неизвестное подвыражение: " inside)))))

    (b-and? expr) (apply b-and (map push-negations (b-args expr)))
    (b-or?  expr) (apply b-or (map push-negations (b-args expr)))
    (b-implies? expr)
    ;; a => b === not a or b
    (let [[_ a b] expr]
      (push-negations (b-or (b-not a) b)))
    :else (throw (Exception. (str "push-negations: неизвестный expr: " expr)))))
