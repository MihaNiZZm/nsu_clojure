(ns lab4.dnf
  (:use [lab4.exprs]
        [lab4.expr-examples]))

(defn simplify-ops
  "Приводит выражение к композиции операций b-and, b-or и b-not."
  [expr]
  (cond
    (b-const? expr) expr

    (b-var? expr) expr

    (b-not? expr)
    (b-not (simplify-ops (first (b-args expr))))

    (b-and? expr)
    (apply b-and (map simplify-ops (b-args expr)))

    (b-or? expr)
    (apply b-or (map simplify-ops (b-args expr)))

    (b-implies? expr)
    (b-or
     (b-not (simplify-ops (first (b-args expr))))
     (simplify-ops (second (b-args expr))))

    :else
    (throw (Exception. (str "Unknown operation/type: " (first expr))))))

(defn de-morganize
  "Спускает отрицания до уровня переменных и убирает двойные отрицания:
   1. ¬(x ∨ y) ~ (¬x ∧ ¬y);
   2. ¬(¬(x)) ~ x."
  [expr]
  (cond
    (b-const? expr) expr

    (b-var? expr) expr

    (b-not? expr)
    (let [inside (first (b-args expr))]
      (cond
        (b-const? inside)
        (b-const (not (b-const-val inside)))

        (b-var? inside) (b-not inside)

        (b-not? inside)
        (de-morganize (first (b-args inside)))

        (b-and? inside)
        (apply b-or (map #(de-morganize (b-not %)) (b-args inside)))

        (b-or? inside)
        (apply b-and (map #(de-morganize (b-not %)) (b-args inside)))

        :else
        (throw (Exception. (str "Unknown operation/type: " (first inside))))))

    (b-and? expr)
    (apply b-and (map de-morganize (b-args expr)))

    (b-or? expr)
    (apply b-or (map de-morganize (b-args expr)))

    :else
    (throw (Exception. (str "Unknown operation/type: " (first expr))))))

(defn simplify-by-distr-law
  "Упростить выражение, используя закон дистрибутивности:
   1. (x ∧ (y ∨ z)) ~ ((x ∧ y) ∨ (x ∧ z))."
  [expr]
  (cond
    (or (b-const? expr) (b-var? expr) (b-not? expr)) expr

    (b-or? expr)
    (apply b-or (map simplify-by-distr-law (b-args expr)))

    (b-and? expr)
    (let [[a b & rest] (b-args expr)]
      (cond
        (nil? b)
        (simplify-by-distr-law a)

        :else
        (let [left (simplify-by-distr-law a)
              right (simplify-by-distr-law (apply b-and (cons b rest)))]
          (cond
            (b-or? left)
            (apply b-or (map #(simplify-by-distr-law (b-and % right)) (b-args left)))

            (b-or? right)
            (apply b-or (map #(simplify-by-distr-law (b-and left %)) (b-args right)))

            :else
            (b-and left right)))))

    :else
    (throw (Exception. (str "Unknown operation/type: " (first expr))))))

(defn flatten-expr
  "Рекурсивно сворачивает вложенные конъюнкции или дизъюнкции:
   1. (x ∧ (w ∧ y)) ~ (x ∧ w ∧ y)."
  [expr]
  (cond
    (b-and? expr)
    (apply b-and (mapcat #(if (b-and? %) (rest %) [%]) (map flatten-expr (b-args expr))))

    (b-or? expr)
    (apply b-or (mapcat #(if (b-or?  %) (rest %) [%]) (map flatten-expr (b-args expr))))

    :else expr))

(defn negation?
  "Проверяет, что два выражения являются противоположностями друг друга:
   1. ((x ∧ w ∧ y) ∨ (x ∧ w ∧ z) ∨ (x ∧ w ∧ q)) и ¬((x ∧ w ∧ y) ∨ (x ∧ w ∧ z) ∨ (x ∧ w ∧ q)) = true;
   2. (x ∧ w ∧ (y ∨ z ∨ q)) и ¬(((x → ¬z) ∧ ((x ∨ y) ∧ (y ∨ z)))) = false."
  [e1 e2]
  (or (and (b-not? e1) (= (second e1) e2))
      (and (b-not? e2) (= (second e2) e1))))

(defn simplify-and
  "Упростить все конъюнкции:
   1. (... ∧ x ∧ ... ∧ x ∧ ...) → убрать дубликаты: x;
   2. (... ∧ x ∧ ... ∧ ¬x ∧ ...) → всегда ⊥;
   3. (... ∧ ⊥ ∧ ...) → всегда ⊥;
   4. (... ∧ ⊤ ∧ ...) → все ⊤ можно выбрасывать (но если остался только ⊤, оставить ⊤)."
  [& args]
  (let [sargs (distinct args)
        has-false? (some #(and (b-const? %) (= false (b-const-val %))) sargs)
        has-contradiction? (some (fn [a] (some #(negation? a %) sargs)) sargs)
        filtered (remove #(and (b-const? %) (= true (b-const-val %))) sargs)]
    (cond
      has-false? (b-const false)           ; если хоть один аргумент ⊥/false — ВСЁ выражение ложно

      has-contradiction? (b-const false)   ; если есть x и ¬x — тоже ложно

      (empty? filtered) (b-const true)     ; осталась одна истина — всё выражение истина

      (= 1 (count filtered)) (first filtered)

      :else (apply b-and filtered))))

(defn simplify-or
  "Упростить все дизъюнкции:
   1. (... ∨ x ∨ ... ∨ x ∨ ...) → убрать дубликаты: x;
   2. (... ∨ x ∨ ... ∨ ¬x ∨ ...) → всегда ⊤;
   3. (... ∨ ⊤ ∨ ...) → всегда ⊤;
   4. (... ∨ ⊥ ∨ ...) → все ⊥ можно выбрасывать (но если остался только ⊥, оставить ⊥)."
  [& args]
  (let [sargs (distinct args)
        has-true? (some #(and (b-const? %) (= true (b-const-val %))) sargs)
        has-contradiction? (some (fn [a] (some #(negation? a %) sargs)) sargs)
        filtered (remove #(and (b-const? %) (= false (b-const-val %))) sargs)]
    (cond
      has-true? (b-const true)             ; если хотя бы одна истина — выражение истинно

      has-contradiction? (b-const true)    ; если есть x и ¬x — тоже истинно

      (empty? filtered) (b-const false)    ; осталась одна ложь — всё выражение ложно

      (= 1 (count filtered)) (first filtered)

      :else (apply b-or filtered))))

(defn deep-simplify
  "Упростить выражение, пользуясь следующими правилами:
   1. (... ∧ x ∧ ... ∧ x ∧ ...) → убрать дубликаты: x;
   2. (... ∧ x ∧ ... ∧ ¬x ∧ ...) → всегда ⊥;
   3. (... ∧ ⊥ ∧ ...) → всегда ⊥;
   4. (... ∧ ⊤ ∧ ...) → все ⊤ можно выбрасывать (но если остался только ⊤, оставить ⊤);
   5. (... ∨ x ∨ ... ∨ x ∨ ...) → убрать дубликаты: x;
   6. (... ∨ x ∨ ... ∨ ¬x ∨ ...) → всегда ⊤;
   7. (... ∨ ⊤ ∨ ...) → всегда ⊤;
   8. (... ∨ ⊥ ∨ ...) → все ⊥ можно выбрасывать (но если остался только ⊥, оставить ⊥).
   
   При этом на месте переменных могут быть и эквивалентные сложные выражения."
  [expr]
  (cond
    (b-const? expr) expr

    (b-var? expr) expr

    (b-not? expr)
    (b-not (deep-simplify (second expr)))

    (b-and? expr)
    (apply simplify-and (map deep-simplify (b-args expr)))

    (b-or? expr)
    (apply simplify-or (map deep-simplify (b-args expr)))

    :else expr))

(defn convert-to-dnf
  "Привести выражение к его ДНФ."
  [expr]
  (->> expr
       simplify-ops
       ; Первое упрощение делается для случая, если есть конъюнкции или дизъюнкции
       ; с противоположными значениями в аргументах.
       deep-simplify
       de-morganize
       simplify-by-distr-law
       flatten-expr
       ; Второе упрощение делается для того, чтобы упростить итоговые конъюнкции и дизъюнкции в ДНФ.
       deep-simplify))
