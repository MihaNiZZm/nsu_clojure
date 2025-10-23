(ns test-utils.test-utils
  (:gen-class))

(defn approx=
  "Проверяет, что a и b равны с точностью eps. По умолчанию eps = 1e-4."
  ([a b] (approx= a b 1e-4))
  ([a b eps] (<= (Math/abs (- a b)) eps)))

(defn elapsed-ms 
  "Показывает, сколько времени заняло выполнение функции thunk в ms."
  [thunk]
  (let [t0 (System/nanoTime)]
    (thunk)
    (let [t1 (System/nanoTime)]
      (/ (- t1 t0) 1e6))))
