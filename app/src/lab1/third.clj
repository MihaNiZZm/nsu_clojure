(ns lab1.third
  (:gen-class))

(defn my-map
  [f coll]
  (reduce (fn [acc x] (conj acc (f x))) [] coll))

(defn my-filter [predicate coll]
  (reverse
   (reduce (fn [acc x]
             (if (predicate x)
               (cons x acc)
               acc))
           '()
           coll)))
