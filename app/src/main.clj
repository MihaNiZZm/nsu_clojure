(ns main
  (:gen-class)
  (:require [lab2.integral :as simple]
            [lab2.integral-memo :as memo]
            [lab2.lazy-seq-integral :as lazy-seq]))

(defn -main []
  (println ((simple/integral (fn [x] x)) 10))
  (println ((memo/mem-integral (fn [x] x)) 10))
  (println ((lazy-seq/integral (fn [x] x)) 10)))