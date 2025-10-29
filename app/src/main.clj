(ns main
  (:gen-class)
  (:require [lab3.future-filter :refer [future-filter]]
            [lab3.lazy-future-filter :refer [lazy-filter]]))

(defn -main []
  (println (future-filter even? (range 1000)))
  (println (lazy-filter even? (range 1000))))