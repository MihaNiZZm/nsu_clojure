(ns lab3.lazy-future-filter-test
  (:require [clojure.test :refer [deftest is]]
            [lab3.lazy-future-filter :refer [lazy-filter]]
            [test-utils.test-utils :refer [elapsed-ms]]))

(defn slow-pred [x] (Thread/sleep 1) (even? x))

(def data (range 1000))

(deftest correct-filtering-test
  (let [lazy-filter-result (take 471 (lazy-filter slow-pred data))
        regular-filter-result (take 471 (filter slow-pred data))]
    (is (= (set lazy-filter-result) (set regular-filter-result)))))

(deftest performance-test
  (let [lazy-filter-time (elapsed-ms #(doall (take 471 (lazy-filter slow-pred data))))
        regular-filter-time (elapsed-ms #(doall (take 471 (filter slow-pred data))))]
    (println "lazy-filter time:" lazy-filter-time)
    (println "regular filter time:" regular-filter-time)
    (println "lazy-filter is" (/ regular-filter-time lazy-filter-time) "times faster than regular filter")
    (is (< lazy-filter-time regular-filter-time))))