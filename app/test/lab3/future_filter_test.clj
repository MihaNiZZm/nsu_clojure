(ns lab3.future-filter-test
  (:require [clojure.test :refer [deftest is]]
            [lab3.future-filter :refer [future-filter]]
            [test-utils.test-utils :refer [elapsed-ms]]))

(defn slow-pred [x] (Thread/sleep 1) (even? x))
(defn fast-pred [x] (even? x))

(def data (range 1000))

(deftest correct-filtering-test
  (let [future-filter-result (future-filter fast-pred data)
        regular-filter-result (filter fast-pred data)]
    (is (= (set future-filter-result) (set regular-filter-result)))))

(deftest performance-test
  (let [future-filter-time (elapsed-ms #(doall (future-filter slow-pred data)))
        regular-filter-time (elapsed-ms #(doall (filter slow-pred data)))]
    (println "future-filter time:" future-filter-time)
    (println "regular filter time:" regular-filter-time)
    (println "amount of blocks: 25")
    (println "future-filter is" (/ regular-filter-time future-filter-time) "times faster than regular filter")
    (is (< future-filter-time regular-filter-time))))
