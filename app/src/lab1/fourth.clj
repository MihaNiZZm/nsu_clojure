(ns lab1.fourth
  (:gen-class)
  (:require [lab1.third :as utils]))

(defn get-valid-symbols
  [word alphabet]
  (utils/my-filter (fn [x] (not= x (str (last word)))) alphabet))

(defn append-word
  [word symbols]
  (utils/my-map (fn [x] (str word x)) symbols))

(defn expand-words
  [words alphabet]
  (reduce concat
          (utils/my-map (fn [word]
                 (append-word word
                              (get-valid-symbols word alphabet)))
               words)))

(defn build-words
  [len alphabet]
  (if (= len 0)
    '("")
    (let [shorter-words (build-words (dec len) alphabet)]
      (expand-words shorter-words alphabet))))
