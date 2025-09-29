(ns main
  (:gen-class)
  (:require [lab1.first]
            [lab1.second]
            [lab1.fourth]))

(defn -main
  []
  (let [alphabet '("а" "б" "о")
        len 5]
    (println (lab1.first/build-words len alphabet))
    (println (lab1.second/build-words len alphabet))
    (println (lab1.fourth/build-words len alphabet))))