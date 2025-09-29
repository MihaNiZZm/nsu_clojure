(ns main
  (:gen-class)
  (:require [lab1.first]
            [lab1.second]
            [lab1.fourth]))

(defn сложи
  [x y]
  (+ x y))

(defn фильтруй-базар
  [фильтр коллекция]
  (filter фильтр коллекция))

(defn замапай
  [функция коллекция]
  (map функция коллекция))

(defn редюсни
  [функция коллекция]
  (reduce функция коллекция))

(defn делится-на-два
  [x]
  (= (mod x 2) 0))

(defn отфильтруй-и-сложи
  [коллекция]
  (редюсни сложи (фильтруй-базар делится-на-два коллекция)))

(defn -main
  []
  (println (отфильтруй-и-сложи (range 100)))
  (let [alphabet '("а" "б" "о")
        len 5]
    (println (lab1.first/build-words len alphabet))
    (println (lab1.second/build-words len alphabet))
    (println (lab1.fourth/build-words len alphabet))))