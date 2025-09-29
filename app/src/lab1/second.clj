(ns lab1.second
  (:gen-class))

(defn get-valid-symbols
  [word alphabet result]
  (let [banned-symbol (str (first word))]
    (if (= (count alphabet) 0)
      result
      (let [current (first alphabet)
            rest-alphabet (rest alphabet)]
        (if (= current banned-symbol)
          (recur word rest-alphabet result)
          (recur word rest-alphabet (conj result current)))))))

(defn append-word
  [word alphabet result]
  (let [valid-symbols (get-valid-symbols word alphabet '())]
    (if (= (count valid-symbols) 0)
      result
      (let [cur-symbol (first valid-symbols)
            rest-symbols (rest valid-symbols)]
        (recur word rest-symbols (conj result (str cur-symbol word)))))))

(defn expand-words
  [words alphabet result]
  (if (= (count words) 0) 
    result
    (let [word (first words)
          rest-words (rest words)
          valid-next-symbols (get-valid-symbols word alphabet '())
          new-words (append-word word valid-next-symbols '())]
      (recur rest-words alphabet (concat result new-words)))))

(defn build-words
  ([len alphabet]
   (build-words len alphabet '("")))
  ([len alphabet words]
   (if (= 0 len)
     words
     (recur (dec len)
            alphabet
            (expand-words words alphabet '())))))