(ns lab1.second
  (:gen-class))

(defn get-valid-symbols
  [word alphabet result]
  (let [banned-symbol (str (last word))]
    (if (= (count alphabet) 0)
      result
      (let [current (str (first alphabet))
            rest-alphabet (rest alphabet)]
        (if (= current banned-symbol)
          (recur word rest-alphabet result)
          (recur word rest-alphabet (conj result current)))))))

(defn append-word
  [word alphabet result]
  (if (= (count alphabet) 0)
    result
    (let [cur-symbol (first alphabet)
          rest-symbols (rest alphabet)]
      (recur word
             rest-symbols
             (conj result (str word cur-symbol))))))

(defn expand-words
  [words alphabet result]
  (if (empty? words)
    result
    (let [word (first words)
          rest-words (rest words)
          valid-next-symbols (get-valid-symbols word alphabet [])
          new-words (append-word word valid-next-symbols [])]
      (recur rest-words alphabet (vec (concat result new-words))))))

(defn build-words
  ([len alphabet]
   (seq (build-words len alphabet [""])))
  ([len alphabet words]
   (if (= len 0)
     words
     (recur (dec len)
            alphabet
            (expand-words words alphabet [])))))