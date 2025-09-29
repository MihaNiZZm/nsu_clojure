(ns lab1.first
  (:gen-class))

(defn get-valid-symbols
  [word alphabet]
  (let [banned-symbol (str (first word))]
    (if (= (count alphabet) 0)
      '()
      (let [current (first alphabet)
            rest-alphabet (rest alphabet)]
        (if (= current banned-symbol)
          (get-valid-symbols word rest-alphabet)
          (cons current (get-valid-symbols word rest-alphabet)))))))

(defn append-word
  [word alphabet]
  (let [valid-symbols (get-valid-symbols word alphabet)]
    (if (= (count valid-symbols) 0)
      '()
      (let [cur-symbol (first valid-symbols)
            rest-symbols (rest valid-symbols)]
        (cons (str cur-symbol word)
              (append-word word rest-symbols))))))

(defn expand-words
  [words alphabet]
  (if (= (count words) 0)
    '()
    (let [word (first words)
          rest-words (rest words)
          valid-next-symbols (get-valid-symbols word alphabet)
          new-words (append-word word valid-next-symbols)]
      (concat new-words (expand-words rest-words alphabet)))))

(defn build-words
  [len alphabet]
  (if (= len 0)
    '("")
    (let [shorter-words (build-words (dec len) alphabet)]
      (expand-words shorter-words alphabet))))