(ns lab3.future-filter
  (:gen-class))

(defn split-into-n-parts
  [coll n]
  (let [cnt (count coll)
        base-size (quot cnt n)
        rest (mod cnt n)]
    (loop [c coll
           idx 0
           acc []]
      (if (< idx n)
        (let [size (+ base-size (if (< idx rest) 1 0))
              part (take size c)]
          (recur (drop size c) (inc idx) (conj acc part)))
        acc))))

(defn future-filter
  ([pred coll] (future-filter pred coll 25))
  ([pred coll blocks-count]
   (->> (split-into-n-parts coll blocks-count)
        (map #(future (doall (filter pred %))))
        (mapcat deref))))
