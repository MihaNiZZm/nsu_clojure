(ns lab3.lazy-future-filter
  (:gen-class))

(def threads-number (.availableProcessors (Runtime/getRuntime)))
(def block-size 4)

(defn take-blocks
  [block-size coll]
  (when (not-empty coll)
    (lazy-seq
     (cons
      (take block-size coll)
      (take-blocks block-size (drop block-size coll))))))

(defn get-correct-block-size
  [coll]
  (if (< (count coll) (* block-size threads-number))
    (inc (quot (count coll) threads-number))
    block-size))

(defn lazy-filter
  [pred coll]
  (->> coll
       (take-blocks (* threads-number block-size))
       (map #(take-blocks (get-correct-block-size %) %))
       (mapcat (fn [block]
                 (->> block
                      (map #(future (doall (filter pred %))))
                      (doall))))
       (mapcat deref)))
