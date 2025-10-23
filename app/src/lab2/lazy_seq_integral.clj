(ns lab2.lazy-seq-integral
  (:gen-class))

(def h 1/100)

(defn trapezoid-area [f x0 x1]
  (double (* (- x1 x0) (/ (+ (f x0) (f x1)) 2))))

(defn lazy-integral [f x0 x1 h]
  (let [xs (take-while #(< % x1) (iterate #(+ % h) x0))
        areas (map #(trapezoid-area f % (+ % h)) xs)]
    (reduce + areas)))

(defn integral [f]
  (fn [x] (if (neg? x)
            (lazy-integral f x 0 h)
            (lazy-integral f 0 x h))))
