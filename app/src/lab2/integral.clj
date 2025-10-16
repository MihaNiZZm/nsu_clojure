(ns lab2.integral
  (:gen-class))

(defn integral
  ([f] (integral f 1e-3))
  ([f h]
   (fn [x]
     (let [sign   (if (neg? x) -1 1)
           x      (* sign x)
           n      (long (Math/ceil (/ x h)))
           xs     (map #(* % h) (range 1 n))
           sum-fx (reduce + (map f xs))]
       (* sign h
          (+ (/ (+ (f 0) (f x)) 2) sum-fx))))))

(defn integral-memo
  ([f] (integral-memo f 1e-3))
  ([f h]
   (let [int-fn (integral f h)]
     (memoize int-fn))))

(defn integral-seq
  ([f] (integral-seq f 1e-3))
  ([f h]
   (let [partial-sums
         (reductions
          (fn [[_ acc] x]
            [x (+ acc (* h (/ (+ (f x) (f (- x h))) 2)))])
          [0 0.0]
          (map #(* h %) (iterate inc 1)))]
     (fn [x]
       (let [sign (if (neg? x) -1 1)
             x    (Math/abs x)
             idx  (long (Math/round (/ x h)))
             [xk intk] (nth partial-sums idx)
             delta (- x xk)
             tail-area (* delta (/ (+ (f xk) (f x)) 2))]
         (* sign (+ intk tail-area)))))))

(defn check-result
  []
  (let [f-x (fn [x] x)
        integral-f-x (integral f-x 0.001)]
    (println "Integral 0 to 1:" (integral-f-x 1.0) ", expected:" (/ 1.0 2))
    (println "Integral 0 to 2:" (integral-f-x 2.0) ", expected:" (/ 4.0 2))
    (println "Integral 0 to 3:" (integral-f-x 3.0) ", expected:" (/ 9.0 2))))