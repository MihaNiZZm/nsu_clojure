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

(defn check-result
  []
  (let [f-x (fn [x] x)
        integral-f-x (integral f-x 0.001)]
    (println "Integral 0 to 1:" (integral-f-x 1.0) ", expected:" (/ 1.0 2))
    (println "Integral 0 to 2:" (integral-f-x 2.0) ", expected:" (/ 4.0 2))
    (println "Integral 0 to 3:" (integral-f-x 3.0) ", expected:" (/ 9.0 2))))
