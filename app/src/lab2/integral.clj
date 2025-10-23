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
