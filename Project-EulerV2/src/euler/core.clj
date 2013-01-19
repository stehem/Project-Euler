(ns euler.core)
(use '[clojure.string :only (join split)])


; problem 26, brute force version, assumes the reccurring pattern starts immediately after comma...
(defn to-big-float
  [n]
  (with-precision 5000 (/ 1M n)))

(defn decimal-to-string
  [n]
  (last (split (str (to-big-float n)) #"\.")))

(defn splitter
  [string]
  (loop [i 1]
    (let [
          sub (subs string 0 i) 
          splitted (butlast (rest (split string (re-pattern sub))))
          matched? (empty? (remove empty? splitted))
          leng (count string)
          ]
      (if (or matched? (= leng i))
        sub
        (recur (inc i))))))

(defn reciprocal-cycles
  []
  (first
  (reduce
    (fn[memo x] 
      (let [
            step [x (count (splitter (decimal-to-string x)))]
            [n cnt] step
            [_ cnt_] (last memo)
           ]
        (if (> cnt cnt_) 
          (assoc memo 0 [n cnt])
          memo)))
    [[1 (count (splitter (decimal-to-string 1)))]]
    (range 2 1001))))
;/problem 26


