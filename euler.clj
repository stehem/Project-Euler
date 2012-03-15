; http://projecteuler.net/problems
; http://code.google.com/p/projecteuler-solutions/wiki/ProjectEulerSolutions

(require 'clojure.string)

(ns clojure.test.euler
  (:use clojure.test))


; problem 2
(defn fibonacci
  [fib lastt]
  (if (< 4000000 lastt)
    (reduce + (reverse(rest(reverse (filter #(odd? %) fib)))))
    (let [newfib (concat fib (list (+ (first (reverse fib)) (second (reverse fib)))))]
    (recur newfib (last newfib)))))

(println "problem 2 :" (fibonacci (list 0 1) 1))

(deftest test-p1
  (is (= 4613732 (fibonacci (list 0 1) 1))))
; / problem 2



; problem 3
(defn is-prime?
  [n]
  (empty? (filter #(= 0 (rem n %)) (range 2 n))))

(defn prime-factor
  [n i]
  (if (and (= 0 (rem n i)) (is-prime? (/ n i)))
    (/ n i)
    (recur n (inc i))))

(println "problem 3 :" (prime-factor 600851475143 2))

(deftest test-p2
  (is (= 6857 (prime-factor 600851475143 2))))
; /problem 3


; problem 4
(defn is-palindrome?
  [n]
  (= (str n) (apply str(reverse(str n)))))

(defn big-pal
  [n]
  ; went from 8 secs and stack overflowing on larger numbers to runnning in 1 sec 
  ; and happily chugging along on 5 digits numbers
  (apply max 
    (reduce
      (fn[memo f] (lazy-cat (filter #(is-palindrome? %) (map #(* f %) (range 100 n))) memo)) 
      (list) (range 100 n))))

(println "problem 4 :" (big-pal 999))

(deftest test-palindrome
  (is (= true (is-palindrome? 9009)))
  (is (= false (is-palindrome? 9018))))

(deftest test-p4
  (is (= 906609 (big-pal 999))))
; /problem 4


; problem 5
(defn dividable?
  [n i]
  ; takes about 10 secs to complete
  (if (not= 0 (rem n i))
    false
    (if (= 1 i) true (recur n (dec i)))))

(defn big-divide
  [n]
  (if (dividable? n 20)
    n (recur (inc n))))

(println "problem 5 :" (big-divide 1))

(deftest test-p5
  (is (= 232792560 (big-divide 1))))
; /problem 5


; problem 6
(def sum-square-sum
  (- (* (reduce + (range 1 101)) (reduce + (range 1 101)))
    (reduce + (map #(* % %) (range 1 101)))))

(println "problem 6 :" sum-square-sum)

(deftest test-sumsquare
  (is (= 25164150 sum-square-sum)))
; /problem 6


; problem 7
(defn big-prime
  [memo n]
  (if (= 10001 (count memo))
    (first memo)
    (recur (lazy-cat (if (is-prime? n) (list n)) memo) (inc n))))

(println "problem 7 :" (big-prime (list) 2))

(deftest test-p7
  (is (= 104743 (big-prime (list) 2))))
; /problem 7
  

; problem 8
(defn greatest-product
  [memo start end]
  (let [biggie (clojure.string/split (clojure.string/replace 
              "73167176531330624919225119674426574742355349194934
               96983520312774506326239578318016984801869478851843
               85861560789112949495459501737958331952853208805511
               12540698747158523863050715693290963295227443043557
               66896648950445244523161731856403098711121722383113
               62229893423380308135336276614282806444486645238749
               30358907296290491560440772390713810515859307960866
               70172427121883998797908792274921901699720888093776
               65727333001053367881220235421809751254540594752243
               52584907711670556013604839586446706324415722155397
               53697817977846174064955149290862569321978468622482
               83972241375657056057490261407972968652414535100474
               82166370484403199890008895243450658541227588666881
               16427171479924442928230863465674813919123162824586
               17866458359124566529476545682848912883142607690042
               24219022671055626321111109370544217506941658960408
               07198403850962455444362981230987879927244284909188
               84580156166097919133875499200524063689912560717606
               05886116467109405077541002256983155200055935729725
               71636269561882670428252483600823257530420752963450" #"[^\w]" "") #"")]
    (if (= 1000 end)
      (apply max memo)
      (recur 
        (concat memo (list (reduce * (map #(Integer/parseInt %) (subvec biggie start end))))) 
        (inc start) (inc end)))))

(deftest test-biggie
  (is (= 40824 (greatest-product (list) 1 6))))
; /problem 8


; problem 9
(defn dedupe
  [el]
  (apply list (set el)))

(defn sums
  [n]
  (->>
    (reduce (fn[memo f] (conj memo (list f (- n f)))) (list) (range 1 (inc n)))
    (map #(sort %))
    (dedupe) ))

(defn to-ternary
  [n]
  (map #(concat (list (first n)) %) (sums (second n))))

(defn is-triplet?
  [triplet]
  (let [sorted (reverse (sort triplet))]
    (let [c (first sorted)]
      (let [b (first (rest sorted))]
        (let [a (second (rest sorted))]
          (= (* c c) (+ (* b b) (* a a))))))))

; runs in 8 secs
(defn pyth-triplet
  [n]
  (->>
  (reduce (fn[memo f] (lazy-cat memo (to-ternary f))) (list) (sums n))
  (map #(sort %))
  (dedupe)
  (filter #(is-triplet? %))
  (map #(* (nth % 0) (nth % 1) (nth % 2)))
  (apply max) ))

(deftest test-is-triplet?
  (is (= false (is-triplet? (list 3 2 4))))
  (is (= true (is-triplet? (list 4 3 5)))))

(deftest test-pyth-triplet
  (is (= 31875000 (pyth-triplet 1000))))
; /problem 9


(run-all-tests #"clojure.test.euler")
