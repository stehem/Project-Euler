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


; problem 10
; this uses the Sieve of Eratosthenes to generate prime numbers, it is not the "true" sieve because
; it uses trial divisions but thanks to some decent optimizing it calculates and sums all the primes
; under 2M in about 30 secs. There is still ample room for optimizing, i have seen a couple Clojure
; exemples under 1 sec, but that will do for now.
;
; http://en.wikipedia.org/wiki/Sieve_of_Eratosthenes

(defn crosser
  [mem i]
  (filter #(and (= 0 (rem % i)) (not= % i)) (drop i mem)))

(defn remover
  [li1 li2]
  (if (empty? li2)
    li1
    (recur (disj li1 (first li2)) (rest li2))))

(defn sieve
  [n memo done]
    (let [next-number (first (drop (count done) memo))]
    (if (> next-number (Math/sqrt n))
      memo
      (recur n (remover memo (crosser memo next-number)) (concat done (list next-number))))))

(defn lotsa-primes
  [n]
  (reduce + (sieve n (apply sorted-set (range 2 (+ 1 n))) (list))))

(deftest test-lotsa-primes
  (is (= 142913828922 (lotsa-primes 2000000))))
; /problem 10


; problem 11
(def grid
  (let [biggie (clojure.string/split  
   "08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
    49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
    81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
    52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
    22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
    24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
    32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
    67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
    24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
    21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
    78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
    16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
    86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
    19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
    04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
    88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
    04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
    20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
    20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
    01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48" #"\s+")]
      biggie
  ))

;(println grid )

(defn indexes
  [i]
  (let [hor-right (list i (+ i 1) (+ i 2) (+ i 3))]
  (let [hor-left (list i (- i 1) (- i 2) (- i 3))] 
  (let [ver-up (list i (- i 20) (- i 40) (- i 60))]
  (let [ver-down (list i (+ i 20 ) (+ i 40) (+ i 60))]
  (let [dia-up-right (list i (- i 19) (- i 38) (- i 57))]
  (let [dia-up-left (list i (- i 21) (- i 42) (- i 63))]
  (let [dia-down-right (list i (+ i 21 ) (+ i 42) (+ i 63))]
  (let [dia-down-left (list i (+ i 19 ) (+ i 38) (+ i 57))]
    (list hor-right hor-left ver-up ver-down dia-up-right dia-up-left dia-down-right dia-down-left))))))))))

(println (indexes 44))
(defn product
  [lis]
  (if (some #(> 0 %) lis)
    0
    (reduce * 
      (reduce (fn[memo f] (concat memo (list (Integer/parseInt(get grid f))))) 
        (list) lis))))

(defn products
  [n]
  (reduce
    (fn[memo f] (concat memo (list (product f))))
    (list)
    (indexes n)))

(def grid-products
  (reduce
    (fn[memo f] (concat memo (products (.indexOf grid f))))
    (list)
    grid
  ) 
)


(println grid-products)
; /problem 11


(run-all-tests #"clojure.test.euler")
