(ns euler.test.core
  (:use [euler.core])
  (:use [clojure.test]))

(deftest test-reciprocal-cycles
  (let [res (reciprocal-cycles)]
    (is (= 983 (first res)))
    (is (= 982 (second res)))))
         
