(ns adventofcode2021.core-test
  (:require [clojure.test :refer :all]
            [adventofcode2021.core :refer :all]))

(deftest day1-1-test
  (is (= (day1-1 "1-example.txt") 7))
  (is (= (day1-1 "1.txt") 1665)))

(deftest day1-2-test
  (is (= (day1-2 "1-example.txt") 5))
  (is (= (day1-2 "1.txt") 1702)))

(deftest day2-1-test
  (is (= (day2-1 "2-example.txt") 150))
  (is (= (day2-1 "2.txt") 1250395)))

(deftest day2-2-test
  (is (= (day2-2 "2-example.txt") 900))
  (is (= (day2-2 "2.txt") 1451210346)))
