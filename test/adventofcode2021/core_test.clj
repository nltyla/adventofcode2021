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

(deftest day3-1-test
  (is (= (day3-1 "3-example.txt") 198))
  (is (= (day3-1 "3.txt") 4006064)))

(deftest day3-2-test
  (is (= (day3-2 "3-example.txt") 230))
  (is (= (day3-2 "3.txt") 5941884)))

(deftest day4-1-test
  (is (= (day4-1 "4-example.txt") 4512))
  (is (= (day4-1 "4.txt") 49860)))

(deftest day4-2-test
  (is (= (day4-2 "4-example.txt") 1924))
  (is (= (day4-2 "4.txt") 24628)))

(deftest day5-1-test
  (is (= (day5-1 "5-example.txt") 5))
  (is (= (day5-1 "5.txt") 5690)))

(deftest day5-2-test
  (is (= (day5-2 "5-example.txt") 12))
  (is (= (day5-2 "5.txt") 17741)))
