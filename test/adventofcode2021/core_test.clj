(ns adventofcode2021.core-test
  (:require [clojure.test :refer :all]
            [adventofcode2021.core :refer :all]))

(deftest day1-1-test
  (is (= 7 (day1-1 "1-example.txt")))
  (is (= 1665 (day1-1 "1.txt"))))
