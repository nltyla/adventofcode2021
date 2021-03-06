(ns adventofcode2021.core-test
  (:require [clojure.test :refer :all]
            [adventofcode2021.core :refer :all]
            [clojure.zip :as z]))

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

(deftest day6-1-test
  (is (= (day6-1 "6-example.txt") 5934))
  (is (= (day6-1 "6.txt") 362666)))

(deftest day6-2-test
  (is (= (day6-2 "6-example.txt") 26984457539))
  (is (= (day6-2 "6.txt") 1640526601595)))

(deftest day7-1-test
  (is (= (day7-1 "7-example.txt") 37))
  (is (= (day7-1 "7.txt") 352254)))

(deftest day7-2-test
  (is (= (day7-2 "7-example.txt") 168))
  (is (= (day7-2 "7.txt") 99053143)))

(deftest day8-1-test
  (is (= (day8-1 "8-example.txt") 26))
  (is (= (day8-1 "8.txt") 543)))

(deftest day8-2-test
  (is (= (day8-2 "8-example.txt") 61229))
  (is (= (day8-2 "8.txt") 994266)))

(deftest day9-1-test
  (is (= (day9-1 "9-example.txt") 15))
  (is (= (day9-1 "9.txt") 500)))

(deftest day9-2-test
  (is (= (day9-2 "9-example.txt") 1134))
  (is (= (day9-2 "9.txt") 970200)))

(deftest day10-1-test
  (is (= (day10-1 "10-example.txt") 26397))
  (is (= (day10-1 "10.txt") 387363)))

(deftest day10-2-test
  (is (= (day10-2 "10-example.txt") 288957))
  (is (= (day10-2 "10.txt") 4330777059)))

(deftest day11-1-test
  (is (= (day11-1 "11-example.txt") 1656))
  (is (= (day11-1 "11.txt") 1747)))

(deftest day11-2-test
  (is (= (day11-2 "11-example.txt") 195))
  (is (= (day11-2 "11.txt") 505)))

(deftest day12-1-test
  (is (= (day12-1 "12-example1.txt") 10))
  (is (= (day12-1 "12-example2.txt") 19))
  (is (= (day12-1 "12-example3.txt") 226))
  (is (= (day12-1 "12.txt") 4186)))

(deftest day12-2-test
  (is (= (day12-2 "12-example1.txt") 36))
  (is (= (day12-2 "12-example2.txt") 103))
  (is (= (day12-2 "12-example3.txt") 3509))
  (is (= (day12-2 "12.txt") 92111)))

(deftest day13-1-test
  (is (= (day13-1 "13-example.txt") 17))
  (is (= (day13-1 "13.txt") 724)))

(deftest day13-2-test
  (is (= (day13-2 "13.txt") 95)))

(deftest day14-1-test
  (is (= (day14-1 "14-example.txt") 1588))
  (is (= (day14-1 "14.txt") 2321)))

(deftest day14-2-test
  (is (= (day14-2 "14-example.txt") 2188189693529))
  (is (= (day14-2 "14.txt") 2399822193707)))

(deftest day15-1-test
  (is (= (day15-1 "15-example.txt") 40))
  (is (= (day15-1 "15.txt") 656)))

(deftest day15-2-test
  (is (= (day15-2 "15-example.txt") 315))
  (is (= (day15-2 "15.txt") 2979)))

(deftest day16-1-test
  (is (= (day16-1 "16-example1.txt") 16))
  (is (= (day16-1 "16-example2.txt") 12))
  (is (= (day16-1 "16-example3.txt") 23))
  (is (= (day16-1 "16-example4.txt") 31))
  (is (= (day16-1 "16.txt") 1007)))

(deftest day16-2-test
  (is (= (day16-2 "16-example2-1.txt") 3))
  (is (= (day16-2 "16-example2-2.txt") 54))
  (is (= (day16-2 "16-example2-3.txt") 7))
  (is (= (day16-2 "16-example2-4.txt") 9))
  (is (= (day16-2 "16-example2-5.txt") 1))
  (is (= (day16-2 "16-example2-6.txt") 0))
  (is (= (day16-2 "16-example2-7.txt") 0))
  (is (= (day16-2 "16-example2-8.txt") 1))
  (is (= (day16-2 "16.txt") 834151779165)))

(deftest day17-1-test
  (is (= (day17-1 -80) 3160)))

(deftest day17-2-test
  (is (= (day17-2 282 314 -45 -80) 1928)))

(defn explode-vec
  [vec]
  (-> (z/vector-zip vec)
      explode
      z/root))

(defn split-vec
  [vec]
  (-> (z/vector-zip vec)
      split
      z/root))

(deftest day18-1-test
  (is (= (explode-vec [[[[[9,8],1],2],3],4]) [[[[0,9],2],3],4]))
  (is (= (explode-vec [7,[6,[5,[4,[3,2]]]]]) [7,[6,[5,[7,0]]]]))
  (is (= (explode-vec [[6,[5,[4,[3,2]]]],1]) [[6,[5,[7,0]]],3]))
  (is (= (split-vec [[[[0,7],4],[15,[0,13]]],[1,1]]) [[[[0,7],4],[[7,8],[0,13]]],[1,1]]))
  (is (= (split-vec [[[[0,7],4],[[7,8],[0,13]]],[1,1]]) [[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]))
  (is (= (reduce-vec [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]) [[[[0,7],4],[[7,8],[6,0]]],[8,1]]))
  (is (= (day18-1 "18-example.txt") 4140))
  (is (= (day18-1 "18.txt") 4072)))

(deftest day18-2-test
  (is (= (day18-2 "18-example.txt") 3993))
  (is (= (day18-2 "18.txt") 4483)))

(deftest day25-1-test
  (is (= (day25 "25-example.txt") 58))
  (is (= (day25 "25.txt") 520)))
