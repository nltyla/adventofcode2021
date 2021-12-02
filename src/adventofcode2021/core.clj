(ns adventofcode2021.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn inputs
  "read file from resources, apply f to each line, return seq"
  [name f]
  (map f (str/split-lines (slurp (io/resource name)))))

(defn count-increased
  [v]
  (->> v
       (partition 2 1)
       (filter (partial apply <))
       (count)))

(defn day1-1
  "--- Day 1: Sonar Sweep ---"
  [name]
  (let [v (inputs name #(Integer/parseInt %))]
    (count-increased v)))

(defn day1-2
  "--- Day 1 Part Two: Sonar Sweep ---"
  [name]
  (let [v (inputs name #(Integer/parseInt %))]
    (->> v
         (partition 3 1)
         (map (partial reduce +))
         (count-increased))))

(defn day2-parse-row
  [s]
  (map read-string (str/split (str ":" s) #"\s")))

(defn nav
  [[pos depth] [dir step]]
  (condp = dir
    :forward [(+ pos step) depth]
    :down [pos (+ depth step)]
    :up [pos (- depth step)]
    ))

(defn nav2
  [[pos depth aim] [dir step]]
  (condp = dir
    :forward [(+ pos step) (+ depth (* aim step)) aim]
    :down [pos depth (+ aim step)]
    :up [pos depth (- aim step)]
    ))

(defn day2
  "--- Day 2: Dive! ---"
  [fnav initval name]
  (let [v (inputs name day2-parse-row)
        loc (reduce fnav initval v)]
    (* (loc 0) (loc 1))))

(def day2-1 (partial day2 nav [0 0]))
(def day2-2 (partial day2 nav2 [0 0 0]))

