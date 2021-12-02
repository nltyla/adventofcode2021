(ns adventofcode2021.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn inputs
  "read file from resources, apply f to each line, return seq"
  [name f]
  (map f (str/split-lines (slurp (io/resource name)))))

(def count-increased
  (comp
    (partition 2 1)
    (filter (partial apply <))))

(defn day1-1
  "--- Day 1: Sonar Sweep ---"
  [name]
  (let [v (inputs name #(Integer/parseInt %))]
    (into [] count-increased v)))

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

(defn day2-1
  "--- Day 2: Dive! ---"
  [name]
  (let [v (inputs name day2-parse-row)
        loc (reduce nav [0 0] v)]
    (reduce * loc)))