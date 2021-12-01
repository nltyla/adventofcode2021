(ns adventofcode2021.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn inputs
  "read file from resources, apply f to each line, return seq"
  [name f]
  (map f (str/split-lines (slurp (io/resource name)))))

(defn day1-1
  "--- Day 1: Sonar Sweep ---"
  [name]
  (let [v (inputs name #(Integer/parseInt %))]
    (->> v
         (partition 2 1)
         (filter (partial apply <=))
         (count))))