(ns adventofcode2021.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn inputs
  "read file from resources, apply f to each line, return seq"
  [name f]
  (map f (str/split-lines (slurp (io/resource name)))))

(defn parse-int
  [s]
  (Integer/parseInt s))

(defn count-increased
  [v]
  (->> v
       (partition 2 1)
       (filter (partial apply <))
       (count)))

(defn day1-1
  "--- Day 1: Sonar Sweep ---"
  [name]
  (let [v (inputs name parse-int)]
    (count-increased v)))

(defn day1-2
  "--- Day 1 Part Two: Sonar Sweep ---"
  [name]
  (let [v (inputs name parse-int)]
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

(defn parse-binary
  [s]
  (Integer/parseInt s 2))

(defn freqs-binary
  [coll]
  (let [freqs (frequencies coll)]
    [(get freqs \0 0) (get freqs \1 0)]))

(defn most-common
  [[zeros ones]]
  (cond
    (> ones zeros) \1
    (< ones zeros) \0
    :else \1))

(defn day3-1
  "--- Day 3: Binary Diagnostic"
  [name]
  (let [v (inputs name identity)
        gamma (->> v
                   (apply map (partial str))                ; transpose
                   (map freqs-binary)
                   (map most-common)
                   (apply str)
                   (parse-binary))
        allones (dec (bit-set 0 (count (first v))))
        epsilon (bit-and (bit-not gamma) allones)]
    (* gamma epsilon)))

(defn freqs-nth
  [v n]
  (->> v
       (map #(nth % n))
       (freqs-binary)))

(defn least-common
  [[zeros ones]]
  (cond
    (> ones zeros) \0
    (< ones zeros) \1
    :else \0))

(defn find-rating
  [v fcrit]
  (loop [vv v
         n 0]
    (if (= 1 (count vv))
      (parse-binary (first vv))
      (let [freqs (freqs-nth vv n)
            crit (fcrit freqs)
            vv' (filter #(= crit (nth % n)) vv)]
        (recur vv' (inc n))))))

(defn day3-2
  "--- Day 3 Part Two: Binary Diagnostic"
  [name]
  (let [v (inputs name identity)
        generator-rating (find-rating v most-common)
        scrubber-rating (find-rating v least-common)]
    (* generator-rating scrubber-rating)))

(defn parse-ints-into
  [s re to]
  (into to (map parse-int (str/split s re))))

(defn parse-board
  [ss]
  (map #(parse-ints-into % #"\s+" []) ss))

(defn setify
  [board]
  (map set board))

(defn add-xposed
  [board]
  (setify (concat board (apply map vector board))))

(defn mark-number
  [board number]
  (map #(disj % number) board))

(defn bingo?
  "Returns board if bingo, nil if not."
  [board]
  (when (some empty? board) board))

(defn find-bingo
  [boards]
  (some bingo? boards))

(defn mark-number-all
  [boards number]
  (map #(mark-number % number) boards))

(defn winner-score
  [board number]
  (* number (reduce + (mapcat seq (take 5 board)))))

(defn day4-inputs
  [name]
  (let [v (inputs name str/trim)
        board-strs (partition 5 6 (drop 2 v))
        boards (map parse-board board-strs)]
    [(parse-ints-into (first v) #"," []) (map add-xposed boards)]))

(defn day4-1-reducer
  [boards num]
  (let [boards' (mark-number-all boards num)]
    (if-let [winner (find-bingo boards')]
      (reduced (winner-score winner num))
      boards')))

(defn day4-1
  "--- Day 4: Giant Squid ---"
  [name]
  (let [[numbers boards] (day4-inputs name)]
    (reduce day4-1-reducer boards numbers)))

(defn day4-2-reducer
  [boards num]
  (let [boards' (mark-number-all boards num)
        boards'' (filter (complement bingo?) boards')]
    (if (empty? boards'')
      (reduced (winner-score (first boards') num))
      boards'')))

(defn day4-2
  "--- Day 4 Part Two: Giant Squid ---"
  [name]
  (let [[numbers boards] (day4-inputs name)]
    (reduce day4-2-reducer boards numbers)))

(defn day5-parse
  [s]
  (let [groups (re-matches #"(\d+),(\d+) -> (\d+),(\d+)" s)]
    (map parse-int (drop 1 groups))))

(defn draw
  [allow-diag [x0 y0 x1 y1]]
  (if (= x0 x1) ; vertical
    (let [[start end] (if (> y0 y1) [y1 y0] [y0 y1])]
      (for [y (range start (inc end))] [x0 y]))
    (let [[startx starty endx endy] (if (> x0 x1) [x1 y1 x0 y0] [x0 y0 x1 y1]) ; horizontal or diagonal
          ystep (Integer/signum (- endy starty))]
      (if (or (zero? ystep) allow-diag)
        (for [x (range startx (inc endx)) :let [y (+ starty (* (- x startx) ystep))]] [x y])
        `()))))

(defn day5-core
  "--- Day 5: Hydrothermal Venture ---"
  [fdraw name]
  (let [v (vec (inputs name day5-parse))
        filled (mapcat fdraw v)
        intersections (frequencies filled)
        targets (filter #(>= (second %) 2) intersections)]
    (count targets)))

(def day5-1 (partial day5-core (partial draw false)))
(def day5-2 (partial day5-core (partial draw true)))

(defn day6-1-reducer
  [[fishes' births] fish]
  (if (= 0 fish)
    [(conj fishes' 6) (inc births)]
    [(conj fishes' (dec fish)) births]))

(defn advance
  [fishes]
  (let [[fishes' births] (reduce day6-1-reducer [[] 0] fishes)
        bornfishes (repeat births 8)]
    (concat fishes' bornfishes)))

(defn day6-1
  [name]
  (let [v (inputs name identity)
        fishes (map parse-int (str/split (first v) #","))
        simulation (iterate advance fishes)]
    (count (nth simulation 80))))