(ns adventofcode2021.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn inputs
  "read file from resources, apply f to each line, return seq"
  [name f]
  (map f (str/split-lines (slurp (io/resource name)))))

(defn parse-int
  [s]
  (Integer/parseInt s))

(defn parse-ints-csv
  [s]
  (map parse-int (str/split s #",")))

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
  (if (= x0 x1)                                             ; vertical
    (let [[start end] (if (> y0 y1) [y1 y0] [y0 y1])]
      (for [y (range start (inc end))] [x0 y]))
    (let [[startx starty endx endy] (if (> x0 x1) [x1 y1 x0 y0] [x0 y0 x1 y1]) ; horizontal or diagonal
          ystep (Integer/signum (- endy starty))]
      (if (or (zero? ystep) allow-diag)
        (for [x (range startx (inc endx)) :let [y (+ starty (* (- x startx) ystep))]] [x y])
        `()))))

(defn day5-core
  "--- Day 5: Hydrothermal Venture ---"
  [allow-diag name]
  (let [v (vec (inputs name day5-parse))
        filled (mapcat (partial draw allow-diag) v)
        intersections (frequencies filled)
        targets (filter #(>= (second %) 2) intersections)]
    (count targets)))

(def day5-1 (partial day5-core false))
(def day5-2 (partial day5-core true))

(defn day6-advance
  [fishfreqs]
  (let [parentfreq (first fishfreqs)
        rotatedfreqs (conj (subvec fishfreqs 1) parentfreq)
        fishfreqs' (update-in rotatedfreqs [6] + parentfreq)]
    fishfreqs'))

(defn day6
  "--- Day 6: Lanternfish ---"
  [days name]
  (let [fishes (first (inputs name parse-ints-csv))
        freqs (frequencies fishes)
        fishfreqs (vec (map #(get freqs % 0) (range 9)))
        simulation (iterate day6-advance fishfreqs)]
    (reduce + (nth simulation days))))

(def day6-1 (partial day6 80))
(def day6-2 (partial day6 256))

(defn day7-1-cost
  [p1 p2]
  (Math/abs ^int (- p1 p2)))

(defn fuel
  [fcost crabs pos]
  (transduce (map #(fcost pos %)) + crabs))

(defn day7
  "--- Day 7: The Treachery of Whales ---"
  [fcost name]
  (let [crabs (first (inputs name parse-ints-csv))
        fuels (map #(fuel fcost crabs %) (range (apply min crabs) (apply max crabs)))]
    (apply min fuels)))

(defn day7-2-cost
  [p1 p2]
  (let [d (Math/abs ^int (- p1 p2))]
    (/ (* d (inc d)) 2)))                                   ; sum of arithmetic series 1 + 2 + ... + d

(def day7-1 (partial day7 day7-1-cost))
(def day7-2 (partial day7 day7-2-cost))

(defn split-to-sets
  [s]
  (map set (str/split s #" ")))

(defn day8-parse
  [s]
  (let [[all displayed] (str/split s #" \| ")]
    [(split-to-sets all) (split-to-sets displayed)]))

(defn day8-1
  "--- Day 8: Seven Segment Search ---"
  [name]
  (let [v (inputs name day8-parse)
        displayed (map second v)
        knowns #{2 4 3 7}
        lengths (mapcat #(map count %) displayed)
        known-lengths (filter knowns lengths)]
    (count known-lengths)))

(defn find069
  [digs dig]
  (let [seg (first (set/difference (digs 8) dig))]
    (cond
      (contains? (digs 1) seg) (assoc digs 6 dig)
      (contains? (digs 4) seg) (assoc digs 0 dig)
      :else (assoc digs 9 dig))))

(defn find235
  [digs dig]
  (cond
    (empty? (set/difference (digs 7) dig)) (assoc digs 3 dig)
    (= 7 (count (set/union (digs 4) dig))) (assoc digs 2 dig)
    :else (assoc digs 5 dig)))

(defn solve-display
  [[all displayed]]
  (let [by-seg-count (group-by count all)
        digs1478 {1 (first (by-seg-count 2))
                  4 (first (by-seg-count 4))
                  7 (first (by-seg-count 3))
                  8 (first (by-seg-count 7))}
        digs0146789 (reduce find069 digs1478 (by-seg-count 6))
        digs0123456789 (reduce find235 digs0146789 (by-seg-count 5))
        segs-to-dig (set/map-invert digs0123456789)]
    (parse-int (apply str (map segs-to-dig displayed)))))

(defn day8-2
  "--- Day 8 Part Two: Seven Segment Search ---"
  [name]
  (let [v (inputs name day8-parse)
        vals (map solve-display v)]
    (reduce + vals)))

(defn day9-pad
  [s]
  (vec (map #(parse-int (str %)) (str \9 s \9))))

(defn risk-level
  [top center bottom]
  (let [target (second center)]
    (if (and (< target (second top))
             (< target (first center))
             (< target (nth center 2))
             (< target (second bottom)))
      (inc target)
      0)))

(defn vert
  [[top center bottom]]
  (map risk-level (partition 3 1 top) (partition 3 1 center) (partition 3 1 bottom)))

(defn day9-1
  "--- Day 9: Smoke Basin ---"
  [name]
  (let [v (inputs name day9-pad)
        row9 (repeat (count (first v)) 9)
        padded (concat [row9] v [row9])
        horz (partition 3 1 padded)
        risk-levels (mapcat vert horz)]
    (reduce + risk-levels)))

(defn mget
  [m row col]
  (-> m
      (nth row)
      (nth col)))

(defn mupdate
  [m row col f]
  (update-in m [row col] f))

(defn fill4
  "Returns [m' count]"
  [[m cnt] [row col]]
  (if (= 9 (mget m row col))
    [m cnt]
    (-> [(mupdate m row col (constantly 9)) (inc cnt)]
        (fill4 [(dec row) col])
        (fill4 [row (dec col)])
        (fill4 [row (inc col)])
        (fill4 [(inc row) col]))))

(defn day9-2
  "--- Day 9 Part Two: Smoke Basin ---"
  [name]
  (let [v (inputs name day9-pad)
        rowend (dec (count v))
        colend (dec (count (first v)))
        row9 (vec (repeat colend 9))
        padded (vec (concat [row9] v [row9]))
        coords (for [row (range 1 rowend) col (range 1 colend)] [row col])
        [_ risk-levels] (reduce
                          (fn
                            [[m counts] coord]
                            (let [[m count] (fill4 [m 0] coord)]
                              [m (conj counts count)]))
                          [padded []]
                          coords)]
    (reduce * (take 3 (reverse (sort risk-levels))))))