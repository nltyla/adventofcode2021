(ns adventofcode2021.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.data.priority-map :refer [priority-map]]
            [clojure.walk :as w]
            [clojure.zip :as z]
            [clojure.math.combinatorics :as combo]
            [clojure.pprint :as pp]))

(defn inputs
  "read file from resources, apply f to each line, return seq"
  [name f]
  (map f (str/split-lines (slurp (io/resource name)))))

(defn parse-int
  [s]
  (Integer/parseInt s))

(defn parse-long
  [s]
  (Long/parseLong s))

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
  (Long/parseLong s 2))

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

(def pairs {\[ \] \{ \} \( \) \< \>})

(defn consume
  [state tok]
  (let [[status s autocomplete] state]
    (if (some? status)
      (if (empty? s)
        [status s (conj autocomplete tok)]
        state)
      (if (= tok (first s))
        [status (rest s) autocomplete]
        [tok s autocomplete]))))

(defn parse-pairs
  [state]
  (let [[status s _] state]
    (if (some? status)
      state
      (let [tok (first s)]
        (if (contains? pairs tok)
          (-> state
              (consume tok)
              (parse-pairs)
              (consume (pairs tok))
              (parse-pairs))
          state)))))

(def day10-score {\) 3 \] 57 \} 1197 \> 25137})

(defn score-line
  [s]
  (let [[status s _] (parse-pairs [nil s []])]
    (if (and (some? status)
             (not (empty? s)))
      (day10-score (first s))
      0)))

(defn day10-1
  "--- Day 10: Syntax Scoring ---"
  [name]
  (let [v (inputs name identity)
        scores (map score-line v)]
    (reduce + scores)))

(def day10-2-scores {\) 1 \] 2 \} 3 \> 4})

(defn day10-2-score
  [s]
  (reduce (fn [acc v] (+ (* acc 5) (day10-2-scores v))) 0 s))

(defn day10-2-score-line
  [s]
  (let [[status _ autocomplete] (parse-pairs [nil s []])]
    (if (empty? autocomplete)
      0
      (day10-2-score (cons status autocomplete)))))

(defn day10-2
  "--- Day 10 Part Two: Syntax Scoring ---"
  [name]
  (let [v (inputs name identity)
        scores (map day10-2-score-line v)
        clean-scores (sort (filter (complement zero?) scores))]
    (nth clean-scores (/ (count clean-scores) 2))))

(defn day11-parse
  [s]
  (vec (map #(parse-int (str %)) s)))

(defn minc
  [m]
  (reduce-kv (fn [acc k _] (update acc k inc)) m m))

(defn find-flashers
  [m]
  (map first (filter #(> (second %) 9) m)))

(defn incpos [n] (if (pos? n) (inc n) n))

(defn update-if-present
  [m k f]
  (if (contains? m k)
    (update m k f)
    m))

(defn mincsurround
  [m [row col]]
  (-> m
      (update-if-present [(dec row) (dec col)] incpos)
      (update-if-present [(dec row) col] incpos)
      (update-if-present [(dec row) (inc col)] incpos)
      (update-if-present [row (dec col)] incpos)
      (assoc [row col] 0)
      (update-if-present [row (inc col)] incpos)
      (update-if-present [(inc row) (dec col)] incpos)
      (update-if-present [(inc row) col] incpos)
      (update-if-present [(inc row) (inc col)] incpos)))

(defn flash
  [m flashcount]
  (let [flashers (find-flashers m)]
    (if (empty? flashers)
      [m flashcount]
      (let [m' (reduce #(mincsurround %1 %2) m flashers)
            flashcount' (+ flashcount (count flashers))]
        (recur m' flashcount')))))

(defn day11-step
  [[m flashcount]]
  (flash (minc m) flashcount))

(defn day11-as-map
  [m]
  (reduce-kv (fn [acc row line] (reduce-kv (fn [acc col v] (assoc acc [row col] v)) acc line)) {} m))

(defn day11-1
  "--- Day 11: Dumbo Octopus ---"
  [name]
  (let [v (vec (inputs name day11-parse))
        m (day11-as-map v)
        simulation (iterate day11-step [m 0])]
    (second (nth simulation 100))))

(defn day11-2
  "--- Day 11 Part Two: Dumbo Octopus ---"
  [name]
  (let [v (vec (inputs name day11-parse))
        m (day11-as-map v)
        simulation (iterate day11-step [m 0])]
    (inc (count (take-while #(not= 100 %) (map #(- (second (second %)) (second (first %))) (partition 2 1 simulation)))))))

(defn small?
  [s]
  (= s (str/lower-case s)))

(defn large?
  [s]
  (= s (str/upper-case s)))

(defn end?
  [path]
  (= (peek path) "end"))

(defn day12-1-allowed?
  [path to]
  (or (large? to)
      (not (contains? (set path) to))))

(defn day12-2-allowed?
  [path to]
  (or (large? to)
      (and (not= to "start")
           (let [smalls (filter small? path)
                 freqs (frequencies smalls)]
             (and (not= (freqs to) 2)
                  (let [by-freq (group-by second freqs)]
                    (< (count (get by-freq 2 [])) 2)))))))

(defn find-paths
  [[connections fallowed? path paths :as in]]
  (if (end? path)
    [connections fallowed? path (conj paths path)]
    (let [exits (connections (peek path))
          tos (filter (partial fallowed? path) exits)]
      (reduce
        (fn
          [[_ _ _ paths] to]
          (find-paths [connections fallowed? (conj path to) paths]))
        in
        tos))))

(defn day12
  "--- Day 12: Passage Pathing ---"
  [fallowed? name]
  (let [v (inputs name #(str/split % #"-"))
        connections (reduce (fn [acc [a b]] (-> acc
                                                (update a (fnil conj []) b)
                                                (update b (fnil conj []) a))) {} v)
        [_ _ _ paths] (find-paths [connections fallowed? ["start"] [] []])]
    (count paths)))

(def day12-1 (partial day12 day12-1-allowed?))

; TODO really slow (30 secs), try memoization for second occurrences
(def day12-2 (partial day12 day12-2-allowed?))

(defn parse-coords
  [s]
  (let [groups (re-matches #"(\d+),(\d+)" s)]
    (vec (map parse-int (drop 1 groups)))))

(defn mirror
  [n mirr]
  (if (>= n mirr) (+ (- mirr n) mirr) n))

(defn parse-folds
  [s]
  (let [[_ xy d] (re-matches #"fold along ([xy])=(\d+)" s)
        v (parse-int d)]
    (if (= xy "x")
      (fn [[x y]] [(mirror x v) y])
      (fn [[x y]] [x (mirror y v)]))))

(defn day13-1
  "--- Day 13: Transparent Origami ---"
  [name]
  (let [v (inputs name identity)
        [coords folds] (split-with not-empty v)
        coords (map parse-coords coords)
        ffolds (map parse-folds (rest folds))
        folded (set (map (first ffolds) coords))]
    (count folded)))

(defn day13-2
  "--- Day 13 Part Two: Transparent Origami ---"
  [name]
  (let [v (inputs name identity)
        [coords folds] (split-with not-empty v)
        coords (map parse-coords coords)
        ffolds (map parse-folds (rest folds))
        folded (set (reduce (fn [acc f] (map f acc)) coords ffolds))
        [maxx maxy] (reduce (fn [[mx my] [x y]] [(max mx x) (max my y)]) folded)]
    (dotimes [y (inc maxy)]
      (dotimes [x (inc maxx)]
        (if (contains? folded [x y])
          (print \#)
          (print " ")))
      (newline))
    (count folded)))

(defn day14-parse
  [s]
  (let [[_ from to] (re-matches #"(..) -> (.)" s)]
    [from (first to)]))

(defn split-pair
  [rules pair]
  (let [between (rules pair)
        p1 (str (first pair) between)
        p2 (str between (second pair))]
    [p1 p2 between]))

(defn update-freqs
  [rules [single-freqs pair-freqs] pair freq]
  (let [[p1 p2 between] (split-pair rules pair)
        single-freqs' (update single-freqs between (fnil + 0) freq)
        pair-freqs' (-> pair-freqs
                        (update p1 (fnil + 0) freq)
                        (update p2 (fnil + 0) freq)
                        (update pair - freq))]
    [single-freqs' pair-freqs']))

(defn day14-1-step
  [rules [single-freqs pair-freqs]]
  (reduce-kv (partial update-freqs rules) [single-freqs pair-freqs] pair-freqs))

(defn day14
  "--- Day 14: Extended Polymerization ---"
  [iters name]
  (let [v (inputs name identity)
        polymer (first v)
        single-freqs (frequencies polymer)
        pair-freqs (frequencies (map #(apply str %) (partition 2 1 polymer)))
        rules (reduce #(apply assoc %1 (day14-parse %2)) {} (nnext v))
        simulation (iterate (partial day14-1-step rules) [single-freqs pair-freqs])
        [single-freqs' _] (nth simulation iters)
        minfreq (apply min (vals single-freqs'))
        maxfreq (apply max (vals single-freqs'))]
    (- maxfreq minfreq)))

(def day14-1 (partial day14 10))
(def day14-2 (partial day14 40))

(def INF Integer/MAX_VALUE)

(defn day15-risk
  [m dim [row col]]
  (let [p [(mod row dim) (mod col dim)]
        increment (+ (quot row dim) (quot col dim))
        res (+ (m p) increment)]
    (if (> res 9)
      (- res 9)
      res)))

(defn move
  "Dijkstra's algorithm"
  [factor m dim]
  (let [start [0 0]
        dim' (* dim factor)
        dest [(dec dim') (dec dim')]
        q (into (priority-map) (for [row (range dim') col (range dim')] [[row col] INF]))]
    (loop [Q (assoc q start 0)]
      (let [[[row col :as u] distu] (first Q)
            Q' (pop Q)
            neighbors (filter #(contains? Q' %) [[(dec row) col]
                                                 [row (dec col)]
                                                 [row (inc col)]
                                                 [(inc row) col]])
            Q'' (reduce (fn [qq v] (let [distv (day15-risk m dim v)
                                         alt (+ distu distv)]
                                     (update qq v min alt)))
                        Q'
                        neighbors)]
        (if (= dest u)
          distu
          (recur Q''))))))

(defn day15
  "--- Day 15: Chiton ---"
  [factor name]
  (let [v (vec (inputs name day11-parse))
        m (day11-as-map v)
        dim (count v)
        minrisk (move factor m dim)]
    minrisk))

(def day15-1 (partial day15 1))
(def day15-2 (partial day15 5))

(def hex {\0 "0000" \1 "0001" \2 "0010" \3 "0011" \4 "0100" \5 "0101" \6 "0110" \7 "0111" \8 "1000" \9 "1001" \A "1010" \B "1011" \C "1100" \D "1101" \E "1110" \F "1111"})

(defn parse-three-bit
  [[in packet] name]
  (let [packet' (assoc packet name (parse-binary (subs in 0 3)))
        in' (subs in 3)]
    [in' packet']))

(defn parse-literal
  [[in packet]]
  (let [groups (partition 5 in)
        [lead-groups trail-groups] (split-with #(= \1 (first %)) groups)
        all-groups (concat lead-groups (list (first trail-groups)))
        stripped-groups (map rest all-groups)
        str-number (apply str (flatten stripped-groups))
        number (parse-binary str-number)
        in' (subs in (* 5 (count all-groups)))
        packet' (assoc packet :value number)]
    [in' packet']))

(declare parse-packet)

(defn parse-subs-by-len
  [[in packet]]
  (let [len (parse-binary (subs in 0 15))
        in' (subs in 15)]
    (loop [in in'
           sum 0
           subs []]
      (let [[in' subpkt] (parse-packet in)
            sum' (+ sum (- (count in) (count in')))
            subs' (conj subs subpkt)]
        (if (or (= sum' len)
                (nil? subpkt))
          [in' (assoc packet :value subs')]
          (recur in' sum' subs'))))
    ))

(defn parse-subs-by-num
  [[in packet]]
  (let [nr (parse-binary (subs in 0 11))
        in' (subs in 11)]
    (loop [in in'
           i 1
           subs []]
      (let [[in' subpkt] (parse-packet in)
            subs' (conj subs subpkt)]
        (if (= i nr)
          [in' (assoc packet :value subs')]
          (recur in' (inc i) subs'))))
    ))

(defn parse-modes
  [[in packet]]
  (let [mode (first in)
        in' (subs in 1)]
    (condp = mode
      \0 (parse-subs-by-len [in' packet])
      \1 (parse-subs-by-num [in' packet]))))

(defn parse-by-type
  [[_ packet :as state]]
  (condp = (:type packet)
    4 (parse-literal state)
    (parse-modes state)))

(defn parse-packet
  [in]
  (let [state' (-> [in {}]
                   (parse-three-bit :version)
                   (parse-three-bit :type)
                   (parse-by-type))]
    state'))


(defn day16-parse
  [s]
  (apply str (map #(hex %) s)))

(defn day16-1
  "--- Day 16: Packet Decoder ---"
  [name]
  (let [v (inputs name day16-parse)
        in (first v)
        [_ packet] (parse-packet in)]
    (w/postwalk (fn [x] (if (map? x)
                          (if (vector? (:value x))
                            (+ (:version x) (reduce + (:value x)))
                            (:version x))
                          x)) packet)))

(defn bool01
  [bool]
  (if bool 1 0))

(defn day16-2
  "--- Day 16 Part Two: Packet Decoder ---"
  [name]
  (let [v (inputs name day16-parse)
        in (first v)
        [_ packet] (parse-packet in)]
    (w/postwalk (fn [x] (if (map? x)
                          (let [v (:value x)]
                            (condp = (:type x)
                              0 (apply + v)
                              1 (apply * v)
                              2 (apply min v)
                              3 (apply max v)
                              4 v
                              5 (bool01 (apply > v))
                              6 (bool01 (apply < v))
                              7 (bool01 (apply = v))))
                          x)) packet)))

(defn day17-1
  "--- Day 17: Trick Shot ---"
  [miny]
  (let [d (Math/abs ^int miny)]
    (/ (* (dec d) d) 2)))                                   ; sum of arithmetic series 0 + 1 + 2 + ... + d

(defn hit?
  [vx vy xnear xfar ynear yfar]
  (loop [xpos 0
         ypos 0
         vx' vx
         vy' vy]
    (cond (and (>= ynear ypos yfar) (<= xnear xpos xfar)) [vx vy]
          (or (< ypos yfar) (> xpos xfar)) nil
          :else (recur (+ xpos vx') (+ ypos vy') (if (zero? vx') 0 (dec vx')) (dec vy')))))

(defn day17-2
  "--- Day 17 Part Two: Trick Shot ---"
  [xnear xfar ynear yfar]
  (let [shots (for [vy (range ynear (- yfar))
                    vx (range 1 xnear)] [vx vy])
        hitcoll (filter some? (map (fn [[vx vy]] (hit? vx vy xnear xfar ynear yfar)) shots))]
    (+ (count hitcoll) (* (inc (- xfar xnear)) (inc (- ynear yfar))))))

(defn add-and-walk-back
  [loc val fback steps]
  (let [edited (z/edit loc + val)]
    (reduce (fn [acc _] (fback acc)) edited (range steps))))

(defn top?
  [loc]
  (nil? (z/path loc)))

(defn add-to-nearest
  [fforward fback fdone? loc]
  (let [val (z/node loc)]
    (loop [loc' (fforward loc)
           n 1]
      (if (fdone? loc')
        loc
        (if (z/branch? loc')
          (recur (fforward loc') (inc n))
          (add-and-walk-back loc' val fback n))))))

(def add-to-nearest-right (partial add-to-nearest z/next z/prev z/end?))
(def add-to-nearest-left (partial add-to-nearest z/prev z/next top?))

(defn find-explodeable
  [loc]
  (if (z/end? loc)
    nil
    (if (and (z/branch? loc)
             (= 4 (count (z/path loc))))
      loc
      (recur (z/next loc)))))

(defn top
  [loc]
  (if-let [loc' (z/up loc)]
    (recur loc')
    loc))

(defn explode
  [loc]
  (when-let [explodeable (find-explodeable loc)]
    (-> explodeable
        z/next
        (add-to-nearest-left)
        z/next
        (add-to-nearest-right)
        z/prev
        z/prev
        (z/replace 0))))

(defn find-splittable
  [loc]
  (if (z/end? loc)
    nil
    (if (and (not (z/branch? loc))
             (>= (z/node loc) 10))
      loc
      (recur (z/next loc)))))

(defn split-num
  [n]
  (let [q (quot n 2)
        r (rem n 2)]
    [q (+ q r)]))

(defn split
  [loc]
  (when-let [splittable (find-splittable loc)]
    (z/edit splittable split-num)))

(defn reducestep
  "return reduced snail number or nil if it cannot be reduced"
  [loc]
  (if-let [exploded (explode loc)]
    (top exploded)
    (when-let [splitted (split loc)]
      (top splitted))))

(defn reducefully
  [loc]
  (if-let [loc' (reducestep loc)]
    (recur loc')
    loc))

(defn reduce-vec
  [vec]
  (-> (z/vector-zip vec)
      reducefully
      z/root))

(defn snail+
  [v1 v2]
  (reduce-vec [v1 v2]))

(defn add-snail-numbers
  [vecs]
  (reduce snail+ vecs))

(defn snail-magnitude
  [vec]
  (w/postwalk (fn [x] (if (vector? x)
                        (+ (* 3 (first x)) (* 2 (second x)))
                        x)) vec))

(defn day18-1
  "--- Day 18: Snailfish ---"
  [name]
  (let [v (inputs name read-string)]
    (snail-magnitude (add-snail-numbers v))))

(defn day18-2
  "--- Day 18 Part Two: Snailfish ---"
  [name]
  (let [vecs (inputs name read-string)
        combos (combo/permuted-combinations vecs 2)
        sums (map (fn [[v1 v2]] (snail+ v1 v2)) combos)
        mags (map #(snail-magnitude %) sums)]
    (apply max mags)))

(defn vdotv
  [v1 v2]
  (reduce + (map * v1 v2)))

(defn mdotv
  [M v]
  (into [] (map #(vdotv % v)) M))

(defn mdotm
  [m1 m2]
  (vec (apply map vector (map #(mdotv m1 %) (apply map vector m2)))))


; right-handed coordinate system, positive rotation counter-clockwise
(def m-ident [[1 0 0] [0 1 0] [0 0 1]])
(def m-rot-z090 [[0 -1 0] [1 0 0] [0 0 1]])
(def m-rot-z180 (mdotm m-rot-z090 m-rot-z090))
(def m-rot-z270 (mdotm m-rot-z090 m-rot-z180))
(def m-rot-x090 [[1 0 0] [0 0 -1] [0 1 0]])
(def m-rot-x180 (mdotm m-rot-x090 m-rot-x090))
(def m-rot-x270 (mdotm m-rot-x090 m-rot-x180))
(def m-rot-y090 [[0 0 -1] [0 1 0] [1 0 0]])
(def m-rot-y180 (mdotm m-rot-y090 m-rot-y090))
(def m-rot-y270 (mdotm m-rot-y090 m-rot-y180))
(def m-mirror-x [[-1 0 0] [0 1 0] [0 0 1]])
(def m-mirror-y [[1 0 0] [0 -1 0] [0 0 1]])
(def m-mirror-z [[1 0 0] [0 1 0] [0 0 -1]])
(def all-rots [m-ident m-rot-x090 m-rot-x180 m-rot-x270])
(def all-dirs [m-ident m-rot-x180 m-rot-z090 m-rot-z270 m-rot-y090 m-rot-y270])
(def all-orientations (mapcat (fn [dir] (map (fn [rot] (mdotm rot dir)) all-rots)) all-dirs))

(defn parse25
  [s]
  (map #(condp = %
          \> :right
          \v :down
          \. :empty) s))

(defn make-coord-map
  [grid]
  (into {} (filter
             (fn [[_ item]] (not= \. item))
             (apply concat (map-indexed (fn
                                          [rowindex row]
                                          (map-indexed (fn
                                                         [colindex item]
                                                         [[rowindex colindex] item])
                                                       row))
                                        grid)))))

(defn nextgen
  [target frow fcol gen dim]
  (into {} (map (fn [[[row col] item :as cell]] (if (= target item)
                                                  (let [nextpos [(frow row dim) (fcol col dim)]]
                                                    (if (some? (get gen nextpos))
                                                      cell
                                                      [nextpos target]))
                                                  cell))
                gen)))

(defn incmod [n dim] (mod (inc n) dim))
(defn noop [n _] n)
(def nextgenright (partial nextgen \> noop incmod))
(def nextgendown (partial nextgen \v incmod noop))

(defn nextgen25
  [rowdim coldim gen]
  (-> gen
      (nextgenright coldim)
      (nextgendown rowdim)))

(defn printgen
  [gen rowdim coldim]
  (let [row (vec (repeat coldim \.))
        grid (vec (repeat rowdim row))
        pgrid (reduce (fn [acc [[row col] item]] (assoc-in acc [row col] item)) grid gen)
        pgridstr (map #(apply str %) pgrid)]
    (pp/pprint pgridstr)))

(defn day25
  "--- Day 25: Sea Cucumber ---"
  [name]
  (let [v (inputs name identity)
        rowdim (count v)
        coldim (count (first v))
        gen0 (make-coord-map v)
        gens (iterate (partial nextgen25 rowdim coldim) gen0)
        genpairs (partition 2 1 gens)
        end (take-while (fn [[a b]] (not= a b)) genpairs)
        cnt (inc (count end))]
    cnt))