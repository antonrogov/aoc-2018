(ns aoc.day17
  (:require [clojure.test :as test]
            [clojure.string :as str]))

(defn cell-at [field [y x]]
  (get (get field y []) x :border))


(defn change-cell-at [field [y x] value]
  (let [row (get field y)]
    (assoc field y (assoc row x value))))


(defn can-pass? [cell]
  (contains? #{\. \|} cell))

(defn border? [cell]
  (= cell :border))

(defn wall? [cell]
  (= cell \#))


(defn can-go-down? [field [y x]]
  (can-pass? (cell-at field [(inc y) x])))


(defn find-stop [field pos dx]
  (loop [pos pos]
    (let [[y x] pos
          cell (cell-at field pos)]
      (if (can-go-down? field pos)
        [x cell]
        (if (can-pass? cell)
          (recur [y (+ x dx)])
          [(- x dx) cell])))))

(defn left-stop [field pos]
  (find-stop field pos -1))

(defn right-stop [field pos]
  (find-stop field pos 1))

(test/is (= (left-stop [[\# \. \. \#]] [0 2]) [1 \#]))
(test/is (= (left-stop [[\. \. \. \#]] [0 2]) [0 :border]))
(test/is (= (left-stop [[\# \. \. \#]
                        [\# \. \# \#]] [0 2]) [1 \.]))
(test/is (= (right-stop [[\# \. \. \#]] [0 1]) [2 \#]))
(test/is (= (right-stop [[\# \. \. \.]] [0 1]) [3 :border]))
(test/is (= (right-stop [[\# \. \. \#]
                        [\# \# \. \#]] [0 1]) [2 \.]))


(defn fill-volume [field y left right c]
  (let [row (get field y)]
    (assoc field y (vec (concat (subvec row 0 left)
                                (repeat (- right left -1) c)
                                (subvec row (inc right)))))))

(test/is (= (fill-volume [[\# \. \. \#]] 0 1 2 \~) [[\# \~ \~ \#]]))


(defn print-field [field]
  (doseq [row field]
    (println (apply str row)))
  (println))

(defn process [field spring]
  (loop [field field
         positions []
         pos spring]
    ; (prn pos)
    ; (print-field field)
    (let [[y x] pos
          dst [(inc y) x]
          [dy dx] dst
          curr-cell (cell-at field pos)
          new-field (change-cell-at field dst \|)
          down [(inc dy) dx]
          down-cell (cell-at field down)]
      (if (can-pass? down-cell)
        (recur new-field positions dst)
        (if (border? down-cell)
          (if (empty? positions)
            new-field
            (recur new-field (rest positions) (first positions)))
          (let [[left left-c] (left-stop field dst)
                [right right-c] (right-stop field dst)]
                ; _ (prn left right left-c right-c (wall? left-c) (wall? right-c) curr-cell)]
            (if (and (wall? left-c) (wall? right-c))
              (recur (fill-volume new-field dy left right \~)
                     positions spring)
              (let [new-field (fill-volume new-field dy left right \|)]
                (if (wall? left-c)
                  (recur new-field positions [dy right])
                  (if (wall? right-c)
                    (recur new-field positions [dy left])
                    (recur new-field
                           (conj positions [dy right])
                           [dy left])))))))))))


(defn count-water [field]
  (count (filter #(contains? #{\| \~} %) (flatten field))))


(defn parse-ranges [lines]
  (map #(let [[_ xy v1 v2 v3] (re-matches #"([xy])=(\d+), [xy]=(\d+)\.\.(\d+)" %)
              v1 (Integer/parseInt v1)
              v2 (Integer/parseInt v2)
              v3 (Integer/parseInt v3)]
          (if (= xy "x")
            [[v1 v1] [v2 v3]]
            [[v2 v3] [v1 v1]])) lines))

(defn range-includes? [[[x1 x2] [y1 y2]] x y]
  (and (<= x1 x x2)
       (<= y1 y y2)))

(defn parse-scan [lines]
  (let [ranges (parse-ranges lines)
        xs (flatten (map first ranges))
        ys (flatten (map second ranges))
        min-x (apply min xs)
        min-y (apply min ys)
        max-x (apply max xs)
        max-y (apply max ys)
        height (inc max-y)]
    (reduce
      (fn [field y]
        (conj
          field
          (reduce
            (fn [row x]
              (conj row
                    (if (some #(range-includes? % x y) ranges) \# \.)))
            []
            (range min-x (inc max-x)))))
      []
      (range 0 height))))


(test/is (= (parse-scan ["x=495, y=2..7"
                         "y=7, x=495..501"
                         "x=501, y=3..7"
                         "x=498, y=2..4"
                         "x=506, y=1..2"
                         "x=498, y=10..13"
                         "x=504, y=10..13"
                         "y=13, x=498..504"])
            (mapv #(vec (seq %)) ["............"
                                  "...........#"
                                  "#..#.......#"
                                  "#..#..#....."
                                  "#..#..#....."
                                  "#.....#....."
                                  "#.....#....."
                                  "#######....."
                                  "............"
                                  "............"
                                  "...#.....#.."
                                  "...#.....#.."
                                  "...#.....#.."
                                  "...#######.."])))


(test/is (= (count-water
              (process
                (parse-scan ["x=495, y=2..7"
                             "y=7, x=495..501"
                             "x=501, y=3..7"
                             "x=498, y=2..4"
                             "x=506, y=1..2"
                             "x=498, y=10..13"
                             "x=504, y=10..13"
                             "y=13, x=498..504"]) [0 6])) 57))

; (println
;   (print-field
;     (parse-scan
;       (str/split (slurp "day17.txt") #"\n"))))
(println
  (count-water
    (process
      (parse-scan
        (str/split (slurp "day17.txt") #"\n")) [0 6])))
