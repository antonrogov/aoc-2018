(ns day17
  (:require [clojure.test :as test]
            [clojure.string :as str]))

(defn cell-at [field [y x]]
  (get (get field y []) x :border))


(defn change-cell-at [field [y x] value]
  (let [row (get field y)]
    (assoc field y (assoc row x value))))


(defn can-pass? [cell]
  (contains? #{\. \|} cell))

(defn free? [cell]
  (= cell \.))

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

(defn process [[field start]]
  (loop [field field
         positions []
         pos start]
    (let [[y x] pos
          curr-cell (cell-at field pos)
          new-field (change-cell-at field pos \|)
          down [(inc y) x]
          down-cell (cell-at field down)]
      (if (can-pass? down-cell)
        (recur new-field positions down)
        (if (border? down-cell)
          (if (empty? positions)
            new-field
            (recur new-field (rest positions) (first positions)))
          (let [[left left-c] (left-stop field pos)
                [right right-c] (right-stop field pos)]
            (if (and (wall? left-c) (wall? right-c))
              (recur (fill-volume new-field y left right \~)
                     positions [(dec y) x])
              (let [new-field (fill-volume new-field y left right \|)]
                (if (free? right-c)
                  (if (free? left-c)
                    (recur new-field
                           (conj positions [y right])
                           [y left])
                    (recur new-field positions [y right]))
                  (if (free? left-c)
                    (recur new-field positions [y left])
                    (if (empty? positions)
                      new-field
                      (recur new-field (rest positions) (first positions)))))))))))))


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

(defn build-cell [ranges x y]
  (if (some #(range-includes? % x y) ranges) \# \.))

(defn range-points [[[x1 x2] [y1 y2]]]
  (for [x (range x1 (inc x2))
        y (range y1 (inc y2))]
    [y x]))

(defn add-range [field r]
  (reduce #(change-cell-at %1 %2 \#)
          field (range-points r)))

(defn add-ranges [field rs]
  (reduce #(add-range %1 %2) field rs))

(defn parse-scan [lines]
  (let [ranges (parse-ranges lines)
        xs (flatten (map first ranges))
        ys (flatten (map second ranges))
        min-x (dec (apply min xs))
        min-y (apply min ys)
        max-x (inc (apply max xs))
        max-y (apply max ys)
        width (- max-x min-x -1)
        height (- max-y min-y -1)
        field (vec (repeat height (vec (repeat width \.))))
        wall-points (map (fn [[y x]] [(- y min-y) (- x min-x)])
                         (apply concat
                                (map #(range-points %) ranges)))
        field (reduce #(change-cell-at %1 %2 \#) field wall-points)]
    [field [0 (- 500 min-x)]]))



(defn count-water [field]
  (count (filter #(contains? #{\| \~} %) (flatten field))))

(test/is (= (count-water
              (process
                (parse-scan ["x=495, y=2..7"
                             "y=7, x=495..501"
                             "x=501, y=3..7"
                             "x=498, y=2..4"
                             "x=506, y=1..2"
                             "x=498, y=10..13"
                             "x=504, y=10..13"
                             "y=13, x=498..504"]))) 57))


(defn count-still-water [field]
  (count (filter #(= % \~) (flatten field))))

(test/is (= (count-still-water
              (process
                (parse-scan ["x=495, y=2..7"
                             "y=7, x=495..501"
                             "x=501, y=3..7"
                             "x=498, y=2..4"
                             "x=506, y=1..2"
                             "x=498, y=10..13"
                             "x=504, y=10..13"
                             "y=13, x=498..504"]))) 29))

(println
  (count-still-water
    (process
      (parse-scan
        (str/split (slurp "data/day17.txt") #"\n")))))
