(ns aoc.day9
  (:require [clojure.test :as test]
            [clojure.string :as str]))

(defn new-marble [value cw ccw]
  (volatile! {:value value :cw cw :ccw ccw}))

(defn marble-value [*marble*]
  (:value @*marble*))

(defn marble-cw [*marble*]
  (:cw @*marble*))

(defn marble-ccw [*marble*]
  (:ccw @*marble*))

(defn marble-link-cw [*marble* *cw*]
  (vswap! *marble* assoc :cw *cw*))

(defn marble-link-ccw [*marble* *ccw*]
  (vswap! *marble* assoc :ccw *ccw*))

(defn new-field []
  (let [*marble* (new-marble 0 nil nil)]
    (vswap! *marble* assoc :cw *marble* :ccw *marble*)
    *marble*))

(defn field-insert-cw12 [*current* value]
  (let [*cw1* (marble-cw *current*)
        *cw2* (marble-cw *cw1*)
        *marble* (new-marble value *cw2* *cw1*)]
    (marble-link-cw *cw1* *marble*)
    (marble-link-ccw *cw2* *marble*)
    *marble*))

(defn field-remove-ccw7 [*current*]
  (let [*remove* (nth (iterate marble-ccw *current*) 7)
        *cw* (marble-cw *remove*)
        *ccw* (marble-ccw *remove*)
        value (marble-value *remove*)]
    (marble-link-ccw *cw* *ccw*)
    (marble-link-cw *ccw* *cw*)
    [*cw* value]))

(defn play-turn [field turn]
  (if (= 0 (mod turn 23))
    (let [[field removed] (field-remove-ccw7 field)]
      [field (+ turn removed)])
    [(field-insert-cw12 field turn) 0]))

(defn winning-score [num-players num-turns]
  (loop [field (new-field)
         scores (vec (repeat num-players 0))
         turn 1]
    (if (> turn num-turns)
      (apply max scores)
      (let [[field score] (play-turn field turn)
            player (mod (dec turn) num-players)
            old-score (get scores player)]
        (recur field
               (assoc scores player (+ old-score score))
               (inc turn))))))

(test/is (= (winning-score 9 25) 32))
(test/is (= (winning-score 10 1618) 8317))
(test/is (= (winning-score 13 7999) 146373))
(test/is (= (winning-score 17 1104) 2764))
(test/is (= (winning-score 21 6111) 54718))
(test/is (= (winning-score 30 5807) 37305))

(defn parse-input [string]
  (let [[_ players turns]
        (re-matches #"(\d+) players?; last marble is worth (\d+) points?" string)]
    (map #(Integer/parseInt %) [players turns])))

(test/is (= (parse-input
              "10 players; last marble is worth 1618 points")
            [10 1618]))

(let [[players turns] (parse-input (str/trim (slurp "day9.txt")))]
  (println (winning-score players (* turns 100))))
