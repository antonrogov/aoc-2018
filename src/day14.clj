(ns day14
  (:require [clojure.test :as test]
            [clojure.string :as str]))

(defn last-ten-recipes [improve-count]
  (loop [scoreboard [3 7]
         currents [0 1]]
    (if (>= (count scoreboard) (+ improve-count 10))
      (apply str (subvec scoreboard improve-count (+ improve-count 10)))
      (let [recipes (map #(get scoreboard %) currents)
            sum (reduce + recipes)
            new-recipes (if (> sum 9)
                          [(quot sum 10) (mod sum 10)]
                          [sum])
            new-scoreboard (into scoreboard new-recipes)
            new-currents (mapv
                           (fn [r i] (mod (+ i (inc r)) (count new-scoreboard)))
                           recipes currents)]
        (recur new-scoreboard new-currents)))))

(test/is (= (last-ten-recipes 9) "5158916779"))
(test/is (= (last-ten-recipes 5) "0124515891"))
(test/is (= (last-ten-recipes 18) "9251071085"))
(test/is (= (last-ten-recipes 2018) "5941429882"))


(defn parse-scores [s]
  (mapv #(- (int %) (int \0)) (seq s)))

(test/is (= (parse-scores "51589") [5 1 5 8 9]))


(defn count-recipes-before [scores]
  (loop [scoreboard [3 7]
         currents [0 1]
         pos 0]
    (if (>= (- (count scoreboard) pos) (count scores))
      (if (= scores (subvec scoreboard pos (+ pos (count scores))))
        pos
        (recur scoreboard currents (inc pos)))
      (let [recipes (map #(get scoreboard %) currents)
            sum (reduce + recipes)
            new-recipes (if (> sum 9)
                          [(quot sum 10) (mod sum 10)]
                          [sum])
            new-scoreboard (into scoreboard new-recipes)
            new-currents (mapv
                           (fn [r i] (mod (+ i (inc r)) (count new-scoreboard)))
                           recipes currents)]
        (recur new-scoreboard new-currents pos)))))

(test/is (= (count-recipes-before (parse-scores "51589")) 9))
(test/is (= (count-recipes-before (parse-scores "5158916")) 9))
(test/is (= (count-recipes-before (parse-scores "01245")) 5))
(test/is (= (count-recipes-before (parse-scores "92510")) 18))
(test/is (= (count-recipes-before (parse-scores "59414")) 2018))

; (println (last-ten-recipes 894501))
(println (count-recipes-before (parse-scores "894501")))
