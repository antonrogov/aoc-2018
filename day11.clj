(ns aoc.day11
  (:require [clojure.test :as test]
            [clojure.string :as str]))

(defn power-at [x y grid-serial]
  (let [rack-id (+ x 10)
        power (+ (* rack-id y) grid-serial)
        power (* power rack-id)
        power (quot (mod power 1000) 100)]
    (- power 5)))

(test/is (= (power-at 3 5 8) 4))
(test/is (= (power-at 122 79 57) -5))
(test/is (= (power-at 217 196 39) 0))
(test/is (= (power-at 101 153 71) 4))

(defn square-power [x y conv-size grid-serial]
  (reduce
    +
    (for [dy (range 0 conv-size)]
      (reduce
        +
        (for [dx (range 0 conv-size)]
          (power-at (+ x dx) (+ y dy) grid-serial))))))

(defn column-power [x y conv-size grid-serial]
  (reduce
    +
    (for [dy (range 0 conv-size)]
      (power-at x (+ y dy) grid-serial))))

(defn square-power-fast [x y conv-size grid-serial prev-power]
  (if (= x 1)
    (square-power x y conv-size grid-serial)
    (+ prev-power
       (- (column-power (dec x) y conv-size grid-serial))
       (column-power (+ x conv-size -1) y conv-size grid-serial))))

(defn grid-square-conv [grid-size grid-serial conv-size]
  (let [process-size (max 2 (- grid-size (dec conv-size)))
        *prev-power (volatile! nil)]
    (for [y (range 1 process-size)
          x (range 1 process-size)]
      (let [power (square-power-fast x y conv-size grid-serial @*prev-power)]
        (vreset! *prev-power power)
        {:x x :y y :power power}))))

(defn square-with-largest-power [grid-size grid-serial conv-size]
  (apply max-key :power (grid-square-conv grid-size grid-serial conv-size)))

(test/is (= (time (square-with-largest-power 300 18 3)) {:x 33 :y 45 :power 29}))
(test/is (= (time (square-with-largest-power 300 42 3)) {:x 21 :y 61 :power 30}))

(defn conv-with-largest-power [grid-size grid-serial]
  (apply max-key :power
         (for [conv-size (range 1 (inc grid-size))]
           (assoc
             (square-with-largest-power grid-size grid-serial conv-size)
             :size conv-size))))

; (test/is (= (time (conv-with-largest-power 300 18)) {:x 90 :y 269 :power 113 :size 16}))

(prn (time (conv-with-largest-power 300 8868)))
