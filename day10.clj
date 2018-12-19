(ns aoc.day10
  (:require [clojure.test :as test]
            [clojure.string :as str]))

(defn tick [points]
  (mapv #(assoc % :point (mapv + (:point %) (:velocity %))) points))

(defn bounding-box [points]
  (let [xs (map #(first (:point %)) points)
        ys (map #(second (:point %)) points)]
    [(apply min xs) (apply min ys) (apply max xs) (apply max ys)]))

(defn points-area [points]
  (let [[min-x min-y max-x max-y] (bounding-box points)]
    [(- max-x min-x) (- max-y min-y)]))

(defn draw [points]
  (let [[min-x min-y max-x max-y] (bounding-box points)
        dx (- max-x min-x)
        dy (- max-y min-y)]
    (dotimes [y (inc dy)]
      (dotimes [x (inc dx)]
        (print (if (some #(and (= (first (:point %)) (+ x min-x))
                               (= (second (:point %)) (+ y min-y))) points)
                 "#" ".")))
      (println))))

(defn parse-point [s]
  (let [[x y vx vy] (re-seq #"-?\d+" s)]
    {:point (map #(Integer/parseInt %) [x y])
     :velocity (map #(Integer/parseInt %) [vx vy])}))

(defn parse-points [lines]
  (mapv parse-point lines))

; (let [points (parse-points ["position=< 9,  1> velocity=< 0,  2>"
;                             "position=< 7,  0> velocity=<-1,  0>"
;                             "position=< 3, -2> velocity=<-1,  1>"
;                             "position=< 6, 10> velocity=<-2, -1>"
;                             "position=< 2, -4> velocity=< 2,  2>"
;                             "position=<-6, 10> velocity=< 2, -2>"
;                             "position=< 1,  8> velocity=< 1, -1>"
;                             "position=< 1,  7> velocity=< 1,  0>"
;                             "position=<-3, 11> velocity=< 1, -2>"
;                             "position=< 7,  6> velocity=<-1, -1>"
;                             "position=<-2,  3> velocity=< 1,  0>"
;                             "position=<-4,  3> velocity=< 2,  0>"
;                             "position=<10, -3> velocity=<-1,  1>"
;                             "position=< 5, 11> velocity=< 1, -2>"
;                             "position=< 4,  7> velocity=< 0, -1>"
;                             "position=< 8, -2> velocity=< 0,  1>"
;                             "position=<15,  0> velocity=<-2,  0>"
;                             "position=< 1,  6> velocity=< 1,  0>"
;                             "position=< 8,  9> velocity=< 0, -1>"
;                             "position=< 3,  3> velocity=<-1,  1>"
;                             "position=< 0,  5> velocity=< 0, -1>"
;                             "position=<-2,  2> velocity=< 2,  0>"
;                             "position=< 5, -2> velocity=< 1,  2>"
;                             "position=< 1,  4> velocity=< 2,  1>"
;                             "position=<-2,  7> velocity=< 2, -2>"
;                             "position=< 3,  6> velocity=<-1, -1>"
;                             "position=< 5,  0> velocity=< 1,  0>"
;                             "position=<-6,  0> velocity=< 2,  0>"
;                             "position=< 5,  9> velocity=< 1, -2>"
;                             "position=<14,  7> velocity=<-2,  0>"
;                             "position=<-3,  6> velocity=< 2, -1>"])]
; (loop [points points
;        i 0]
;   (draw points)
;   (if (= i 5)
;     i
;     (let [new-points (tick points)]
;       (Thread/sleep 1000)
;       (println)
;       (recur new-points (inc i))))))

(with-open [rdr (clojure.java.io/reader "day10.txt")]
  (loop [points (parse-points (line-seq rdr))
         size (points-area points)
         sec 0]
    (let [new-points (tick points)
          new-size (points-area new-points)]
      (if (or (> (first new-size) (first size))
              (> (second new-size) (second size)))
        (prn sec)
        (recur new-points new-size (inc sec))))))
