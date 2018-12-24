(ns day10
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

(with-open [rdr (clojure.java.io/reader "data/day10.txt")]
  (loop [points (parse-points (line-seq rdr))
         size (points-area points)
         sec 0]
    (let [new-points (tick points)
          new-size (points-area new-points)]
      (if (or (> (first new-size) (first size))
              (> (second new-size) (second size)))
        (prn sec)
        (recur new-points new-size (inc sec))))))
