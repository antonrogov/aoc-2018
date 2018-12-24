(ns day6
  (:require [clojure.test :as test]
            [clojure.string :as str]))

(defn parse-points [lines]
  (set (map (fn [id line]
              (let [[x y] (map #(Integer/parseInt %) (str/split line #", "))]
                {:id (inc id) :x x :y y})) (range) lines)))

(defn distance [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2))
     (Math/abs (- y1 y2))))

(defn closest-point-id [x y points]
  (let [with-dist (map #(assoc % :dist (distance [x y] [(:x %) (:y %)]))
                       points)
        sorted (sort-by :dist with-dist)]
    (if (= (:dist (first sorted))
           (:dist (second sorted)))
      nil
      (:id (first sorted)))))

(defn safe-point? [safe-distance x y points]
  (if (< (reduce +
                 (map #(distance [x y] [(:x %) (:y %)])
                      points))
         safe-distance)
    :good
    :bad))

(defn map-with-points-f [points f]
  (let [xs (map :x points)
        ys (map :y points)
        min-x (apply min xs)
        max-x (apply max xs)
        min-y (apply min ys)
        max-y (apply max ys)]
    (for [y (range min-y (inc max-y))]
      (for [x (range min-x (inc max-x))]
        (f x y points)))))

(defn map-with-points [points]
  (map-with-points-f points closest-point-id))

(defn largest-safe-area-size [points]
  (let [filled-map (map-with-points-f points closest-point-id)
        infinite-ids (set
                       (distinct
                         (concat
                           (first filled-map)
                           (last filled-map)
                           (map first filled-map)
                           (map last filled-map))))
        flattened (apply concat filled-map)
        cleared (map #(if (contains? infinite-ids %) 0 %) flattened)]
      (apply max (map val (filter #(> (key %) 0) (frequencies cleared))))))

(test/is (=
  (largest-safe-area-size
    (parse-points ["1, 1"
                   "1, 6"
                   "8, 3"
                   "3, 4"
                   "5, 5"
                   "8, 9"]))) 17)

(defn largest-good-area-size [points safe-distance]
  (let [filled-map (map-with-points-f points (partial safe-point? safe-distance))
        flattened (apply concat filled-map)]
      (:good (frequencies flattened))))

(test/is (=
  (largest-good-area-size
    (parse-points ["1, 1"
                   "1, 6"
                   "8, 3"
                   "3, 4"
                   "5, 5"
                   "8, 9"]) 32)) 16)

(with-open [rdr (clojure.java.io/reader "data/day6.txt")]
  (println (largest-good-area-size (parse-points (line-seq rdr)) 10000)))
