(ns day25
  (:require [clojure.test :as test]
            [clojure.string :as str]))

(defn parse-line [line]
  (mapv #(Integer/parseInt %) (re-seq #"[-\d]+" line)))

(test/is (= (parse-line "pos=<1,2,3>, r=4") [1 2 3 4]))

(defn parse-data [data]
  (mapv parse-line data))


(defn distance [p1 p2]
  (reduce + (mapv #(Math/abs (- %1 %2)) p1 p2)))

(test/is (= (distance [0 0 0 0] [3 0 0 0]) 3))

(defn constellations [points]
  (loop [left points
         colors (apply assoc {} (interleave points (range)))]
    (if (empty? left)
      (count (reduce #(assoc %1 (second %2) (first %2)) {} colors))
      (let [point (first left)
            color (get colors point)
            connected (filter #(<= (distance % point) 3) points)
            connected-colors (mapv #(get colors %) connected)
            colors (reduce
                     (fn [cs [p c]]
                       (assoc cs p (if (.contains connected-colors c) color c)))
                     {} colors)]
        (recur (rest left) colors)))))


(test/is (= (constellations
              (parse-data ["0,0,0,0"
                           "3,0,0,0"
                           "0,3,0,0"
                           "0,0,3,0"
                           "0,0,0,3"
                           "0,0,0,6"
                           "9,0,0,0"
                           "12,0,0,0"])) 2))

(test/is (= (constellations
              (parse-data ["-1,2,2,0"
                           "0,0,2,-2"
                           "0,0,0,-2"
                           "-1,2,0,0"
                           "-2,-2,-2,2"
                           "3,0,2,-1"
                           "-1,3,2,2"
                           "-1,0,-1,0"
                           "0,2,1,-2"
                           "3,0,0,0"])) 4))

(test/is (= (constellations
              (parse-data ["1,-1,0,1"
                           "2,0,-1,0"
                           "3,2,-1,0"
                           "0,0,3,1"
                           "0,0,-1,-1"
                           "2,3,-2,0"
                           "-2,2,0,0"
                           "2,-2,0,-1"
                           "1,-1,0,-1"
                           "3,2,0,2"])) 3))

(test/is (= (constellations
              (parse-data ["1,-1,-1,-2"
                           "-2,-2,0,1"
                           "0,2,1,3"
                           "-2,3,-2,1"
                           "0,2,3,-2"
                           "-1,-1,1,-2"
                           "0,-2,-1,0"
                           "-2,2,3,-1"
                           "1,2,2,0"
                           "-1,-2,0,-2"])) 8))

(prn (constellations (parse-data (str/split (slurp "data/day25.txt") #"\n"))))
