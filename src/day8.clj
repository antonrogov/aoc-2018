(ns day8
  (:require [clojure.test :as test]
            [clojure.string :as str]))

(defn parse-node [start]
  (let [num-meta (Integer/parseInt (second start))
        [children pos] (loop [pos (nthrest start 2)
                              left (Integer/parseInt (first start))
                              children []]
                         (if (= 0 left)
                           [children pos]
                           (let [[child new-pos] (parse-node pos)]
                             (recur new-pos (dec left) (conj children child)))))
        meta-data (map #(Integer/parseInt %) (take num-meta pos))]
    [{:children children :meta meta-data} (nthrest pos num-meta)]))

(defn parse-tree [string]
  (first (parse-node (str/split string #" "))))

(defn sum-meta [node]
  (+ (reduce + (:meta node))
     (reduce + (map sum-meta (:children node)))))

(test/is (= (sum-meta (parse-tree "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2")) 138))

(defn sum-value [node]
  (if node
    (if (empty? (:children node))
      (reduce + (:meta node))
      (reduce + (map #(sum-value (get (:children node) (dec %))) (:meta node))))
    0))

(test/is (= (sum-value (parse-tree "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2")) 66))

(println (sum-value (parse-tree (str/trim (slurp "data/day8.txt")))))
