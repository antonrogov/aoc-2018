(ns day12
  (:require [clojure.test :as test]
            [clojure.string :as str]))

(defn parse-pot [c]
  (= c \#))

(defn parse-pots [s]
  (mapv parse-pot (seq s)))

(test/is (= (parse-pots "#..#") [true false false true]))


(defn parse-rule [s]
  (let [[pots result] (str/split s #" => ")]
    [(parse-pots pots) (parse-pot (first result))]))

(test/is (= (parse-rule "..#.. => .") [[false false true false false] false]))
(test/is (= (parse-rule ".##.. => #") [[false true true false false] true]))


(defn parse-rules [lines]
  (map parse-rule lines))

(defn new-state [pots offset]
  (reduce
    (fn [state [p i]] (assoc state (+ i offset) p))
    {}
    (map (fn [p i] [p i]) pots (range))))

(defn parse-state
  ([s] (parse-state s 0))
  ([s offset] (new-state (parse-pots s) offset)))

(test/is (= (parse-state "#..#") {0 true 1 false 2 false 3 true}))


(defn parse-input [lines]
  (let [[_ state] (str/split (first lines) #": ")]
    [(parse-state state)
     (parse-rules (nthrest lines 2))]))

(test/is (= (parse-input ["initial state: .##"
                          ""
                          "#..#. => ."])
            [(parse-state ".##") [(parse-rule "#..#. => .")]]))


(defn rule-matches? [rule state pos]
  (every? #(= (get state (+ pos %) false)
              (get (first rule) (+ % 2)))
          (range -2 3)))

(defn matching-rule [state rules pos]
  (some #(when (rule-matches? % state pos) %) rules))

(defn min-pot [state]
  (apply min (mapv key state)))

(defn max-pot [state]
  (apply max (mapv key state)))

(defn format-state [state]
  (apply str
         (mapv
           (fn [i] (if (get state i) "#" "."))
           (range (min-pot state) (inc (max-pot state))))))

(defn next-state [state rules]
  (let [start (min-pot state)
        stop (max-pot state)]
    (reduce
      (fn [new-state i]
        (let [rule (matching-rule state rules i)
              new-pot (if rule (second rule) false)]
          (if (or new-pot (contains? state i))
            (assoc new-state i new-pot)
            new-state)))
      {} (range (- start 20) (+ stop 20)))))

(defn process [state rules n]
  (loop [left n
         current state]
    (if (< left 0)
      current
      (let [new-state (next-state current rules)]
        (recur (dec left) new-state)))))

(defn sum-pots-with-plants [state]
  (reduce
    (fn [acc [i p]] (if p (+ acc i) acc))
    0 state))

(test/is (= (sum-pots-with-plants
              (parse-state "#....##....#####...#######....#.#..##" -2))
            325))


(defn sum-pots-with-plants-after [state rules gen]
  (let [final-num 88
        final (process state rules final-num)
        num-plants (count (filter val final))]
    (+ (sum-pots-with-plants final)
       (* (- gen final-num 1) num-plants))))

(with-open [rdr (clojure.java.io/reader "data/day12.txt")]
  (let [[state rules] (parse-input (line-seq rdr))]
    (println (sum-pots-with-plants-after state rules 50000000000))))
