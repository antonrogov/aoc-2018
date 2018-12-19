(ns aoc.day13
  (:require [clojure.test :as test]
            [clojure.string :as str]))

(defn parse-line [line y]
  (reduce
    (fn [[cells carts] [c x]]
      (cond (contains? #{\^ \v} c)
            [(conj cells \|) (assoc carts [y x] {:dir c :xs 0})]
            (contains? #{\< \>} c)
            [(conj cells \-) (assoc carts [y x] {:dir c :xs 0})]
            :else
            [(conj cells c) carts]))
    [[] (sorted-map)]
    (map vector (seq line) (range))))

(defn parse-map [lines]
  (reduce
    (fn [[rows carts] [line y]]
      (let [[cells row-carts] (parse-line line y)]
        [(conj rows cells) (merge carts row-carts)]))
    [[] (sorted-map)]
    (map vector lines (range))))

(test/is (= (parse-map ["|" "v" "|" "|" "|" "^" "|"])
            [(vec (repeat 7 [\|])) {[1 0] {:dir \v :xs 0} [5 0] {:dir \^ :xs 0}}]))


(defn track-type [tracks [y x]]
  (get (get tracks y) x))

(defn turn-left [dir]
  (case dir
    \v \>
    \> \^
    \^ \<
    \< \v))

(defn turn-right [dir]
  (case dir
    \v \<
    \> \v
    \^ \>
    \< \^))

(defn move-cart [[pos {:keys [dir xs]}] tracks]
  (let [new-pos (case dir
                  \^ [(dec (first pos)) (second pos)]
                  \v [(inc (first pos)) (second pos)]
                  \> [(first pos) (inc (second pos))]
                  \< [(first pos) (dec (second pos))])
        cell (track-type tracks new-pos)
        new-cart (case cell
                   \\ {:dir (case dir
                              \v \>
                              \> \v
                              \^ \<
                              \< \^)
                       :xs xs}
                   \/ {:dir (case dir
                              \v \<
                              \< \v
                              \^ \>
                              \> \^)
                       :xs xs}
                   \+ {:dir (case (mod xs 3)
                              0 (turn-left dir)
                              2 (turn-right dir)
                              dir)
                       :xs (inc xs)}
                   {:dir dir :xs xs})]
    [new-pos new-cart]))

(defn any-cart-at? [carts pos]
  (some #(= (key %) pos) carts))

(defn first-crash [[tracks carts]]
  (loop [tick 0
         moving-carts carts
         moved-carts (sorted-map)]
    (if (empty? moving-carts)
      (recur (inc tick) moved-carts (sorted-map))
      (let [moving (first moving-carts)
            [pos cart] (move-cart moving tracks)
            other-carts (merge moved-carts (rest moving-carts))]
        (if (any-cart-at? other-carts pos)
          pos
          (recur tick
                 (dissoc moving-carts (key moving))
                 (assoc moved-carts pos cart)))))))

(test/is (= (first-crash (parse-map ["|" "v" "|" "|" "|" "^" "|"])) [3 0]))

(test/is (= (first-crash (parse-map ["/->-\\        "
                                     "|   |  /----\\"
                                     "| /-+--+-\\  |"
                                     "| | |  | v  |"
                                     "\\-+-/  \\-+--/"
                                     "  \\------/   "])) [3 7]))

(defn last-cart-pos [[tracks carts]]
  (loop [tick 0
         moving-carts carts
         moved-carts (sorted-map)]
    (if (empty? moving-carts)
      (if (= (count moved-carts) 1)
        (key (first moved-carts))
        (recur (inc tick) moved-carts (sorted-map)))
      (let [moving (first moving-carts)
            [pos cart] (move-cart moving tracks)
            other-carts (merge moved-carts (rest moving-carts))]
        (if (any-cart-at? other-carts pos)
          (recur tick
                 (dissoc moving-carts (key moving) pos)
                 (dissoc moved-carts pos))
          (recur tick
                 (dissoc moving-carts (key moving))
                 (assoc moved-carts pos cart)))))))

(with-open [rdr (clojure.java.io/reader "day13.txt")]
  (println (last-cart-pos (parse-map (line-seq rdr)))))
