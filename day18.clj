(ns aoc.day18
  (:require [clojure.test :as test]
            [clojure.string :as str]))

(def open-cell \.)
(def tree-cell \|)
(def lumber-cell \#)

(defn open? [c] (= c open-cell))
(defn tree? [c] (= c tree-cell))
(defn lumber? [c] (= c lumber-cell))

(defn count-type [cs pred]
  (count (filterv #(pred %) cs)))

(defn column [field x y]
  (filterv #(not= % :border)
           (map #(get (get field (+ y %) []) x :border)
                (range -1 2))))

(defn adjacent [field x y]
  (for [dx (range -1 2)
        dy (range -1 2)
        :when (or (not= dx 0) (not= dy 0))]
    (get (get field (+ y dy) []) (+ x dx) :border)))

(defn process [field]
  (let [w (count (first field))
        h (count field)]
    (mapv (fn [row y]
            (mapv (fn [cell x]
                    (let [cs (adjacent field x y)]
                      (cond (and (open? cell)
                                 (>= (count-type cs tree?) 3)) tree-cell
                            (and (tree? cell)
                                 (>= (count-type cs lumber?) 3)) lumber-cell
                            (lumber? cell) (if (and (>= (count-type cs lumber?) 1)
                                                    (>= (count-type cs tree?) 1))
                                             lumber-cell open-cell)
                            :else cell)))
                  row (range)))
          field (range))))


(defn parse-field [lines]
  (mapv #(vec (seq %)) lines))

(defn print-field [field]
  (str/join "\n" (map #(apply str %) field)))


(defn resource-value [field]
  (let [cells (flatten field)]
    (* (count-type cells tree?)
       (count-type cells lumber?))))

(defn count-resource-value [lines minutes]
  (resource-value
    (nth
      (iterate process (parse-field lines))
      minutes)))

(test/is (= (count-resource-value [".#.#...|#."
                                   ".....#|##|"
                                   ".|..|...#."
                                   "..|#.....#"
                                   "#.#|||#|#|"
                                   "...#.||..."
                                   ".|....|..."
                                   "||...#|.#|"
                                   "|.||||..|."
                                   "...#.|..|."] 10) 1147))

; (println (count-resource-value (str/split (slurp "day18.txt") #"\n") 10))

(defn resource-values [field max-minute]
  (loop [field field
         values []
         minute 0]
    (if (= minute max-minute)
      values
      (recur (process field)
             (conj values (resource-value field))
             (inc minute)))))

(defn value-after [minute pattern start]
  (get pattern (mod (- minute start) (count pattern))))

(let [values (resource-values (str/split (slurp "day18.txt") #"\n") 500)
      start 431
      stop 458
      pattern (subvec values start (inc stop))]
  (println (value-after 1000000000 pattern start)))
