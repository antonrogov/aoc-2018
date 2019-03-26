(ns day23
  (:require [clojure.test :as test]
            [clojure.string :as str]))

(defn parse-line [line]
  (mapv #(Integer/parseInt %) (re-seq #"[-\d]+" line)))

(test/is (= (parse-line "pos=<1,2,3>, r=4") [1 2 3 4]))

(defn parse-data [data]
  (mapv parse-line (str/split data #"\n")))


(defn strongest [bots]
  (apply max-key #(peek %) bots))

(defn distance [[x1 y1 z1] [x2 y2 z2]]
  (+ (Math/abs (- x1 x2)) (Math/abs (- y1 y2)) (Math/abs (- z1 z2))))

(defn point-in-range? [point [bx by bz br]]
  (<= (distance point [bx by bz]) br))

(defn count-in-range [bot bots]
  (count (filterv #(point-in-range? (take 3 %) bot) bots)))

(defn part1 [data]
  (let [bots (parse-data data)]
    (count-in-range (strongest bots) bots)))

(test/is (= (part1 (slurp "data/day23.txt")) 248))

(defn bot-coords-range [bot index]
  (let [v (get bot index) r (get bot 3)]
    [(+ v r) (- v r)]))

(defn coords-range [bots index]
  (let [coords (->> bots
                    (mapv #(bot-coords-range % index))
                    (flatten))]
    [(apply min coords) (apply max coords)]))

(defn ranges-box [bots]
  ; (for [i [0 1 2]
  ;       op [min max]
  ;       :let [vs (mapv #(get % i) bots)]
  (let [[min-x max-x] (coords-range bots 0)
        [min-y max-y] (coords-range bots 1)
        [min-z max-z] (coords-range bots 2)]
    [min-x max-x min-y max-y min-z max-z]))

(defn distance-to-box [[x y z] [min-x max-x min-y max-y min-z max-z]]
  (let [dx (cond (< x min-x) (- min-x x)
                 (> x max-x) (- x max-x)
                 :else 0)
        dy (cond (< y min-y) (- min-y y)
                 (> y max-y) (- y max-y)
                 :else 0)
        dz (cond (< z min-z) (- min-z z)
                 (> z max-z) (- z max-z)
                 :else 0)]
    (+ dx dy dz)))

(defn box-in-range? [[min-x max-x min-y max-y min-z max-z] [x y z r]]
  (<= (distance-to-box [x y z] [min-x max-x min-y max-y min-z max-z]) r))

(defn split-range [a b]
  (if (> (- b a) 1)
    (let [d (quot (Math/abs (- b a)) 2)
          o (+ a d)]
      [[a o] [(inc o) b]])
    [[a a] [b b]]))

(defn divide-box [[min-x max-x min-y max-y min-z max-z]]
  (for [[x1 x2] (split-range min-x max-x)
        [y1 y2] (split-range min-y max-y)
        [z1 z2] (split-range min-z max-z)]
    [x1 x2 y1 y2 z1 z2]))

(defn box-size [[min-x max-x min-y max-y min-z max-z]]
  [(- max-x min-x) (- max-y min-y) (- max-z min-z)])

(defn add-priority [box bots]
  [[(- (count (filterv #(box-in-range? box %) bots)))
    (distance-to-box [0 0 0] box)
    (reduce + (box-size box))]
   box])

(defn subdivide [box bots]
  (->>
    (divide-box box)
    (distinct)
    (mapv #(add-priority % bots))))

(defn min-dist-with-max-bots [bots box]
  (loop [boxes (sorted-map [0 0 -1] box)]
    (let [[[_ dist size :as p] box] (first boxes)]
      (if (= size 0)
        dist
        (recur (into (dissoc boxes p) (subdivide box bots)))))))

(defn part2 [data]
  (let [bots (parse-data data)
        box (ranges-box bots)]
    (min-dist-with-max-bots bots box)))

(test/is (= (part2 "pos=<10,12,12>, r=2
                    pos=<12,14,12>, r=2
                    pos=<16,12,12>, r=4
                    pos=<14,14,14>, r=6
                    pos=<50,50,50>, r=200
                    pos=<10,10,10>, r=5") 36))

(test/is (= (part2 (slurp "data/day23.txt")) 124623002))
