(ns aoc.day15
  (:require [clojure.test :as test]
            [clojure.string :as str]))


(defn new-unit [t x y]
  {:type t :x x :y y :health 200})


(defn parse-line [line y]
  (reduce
    (fn [[cells units] [c x]]
      (if (contains? #{\E \G} c)
        [(conj cells \.) (conj units (new-unit c x y))]
        [(conj cells c) units]))
    [[] []]
    (mapv vector (seq line) (range))))


(defn parse-map [lines]
  (reduce (fn [[field units] [line y]]
            (let [[row row-units] (parse-line line y)]
              [(conj field row) (into units row-units)]))
          [[] []]
          (mapv vector lines (range))))

(test/is (= (parse-map ["####"
                        "#.G#"
                        "#E.#"
                        "####"]) [[[\# \# \# \#]
                                   [\# \. \. \#]
                                   [\# \. \. \#]
                                   [\# \# \# \#]] [(new-unit \G 2 1)
                                                   (new-unit \E 1 2)]]))

(defn cell-open? [[y x] field]
  (= (get (get field y []) x nil) \.))


(defn any-unit-at? [units x y]
  (some #(and (= x (:x %)) (= y (:y %))) units))


(defn initial-distances [field units [sy sx]]
  (reduce (fn [acc [row y]]
            (merge
              acc
              (reduce (fn [cells [c x]]
                        (if (and (= c \.)
                                 (not (any-unit-at? units x y)))
                          (let [value (if (and (= x sx) (= y sy))
                                        [0 [nil]] [nil []])]
                                        ; [0 [[]]] [nil []])]
                            (assoc cells [y x] value))
                          cells))
                      {}
                      (map vector row (range)))))
          {}
          (map vector field (range))))


(defn adjacent [[y x]]
  [[(dec y) x]
   [y (dec x)]
   [y (inc x)]
   [(inc y) x]])

(defn next-current [unvisited]
  (apply min-key #(first (val %))
         (filter #(not (nil? (first (val %)))) unvisited)))

(defn unvisited-neighbors [[pos value] unvisited]
  (reduce (fn [acc pos]
            (if-let [value (get unvisited pos)]
              (assoc acc pos value)
              acc))
          {} (adjacent pos)))

(defn unit-pos [unit]
  [(:y unit) (:x unit)])


(defn process-neighbor [neighbor current unvisited]
  (let [[tpos [tdist tpaths]] neighbor
        [pos [dist paths]] current
        new-dist (inc dist)]
    (if (or (nil? tdist) (< new-dist tdist))
      (assoc unvisited tpos [new-dist (set (mapv #(if (nil? %) tpos %) paths))])
      (if (= new-dist tdist)
        (assoc unvisited tpos [new-dist (into tpaths
                                              (mapv #(if (nil? %) tpos %) paths))])
        unvisited))))


(defn paths-to [targets field units start]
  (loop [unvisited (initial-distances field units start)
         remaining (set targets)
         paths {}
         current nil
         neighbors []]
    (if (or (empty? remaining) (every? #(nil? (first (val %))) unvisited))
      paths
      (if current
        (if (empty? neighbors)
          (let [[pos value] current]
            (if (contains? remaining pos)
              (recur (dissoc unvisited pos)
                     (disj remaining pos)
                     (assoc paths pos value)
                     nil [])
              (recur (dissoc unvisited pos) remaining paths nil [])))
          (recur (process-neighbor (first neighbors) current unvisited)
                 remaining
                 paths
                 current
                 (rest neighbors)))
        (let [current (next-current unvisited)
              neighbors (unvisited-neighbors current unvisited)]
          (recur unvisited remaining paths current neighbors))))))


(defn distance [{ax :x ay :y} {bx :x by :y}]
  (+ (Math/abs (- ax bx))
     (Math/abs (- ay by))))


(defn next-step [paths]
  (if (empty? paths)
    nil
    (get
      (first
        (sort
          (apply concat
                 (map (fn [[dst [len ps]]]
                        (map #(vector len dst %) ps)) paths)))) 2)))


(defn move [unit step]
  (if step
    (assoc unit :x (second step) :y (first step))
    unit))


(defn first-target [targets]
  (val (first (reduce #(assoc %1 [(:health %2) (:y %2) (:x %2)] %2)
                      (sorted-map)
                      targets))))


(defn elf? [unit]
  (= (:type unit) \E))


(defn attack [unit target elven-power]
  (let [attack (if (elf? unit) elven-power 3)
        health (:health target)]
    (assoc target :health (- health attack))))


(defn add-unit-to [m unit]
  (assoc m (unit-pos unit) unit))


(defn outcome [turn units]
  (* turn (reduce + (map #(:health (val %)) units))))


(defn print-field [turn field units]
  (println turn
           (str/join ", "
                (mapv (fn [[pos unit]] (str (:type unit) (:health unit)))
                      units)))
  (doseq [[row y] (map vector field (range))]
    (doseq [[c x] (map vector row (range))]
      (if-let [unit (get units [y x])]
        (print (:type unit))
        (print c)))
    (println)))


(defn process-attack [unit targets elven-power]
  (let [targets-in-range (filter #(= 1 (distance unit %)) targets)]
    (if (empty? targets-in-range)
      nil
      (let [target (first-target targets-in-range)]
        (attack unit target elven-power)))))


(defn process-turn [unit targets field other-units elven-power]
  (if-let [target (process-attack unit targets elven-power)]
    [unit target]
    (let [adj-cells (vec (apply concat
                                (mapv #(adjacent (unit-pos %))
                                      targets)))
          target-cells (filterv #(cell-open? % field) adj-cells)
          pos (unit-pos unit)
          paths (paths-to target-cells field other-units pos)
          step (next-step paths)
          unit (move unit step)]
      [unit (process-attack unit targets elven-power)])))


(defn process [[field units] elven-power]
  (loop [queue (reduce #(add-unit-to %1 %2) (sorted-map) units)
         played (sorted-map)
         turn 0]
    (if (empty? queue)
      (recur played (sorted-map) (inc turn))
      (let [[pos unit] (first queue)
            rest-queue (dissoc queue pos)
            other-units (vals (merge played rest-queue))
            targets (filterv #(not= (:type unit) (:type %)) other-units)]
        (if (empty? targets)
          (outcome turn (merge played queue))
          (let [[unit target] (process-turn unit targets field other-units elven-power)]
            (if target
              (let [tpos (unit-pos target)
                    dead (< (:health target) 0)]
                (if (get rest-queue tpos)
                  (let [new-queue (dissoc rest-queue tpos)]
                    (recur (if dead new-queue (assoc new-queue tpos target))
                           (add-unit-to played unit)
                           turn))
                  (let [new-played (add-unit-to (dissoc played tpos) unit)]
                    (recur rest-queue
                           (if dead new-played (assoc new-played tpos target))
                           turn))))
              (recur rest-queue (add-unit-to played unit) turn))))))))

(test/is (= (process (parse-map ["####"
                                 "#.G#"
                                 "#E.#"
                                 "####"]) 3) 134))
(test/is (= (process (parse-map ["#######"
                                 "#.G...#"
                                 "#...EG#"
                                 "#.#.#G#"
                                 "#..G#E#"
                                 "#.....#"
                                 "#######"]) 3) 27730))
(test/is (= (process (parse-map ["#######"
                                 "#G..#E#"
                                 "#E#E.E#"
                                 "#G.##.#"
                                 "#...#E#"
                                 "#...E.#"
                                 "#######"]) 3) 36334))
(test/is (= (process (parse-map ["#######"
                                 "#E..EG#"
                                 "#.#G.E#"
                                 "#E.##E#"
                                 "#G..#.#"
                                 "#..E#.#"
                                 "#######"]) 3) 39514))
(test/is (= (process (parse-map ["#######"
                                 "#E.G#.#"
                                 "#.#G..#"
                                 "#G.#.G#"
                                 "#G..#.#"
                                 "#...E.#"
                                 "#######"]) 3) 27755))
(test/is (= (process (parse-map ["#######"
                                 "#.E...#"
                                 "#.#..G#"
                                 "#.###.#"
                                 "#E#G#G#"
                                 "#...#G#"
                                 "#######"]) 3) 28944))
(test/is (= (process (parse-map ["#########"
                                 "#G......#"
                                 "#.E.#...#"
                                 "#..##..G#"
                                 "#...##..#"
                                 "#...#...#"
                                 "#.G...G.#"
                                 "#.....G.#"
                                 "#########"]) 3) 18740))


(defn elven-victory-with-lowest-power [[field units]]
  (let [units-map (reduce #(add-unit-to %1 %2) (sorted-map) units)]
    (loop [queue units-map
           played (sorted-map)
           turn 0
           elven-power 4]
      (if (empty? queue)
        (recur played (sorted-map) (inc turn) elven-power)
        (let [[pos unit] (first queue)
              rest-queue (dissoc queue pos)
              other-units (vals (merge played rest-queue))
              targets (filterv #(not= (:type unit) (:type %)) other-units)]
          (if (empty? targets)
            (outcome turn (merge played queue))
            (let [[unit target] (process-turn unit targets field other-units elven-power)]
              (if target
                (let [tpos (unit-pos target)
                      dead (< (:health target) 0)]
                  (if (and dead (elf? target))
                    (recur units-map (sorted-map) 0 (inc elven-power))
                    (if (get rest-queue tpos)
                      (let [new-queue (dissoc rest-queue tpos)]
                        (recur (if dead new-queue (assoc new-queue tpos target))
                               (add-unit-to played unit)
                               turn elven-power))
                      (let [new-played (add-unit-to (dissoc played tpos) unit)]
                        (recur rest-queue
                               (if dead new-played (assoc new-played tpos target))
                               turn elven-power)))))
                  (recur rest-queue
                         (add-unit-to played unit)
                         turn elven-power)))))))))

(test/is (= (elven-victory-with-lowest-power (parse-map ["#######"
                                                         "#.G...#"
                                                         "#...EG#"
                                                         "#.#.#G#"
                                                         "#..G#E#"
                                                         "#.....#"
                                                         "#######"])) 4988))
(test/is (= (elven-victory-with-lowest-power (parse-map ["#######"
                                                         "#E..EG#"
                                                         "#.#G.E#"
                                                         "#E.##E#"
                                                         "#G..#.#"
                                                         "#..E#.#"
                                                         "#######"])) 32028))
(test/is (= (elven-victory-with-lowest-power (parse-map ["#######"
                                                         "#E.G#.#"
                                                         "#.#G..#"
                                                         "#G.#.G#"
                                                         "#G..#.#"
                                                         "#...E.#"
                                                         "#######"])) 3478))
(test/is (= (elven-victory-with-lowest-power (parse-map ["#######"
                                                         "#.E...#"
                                                         "#.#..G#"
                                                         "#.###.#"
                                                         "#E#G#G#"
                                                         "#...#G#"
                                                         "#######"])) 6474))
(test/is (= (elven-victory-with-lowest-power (parse-map ["#########"
                                                         "#G......#"
                                                         "#.E.#...#"
                                                         "#..##..G#"
                                                         "#...##..#"
                                                         "#...#...#"
                                                         "#.G...G.#"
                                                         "#.....G.#"
                                                         "#########"])) 1140))


(with-open [rdr (clojure.java.io/reader "day15.txt")]
  (println (time (elven-victory-with-lowest-power (parse-map (line-seq rdr))))))
