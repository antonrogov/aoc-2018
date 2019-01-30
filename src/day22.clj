(ns day22
  (:require [clojure.test :as test]
            [clojure.string :as str]))

(defn erosion-level [geo-index]
  (mod geo-index 20183))

(defn geo-index [x y prev-cell prev-row target-x target-y]
  (if (or (and (= x 0) (= y 0))
          (and (= x target-x) (= y target-y)))
    0
    (if (= y 0)
      (* x 16807)
      (if (= x 0)
        (* y 48271)
        (* prev-cell (get prev-row x))))))

(defn row-erosion-levels [y prev-row depth target-x target-y]
  (reduce
    (fn [cells x]
      (let [index (geo-index x y (peek cells) prev-row target-x target-y)
            level (erosion-level (+ index depth))]
        (conj cells level)))
    [] (range 0 (inc target-x))))

(defn erosion-levels [depth target-x target-y]
  (reduce
    (fn [rows y]
      (conj rows (row-erosion-levels y (peek rows) depth target-x target-y)))
    [] (range 0 (inc target-y))))

(defn risk-level [depth target-x target-y]
  (reduce + (map #(mod % 3)
                 (flatten (erosion-levels depth target-x target-y)))))

; (defn print-map [depth target-x target-y]
;   (doseq [row (erosion-levels depth target-x target-y)]
;     (doseq [cell row]
;       (print (case (mod cell 3) 0 \. 1 \= 2 \|)))
;     (println)))

; (print-map 510 10 10)
; (println (risk-level 4848 15 700))


(defn immediate-geo-index [[y x] cave]
  (if (or (and (= x 0) (= y 0))
          (and (= [y x] (:target cave))))
    0
    (if (= y 0)
      (* x 16807)
      (if (= x 0)
        (* y 48271)
        nil))))

(defn add-erosion-level-at [pos cave geo-index]
  (let [level (erosion-level (+ geo-index (:depth cave)))]
    (assoc-in cave [:cells pos] level)))

(defn cave-with-erosion-level-at [[y x] cave]
  (if (contains? (:cells cave) [y x])
    cave
    (if-let [geo-index (immediate-geo-index [y x] cave)]
      (add-erosion-level-at [y x] cave geo-index)
      (let [top [(dec y) x]
            left [y (dec x)]
            cave (cave-with-erosion-level-at top cave)
            cave (cave-with-erosion-level-at left cave)
            cells (:cells cave)
            geo-index (* (get cells top) (get cells left))]
        (add-erosion-level-at [y x] cave geo-index)))))


(defn next-current [pending]
  (apply min-key val pending))

(defn adjacent [[y x]]
  (filterv (fn [[y x]] (and (>= x 0) (>= y 0)))
           [[y (dec x)]
            [(dec y) x]
            [y (inc x)]
            [(inc y) x]]))

(defn possible-tools [scell dcell tool]
  (case scell
    0 (case dcell ; equipped either :torch or :climb
        0 [[tool 1]
           [(if (= tool :torch) :climb :torch) 8]]
        1 [[:climb (if (= tool :climb) 1 8)]]
        2 [[:torch (if (= tool :torch) 1 8)]])
    1 (case dcell ; equipped either :climb or :none
        0 [[:climb (if (= tool :climb) 1 8)]]
        1 [[tool 1]
           [(if (= tool :climb) :none :climb) 8]]
        2 [[:none (if (= tool :none) 1 8)]])
    2 (case dcell ; equipped either :torch or :none
        0 [[:torch (if (= tool :torch) 1 8)]]
        1 [[:none (if (= tool :none) 1 8)]]
        2 [[tool 1]
           [(if (= tool :torch) :none :torch) 8]])))

(defn unvisited-neighbors [[[spos tool] dist] cave]
  (reduce
    (fn [[neighbors cave] dpos]
      (let [cave (cave-with-erosion-level-at spos cave)
            cave (cave-with-erosion-level-at dpos cave)
            scell (mod (get (:cells cave) spos) 3)
            dcell (mod (get (:cells cave) dpos) 3)
            moves (mapv
                    (fn [[tool cost]]
                      (if (and (= dpos (:target cave))
                               (not= tool :torch))
                        [[dpos :torch] (+ dist cost 7)]
                        [[dpos tool] (+ dist cost)]))
                    (possible-tools scell dcell tool))
            unvisited (filterv #(not (contains? (:visited cave) (first %)))
                               moves)]
        [(into neighbors unvisited) cave]))
    [[] cave]
    (adjacent spos)))

(defn process-neighbor [neighbor pending]
  (let [[coords new-dist] neighbor]
    (if (or (not (contains? pending coords))
            (< new-dist (get pending coords)))
      (assoc pending coords new-dist)
      pending)))

(defn fastest-path [cave]
  (loop [cave cave
         pending {[[0 0] :torch] 0}
         current nil
         neighbors []]
    (if (empty? pending)
      nil
      (if current
        (if (empty? neighbors)
          (let [[pos dist] current]
            (recur (assoc-in cave [:visited pos] dist)
                   (dissoc pending pos) nil []))
          (recur cave
                 (process-neighbor (first neighbors) pending)
                 current
                 (rest neighbors)))
        (let [current (next-current pending)
              [[pos _] dist] current]
          (if (= pos (:target cave))
            dist
            (let [[neighbors cave] (unvisited-neighbors current cave)]
              (recur cave pending current neighbors))))))))

(defn new-cave [depth target-x target-y]
  {:depth depth
   :target [target-y target-x]
   :cells {}
   :visited {}})

(defn print-map [cave]
  (let [width (apply max (map #(first (key %)) (:cells cave)))
        height (apply max (map #(second (key %)) (:cells cave)))]
  (doseq [y (range (inc height))]
    (doseq [x (range (inc width))]
      (if-let [cell (get (:cells cave) [y x])]
        (print (case (mod cell 3) 0 \. 1 \= 2 \|))
        (print \N)))
    (println))))

; (println (fastest-path (new-cave 510 10 10)))
(println (fastest-path (new-cave 4848 15 700)))
