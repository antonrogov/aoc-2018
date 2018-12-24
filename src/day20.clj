(ns day20
  (:require [clojure.test :as test]
            [clojure.string :as str]))


(defn longest-path [s]
  (loop [s s
         len 0]
    (let [c (first s)
          next-s (rest s)]
      (cond (or (= c \$)
                (= c \))) [len next-s]
            (= c \|) (let [[alt-len next-s] (longest-path next-s)]
                       [(if (or (= len 0) (= alt-len 0))
                          0 (max len alt-len))
                        next-s])
            (= c \() (let [[sub-len next-s] (longest-path (rest s))]
                       (recur next-s (+ len sub-len)))
            (= c \^) (recur next-s len)
            :else (recur (rest s) (inc len))))))

(test/is (= (first (longest-path "^WNE$")) 3))
(test/is (= (first (longest-path "^ENWWW(NEEE|SSE(EE|N))$")) 10))
(test/is (= (first (longest-path "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$")) 18))
(test/is (= (first (longest-path "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$")) 23))
(test/is (= (first (longest-path "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$")) 31))

; (println (first (longest-path (slurp "data/day20.txt"))))



(defn new-room []
  [false false false false])

(defn room-doors [rooms pos]
  (get rooms pos (new-room)))

(defn add-door [rooms pos dir]
  (let [doors (room-doors rooms pos)]
    (assoc rooms pos (assoc doors dir true))))

(defn has-door? [rooms x y dir]
  (let [[dir2 x2 y2] (case dir 0 [2 x (dec y)]
                              1 [3 (inc x) y]
                              2 [0 x (inc y)]
                              3 [1 (dec x) y])]
    (or (get (room-doors rooms [y x]) dir)
        (get (room-doors rooms [y2 x2]) dir2))))


(defn build-sub-map [s rooms prev-pos]
  (loop [s s
         rooms rooms
         pos prev-pos
         ends []]
    (let [c (first s)
          next-s (rest s)]
      (cond (= c \$) (if (empty? ends)
                       [rooms next-s []]
                       (let [[new-pos next-s] (peek ends)]
                         (recur next-s rooms new-pos (pop ends))))
            (= c \)) [rooms next-s (conj ends [pos next-s])]
            (= c \|) (let [[rooms next-s alt-ends]
                           (build-sub-map next-s rooms prev-pos)]
                       [rooms next-s (into (conj ends [pos next-s]) alt-ends)])
            (= c \() (let [[rooms next-s sub-ends]
                           (build-sub-map next-s rooms pos)
                           [new-pos _] (peek sub-ends)
                           new-ends (mapv (fn [[pos _]] [pos next-s])
                                          (pop sub-ends))]
                       (recur next-s rooms new-pos (into ends new-ends)))
            (= c \^) (recur next-s rooms pos ends)
            (= c \W) (let [[y x] pos
                           new-pos [y (dec x)]]
                       (recur next-s (add-door rooms new-pos 1) new-pos ends))
            (= c \E) (let [[y x] pos
                           new-pos [y (inc x)]]
                       (recur next-s (add-door rooms new-pos 3) new-pos ends))
            (= c \N) (let [[y x] pos
                           new-pos [(dec y) x]]
                       (recur next-s (add-door rooms new-pos 2) new-pos ends))
            (= c \S) (let [[y x] pos
                           new-pos [(inc y) x]]
                       (recur next-s (add-door rooms new-pos 0) new-pos ends))
            :else (throw (Exception. (str "unknown char" c)))))))

(defn build-map [s]
  (first (build-sub-map s {[0 0] (new-room)} [0 0])))


(defn print-map [rooms]
  (let [coords (keys rooms)
        xs (map second coords)
        ys (map first coords)
        min-x (apply min xs)
        max-x (apply max xs)
        min-y (apply min ys)
        max-y (apply max ys)]
    (doseq [y (range min-y (inc max-y))]
      (println
        (apply str
               (conj
                 (vec
                   (flatten
                     (for [x (range min-x (inc max-x))]
                       [\# (if (has-door? rooms x y 0) \- \#)])))
                 \#)))
      (println
        (apply str
               (conj
                 (vec
                   (flatten
                     (for [x (range min-x (inc max-x))]
                       [(if (has-door? rooms x y 3) \| \#)
                        (if (and (= x 0) (= y 0))
                          \X
                          (if-let [cell (get rooms [y x])]
                            \. \#))])))
                 (if (has-door? rooms max-x y 1) \| \#)))))
    (println
      (apply str
             (conj
               (vec
                 (flatten
                   (for [x (range min-x (inc max-x))]
                     [\# (if (has-door? rooms x max-y 2) \- \#)])))
               \#)))))


(defn process-neighbor [neighbor current unvisited]
  (let [[tpos tdist] neighbor
        [pos dist] current
        new-dist (inc dist)]
    (if (or (nil? tdist) (< new-dist tdist))
      (assoc unvisited tpos new-dist)
      unvisited)))

(defn next-current [unvisited]
  (apply min-key val
         (filter #(not (nil? (val %))) unvisited)))

(defn adjacent [rooms pos]
  (let [[y x] pos
        n (if (has-door? rooms x y 0) [(dec y) x] nil)
        e (if (has-door? rooms x y 1) [y (inc x)] nil)
        s (if (has-door? rooms x y 2) [(inc y) x] nil)
        w (if (has-door? rooms x y 3) [y (dec x)] nil)]
    (filter #(not (nil? %)) [n s e w])))

(defn unvisited-neighbors [[pos value] rooms unvisited]
  (reduce (fn [acc pos]
            (let [value (get unvisited pos :none)]
              (if (= value :none)
                acc
                (assoc acc pos value))))
          {} (adjacent rooms pos)))

(defn count-rooms-further-than [rooms limit]
  (loop [unvisited (reduce #(assoc %1 (key %2)
                                   (if (= (key %2) [0 0]) 0 nil))
                           {} rooms)
         distances {}
         current nil
         neighbors []]
    (if (or (empty? unvisited)
            (every? #(nil? (val %)) unvisited))
      (reduce #(if (>= (val %2) limit) (inc %1) %1) 0 distances)
      (if current
        (if (empty? neighbors)
          (let [[pos value] current]
            (recur (dissoc unvisited pos) (assoc distances pos value) nil []))
          (recur (process-neighbor (first neighbors) current unvisited)
                 distances
                 current
                 (rest neighbors)))
        (let [current (next-current unvisited)
              neighbors (unvisited-neighbors current rooms unvisited)]
          (recur unvisited distances current neighbors))))))


; (print-map (build-map' "^WNE$"))
; (print-map (build-map' "^N(E|W)N$"))
; (print-map (build-map' "^ENWWW(NEEE|SSE(EE|N))$"))
; (println (calc-distance (build-map "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$") 1))
; (print-map (build-map' "^ENNWSWW(NEWS|)$"))
(println (count-rooms-further-than (build-map (slurp "data/day20.txt")) 1000))
