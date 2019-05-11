(ns day24
  (:require [clojure.test :as test]
            [clojure.string :as str]))

(defn parse-special [special]
  (let [res {:weak [] :immune []}]
    (if (nil? special)
      res
      (->>
        (str/split special #"; ")
        (mapv #(rest (re-matches #"(weak|immune) to (.+)" %)))
        (reduce (fn [acc, [op s]] (assoc acc (keyword op) (str/split s #", ")))
                res)))))

(test/is (= (parse-special "weak to radiation, bludgeoning; immune to slashing")
            {:weak ["radiation", "bludgeoning"] :immune ["slashing"]}))

(defn parse-line [line]
  (let [[_ units health special attack kind initiative]
        (re-matches #"(\d+) units? each with (\d+) hit points? (?:\(([^)]+)\) )?with an attack that does (\d+) (\w+) damage at initiative (\d+)" line)]
    (assoc (parse-special special)
           :units (Integer/parseInt units)
           :health (Integer/parseInt health)
           :attack (Integer/parseInt attack)
           :kind kind
           :initiative (Integer/parseInt initiative))))

(defn parse-group [line id side]
  (assoc (parse-line line)
         :id id
         :side side))

(defn parse-data [lines]
  (loop [lines lines
         groups []
         side nil
         next-id 1]
    (if (empty? lines)
      groups
      (let [line (first lines)
            lines (rest lines)]
        (cond (= line "Immune System:") (recur lines groups :immune next-id)
              (= line "Infection:") (recur lines groups :infection next-id)
              (empty? line) (recur lines groups side next-id)
              :else (recur lines
                           (conj groups (parse-group line next-id side))
                           side
                           (inc next-id)))))))

(defn effective-power [group]
  (* (:units group) (:attack group)))

(defn potential-damage-to [group target]
  (let [damage (effective-power group)
        kind (:kind group)]
    (cond (.contains (:weak target) kind) (* damage 2)
          (.contains (:immune target) kind) 0
          :else damage)))

(defn select-target [group targets]
  (->>
    targets
    (mapv (fn [target] [target (potential-damage-to group target)]))
    (filter #(> (second %) 0))
    (sort-by (fn [[target damage]]
               [(- damage)
                (- (effective-power target))
                (- (:initiative target))]))
    (first)))

(defn select-targets [groups targets]
  (loop [groups (sort-by (fn [group] [(- (effective-power group))
                                      (- (:initiative group))])
                         groups)
         targets (set targets)
         selections {}]
    (if (empty? groups)
      (into (sorted-map-by #(> (:initiative %1) (:initiative %2))) selections)
      (let [group (first groups)
            left (rest groups)
            [target damage] (select-target group targets)]
        (if target
          (recur left (disj targets target)
                      (assoc selections group (:id target)))
          (recur left targets selections))))))

(defn target-selection [immune infection]
  (->
    (sorted-map-by #(> (:initiative %1) (:initiative %2)))
    (into (select-targets immune infection))
    (into (select-targets infection immune))))

(defn attacking [groups selections]
  (loop [groups groups
         selections selections]
    ; (prn groups selections)
    (if (empty? selections)
      (vals groups)
      (let [[group target-id] (first selections)
            group-id (:id group)
            group (get groups group-id)
            target (get groups target-id)
            damage (potential-damage-to group target)
            units-killed (quot damage (:health target))]
        ; (prn group-id target-id damage units-killed)
        (cond (>= units-killed (:units target))
              (recur (dissoc groups target-id)
                     (dissoc selections group target))
              (> units-killed 0)
              (recur (update-in groups [target-id :units] - units-killed)
                     (dissoc selections group))
              :else
              (recur groups (dissoc selections group)))))))

(defn fight [immune infection]
  (let [groups (reduce #(assoc %1 (:id %2) %2) {} (concat immune infection))]
    (attacking groups (target-selection immune infection))))

(defn combat [groups]
  (loop [groups groups]
    (let [immune (filter #(= (:side %) :immune) groups)
          infection (filter #(= (:side %) :infection) groups)]
      (if (or (empty? immune) (empty? infection))
        [(:side (first groups)) (reduce + (map :units groups))]
        (recur (fight immune infection))))))

(defn boost-immune [groups boost]
  (mapv #(if (= (:side %) :immune)
           (update-in % [:attack] + boost) %) groups))

(defn combat-with-immune-boost [groups boost]
  (combat (boost-immune groups boost)))

(let [groups (parse-data
               ["Immune System:"
                "17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2"
                "989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3"
                ""
                "Infection:"
                "801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1"
                "4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4"])]
  (test/is (= (combat groups) [:infection 5216]))
  (test/is (= (combat-with-immune-boost groups 1570) [:immune 51])))

(prn (combat-with-immune-boost (parse-data (str/split (slurp "data/day24.txt") #"\n")) 49))
