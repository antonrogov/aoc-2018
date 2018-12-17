(require '[clojure.test :as test])

(defn parse-guard-log [entries]
  (loop [remaining entries
         log []]
    (let [entry (first remaining)]
      (if (or (empty? remaining) (re-find #"begins shift" entry))
        [log remaining]
        (let [next-remaining (rest remaining)
              [_ start] (re-find #":(\d+)] falls asleep" entry)
              [_ stop] (re-find #":(\d+)] wakes up" (first next-remaining))]
          (if (and start stop)
            (recur (rest next-remaining)
                   (conj log (map #(Integer/parseInt %) [start stop])))
            (recur next-remaining log)))))))

(defn parse-log [entries]
  (loop [remaining entries
         log {}]
    (if (empty? remaining)
      log
      (let [entry (first remaining)]
        (if-let [[_ id] (re-find #"Guard #(\d+) begins shift" entry)]
          (let [guard-id (Integer/parseInt id)
                [guard-log next-lines] (parse-guard-log (rest remaining))
                old-log (get log guard-id [])
                new-log (concat old-log guard-log)]
            (recur next-lines
                   (assoc log guard-id new-log)))
          (recur (rest remaining) log))))))

(defn most-sleepy-guard [log]
  (apply max-key
         (fn [[_ guard-log]]
           (reduce + (map (fn [[start stop]] (- stop start)) guard-log)))
         log))

(defn most-sleepy-minute [guard-log]
  (if (empty? guard-log)
    [nil 0]
    (apply max-key val
           (reduce (fn [counts minute]
                     (assoc counts minute (inc (get counts minute 0))))
                   {} (flatten (map #(apply range %) guard-log))))))

(defn most-sleepy-guard-strategy [log]
  (let [[id guard-log] (most-sleepy-guard log)
        minute (key (most-sleepy-minute guard-log))]
    (* id minute)))

(test/is (=
           (most-sleepy-guard-strategy
             (parse-log ["[1518-11-01 00:00] Guard #10 begins shift"
                         "[1518-11-01 00:05] falls asleep"
                         "[1518-11-01 00:25] wakes up"
                         "[1518-11-01 00:30] falls asleep"
                         "[1518-11-01 00:55] wakes up"
                         "[1518-11-01 23:58] Guard #99 begins shift"
                         "[1518-11-02 00:40] falls asleep"
                         "[1518-11-02 00:50] wakes up"
                         "[1518-11-03 00:05] Guard #10 begins shift"
                         "[1518-11-03 00:24] falls asleep"
                         "[1518-11-03 00:29] wakes up"
                         "[1518-11-04 00:02] Guard #99 begins shift"
                         "[1518-11-04 00:36] falls asleep"
                         "[1518-11-04 00:46] wakes up"
                         "[1518-11-05 00:03] Guard #99 begins shift"
                         "[1518-11-05 00:45] falls asleep"
                         "[1518-11-05 00:55] wakes up"])) 240))


(defn most-sleepy-minute-strategy [log]
  (let [minutes (map (fn [[id guard-log]]
                       (let [[minute times] (most-sleepy-minute guard-log)]
                         {:id id :minute minute :times times})) log)
        minute (apply max-key :times minutes)]
    (* (:id minute) (:minute minute))))

(test/is (=
           (most-sleepy-minute-strategy
             (parse-log ["[1518-11-01 00:00] Guard #10 begins shift"
                         "[1518-11-01 00:05] falls asleep"
                         "[1518-11-01 00:25] wakes up"
                         "[1518-11-01 00:30] falls asleep"
                         "[1518-11-01 00:55] wakes up"
                         "[1518-11-01 23:58] Guard #99 begins shift"
                         "[1518-11-02 00:40] falls asleep"
                         "[1518-11-02 00:50] wakes up"
                         "[1518-11-03 00:05] Guard #10 begins shift"
                         "[1518-11-03 00:24] falls asleep"
                         "[1518-11-03 00:29] wakes up"
                         "[1518-11-04 00:02] Guard #99 begins shift"
                         "[1518-11-04 00:36] falls asleep"
                         "[1518-11-04 00:46] wakes up"
                         "[1518-11-05 00:03] Guard #99 begins shift"
                         "[1518-11-05 00:45] falls asleep"
                         "[1518-11-05 00:55] wakes up"])) 4455))

(with-open [rdr (clojure.java.io/reader "day4.txt")]
  (println (most-sleepy-minute-strategy (parse-log (sort (line-seq rdr))))))
