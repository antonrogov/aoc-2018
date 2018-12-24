(ns day1)

(defn resulting-frequency [signals]
  (reduce + 0 signals))

(defn find-duplicate-frequency [signals]
  (reduce (fn [fs f]
            (if (contains? fs f)
              (reduced f)
              (conj fs f)))
          #{} (reductions + 0 (cycle signals))))

(prn (= (find-duplicate-frequency [3 3 4 -2 -4]) 10))
(prn (= (find-duplicate-frequency [-6 3 8 5 -6]) 5))

(with-open [rdr (clojure.java.io/reader "data/day1.txt")]
  (println (find-duplicate-frequency (map #(Integer/parseInt %) (line-seq rdr)))))
