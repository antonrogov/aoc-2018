(ns day2)

(defn box-id-checksum [box-ids]
  (apply *
         (reduce (fn [[twos threes] [two three]]
                   [(+ twos two) (+ threes three)])
                 [0 0]
                 (map (fn [box-id]
                        (let [fs (frequencies box-id)
                              has-two (some (fn [[c n]] (= n 2)) fs)
                              has-three (some (fn [[c n]] (= n 3)) fs)]
                          [(if has-two 1 0) (if has-three 1 0)]))
                      box-ids))))

(prn (= (box-id-checksum ["abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab"]) 12))


(defn common-part [s1 s2]
  (apply str
         (map first
              (filter (fn [[a b]] (= a b))
                      (map vector (seq s1) (seq s2))))))

(defn common-letters-of-correct-box-ids [[box-id & box-ids]]
  (if-let [common (some #(if (= (count %) (dec (count box-id))) %)
                        (map #(common-part box-id %) box-ids))]
    common
    (common-letters-of-correct-box-ids box-ids)))

(prn (= (common-letters-of-correct-box-ids ["abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz"]) "fgij"))


(with-open [rdr (clojure.java.io/reader "data/day2.txt")]
  (println (common-letters-of-correct-box-ids (line-seq rdr))))
