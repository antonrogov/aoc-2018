(require '[clojure.test :as test])

(defn claiming-points [claim]
  (let [left (:left claim)
        top (:top claim)]
    (apply concat
      (for [dx (range 0 (:width claim))]
        (for [dy (range 0 (:height claim))]
          [(+ left dx) (+ top dy)])))))

(defn claim-point [claimed point]
  (let [old-value (get claimed point)
        new-value (if old-value (inc old-value) 1)]
    (assoc claimed point new-value)))

(defn claim-all-points [claims]
  (reduce claim-point {} (apply concat (map claiming-points claims))))

(defn count-overlapping-points [claims]
  (count
    (filter (fn [[point n]] (> n 1))
      (claim-all-points claims))))

(test/is (= (count-overlapping-points [{:id 1 :left 1 :top 3 :width 4 :height 4 }
                                       {:id 2 :left 3 :top 1 :width 4 :height 4 }
                                       {:id 3 :left 5 :top 5 :width 2 :height 2 }]) 4))


(defn first-non-overlapping-claim-id [claims]
  (let [claimed-points (claim-all-points claims)]
    (some (fn [claim]
            (if (every? #(= (get claimed-points %) 1)
                        (claiming-points claim))
              (:id claim)))
          claims)))

(test/is
      (= (first-non-overlapping-claim-id [{:id 1 :left 1 :top 3 :width 4 :height 4 }
                                          {:id 2 :left 3 :top 1 :width 4 :height 4 }
                                          {:id 3 :left 5 :top 5 :width 2 :height 2 }]) 3))

(defn parse-claim [s]
  (let [[id left top width height]
        (map #(Integer/parseInt %)
             (rest (re-matches #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" s)))]
    {:id id :left left :top top :width width :height height}))

(test/is (= (parse-claim "#1 @ 258,327: 19x22")
            {:id 1 :left 258 :top 327 :width 19 :height 22}))

(with-open [rdr (clojure.java.io/reader "day3.txt")]
  (println (first-non-overlapping-claim-id (map parse-claim (line-seq rdr)))))
