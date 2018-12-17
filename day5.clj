(ns aoc.day5
  (:require [clojure.test :as test]
            [clojure.string :as str]))

(defn react [polymer]
  (loop [remaining (seq polymer)
         result []]
    (if (empty? remaining)
      (str/join result)
      (let [prev (peek result)
            curr (first remaining)]
        (if (and (not= prev curr)
                 (= (str/upper-case (str prev))
                    (str/upper-case (str curr))))
          (recur (rest remaining) (pop result))
          (recur (rest remaining) (conj result curr)))))))

(defn shortest-polymer-without-one-type [polymer]
  (apply min
    (map
      (fn [t]
        (count (react (filter #(not= (str t) (str/lower-case %)) polymer))))
      (distinct (str/lower-case polymer))))
  )

(test/is (= (react "aA") ""))
(test/is (= (react "aBbA") ""))
(test/is (= (react "abAB") "abAB"))
(test/is (= (react "aabAAB") "aabAAB"))
(test/is (= (react "dabAcCaCBAcCcaDA") "dabCBAcaDA"))

(test/is (= (shortest-polymer-without-one-type "dabAcCaCBAcCcaDA") 4))

(println (shortest-polymer-without-one-type (str/trim (slurp "day5.txt"))))
