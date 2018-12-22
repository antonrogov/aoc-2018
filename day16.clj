(ns aoc.day16
  (:require [clojure.test :as test]
            [clojure.string :as str]
            [clojure.set :as cset]))


(def ops {:addr (fn [regs a b c] (assoc regs c (+ (get regs a) (get regs b))))
          :addi (fn [regs a b c] (assoc regs c (+ (get regs a) b)))
          :mulr (fn [regs a b c] (assoc regs c (* (get regs a) (get regs b))))
          :muli (fn [regs a b c] (assoc regs c (* (get regs a) b)))
          :banr (fn [regs a b c] (assoc regs c (bit-and (get regs a) (get regs b))))
          :bani (fn [regs a b c] (assoc regs c (bit-and (get regs a) b)))
          :borr (fn [regs a b c] (assoc regs c (bit-or (get regs a) (get regs b))))
          :bori (fn [regs a b c] (assoc regs c (bit-or (get regs a) b)))
          :setr (fn [regs a b c] (assoc regs c (get regs a)))
          :seti (fn [regs a b c] (assoc regs c a))
          :gtir (fn [regs a b c] (assoc regs c (if (> a (get regs b)) 1 0)))
          :gtri (fn [regs a b c] (assoc regs c (if (> (get regs a) b) 1 0)))              ; 11
          :gtrr (fn [regs a b c] (assoc regs c (if (> (get regs a) (get regs b)) 1 0)))   ; 9
          :eqir (fn [regs a b c] (assoc regs c (if (= a (get regs b)) 1 0)))              ; 3
          :eqri (fn [regs a b c] (assoc regs c (if (= (get regs a) b) 1 0)))
          :eqrr (fn [regs a b c] (assoc regs c (if (= (get regs a) (get regs b)) 1 0)))}) ; 1


(defn parse-line [line]
  (mapv #(Integer/parseInt %) (re-seq #"\d+" line)))


(defn parse-op [block]
  (let [[before command after] (str/split block #"\n")]
    {:before (parse-line before)
     :command (parse-line command)
     :after (parse-line after)}))


(defn possible-ops [sample]
  (let [[_ a b c] (:command sample)]
    (set (mapv first
               (filter
                 (fn [[_ f]]
                   (= (f (:before sample) a b c) (:after sample)))
                 ops)))))

(defn count-possible-ops [sample]
  (count (possible-ops sample)))

(test/is (= (count-possible-ops
              (parse-op "Before: [3, 2, 1, 1]\n9 2 1 2\nAfter:  [3, 2, 2, 1]"))
            3))


(defn guess-ops [samples known-ops]
  (reduce
    #(let [code (first (:command %2))
           matches (cset/difference (possible-ops %2) known-ops)]
       (if (= (count matches) 1)
         (assoc %1 code (first matches))
         %1))
    (hash-map)
    samples))


(defn map-ops [samples]
  (loop [known (hash-map)]
    (if (= (count known) 16)
      known
      (let [guessed (guess-ops samples (set (vals known)))]
        (recur (merge known guessed))))))


(let [[samples code] (str/split (slurp "day16.txt") #"\n\n\n\n")
      code (map parse-line (str/split (str/trim-newline code) #"\n"))
      samples (map parse-op (str/split samples #"\n\n"))
      codes (map-ops samples)]
  (println
    (first
      (reduce
        (fn [regs [op a b c]]
          (let [op-name (get codes op)
                op (get ops op-name)]
            (op regs a b c)))
        [0 0 0 0]
        code))))
