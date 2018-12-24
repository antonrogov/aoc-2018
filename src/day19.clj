(ns day19
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
          :gtri (fn [regs a b c] (assoc regs c (if (> (get regs a) b) 1 0)))
          :gtrr (fn [regs a b c] (assoc regs c (if (> (get regs a) (get regs b)) 1 0)))
          :eqir (fn [regs a b c] (assoc regs c (if (= a (get regs b)) 1 0)))
          :eqri (fn [regs a b c] (assoc regs c (if (= (get regs a) b) 1 0)))
          :eqrr (fn [regs a b c] (assoc regs c (if (= (get regs a) (get regs b)) 1 0)))})


(defn run
  ([prog] (run prog (vec (repeat 6 0))))
  ([[code ireg] regs]
   (loop [regs regs]
     (if-let [[op-name a b c] (get code (get regs ireg))]
       (let [op (get ops op-name)
             new-regs (op regs a b c)
             fixed-regs (if (= (get new-regs 1) 10551260)
                          (assoc new-regs 1 3)
                          regs)]
         (recur (update new-regs ireg inc)))
       (first regs)))))

(defn parse-line [line]
  (let [[op-name a b c] (str/split line #"\s+")]
    [(keyword op-name)
     (Integer/parseInt a)
     (Integer/parseInt b)
     (Integer/parseInt c)]))

(defn parse-code [lines]
  [(mapv parse-line (rest lines))
   (Integer/parseInt (re-find #"\d+" (first lines)))])
(test/is (= (run (parse-code ["#ip 0"
                              "seti 5 0 1"
                              "seti 6 0 2"
                              "addi 0 1 0"
                              "addr 1 2 3"
                              "setr 1 0 0"
                              "seti 8 0 4"
                              "seti 9 0 5"])) 7))

; (prn (run (parse-code (str/split (slurp "data/day19.txt") #"\n")) [1 0 0 0 0 0]))

(defn quant [n]
  (reduce + (filter #(= (mod n %) 0) (range 1 (inc n)))))

(println (quant 10551260))
