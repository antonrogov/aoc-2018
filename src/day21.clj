(ns day21
  (:require [clojure.test :as test]
            [clojure.string :as str]))

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

(defn run [[code ireg] regs limit]
  (loop [regs regs
         iter 1]
    (if (and (not (nil? limit))
             (>= iter limit))
      [false regs]
      (let [line (get regs ireg)]
        (if-let [[op-name a b c] (get code line)]
          (let [op (get ops op-name)
                new-regs (op regs a b c)]
            (when (= line 28)
              (prn iter regs op-name a b c))
            (recur (update new-regs ireg inc) (inc iter)))
          [true iter])))))

(defn parse-line [line]
  (let [[op-name a b c] (str/split line #"\s+")]
    [(keyword op-name)
     (Integer/parseInt a)
     (Integer/parseInt b)
     (Integer/parseInt c)]))

(defn parse-code [lines]
  [(mapv parse-line (rest lines))
   (Integer/parseInt (re-find #"\d+" (first lines)))])

; (println
;   (run
;     (parse-code (str/split (slurp "data/day21.txt") #"\n"))
;     [0 0 0 0 0 0] 10000))


(defn prog [a]
  (loop [b (bit-or a 0x10000)
         a 2176960]
    (let [a (bit-and
              (*
                (bit-and
                  (+ a (bit-and b 0xff))
                  0xffffff)
                65899)
              0xffffff)]
      (if (> 256 b)
        a
        (recur (bit-shift-right b 8) a)))))

(defn lowest-before-loop []
  (loop [a 0
         as []]
    (let [index (.indexOf as a)]
      (if (> index -1)
        (peek as)
        (recur (prog a) (conj as a))))))

(println (lowest-before-loop))
