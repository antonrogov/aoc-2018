(ns aoc.day7
  (:require [clojure.test :as test]
            [clojure.string :as str]))

(defn duration [id duration-offset]
  (+ (- (int (first id)) (int \A)) 1 duration-offset))

(defn new-step [id duration-offset]
  {:id id :duration (duration id duration-offset) :deps []})

(defn parse-steps [lines duration-offset]
  (loop [remaining lines
         steps {}]
    (if (empty? remaining)
      (set (vals steps))
      (let [[_ dep-id id]
            (re-matches
              #"Step (.+) must be finished before step (.+) can begin."
              (first remaining))
            step (get steps id (new-step id duration-offset))
            dep (get steps dep-id (new-step dep-id duration-offset))]
        (recur (rest remaining)
               (assoc steps
                      dep-id dep
                      id (assoc step :deps (conj (:deps step) dep-id))))))))

(defn first-available-step [steps done]
  (first
    (sort-by :id
             (filter
               (fn [step]
                 (every?
                   (fn [dep] (some #(= dep %) done))
                   (:deps step)))
               steps))))

(defn process-order [steps]
  (loop [remaining steps
         done []]
    (if-let [step (first-available-step remaining done)]
      (recur (disj remaining step) (conj done (:id step)))
      (str/join done))))

(test/is (= (process-order
              (parse-steps ["Step C must be finished before step A can begin."
                            "Step C must be finished before step F can begin."
                            "Step A must be finished before step B can begin."
                            "Step A must be finished before step D can begin."
                            "Step B must be finished before step E can begin."
                            "Step D must be finished before step E can begin."
                            "Step F must be finished before step E can begin."] 0))
              "CABDFE"))

(defn find-finished [jobs]
  (some #(when (= (val %) 0) (key %)) jobs))

(defn process-jobs [jobs]
  (reduce
    (fn [acc [id t]] (assoc acc id (dec t)))
    {} jobs))

(defn time-to-process [steps num-workers]
  (loop [remaining steps
         jobs {}
         done []
         sec 0]
    (if (and (empty? remaining) (empty? jobs))
      sec
      (if-let [step-id (find-finished jobs)]
        (recur remaining
               (dissoc jobs step-id)
               (conj done step-id)
               sec)
        (let [step (first-available-step remaining done)]
          (if (and step (< (count jobs) num-workers))
            (recur (disj remaining step)
                   (assoc jobs (:id step) (:duration step))
                   done
                   sec)
            (recur remaining (process-jobs jobs) done (inc sec))))))))

(test/is (= (time-to-process
              (parse-steps ["Step C must be finished before step A can begin."
                            "Step C must be finished before step F can begin."
                            "Step A must be finished before step B can begin."
                            "Step A must be finished before step D can begin."
                            "Step B must be finished before step E can begin."
                            "Step D must be finished before step E can begin."
                            "Step F must be finished before step E can begin."] 0) 2)
              15))

(with-open [rdr (clojure.java.io/reader "day7.txt")]
  (println (time-to-process (parse-steps (line-seq rdr) 60) 5)))
