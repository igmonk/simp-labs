(ns simp-labs.lab3.main
  (:require [simp-labs.lab1.main :as lab1]
            [simp-labs.lab1.testable-gate :as lab1-gate]
            [simp-labs.lab3.lfsr :refer :all]))

(defn test-set-map->test-set [test-set-map]
  (set (map vals (:test-set test-set-map))))

(defn n-seq [n coll]
  (lazy-seq (cons (take n coll)
                  (n-seq n (drop n coll)))))

(def not-contains? (complement contains?))

(defn not-contains-x? [x]
  (fn [s] (not-contains? s x)))

(defn lfsr-cycles [test-sets lfsr-states total]
  (if (or (empty? test-sets) (empty? lfsr-states))
    total
    (let [not-contains-state? (not-contains-x? (first lfsr-states))
          filtered-test-sets (filter not-contains-state? test-sets)
          was-found? (not= (count test-sets) (count filtered-test-sets))]
      (recur filtered-test-sets
             (if was-found? lfsr-states (rest lfsr-states))
             (if was-found? total (inc total))))))

(defn apply-x8+x7+x6+x3+x2+x1+1 [test-sets lfsr8-init-states]
  (letfn [(reducer [result lfsr-state]
            (let [lfsr8-coll (apply lfsr-seq-8 lfsr-state)
                  lfsr-states (take (count lfsr8-init-states) lfsr8-coll)
                  lfsr-states-7 (map rest lfsr-states)
                  cycles (lfsr-cycles test-sets lfsr-states-7 1)]
              (if (< cycles (:lfsr-cycles result))
                {:init-state lfsr-state :lfsr-cycles cycles}
                result)))]
    (reduce reducer {:init-state nil :lfsr-cycles Integer/MAX_VALUE} lfsr8-init-states)))

(defn test-set-mapper [fault]
  (fn [[k v]]
    {:gate k
     :fault fault
     :test-set v}))

(defn lfsr-test-sets []
  (let [circuit (lab1/create-circuit)
        f1-test-sets (lab1-gate/test-inputs circuit [] 0)
        f0-test-sets (lab1-gate/test-inputs circuit [] 1)

        f1-test-maps (map (test-set-mapper 1) f1-test-sets)
        f0-test-maps (map (test-set-mapper 0) f0-test-sets)
        lab1-test-set-maps (concat f1-test-maps f0-test-maps)
        lab1-test-sets-all (map test-set-map->test-set lab1-test-set-maps)

        lfsr-m8 8
        lfsr-m8-l (- (Math/pow 2 lfsr-m8) 1)
        lfsr8-coll (lfsr-seq-8 1 1 1 1 1 1 1 1)
        lfsr8-states (take lfsr-m8-l lfsr8-coll)

        lab1-result-8 (apply-x8+x7+x6+x3+x2+x1+1 lab1-test-sets-all lfsr8-states)]

    (prn "x8+x7+x6+x3+x2+x1+1 LFSR applied to lab1 test sets:")
    (prn lab1-result-8)))
