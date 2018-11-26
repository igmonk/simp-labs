(ns simp-labs.lab3.main
  (:require [simp-labs.lab1.main :as lab1]
            [simp-labs.lab1.testable-gate :as lab1-gate]
            [simp-labs.lab3.lfsr :refer :all]))

(defn unique-lab1-test-sets [f1 f2]
  (->> (flatten (vals f1))
       (concat (flatten (vals f2)))
       distinct
       (map vals)
       set))

(defn n-seq [n coll]
  (lazy-seq (cons (take n coll)
                  (n-seq n (drop n coll)))))

(def not-contains? (complement contains?))

(defn lfsr-cycles [test-sets lfsr-states total]
  (if-not (or (empty? test-sets)
              (empty? lfsr-states))
    (let [count-to-skip (count (take-while #(not-contains? test-sets %) lfsr-states))
          consumed (inc count-to-skip)]
      (recur (disj test-sets (first (drop count-to-skip lfsr-states)))
             (drop consumed lfsr-states)
             (+ total consumed)))
    total))

(defn apply-x8+x7+x6+x3+x2+x1+1 [test-sets lfsr8-init-states]
  (letfn [(reducer [result lfsr-state]
            (let [lfsr8-coll (apply lfsr-seq-8 lfsr-state)
                  lfsr-states (take (count lfsr8-init-states) lfsr8-coll)
                  lfsr-states-7 (map rest lfsr-states)
                  cycles (lfsr-cycles test-sets lfsr-states-7 0)]
              (if (< cycles (:lfsr-cycles result))
                {:init-state lfsr-state :lfsr-cycles cycles}
                result)))]
    (reduce reducer {:init-state nil :lfsr-cycles Integer/MAX_VALUE} lfsr8-init-states)))

(defn lfsr-test-sets []
  (let [circuit (lab1/create-circuit)
        f1-test-sets (lab1-gate/test-inputs circuit [] 0)
        f0-test-sets (lab1-gate/test-inputs circuit [] 1)

        lab1-test-sets (unique-lab1-test-sets f1-test-sets f0-test-sets)

        lfsr-m8 8
        lfsr-m8-l (- (Math/pow 2 lfsr-m8) 1)
        lfsr8-coll (lfsr-seq-8 1 1 1 1 1 1 1 1)
        lfsr8-states (take lfsr-m8-l lfsr8-coll)

        results-8 (apply-x8+x7+x6+x3+x2+x1+1 lab1-test-sets lfsr8-states)]

    (prn (str "Num of unique test input sets = " (count lab1-test-sets)))
    (prn (str "Period = " lfsr-m8-l))

    (prn "test sets:")
    (prn lab1-test-sets)
    (newline)

    (prn "x8+x7+x6+x3+x2+x+1 LFSR states:")
    (prn lfsr8-states)
    (newline)

    (prn "x8+x7+x6+x3+x2+x1+1 LFSR applied to lab1 test sets:")
    (prn results-8)))
