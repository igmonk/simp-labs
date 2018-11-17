(ns simp-labs.lab1.main
  (:require [clojure.pprint :refer :all]
            [simp-labs.common.gate :refer :all]
            [simp-labs.common.util :as util]
            [simp-labs.lab1.testable-gate :refer :all]))

(defn create-circuit []
  (let [x1 (->GateIn "x1")
        x2 (->GateIn "x2")
        x3 (->GateIn "x3")
        x4 (->GateIn "x4")
        x5 (->GateIn "x5")
        x6 (->GateIn "x6")
        x7 (->GateIn "x7")

        f1 (->Gate2Or "f1" x1 x2)
        f2 (->GateNot "f2" x3)
        f3 (->Gate2Nand "f3" x5 x6)
        f4 (->Gate3Nand "f4" x4 f3 x7)
        f5 (->Gate2Nand "f5" f2 f4)
        f6 (->Gate2Or "f6" f1 f5)]
    f6))

(defn pp-test-set [constant-fault test-sets]
  (doseq [[gate-id test-set] test-sets]
    (prn (str "Gate: " gate-id ", Constant Fault: " constant-fault))
    (print-table (util/sort-maps test-set))
    (newline)))

(defn define-test-sets []
  (let [circuit (create-circuit)
        f1-test-sets (test-inputs circuit [] 0)
        f0-test-sets (test-inputs circuit [] 1)]
    (pp-test-set "F=1" f1-test-sets)
    (newline)
    (pp-test-set "F=0" f0-test-sets)))
