(ns simp-labs.lab4.test-utils
  (:require [clojure.test :refer :all]
            [simp-labs.lab4.ram :refer :all]
            [simp-labs.lab4.memory-cell :refer :all]
            [simp-labs.lab4.ramable :refer :all]))

(defn error-detection-probability [test-results num-of-faults]
  (/ (count (filter false? test-results))
     num-of-faults))

(defn print-test-report [test fault probability]
  (prn (str "Test: " test ", Fault: " fault ", Detection probability: " (format "%.3f" (float probability)))))

(defn flat-index [n row col]
  (+ (* row n) col))

; Coupling Fault common

(defn initCouplingFaultRam [n a-row a-col v-row v-col aggressor victim]
  (let [indexes (range 0 n)]
    (->> (for [row indexes
               col indexes]
           (cond
             (and (= row a-row) (= col a-col)) aggressor
             (and (= row v-row) (= col v-col)) victim
             :else (->SimpleMemoryCell (atom 0))))
         (partition n)
         ->SimpleRAM)))

(defn initCouplingFaultLRams [n a-row a-col init-ram-fn]
  (let [indexes (range 0 n)]
    (for [v-row indexes
          v-col indexes
          :when (< (flat-index n a-row a-col) (flat-index n v-row v-col))]
      (init-ram-fn v-row v-col))))

(defn initCouplingFaultGRams [n a-row a-col init-ram-fn]
  (let [indexes (range 0 n)]
    (for [v-row indexes
          v-col indexes
          :when (> (flat-index n a-row a-col) (flat-index n v-row v-col))]
      (init-ram-fn v-row v-col))))

; SAF

(defn initSAFRam [n saf-row saf-col saf-value]
  (let [indexes (range 0 n)]
    (->> (for [row indexes
               col indexes]
           (if (and (= row saf-row) (= col saf-col))
             (->ImmutableMemoryCell (atom saf-value))
             (->SimpleMemoryCell (atom 0))))
         (partition n)
         ->SimpleRAM)))

(defn initSAFRams [n saf-value]
  (let [indexes (range 0 n)]
    (for [saf-row indexes
          saf-col indexes]
      (initSAFRam n saf-row saf-col saf-value))))

; CFin

; 01 = aggressor acts on rising edge
(defn initCFin01Ram [n a-row a-col v-row v-col]
  (let [victim (->SimpleMemoryCell (atom 0))
        aggressor (->CFin01MemoryCell (atom 0) victim)]
    (initCouplingFaultRam n a-row a-col v-row v-col aggressor victim)))

; 10 = aggressor acts on falling edge
(defn initCFin10Ram [n a-row a-col v-row v-col]
  (let [victim (->SimpleMemoryCell (atom 0))
        aggressor (->CFin10MemoryCell (atom 0) victim)]
    (initCouplingFaultRam n a-row a-col v-row v-col aggressor victim)))

; L = address(aggressor) < address(victim)
(defn initCFinL01Rams [n a-row a-col]
  (initCouplingFaultLRams n a-row a-col #(initCFin01Ram n a-row a-col %1 %2)))

; L = address(aggressor) < address(victim)
(defn initAllCFinL01Rams [n]
  (let [indexes (range 0 n)]
    (-> (for [a-row indexes
              a-col indexes]
          (initCFinL01Rams n a-row a-col))
        flatten)))

; G = address(aggressor) > address(victim)
(defn initCFinG01Rams [n a-row a-col]
  (initCouplingFaultGRams n a-row a-col #(initCFin01Ram n a-row a-col %1 %2)))

; G = address(aggressor) > address(victim)
(defn initAllCFinG01Rams [n]
  (let [indexes (range 0 n)]
    (-> (for [a-row indexes
              a-col indexes]
          (initCFinG01Rams n a-row a-col))
        flatten)))

; L = address(aggressor) < address(victim)
(defn initCFinL10Rams [n a-row a-col]
  (initCouplingFaultLRams n a-row a-col #(initCFin10Ram n a-row a-col %1 %2)))

; L = address(aggressor) < address(victim)
(defn initAllCFinL10Rams [n]
  (let [indexes (range 0 n)]
    (-> (for [a-row indexes
              a-col indexes]
          (initCFinL10Rams n a-row a-col))
        flatten)))

; G = address(aggressor) > address(victim)
(defn initCFinG10Rams [n a-row a-col]
  (initCouplingFaultGRams n a-row a-col #(initCFin10Ram n a-row a-col %1 %2)))

; G = address(aggressor) > address(victim)
(defn initAllCFinG10Rams [n]
  (let [indexes (range 0 n)]
    (-> (for [a-row indexes
              a-col indexes]
          (initCFinG10Rams n a-row a-col))
        flatten)))

; CFid

; 01 = aggressor acts on rising edge
; 0 = aggressor sets 0 to its victim
(defn initCFid01-0-Ram [n a-row a-col v-row v-col]
  (let [victim (->SimpleMemoryCell (atom 0))
        aggressor (->CFid01MemoryCell0 (atom 0) victim)]
    (initCouplingFaultRam n a-row a-col v-row v-col aggressor victim)))

; 01 = aggressor acts on rising edge
; 1 = aggressor sets 1 to its victim
(defn initCFid01-1-Ram [n a-row a-col v-row v-col]
  (let [victim (->SimpleMemoryCell (atom 0))
        aggressor (->CFid01MemoryCell1 (atom 0) victim)]
    (initCouplingFaultRam n a-row a-col v-row v-col aggressor victim)))

; 10 = aggressor acts on falling edge
; 0 = aggressor sets 0 to its victim
(defn initCFid10-0-Ram [n a-row a-col v-row v-col]
  (let [victim (->SimpleMemoryCell (atom 0))
        aggressor (->CFid10MemoryCell0 (atom 0) victim)]
    (initCouplingFaultRam n a-row a-col v-row v-col aggressor victim)))

; 10 = aggressor acts on falling edge
; 1 = aggressor sets 1 to its victim
(defn initCFid10-1-Ram [n a-row a-col v-row v-col]
  (let [victim (->SimpleMemoryCell (atom 0))
        aggressor (->CFid10MemoryCell1 (atom 0) victim)]
    (initCouplingFaultRam n a-row a-col v-row v-col aggressor victim)))

; L = address(aggressor) < address(victim)

(defn initCFidL01-0-rams [n a-row a-col]
  (initCouplingFaultLRams n a-row a-col #(initCFid01-0-Ram n a-row a-col %1 %2)))

(defn initAllCFidL01-0-rams [n]
  (let [indexes (range 0 n)]
    (-> (for [a-row indexes
              a-col indexes]
          (initCFidL01-0-rams n a-row a-col))
        flatten)))

(defn initCFidL01-1-rams [n a-row a-col]
  (initCouplingFaultLRams n a-row a-col #(initCFid01-1-Ram n a-row a-col %1 %2)))

(defn initAllCFidL01-1-rams [n]
  (let [indexes (range 0 n)]
    (-> (for [a-row indexes
              a-col indexes]
          (initCFidL01-1-rams n a-row a-col))
        flatten)))

(defn initCFidL10-0-rams [n a-row a-col]
  (initCouplingFaultLRams n a-row a-col #(initCFid10-0-Ram n a-row a-col %1 %2)))

(defn initAllCFidL10-0-rams [n]
  (let [indexes (range 0 n)]
    (-> (for [a-row indexes
              a-col indexes]
          (initCFidL10-0-rams n a-row a-col))
        flatten)))

(defn initCFidL10-1-rams [n a-row a-col]
  (initCouplingFaultLRams n a-row a-col #(initCFid10-1-Ram n a-row a-col %1 %2)))

(defn initAllCFidL10-1-rams [n]
  (let [indexes (range 0 n)]
    (-> (for [a-row indexes
              a-col indexes]
          (initCFidL10-1-rams n a-row a-col))
        flatten)))

; R = address(aggressor) > address(victim)

(defn initCFidG01-0-rams [n a-row a-col]
  (initCouplingFaultGRams n a-row a-col #(initCFid01-0-Ram n a-row a-col %1 %2)))

(defn initAllCFidG01-0-rams [n]
  (let [indexes (range 0 n)]
    (-> (for [a-row indexes
              a-col indexes]
          (initCFidG01-0-rams n a-row a-col))
        flatten)))

(defn initCFidG01-1-rams [n a-row a-col]
  (initCouplingFaultGRams n a-row a-col #(initCFid01-1-Ram n a-row a-col %1 %2)))

(defn initAllCFidG01-1-rams [n]
  (let [indexes (range 0 n)]
    (-> (for [a-row indexes
              a-col indexes]
          (initCFidG01-1-rams n a-row a-col))
        flatten)))

(defn initCFidG10-0-rams [n a-row a-col]
  (initCouplingFaultGRams n a-row a-col #(initCFid10-0-Ram n a-row a-col %1 %2)))

(defn initAllCFidG10-0-rams [n]
  (let [indexes (range 0 n)]
    (-> (for [a-row indexes
              a-col indexes]
          (initCFidG10-0-rams n a-row a-col))
        flatten)))

(defn initCFidG10-1-rams [n a-row a-col]
  (initCouplingFaultGRams n a-row a-col #(initCFid10-1-Ram n a-row a-col %1 %2)))

(defn initAllCFidG10-1-rams [n]
  (let [indexes (range 0 n)]
    (-> (for [a-row indexes
              a-col indexes]
          (initCFidG10-1-rams n a-row a-col))
        flatten)))
