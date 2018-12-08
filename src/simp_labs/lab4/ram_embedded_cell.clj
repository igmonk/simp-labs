(ns simp-labs.lab4.ram-embedded-cell
  (:require [simp-labs.lab4.memory-cell :refer :all])
  (:import [simp_labs.lab4.memory_cell SimpleMemoryCell
                                       ImmutableMemoryCell
                                       CFinMemoryCell
                                       CFin01MemoryCell
                                       CFin10MemoryCell
                                       CFid01MemoryCell0
                                       CFid01MemoryCell1
                                       CFid10MemoryCell0
                                       CFid10MemoryCell1]))

(defprotocol RAMEmbeddedCell
  (get-value [cell])
  (set-value [cell new-value]))

(extend-type SimpleMemoryCell
  RAMEmbeddedCell
  (get-value [cell]
    (deref (:value cell)))
  (set-value [cell new-value]
    (reset! (:value cell) new-value)))

(extend-type ImmutableMemoryCell
  RAMEmbeddedCell
  (get-value [cell]
    (deref (:value cell)))
  (set-value [_ _]))

(extend-type CFinMemoryCell
  RAMEmbeddedCell
  (get-value [cell]
    (deref (:value cell)))
  (set-value [{:keys [value victim-cell]} new-value]
    (let [old-value @value
          updated-value (reset! value new-value)]
      (when (not= old-value updated-value)
        (set-value victim-cell
                   (case (get-value victim-cell)
                     0 1
                     1 0))))))

(extend-type CFin01MemoryCell
  RAMEmbeddedCell
  (get-value [cell]
    (deref (:value cell)))
  (set-value [{:keys [value victim-cell]} new-value]
    (let [old-value @value
          updated-value (reset! value new-value)]
      (when (and (not= old-value updated-value) (= 1 updated-value))
        (set-value victim-cell
                   (case (get-value victim-cell)
                     0 1
                     1 0))))))

(extend-type CFin10MemoryCell
  RAMEmbeddedCell
  (get-value [cell]
    (deref (:value cell)))
  (set-value [{:keys [value victim-cell]} new-value]
    (let [old-value @value
          updated-value (reset! value new-value)]
      (when (and (not= old-value updated-value) (= 0 updated-value))
        (set-value victim-cell
                   (case (get-value victim-cell)
                     0 1
                     1 0))))))

(extend-type CFid01MemoryCell0
  RAMEmbeddedCell
  (get-value [cell]
    (deref (:value cell)))
  (set-value [{:keys [value victim-cell]} new-value]
    (let [old-value @value
          updated-value (reset! value new-value)]
      (when (and (not= old-value updated-value) (= 1 updated-value))
        (set-value victim-cell 0)))))

(extend-type CFid01MemoryCell1
  RAMEmbeddedCell
  (get-value [cell]
    (deref (:value cell)))
  (set-value [{:keys [value victim-cell]} new-value]
    (let [old-value @value
          updated-value (reset! value new-value)]
      (when (and (not= old-value updated-value) (= 1 updated-value))
        (set-value victim-cell 1)))))

(extend-type CFid10MemoryCell0
  RAMEmbeddedCell
  (get-value [cell]
    (deref (:value cell)))
  (set-value [{:keys [value victim-cell]} new-value]
    (let [old-value @value
          updated-value (reset! value new-value)]
      (when (and (not= old-value updated-value) (= 0 updated-value))
        (set-value victim-cell 0)))))

(extend-type CFid10MemoryCell1
  RAMEmbeddedCell
  (get-value [cell]
    (deref (:value cell)))
  (set-value [{:keys [value victim-cell]} new-value]
    (let [old-value @value
          updated-value (reset! value new-value)]
      (when (and (not= old-value updated-value) (= 0 updated-value))
        (set-value victim-cell 1)))))
