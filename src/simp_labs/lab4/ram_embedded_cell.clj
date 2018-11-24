(ns simp-labs.lab4.ram-embedded-cell
  (:require [simp-labs.lab4.memory-cell :refer :all])
  (:import [simp_labs.lab4.memory_cell SimpleMemoryCell
                                       ImmutableMemoryCell
                                       CFinMemoryCell
                                       CFid0MemoryCell
                                       CFid1MemoryCell]))

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

(extend-type CFid0MemoryCell
  RAMEmbeddedCell
  (get-value [cell]
    (deref (:value cell)))
  (set-value [{:keys [value victim-cell]} new-value]
    (let [old-value @value
          updated-value (reset! value new-value)]
      (when (not= old-value updated-value)
        (set-value victim-cell 0)))))

(extend-type CFid1MemoryCell
  RAMEmbeddedCell
  (get-value [cell]
    (deref (:value cell)))
  (set-value [{:keys [value victim-cell]} new-value]
    (let [old-value @value
          updated-value (reset! value new-value)]
      (when (not= old-value updated-value)
        (set-value victim-cell 1)))))
