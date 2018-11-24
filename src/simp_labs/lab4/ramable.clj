(ns simp-labs.lab4.ramable
  (:require [simp-labs.lab4.ram :refer :all]
            [simp-labs.lab4.ram-embedded-cell :refer :all])
  (:import [simp_labs.lab4.ram SimpleRAM]))

(defprotocol RAMable
  (get-size [ram])
  (get-cell-value [ram row col])
  (set-cell-value [ram row col new-value]))

(extend-type SimpleRAM
  RAMable
  (get-size [ram]
    (count (:cells-matrix ram)))
  (get-cell-value [ram row col]
    (get-value (nth (nth (:cells-matrix ram) row) col)))
  (set-cell-value [ram row col new-value]
    (set-value (nth (nth (:cells-matrix ram) row) col) new-value)))
