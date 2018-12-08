(ns simp-labs.lab4.walking-01
  (:require [simp-labs.lab4.ramable :refer :all]
            [simp-labs.lab4.ram-util :as ru]))

(defn run [ram cell-value base-cell-value]
  (letfn [(do-step [base-row base-col _ index-range]

            ; Set base cell value
            (set-cell-value ram base-row base-col base-cell-value)

            ; Read and check all cells except the base one.
            (doseq [row index-range
                    col index-range
                    :when (or (not= row base-row) (not= col base-col))]
              (when (= (get-cell-value ram row col) base-cell-value)
                (ru/throw-row-col row col)))

            ; Read base cell value
            (when (= (get-cell-value ram base-row base-col) cell-value)
              (ru/throw-row-col base-row base-col))

            ; Reset base cell value
            (set-cell-value ram base-row base-col cell-value))]

    (ru/write-all ram cell-value)
    (ru/do-all-2d ram do-step)
    true))

(defn run-01 [ram]
  (try
    (run ram 0 1)
    (catch Exception e false)))

(defn run-10 [ram]
  (run ram 1 0))
