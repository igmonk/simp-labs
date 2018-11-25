(ns simp-labs.lab4.walking-01
  (:require [simp-labs.lab4.ramable :refer :all]
            [simp-labs.lab4.ram-util :as ru]))

(defn range-except-index [size except-index]
  (concat (range 0 except-index)
          (range (inc except-index) size)))

(defn run [ram base-cell-value cell-value]
  (letfn [(do-step [base-row base-col size index-range]

            ; Set base cell value
            (set-cell-value ram base-row base-col base-cell-value)

            ; Read and check all cells except the base one.
            (let [col-range (range-except-index size base-col)
                  #_read-row #_(fn [row]
                                 (doseq [col col-range]
                                   (when (= (get-cell-value ram row col) base-cell-value)
                                     (ru/throw-row-col row col))))
                  #_futures #_(doall (map #(future (read-row %1)) index-range))]
              #_(doall (map deref futures))
              (doseq [row index-range
                      col col-range]
                (when (= (get-cell-value ram row col) base-cell-value)
                  (ru/throw-row-col row col))))

            ; Read base cell value
            (when (= (get-cell-value ram base-row base-col) cell-value)
              (ru/throw-row-col base-row base-col))

            ; Reset base cell value
            (set-cell-value ram base-row base-col cell-value))]

    (ru/write-all ram cell-value)
    (ru/do-all-2d ram do-step)

    (prn (str "SUCCESS: Walking 0/1 didn't find any RAM fault. Base cell value: " base-cell-value))
    ram))

(defn run-01 [ram]
  (run ram 0 1))

(defn run-10 [ram]
  (run ram 1 0))
