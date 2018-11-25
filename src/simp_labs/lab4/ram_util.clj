(ns simp-labs.lab4.ram-util
  (:require [simp-labs.lab4.ramable :refer :all]))

(defn do-all-2d [matrix f]
  (let [size (get-size matrix)
        index-range (vec (range 0 size))]
    (doseq [row index-range
            col index-range]
      (f row col size index-range))))

(defn do-all-2d-backward [matrix f]
  (let [size (get-size matrix)
        index-range (vec (reverse (range 0 size)))]
    (doseq [row index-range
            col index-range]
      (f row col size index-range))))

(defn write-all [ram value]
  (do-all-2d ram
             (fn [row col _ _]
               (set-cell-value ram row col value))))

(defn throw-row-col [row col]
  (throw (Exception. (str "Not valid cell (row: " row ", col: " col ")."))))
