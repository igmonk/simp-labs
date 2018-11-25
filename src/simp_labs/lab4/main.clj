(ns simp-labs.lab4.main
  (:require [simp-labs.lab4.ram :refer :all]
            [simp-labs.lab4.memory-cell :refer :all]
            [simp-labs.lab4.walking-01 :as walking-01]
            [simp-labs.lab4.march-b :as march-b]))

(def RAM_SIZE (* 8 (Math/pow 2 20)))

(defn create-smc []
  (->SimpleMemoryCell (atom 0)))

(defn create-smc-vec [n]
  (vec (take n (repeatedly create-smc))))

(defn create-simple-ram [n]
  (let [matrix (take n (repeatedly #(create-smc-vec n)))]
    (->SimpleRAM (vec matrix))))

(defn run-ram-tests []
  (prn (str "RAM Size: " (format "%.0f" RAM_SIZE) " bits"))
  (let [ram100 (create-simple-ram 100)
        ram1000 (create-simple-ram 1000)]

    ; Run Walking 0/1
    (time (walking-01/run-01 ram100))

    ; Run Walking 1/0
    (time (walking-01/run-10 ram100))

    ; Run March-B
    (time (march-b/run ram1000))))
