(ns simp-labs.lab4.walking-01-test
  (:require [clojure.test :refer :all]
            [simp-labs.lab4.walking-01 :refer :all]
            [simp-labs.lab4.ram :refer :all]
            [simp-labs.lab4.memory-cell :refer :all]))

(deftest run-01-test
  (testing "run-01"

    (testing "Valid cells"
      (let [cell-00 (->SimpleMemoryCell (atom 0))
            cell-01 (->SimpleMemoryCell (atom 0))
            cell-02 (->SimpleMemoryCell (atom 0))

            cell-10 (->SimpleMemoryCell (atom 0))
            cell-11 (->SimpleMemoryCell (atom 0))
            cell-12 (->SimpleMemoryCell (atom 0))

            cell-20 (->SimpleMemoryCell (atom 0))
            cell-21 (->SimpleMemoryCell (atom 0))
            cell-22 (->SimpleMemoryCell (atom 0))

            ram (->SimpleRAM [[cell-00 cell-01 cell-02]
                              [cell-10 cell-11 cell-12]
                              [cell-20 cell-21 cell-22]])]
        (is (= ram (run-01 ram)))))

    (testing "SAF0 fault"
      (let [cell-00 (->SimpleMemoryCell (atom 0))
            cell-01 (->SimpleMemoryCell (atom 0))
            cell-02 (->SimpleMemoryCell (atom 0))

            cell-10 (->SimpleMemoryCell (atom 0))
            cell-11 (->SimpleMemoryCell (atom 0))
            cell-12 (->SimpleMemoryCell (atom 0))

            cell-20 (->SimpleMemoryCell (atom 0))
            cell-21 (->SimpleMemoryCell (atom 0))
            cell-22 (->ImmutableMemoryCell (atom 0))

            ram (->SimpleRAM [[cell-00 cell-01 cell-02]
                              [cell-10 cell-11 cell-12]
                              [cell-20 cell-21 cell-22]])]
        (is (thrown? Exception (run-01 ram)))))

    (testing "SAF1 fault"
      (let [cell-00 (->SimpleMemoryCell (atom 0))
            cell-01 (->SimpleMemoryCell (atom 0))
            cell-02 (->SimpleMemoryCell (atom 0))

            cell-10 (->SimpleMemoryCell (atom 0))
            cell-11 (->SimpleMemoryCell (atom 0))
            cell-12 (->SimpleMemoryCell (atom 0))

            cell-20 (->SimpleMemoryCell (atom 0))
            cell-21 (->SimpleMemoryCell (atom 0))
            cell-22 (->ImmutableMemoryCell (atom 1))

            ram (->SimpleRAM [[cell-00 cell-01 cell-02]
                              [cell-10 cell-11 cell-12]
                              [cell-20 cell-21 cell-22]])]
        (is (thrown? Exception (run-01 ram)))))

    (testing "CFin fault"
      (let [cell-00 (->SimpleMemoryCell (atom 0))
            cell-01 (->SimpleMemoryCell (atom 0))
            cell-02 (->SimpleMemoryCell (atom 0))

            cell-10 (->SimpleMemoryCell (atom 0))
            cell-11 (->SimpleMemoryCell (atom 0))
            cell-12 (->SimpleMemoryCell (atom 0))

            cell-20 (->SimpleMemoryCell (atom 0))
            cell-21 (->SimpleMemoryCell (atom 0))
            cell-22 (->CFinMemoryCell (atom 0) cell-21)

            ram (->SimpleRAM [[cell-00 cell-01 cell-02]
                              [cell-10 cell-11 cell-12]
                              [cell-20 cell-21 cell-22]])]
        (is (thrown? Exception (run-01 ram)))))

    (testing "CFid (0) Fault"
      (let [cell-00 (->SimpleMemoryCell (atom 0))
            cell-01 (->SimpleMemoryCell (atom 0))
            cell-02 (->SimpleMemoryCell (atom 0))

            cell-10 (->SimpleMemoryCell (atom 0))
            cell-11 (->SimpleMemoryCell (atom 0))
            cell-12 (->SimpleMemoryCell (atom 0))

            cell-20 (->SimpleMemoryCell (atom 0))
            cell-21 (->SimpleMemoryCell (atom 0))
            cell-22 (->CFid0MemoryCell (atom 0) cell-21)

            ram (->SimpleRAM [[cell-00 cell-01 cell-02]
                              [cell-10 cell-11 cell-12]
                              [cell-20 cell-21 cell-22]])]
        (is (thrown? Exception (run-01 ram)))))))

(deftest run-10-test
  (testing "run-10"

    (testing "Valid cells"
      (let [cell-00 (->SimpleMemoryCell (atom 0))
            cell-01 (->SimpleMemoryCell (atom 0))
            cell-02 (->SimpleMemoryCell (atom 0))

            cell-10 (->SimpleMemoryCell (atom 0))
            cell-11 (->SimpleMemoryCell (atom 0))
            cell-12 (->SimpleMemoryCell (atom 0))

            cell-20 (->SimpleMemoryCell (atom 0))
            cell-21 (->SimpleMemoryCell (atom 0))
            cell-22 (->SimpleMemoryCell (atom 0))

            ram (->SimpleRAM [[cell-00 cell-01 cell-02]
                              [cell-10 cell-11 cell-12]
                              [cell-20 cell-21 cell-22]])]
        (is (= ram (run-10 ram)))))

    (testing "SAF0 fault"
      (let [cell-00 (->SimpleMemoryCell (atom 0))
            cell-01 (->SimpleMemoryCell (atom 0))
            cell-02 (->SimpleMemoryCell (atom 0))

            cell-10 (->SimpleMemoryCell (atom 0))
            cell-11 (->SimpleMemoryCell (atom 0))
            cell-12 (->SimpleMemoryCell (atom 0))

            cell-20 (->SimpleMemoryCell (atom 0))
            cell-21 (->SimpleMemoryCell (atom 0))
            cell-22 (->ImmutableMemoryCell (atom 0))

            ram (->SimpleRAM [[cell-00 cell-01 cell-02]
                              [cell-10 cell-11 cell-12]
                              [cell-20 cell-21 cell-22]])]
        (is (thrown? Exception (run-10 ram)))))

    (testing "SAF1 fault"
      (let [cell-00 (->SimpleMemoryCell (atom 0))
            cell-01 (->SimpleMemoryCell (atom 0))
            cell-02 (->SimpleMemoryCell (atom 0))

            cell-10 (->SimpleMemoryCell (atom 0))
            cell-11 (->SimpleMemoryCell (atom 0))
            cell-12 (->SimpleMemoryCell (atom 0))

            cell-20 (->SimpleMemoryCell (atom 0))
            cell-21 (->SimpleMemoryCell (atom 0))
            cell-22 (->ImmutableMemoryCell (atom 1))

            ram (->SimpleRAM [[cell-00 cell-01 cell-02]
                              [cell-10 cell-11 cell-12]
                              [cell-20 cell-21 cell-22]])]
        (is (thrown? Exception (run-10 ram)))))

    (testing "CFin fault"
      (let [cell-00 (->SimpleMemoryCell (atom 0))
            cell-01 (->SimpleMemoryCell (atom 0))
            cell-02 (->SimpleMemoryCell (atom 0))

            cell-10 (->SimpleMemoryCell (atom 0))
            cell-11 (->SimpleMemoryCell (atom 0))
            cell-12 (->SimpleMemoryCell (atom 0))

            cell-20 (->SimpleMemoryCell (atom 0))
            cell-21 (->SimpleMemoryCell (atom 0))
            cell-22 (->CFinMemoryCell (atom 0) cell-21)

            ram (->SimpleRAM [[cell-00 cell-01 cell-02]
                              [cell-10 cell-11 cell-12]
                              [cell-20 cell-21 cell-22]])]
        (is (thrown? Exception (run-10 ram)))))

    (testing "CFid (1) fault"
      (let [cell-00 (->SimpleMemoryCell (atom 0))
            cell-01 (->SimpleMemoryCell (atom 0))
            cell-02 (->SimpleMemoryCell (atom 0))

            cell-10 (->SimpleMemoryCell (atom 0))
            cell-11 (->SimpleMemoryCell (atom 0))
            cell-12 (->SimpleMemoryCell (atom 0))

            cell-20 (->SimpleMemoryCell (atom 0))
            cell-21 (->SimpleMemoryCell (atom 0))
            cell-22 (->CFid1MemoryCell (atom 0) cell-21)

            ram (->SimpleRAM [[cell-00 cell-01 cell-02]
                              [cell-10 cell-11 cell-12]
                              [cell-20 cell-21 cell-22]])]
        (is (thrown? Exception (run-10 ram)))))))
