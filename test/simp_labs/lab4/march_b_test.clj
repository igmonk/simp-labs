(ns simp-labs.lab4.march-b-test
  (:require [clojure.test :refer :all]
            [simp-labs.lab4.march-b :refer :all]
            [simp-labs.lab4.ram :refer :all]
            [simp-labs.lab4.memory-cell :refer :all]))

(deftest check-cell-value-test
  (testing "check-cell-value"
    (let [cell-00 (->SimpleMemoryCell (atom 0))
          cell-01 (->SimpleMemoryCell (atom 0))

          cell-10 (->SimpleMemoryCell (atom 0))
          cell-11 (->SimpleMemoryCell (atom 1))

          ram (->SimpleRAM [[cell-00 cell-01]
                            [cell-10 cell-11]])]
      (is (nil? (check-cell-value ram 0 0 0)))
      (is (thrown? Exception (check-cell-value ram 0 0 1)))

      (is (nil? (check-cell-value ram 0 1 0)))
      (is (thrown? Exception (check-cell-value ram 0 1 1)))

      (is (nil? (check-cell-value ram 1 0 0)))
      (is (thrown? Exception (check-cell-value ram 1 0 1)))

      (is (thrown? Exception (check-cell-value ram 1 1 0)))
      (is (nil? (check-cell-value ram 1 1 1))))))

(deftest check-cell-0-test
  (testing "check-cell-0"
    (let [cell-00 (->SimpleMemoryCell (atom 0))
          cell-01 (->SimpleMemoryCell (atom 0))

          cell-10 (->SimpleMemoryCell (atom 0))
          cell-11 (->SimpleMemoryCell (atom 1))

          ram (->SimpleRAM [[cell-00 cell-01]
                            [cell-10 cell-11]])]
      (is (nil? (check-cell-0 ram 0 0)))
      (is (nil? (check-cell-0 ram 0 1)))
      (is (nil? (check-cell-0 ram 1 0)))
      (is (thrown? Exception (check-cell-0 ram 1 1))))))

(deftest check-cell-1-test
  (testing "check-cell-1"
    (let [cell-00 (->SimpleMemoryCell (atom 0))
          cell-01 (->SimpleMemoryCell (atom 0))

          cell-10 (->SimpleMemoryCell (atom 0))
          cell-11 (->SimpleMemoryCell (atom 1))

          ram (->SimpleRAM [[cell-00 cell-01]
                            [cell-10 cell-11]])]
      (is (thrown? Exception (check-cell-1 ram 0 0)))
      (is (thrown? Exception (check-cell-1 ram 0 1)))
      (is (thrown? Exception (check-cell-1 ram 1 0)))
      (is (nil? (check-cell-1 ram 1 1))))))

(deftest run-test
  (testing "run"

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
        (is (= ram (run ram)))))

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
        (is (thrown? Exception (run ram)))))

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
        (is (thrown? Exception (run ram)))))

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
        (is (thrown? Exception (run ram)))))

    (testing "CFid (0) fault"
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
        (is (thrown? Exception (run ram)))))

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
        (is (thrown? Exception (run ram)))))))
