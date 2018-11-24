(ns simp-labs.lab4.ramable-test
  (:require [clojure.test :refer :all]
            [simp-labs.lab4.ramable :refer :all]
            [simp-labs.lab4.ram :refer :all]
            [simp-labs.lab4.memory-cell :refer :all]))

(deftest ramable-SimpleRAM-test
  (testing "SimpleRAM"

    (testing "get-size"
      (is (= 0 (get-size (->SimpleRAM []))))
      (is (= 3 (get-size (->SimpleRAM [[1 2 3]
                                       [4 5 6]
                                       [7 8 9]])))))

    (testing "get-cell-value"
      (let [cell-00 (atom 0)
            cell-01 (atom 0)
            cell-10 (atom 1)
            cell-11 (atom 1)
            ram (->SimpleRAM [[(->SimpleMemoryCell cell-00) (->SimpleMemoryCell cell-01)]
                              [(->SimpleMemoryCell cell-10) (->SimpleMemoryCell cell-11)]])]
        (is (= 0 (get-cell-value ram 0 0)))
        (is (= 0 (get-cell-value ram 0 1)))
        (is (= 1 (get-cell-value ram 1 0)))
        (is (= 1 (get-cell-value ram 1 1)))))

    (testing "set-cell-value"
      (let [cell-00 (atom 0)
            cell-01 (atom 0)
            cell-10 (atom 1)
            cell-11 (atom 1)
            ram (->SimpleRAM [[(->SimpleMemoryCell cell-00) (->SimpleMemoryCell cell-01)]
                              [(->SimpleMemoryCell cell-10) (->SimpleMemoryCell cell-11)]])
            _ (set-cell-value ram 0 0 0)
            _ (set-cell-value ram 0 1 1)
            _ (set-cell-value ram 1 0 0)
            _ (set-cell-value ram 1 1 1)]
        (is (= 0 (deref cell-00)))
        (is (= 1 (deref cell-01)))
        (is (= 0 (deref cell-10)))
        (is (= 1 (deref cell-11)))))))
