(ns simp-labs.lab4.ram-util-test
  (:require [clojure.test :refer :all]
            [simp-labs.lab4.ram-util :refer :all]
            [simp-labs.lab4.ram :refer :all]
            [simp-labs.lab4.memory-cell :refer :all]))

(deftest write-all-test
  (testing "write-all"

    (let [cell-00 (atom 0)
          cell-01 (atom 0)
          cell-10 (atom 0)
          cell-11 (atom 0)
          ram (->SimpleRAM [[(->SimpleMemoryCell cell-00) (->SimpleMemoryCell cell-01)]
                            [(->SimpleMemoryCell cell-10) (->SimpleMemoryCell cell-11)]])]

      (testing "value: 1"
        (let [_ (write-all ram 1)]
          (is (= 1 (deref cell-00)))
          (is (= 1 (deref cell-01)))
          (is (= 1 (deref cell-10)))
          (is (= 1 (deref cell-11)))))

      (testing "value: 0"
        (let [_ (write-all ram 0)]
          (is (= 0 (deref cell-00)))
          (is (= 0 (deref cell-01)))
          (is (= 0 (deref cell-10)))
          (is (= 0 (deref cell-11))))))))
