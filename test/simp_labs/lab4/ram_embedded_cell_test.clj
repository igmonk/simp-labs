(ns simp-labs.lab4.ram-embedded-cell-test
  (:require [clojure.test :refer :all]
            [simp-labs.lab4.memory-cell :refer :all]
            [simp-labs.lab4.ram-embedded-cell :refer :all]))

(deftest ram-embedded-SimpleMemoryCell-test
  (testing "SimpleMemoryCell"

    (testing "get-value"
      (is (= 0 (get-value (->SimpleMemoryCell (atom 0)))))
      (is (= 1 (get-value (->SimpleMemoryCell (atom 1))))))

    (testing "set-value"
      (let [test-set [[(atom 0) 0 0]
                      [(atom 1) 0 0]
                      [(atom 0) 1 1]
                      [(atom 1) 1 1]]]
        (doseq [[cell new-value expected] test-set]
          (is (= expected (set-value (->SimpleMemoryCell cell) new-value)))
          (is (= expected (deref cell))))))))

(deftest ram-embedded-ImmutableMemoryCell-test
  (testing "ImmutableMemoryCell"

    (testing "get-value"
      (is (= 0 (get-value (->ImmutableMemoryCell (atom 0)))))
      (is (= 1 (get-value (->ImmutableMemoryCell (atom 1))))))

    (testing "set-value"
      (let [test-set [[(atom 0) 0 0]
                      [(atom 1) 0 1]
                      [(atom 0) 1 0]
                      [(atom 1) 1 1]]]
        (doseq [[cell new-value expected] test-set]
          (let [_ (set-value (->ImmutableMemoryCell cell) new-value)]
            (is (= expected (deref cell)))))))))

(deftest ram-embedded-CFinMemoryCell-test
  (testing "CFinMemoryCell"

    (testing "get-value"
      (is (= 0 (get-value (->CFinMemoryCell (atom 0) nil))))
      (is (= 1 (get-value (->CFinMemoryCell (atom 1) nil)))))

    (testing "set-value"
      (let [victim-1 (atom 0)
            victim-2 (atom 0)
            victim-3 (atom 1)
            victim-4 (atom 1)
            victim-5 (atom 0)
            victim-6 (atom 0)
            victim-7 (atom 1)
            victim-8 (atom 1)
            test-set [[(atom 0) victim-1 (->SimpleMemoryCell victim-1) 0 0 0]
                      [(atom 0) victim-2 (->SimpleMemoryCell victim-2) 1 1 1]
                      [(atom 0) victim-3 (->SimpleMemoryCell victim-3) 0 0 1]
                      [(atom 0) victim-4 (->SimpleMemoryCell victim-4) 1 1 0]
                      [(atom 1) victim-5 (->SimpleMemoryCell victim-5) 0 0 1]
                      [(atom 1) victim-6 (->SimpleMemoryCell victim-6) 1 1 0]
                      [(atom 1) victim-7 (->SimpleMemoryCell victim-7) 0 0 0]
                      [(atom 1) victim-8 (->SimpleMemoryCell victim-8) 1 1 1]]]
        (doseq [[aggressor-atom victim-atom victim-cell new-value aggressor-expected victim-expected] test-set]
          (let [aggressor-cell (->CFinMemoryCell aggressor-atom victim-cell)
                _ (set-value aggressor-cell new-value)]
            (is (= aggressor-expected (deref aggressor-atom)))
            (is (= aggressor-expected (get-value aggressor-cell)))
            (is (= victim-expected (deref victim-atom)))
            (is (= victim-expected (get-value victim-cell)))))))))

(deftest ram-embedded-CFid0MemoryCell-test
  (testing "CFid0MemoryCell"

    (testing "get-value"
      (is (= 0 (get-value (->CFid0MemoryCell (atom 0) nil))))
      (is (= 1 (get-value (->CFid0MemoryCell (atom 1) nil)))))

    (testing "set-value"
      (let [victim-1 (atom 0)
            victim-2 (atom 0)
            victim-3 (atom 1)
            victim-4 (atom 1)
            victim-5 (atom 0)
            victim-6 (atom 0)
            victim-7 (atom 1)
            victim-8 (atom 1)
            test-set [[(atom 0) victim-1 (->SimpleMemoryCell victim-1) 0 0 0]
                      [(atom 0) victim-2 (->SimpleMemoryCell victim-2) 1 1 0]
                      [(atom 0) victim-3 (->SimpleMemoryCell victim-3) 0 0 1]
                      [(atom 0) victim-4 (->SimpleMemoryCell victim-4) 1 1 0]
                      [(atom 1) victim-5 (->SimpleMemoryCell victim-5) 0 0 0]
                      [(atom 1) victim-6 (->SimpleMemoryCell victim-6) 1 1 0]
                      [(atom 1) victim-7 (->SimpleMemoryCell victim-7) 0 0 0]
                      [(atom 1) victim-8 (->SimpleMemoryCell victim-8) 1 1 1]]]
        (doseq [[aggressor-atom victim-atom victim-cell new-value aggressor-expected victim-expected] test-set]
          (let [aggressor-cell (->CFid0MemoryCell aggressor-atom victim-cell)
                _ (set-value aggressor-cell new-value)]
            (is (= aggressor-expected (deref aggressor-atom)))
            (is (= aggressor-expected (get-value aggressor-cell)))
            (is (= victim-expected (deref victim-atom)))
            (is (= victim-expected (get-value victim-cell)))))))))

(deftest ram-embedded-CFid1MemoryCell-test
  (testing "CFid1MemoryCell"

    (testing "get-value"
      (is (= 0 (get-value (->CFid1MemoryCell (atom 0) nil))))
      (is (= 1 (get-value (->CFid1MemoryCell (atom 1) nil)))))

    (testing "set-value"
      (let [victim-1 (atom 0)
            victim-2 (atom 0)
            victim-3 (atom 1)
            victim-4 (atom 1)
            victim-5 (atom 0)
            victim-6 (atom 0)
            victim-7 (atom 1)
            victim-8 (atom 1)
            test-set [[(atom 0) victim-1 (->SimpleMemoryCell victim-1) 0 0 0]
                      [(atom 0) victim-2 (->SimpleMemoryCell victim-2) 1 1 1]
                      [(atom 0) victim-3 (->SimpleMemoryCell victim-3) 0 0 1]
                      [(atom 0) victim-4 (->SimpleMemoryCell victim-4) 1 1 1]
                      [(atom 1) victim-5 (->SimpleMemoryCell victim-5) 0 0 1]
                      [(atom 1) victim-6 (->SimpleMemoryCell victim-6) 1 1 0]
                      [(atom 1) victim-7 (->SimpleMemoryCell victim-7) 0 0 1]
                      [(atom 1) victim-8 (->SimpleMemoryCell victim-8) 1 1 1]]]
        (doseq [[aggressor-atom victim-atom victim-cell new-value aggressor-expected victim-expected] test-set]
          (let [aggressor-cell (->CFid1MemoryCell aggressor-atom victim-cell)
                _ (set-value aggressor-cell new-value)]
            (is (= aggressor-expected (deref aggressor-atom)))
            (is (= aggressor-expected (get-value aggressor-cell)))
            (is (= victim-expected (deref victim-atom)))
            (is (= victim-expected (get-value victim-cell)))))))))
