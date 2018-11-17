(ns simp-labs.lab1.testable-gate-test
  (:require [clojure.test :refer :all]
            [simp-labs.common.gate :refer :all]
            [simp-labs.lab1.testable-gate :refer :all]))

(deftest combine-inputs-test
  (testing "combine-inputs (empty check)"
    (let [inputs1 [{:x1 111 :x2 1110}]
          inputs2 [{:y1 211 :y2 2110}
                   {:y1 212 :y2 2120}]
          inputs3 []
          expected [{:x1 111 :x2 1110 :y1 211 :y2 2110}
                    {:x1 111 :x2 1110 :y1 212 :y2 2120}]
          actual (combine-inputs inputs1 inputs2 inputs3)]
      (is (= expected actual))))

  (testing "combine-inputs"
    (let [inputs1 [{:x1 111 :x2 1110}]
          inputs2 [{:y1 211 :y2 2110}
                   {:y1 212 :y2 2120}]
          inputs3 [{:z1 311 :z2 3110}
                   {:z1 312 :z2 3120}
                   {:z1 313 :z2 3130}]
          expected [{:x1 111 :x2 1110 :y1 211 :y2 2110 :z1 311 :z2 3110}
                    {:x1 111 :x2 1110 :y1 211 :y2 2110 :z1 312 :z2 3120}
                    {:x1 111 :x2 1110 :y1 211 :y2 2110 :z1 313 :z2 3130}
                    {:x1 111 :x2 1110 :y1 212 :y2 2120 :z1 311 :z2 3110}
                    {:x1 111 :x2 1110 :y1 212 :y2 2120 :z1 312 :z2 3120}
                    {:x1 111 :x2 1110 :y1 212 :y2 2120 :z1 313 :z2 3130}]
          actual (combine-inputs inputs1 inputs2 inputs3)]
      (is (= expected actual)))))

(deftest combine-input-gates-test
  (testing "combine-input-gates"
    (let [gate-in-x1 (->GateIn "x1")
          gate-in-x2 (->GateIn "x2")]

      (testing "simple"
        (let [expected [{"x1" 0 "x2" 0}
                        {"x1" 0 "x2" 1}
                        {"x1" 1 "x2" 0}
                        {"x1" 1 "x2" 1}]
              actual (combine-input-gates [(inputs gate-in-x1 0) (inputs gate-in-x2 0)]
                                          [(inputs gate-in-x1 0) (inputs gate-in-x2 1)]
                                          [(inputs gate-in-x1 1) (inputs gate-in-x2 0)]
                                          [(inputs gate-in-x1 1) (inputs gate-in-x2 1)])]
          (is (= expected actual))))

      (testing "complex"
        (let [gate-in-x3 (->GateIn "x3")
              gate-in-x4 (->GateIn "x4")
              gate-2-or (->Gate2Or "2or" gate-in-x1 gate-in-x2)
              gate-2-and (->Gate2And "2and" gate-in-x3 gate-in-x4)
              expected [{"x1" 0 "x2" 0 "x3" 0 "x4" 0}
                        {"x1" 0 "x2" 0 "x3" 0 "x4" 1}
                        {"x1" 0 "x2" 0 "x3" 1 "x4" 0}

                        {"x1" 0 "x2" 0 "x3" 1 "x4" 1}

                        {"x1" 0, "x2" 1, "x3" 0, "x4" 0}
                        {"x1" 0, "x2" 1, "x3" 0, "x4" 1}
                        {"x1" 0, "x2" 1, "x3" 1, "x4" 0}
                        {"x1" 1, "x2" 0, "x3" 0, "x4" 0}
                        {"x1" 1, "x2" 0, "x3" 0, "x4" 1}
                        {"x1" 1, "x2" 0, "x3" 1, "x4" 0}
                        {"x1" 1, "x2" 1, "x3" 0, "x4" 0}
                        {"x1" 1, "x2" 1, "x3" 0, "x4" 1}
                        {"x1" 1, "x2" 1, "x3" 1, "x4" 0}

                        {"x1" 0, "x2" 1, "x3" 1, "x4" 1}
                        {"x1" 1, "x2" 0, "x3" 1, "x4" 1}
                        {"x1" 1, "x2" 1, "x3" 1, "x4" 1}]
              actual (combine-input-gates [(inputs gate-2-or 0) (inputs gate-2-and 0)]
                                          [(inputs gate-2-or 0) (inputs gate-2-and 1)]
                                          [(inputs gate-2-or 1) (inputs gate-2-and 0)]
                                          [(inputs gate-2-or 1) (inputs gate-2-and 1)])]
          (is (= expected actual)))))))

(deftest testable-GateIn-test
  (testing "TestableGate GateIn - inputs"
    (let [gate-in (->GateIn "x1")]
      (is (= [{"x1" 0}] (inputs gate-in 0)))
      (is (= [{"x1" 1}] (inputs gate-in 1))))))

(deftest testable-GateNot-test
  (testing "TestableGate GateNot"
    (let [gate-in-x1 (->GateIn "x1")
          gate-not (->GateNot "not" gate-in-x1)]

      (testing "inputs"
        (is (= [{"x1" 1}]
               (inputs gate-not 0)))
        (is (= [{"x1" 0}]
               (inputs gate-not 1))))

      (testing "fixed-gate-inputs"
        (is (= [] (fixed-gate-inputs gate-not gate-in-x1))))

      (testing "activity-inputs"
        (is (= [] (activity-inputs gate-not gate-in-x1 nil))))

      (testing "test-inputs"
        (is (= {"not" [{"x1" 1}]}
               (test-inputs gate-not nil 0)))
        (is (= {"not" [{"x1" 0}]}
               (test-inputs gate-not nil 1)))))))

(deftest testable-Gate2Or-test
  (testing "TestableGate Gate2Or"
    (let [gate-in-x1 (->GateIn "x1")
          gate-in-x2 (->GateIn "x2")
          gate-2-or (->Gate2Or "2or" gate-in-x1 gate-in-x2)]

      (testing "inputs"
        (is (= [{"x1" 0 "x2" 0}]
               (inputs gate-2-or 0)))
        (is (= [{"x1" 0 "x2" 1}
                {"x1" 1 "x2" 0}
                {"x1" 1 "x2" 1}]
               (inputs gate-2-or 1))))

      (testing "fixed-gate-inputs"
        (is (= [{"x2" 0}]
               (fixed-gate-inputs gate-2-or gate-in-x1)))
        (is (= [{"x1" 0}]
               (fixed-gate-inputs gate-2-or gate-in-x2))))

      (testing "activity-inputs"
        (is (= [{"x2" 0}]
               (activity-inputs gate-2-or gate-in-x1 nil)))
        (is (= [{"x1" 0}]
               (activity-inputs gate-2-or gate-in-x2 nil)))))))

(deftest testable-Gate2And-test
  (testing "TestableGate Gate2And"
    (let [gate-in-x1 (->GateIn "x1")
          gate-in-x2 (->GateIn "x2")
          gate-2-and (->Gate2And "2and" gate-in-x1 gate-in-x2)]

      (testing "inputs"
        (is (= [{"x1" 0 "x2" 0}
                {"x1" 0 "x2" 1}
                {"x1" 1 "x2" 0}]
               (inputs gate-2-and 0)))
        (is (= [{"x1" 1 "x2" 1}]
               (inputs gate-2-and 1))))

      (testing "fixed-gate-inputs"
        (is (= [{"x2" 1}]
               (fixed-gate-inputs gate-2-and gate-in-x1)))
        (is (= [{"x1" 1}]
               (fixed-gate-inputs gate-2-and gate-in-x2))))

      (testing "activity-inputs"
        (is (= [{"x2" 1}]
               (activity-inputs gate-2-and gate-in-x1 nil)))
        (is (= [{"x1" 1}]
               (activity-inputs gate-2-and gate-in-x2 nil)))))))

(deftest testable-Gate2Nor-test
  (testing "TestableGate Gate2Nor"
    (let [gate-in-x1 (->GateIn "x1")
          gate-in-x2 (->GateIn "x2")
          gate-2-nor (->Gate2Nor "2nor" gate-in-x1 gate-in-x2)]

      (testing "inputs"
        (is (= [{"x1" 0 "x2" 1}
                {"x1" 1 "x2" 0}
                {"x1" 1 "x2" 1}]
               (inputs gate-2-nor 0)))
        (is (= [{"x1" 0 "x2" 0}]
               (inputs gate-2-nor 1))))

      (testing "fixed-gate-inputs"
        (is (= [{"x2" 0}]
               (fixed-gate-inputs gate-2-nor gate-in-x1)))
        (is (= [{"x1" 0}]
               (fixed-gate-inputs gate-2-nor gate-in-x2))))

      (testing "activity-inputs"
        (is (= [{"x2" 0}]
               (activity-inputs gate-2-nor gate-in-x1 nil)))
        (is (= [{"x1" 0}]
               (activity-inputs gate-2-nor gate-in-x2 nil)))))))

(deftest testable-Gate2Nand-test
  (testing "TestableGate Gate2Nand"
    (let [gate-in-x1 (->GateIn "x1")
          gate-in-x2 (->GateIn "x2")
          gate-2-nand (->Gate2Nand "2nand" gate-in-x1 gate-in-x2)]

      (testing "inputs"
        (is (= [{"x1" 1 "x2" 1}]
               (inputs gate-2-nand 0)))
        (is (= [{"x1" 0 "x2" 0}
                {"x1" 0 "x2" 1}
                {"x1" 1 "x2" 0}]
               (inputs gate-2-nand 1))))

      (testing "fixed-gate-inputs"
        (is (= [{"x2" 1}]
               (fixed-gate-inputs gate-2-nand gate-in-x1)))
        (is (= [{"x1" 1}]
               (fixed-gate-inputs gate-2-nand gate-in-x2))))

      (testing "activity-inputs"
        (is (= [{"x2" 1}]
               (activity-inputs gate-2-nand gate-in-x1 nil)))
        (is (= [{"x1" 1}]
               (activity-inputs gate-2-nand gate-in-x2 nil)))))))

(deftest testable-Gate3Nand-test
  (testing "Testable Gate3Nand"
    (let [gate-in-x1 (->GateIn "x1")
          gate-in-x2 (->GateIn "x2")
          gate-in-x3 (->GateIn "x3")
          gate-3-nand (->Gate3Nand "3nand" gate-in-x1 gate-in-x2 gate-in-x3)]

      (testing "inputs"
        (is (= [{"x1" 1 "x2" 1 "x3" 1}]
               (inputs gate-3-nand 0)))
        (is (= [{"x1" 0 "x2" 0 "x3" 0}
                {"x1" 0 "x2" 0 "x3" 1}
                {"x1" 0 "x2" 1 "x3" 0}
                {"x1" 0 "x2" 1 "x3" 1}
                {"x1" 1 "x2" 0 "x3" 0}
                {"x1" 1 "x2" 0 "x3" 1}
                {"x1" 1 "x2" 1 "x3" 0}]
               (inputs gate-3-nand 1))))

      (testing "fixed-gate-inputs"
        (is (= [{"x2" 1 "x3" 1}]
               (fixed-gate-inputs gate-3-nand gate-in-x1)))
        (is (= [{"x1" 1 "x3" 1}]
               (fixed-gate-inputs gate-3-nand gate-in-x2)))
        (is (= [{"x1" 1 "x2" 1}]
               (fixed-gate-inputs gate-3-nand gate-in-x3))))

      (testing "activity inputs"
        (is (= [{"x2" 1 "x3" 1}]
               (activity-inputs gate-3-nand gate-in-x1 nil)))
        (is (= [{"x1" 1 "x3" 1}]
               (activity-inputs gate-3-nand gate-in-x2 nil)))
        (is (= [{"x1" 1 "x2" 1}]
               (activity-inputs gate-3-nand gate-in-x3 nil)))))))
