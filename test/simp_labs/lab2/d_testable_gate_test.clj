(ns simp-labs.lab2.d-testable-gate-test
  (:require [clojure.test :refer :all]
            [simp-labs.common.gate :refer :all]
            [simp-labs.lab2.d-testable-gate :refer :all]))

(deftest d-op-test
  (testing "d-op"
    (is (= 0 (d-op 0 :xxxx)))
    (is (= 0 (d-op 0 0)))
    (is (= :nd (d-op 0 1)))
    (is (= 0 (d-op 0 :x)))
    (is (= :d (d-op 1 0)))
    (is (= 1 (d-op 1 1)))
    (is (= 1 (d-op 1 :x)))
    (is (= 0 (d-op :x 0)))
    (is (= 1 (d-op :x 1)))
    (is (= :x (d-op :x :x)))))

(deftest d-multi-op-test
  (testing "d-multi-op"
    (let [test-set [[{} {} {}]
                    [{"x1" 0 "x2" 1 "x3" :x}
                     {}
                     {"x1" 0 "x2" 1 "x3" :x}]
                    [{"x1" 0 "x2" 1 "x3" :x}
                     {"x1" 0 "x2" 1 "x3" :x}
                     {"x1" 0 "x2" 1 "x3" :x}]
                    [{"x1" 0 "x2" 1 "x3" :x}
                     {"x1" 1 "x2" :x "x3" 0}
                     {"x1" :nd "x2" 1 "x3" 0}]
                    [{"x1" 0 "x2" 1 "x3" :x}
                     {"x1" :x "x2" 0 "x3" 1}
                     {"x1" 0 "x2" :d "x3" 1}]
                    [{"x1" 1 "x2" :x "x3" 0}
                     {"x1" 0 "x2" 1 "x3" :x}
                     {"x1" :d "x2" 1 "x3" 0}]
                    [{"x1" :x "x2" 0 "x3" 1}
                     {"x1" 0 "x2" 1 "x3" :x}
                     {"x1" 0 "x2" :nd "x3" 1}]]]
      (doseq [[values1 values2 expected] test-set]
        (let [actual (d-multi-op values1 values2)]
          (is (= expected actual)))))))

(deftest d-cubes-from-singulars-test
  (testing "d-cubes-from-singulars"
    (let [singulars-1 [{"x1" 0 "x2" 0 "x3" :x}
                       {"x1" 0 "x2" 1 "x3" :x}
                       {"x1" 1 "x2" 1 "x3" :x}]
          singulars-2 [{"x1" 1 "x2" 0 "x3" 0}
                       {"x1" 1 "x2" 0 "x3" 1}
                       {"x1" 1 "x2" 0 "x3" :x}]
          expected [{"x1" :nd "x2" 0 "x3" 0}
                    {"x1" :nd "x2" 0 "x3" 1}
                    {"x1" :nd "x2" 0 "x3" :x}
                    {"x1" :nd "x2" :d "x3" 0}
                    {"x1" :nd "x2" :d "x3" 1}
                    {"x1" :nd "x2" :d "x3" :x}
                    {"x1" 1 "x2" :d "x3" 0}
                    {"x1" 1 "x2" :d "x3" 1}
                    {"x1" 1 "x2" :d "x3" :x}
                    {"x1" :d "x2" 0 "x3" 0}
                    {"x1" :d "x2" :nd "x3" 0}
                    {"x1" 1 "x2" :nd "x3" 0}
                    {"x1" :d "x2" 0 "x3" 1}
                    {"x1" :d "x2" :nd "x3" 1}
                    {"x1" 1 "x2" :nd "x3" 1}
                    {"x1" :d "x2" 0 "x3" :x}
                    {"x1" :d "x2" :nd "x3" :x}
                    {"x1" 1 "x2" :nd "x3" :x}]]
      (is (= expected (d-cubes-from-singulars singulars-1 singulars-2))))))

(deftest d-intersection-test
  (testing "d-intersection"
    (is (= 0 (d-intersection 0 0)))
    (is (= 1 (d-intersection 1 1)))
    (is (= 0 (d-intersection 0 :x)))
    (is (= 1 (d-intersection 1 :x)))
    (is (= 0 (d-intersection :x 0)))
    (is (= 1 (d-intersection :x 1)))
    (is (= :x (d-intersection :x :x)))
    (is (= :na (d-intersection 1 0)))
    (is (= :na (d-intersection 0 1)))))

(deftest d-cubes-intersection-test
  (testing "d-cubes-intersection"
    (is (= {:a 0 :b 1 :c :d :d :nd :e :x :f :x :g :x :h :x :i :x}
           (d-cubes-intersection {:a :x :b :x :c :x :d :x :e :x :f :x :g :x :h :x :i :x}
                                 {:a 0 :b 1 :c :d :d :nd :e :x})))
    (is (= {:a 0 :b 1 :c 0 :d 1 :e 0 :f 1 :g :x :h :na :i :na}
           (d-cubes-intersection {:a 0 :b 1 :c 0 :d 1 :e :x :f :x :g :x :h 1 :i 0}
                                 {:a 0 :b 1 :c :x :d :x :e 0 :f 1 :g :x :h 0 :i 1})))))

(deftest d-multi-cubes-intersection-test
  (testing "d-multi-cubes-intersection"
    (is (= [{:a 0 :b 1 :c 0}
            {:a 0 :b 1 :c 1}
            {:a 0 :b 1 :c :x}
            {:a 0 :b 0 :c 1}
            {:a 0 :b 1 :c 0}
            {:a 0 :b 1 :c 1}
            {:a 1 :b 0 :c 0}
            {:a 1 :b 0 :c 1}
            {:a 1 :b 1 :c 0}
            {:a 1 :b 1 :c 1}
            {:a :x :b :x :c :x}]
           (d-multi-cubes-intersection [{:a 0 :b 1 :c :x}
                                        {:a :x :b :x :c :x}]
                                       [{:a 0 :b 0 :c 1}
                                        {:a 0 :b 1 :c 0}
                                        {:a 0 :b 1 :c 1}
                                        {:a 1 :b 0 :c 0}
                                        {:a 1 :b 0 :c 1}
                                        {:a 1 :b 1 :c 0}
                                        {:a 1 :b 1 :c 1}
                                        {:a :x :b :x :c :x}])))))

(deftest d-multi-cubes-intersection-first-test
  (testing "d-multi-cubes-intersection-first"
    (is (= [{:a 0 :b 1 :c 0}]
           (d-multi-cubes-intersection-first [{:a 0 :b 1 :c :x}
                                              {:a :x :b :x :c :x}]
                                             [{:a 0 :b 0 :c 1}
                                              {:a 0 :b 1 :c 0}
                                              {:a 0 :b 1 :c 1}
                                              {:a 1 :b 0 :c 0}
                                              {:a 1 :b 0 :c 1}
                                              {:a 1 :b 1 :c 0}
                                              {:a 1 :b 1 :c 1}
                                              {:a :x :b :x :c :x}])))))

(deftest d-testable-GateIn-test
  (testing "DTestableGate GateIn"
    (let [gate-in (->GateIn "x1")]

      (testing "d-backward-intersection"
        (let [cubes [{:a 1}
                     {:b 2}]]
          (is (= cubes (d-backward-intersection gate-in cubes))))))))

(deftest d-testable-GateNot-test
  (testing "DTestableGate GateNot"
    (let [gate-in (->GateIn "x1")
          gate-not (->GateNot "not" gate-in)]

      (testing "singular cubes"
        (is (= [{"x1" 1 "not" 0}]
               (singular-cubes gate-not 0)))
        (is (= [{"x1" 0 "not" 1}]
               (singular-cubes gate-not 1))))

      (testing "d-cubes"
        (is (= [{"x1" :d "not" :nd}
                {"x1" :nd "not" :d}]
               (d-cubes gate-not))))

      (testing "fault-d-cubes"
        (is (= [{"x1" 0 "not" :d}]
               (fault-d-cubes gate-not 0)))
        (is (= [{"x1" 1 "not" :nd}]
               (fault-d-cubes gate-not 1)))))))

(deftest d-testable-Gate2Or-test
  (testing "DTestableGate Gate2Or"
    (let [gate-in-x1 (->GateIn "x1")
          gate-in-x2 (->GateIn "x2")
          gate-not (->GateNot "not" gate-in-x1)
          gate-2-or (->Gate2Or "2or" gate-not gate-in-x2)]

      (testing "singular cubes"
        (is (= [{"not" 0 "x2" 0 "2or" 0}]
               (singular-cubes gate-2-or 0)))
        (is (= [{"not" 1 "x2" :x "2or" 1}
                {"not" :x "x2" 1 "2or" 1}]
               (singular-cubes gate-2-or 1))))

      (testing "d-cubes"
        (is (= [{"not" :nd "x2" 0 "2or" :nd}
                {"not" 0 "x2" :nd "2or" :nd}
                {"not" :d "x2" 0 "2or" :d}
                {"not" 0 "x2" :d "2or" :d}]
               (d-cubes gate-2-or))))

      (testing "fault-d-cubes"
        (is (= [{"not" 1 "x2" :x "2or" :d}
                {"not" :x "x2" 1 "2or" :d}]
               (fault-d-cubes gate-2-or 0)))
        (is (= [{"not" 0 "x2" 0 "2or" :nd}]
               (fault-d-cubes gate-2-or 1)))))))

(deftest d-testable-Gate2And-test
  (testing "DTestableGate Gate2And"
    (let [gate-in-x1 (->GateIn "x1")
          gate-in-x2 (->GateIn "x2")
          gate-not (->GateNot "not" gate-in-x1)
          gate-2-and (->Gate2And "2and" gate-not gate-in-x2)]

      (testing "singular cubes"
        (is (= [{"not" 0 "x2" :x "2and" 0}
                {"not" :x "x2" 0 "2and" 0}]
               (singular-cubes gate-2-and 0)))
        (is (= [{"not" 1 "x2" 1 "2and" 1}]
               (singular-cubes gate-2-and 1))))

      (testing "d-cubes"
        (is (= [{"not" :nd "x2" 1 "2and" :nd}
                {"not" 1 "x2" :nd "2and" :nd}
                {"not" :d "x2" 1 "2and" :d}
                {"not" 1 "x2" :d "2and" :d}]
               (d-cubes gate-2-and))))

      (testing "fault-d-cubes"
        (is (= [{"not" 1 "x2" 1 "2and" :d}]
               (fault-d-cubes gate-2-and 0)))
        (is (= [{"not" 0 "x2" :x "2and" :nd}
                {"not" :x "x2" 0 "2and" :nd}]
               (fault-d-cubes gate-2-and 1)))))))

(deftest d-testable-Gate2Nor-test
  (testing "DTestable Gate2Nor"
    (let [gate-in-x1 (->GateIn "x1")
          gate-in-x2 (->GateIn "x2")
          gate-2-nor (->Gate2Nor "2nor" gate-in-x1 gate-in-x2)]

      (testing "singular cubes"
        (is (= [{"x1" 1 "x2" :x "2nor" 0}
                {"x1" :x "x2" 1 "2nor" 0}]
               (singular-cubes gate-2-nor 0)))
        (is (= [{"x1" 0 "x2" 0 "2nor" 1}]
               (singular-cubes gate-2-nor 1))))

      (testing "d-cubes"
        (is (= [{"x1" :d "x2" 0 "2nor" :nd}
                {"x1" 0 "x2" :d "2nor" :nd}
                {"x1" :nd "x2" 0 "2nor" :d}
                {"x1" 0 "x2" :nd "2nor" :d}]
               (d-cubes gate-2-nor))))

      (testing "fault-d-cubes"
        (is (= [{"x1" 0 "x2" 0 "2nor" :d}]
               (fault-d-cubes gate-2-nor 0)))
        (is (= [{"x1" 1 "x2" :x "2nor" :nd}
                {"x1" :x "x2" 1 "2nor" :nd}]
               (fault-d-cubes gate-2-nor 1)))))))

(deftest d-testable-Gate2Nand-test
  (testing "DTestable Gate2Nand"
    (let [gate-in-x1 (->GateIn "x1")
          gate-in-x2 (->GateIn "x2")
          gate-2-nand (->Gate2Nand "2nand" gate-in-x1 gate-in-x2)]

      (testing "singular cubes"
        (is (= [{"x1" 1 "x2" 1 "2nand" 0}]
               (singular-cubes gate-2-nand 0)))
        (is (= [{"x1" 0 "x2" :x "2nand" 1}
                {"x1" :x "x2" 0 "2nand" 1}]
               (singular-cubes gate-2-nand 1))))

      (testing "d-cubes"
        (is (= [{"x1" :d "x2" 1 "2nand" :nd}
                {"x1" 1 "x2" :d "2nand" :nd}
                {"x1" :nd "x2" 1 "2nand" :d}
                {"x1" 1 "x2" :nd "2nand" :d}]
               (d-cubes gate-2-nand))))

      (testing "fault-d-cubes"
        (is (= [{"x1" 0 "x2" :x "2nand" :d}
                {"x1" :x "x2" 0 "2nand" :d}]
               (fault-d-cubes gate-2-nand 0)))
        (is (= [{"x1" 1 "x2" 1 "2nand" :nd}]
               (fault-d-cubes gate-2-nand 1)))))))

(deftest d-testable-Gate3Nand-test
  (testing "DTestable Gate3Nand"
    (let [gate-in-x1 (->GateIn "x1")
          gate-in-x2 (->GateIn "x2")
          gate-in-x3 (->GateIn "x3")
          gate-3-nand (->Gate3Nand "3nand" gate-in-x1 gate-in-x2 gate-in-x3)]

      (testing "singular cubes"
        (is (= [{"x1" 1 "x2" 1 "x3" 1 "3nand" 0}]
               (singular-cubes gate-3-nand 0)))

        (is (= [{"x1" 0 "x2" :x "x3" :x "3nand" 1}
                {"x1" :x "x2" 0 "x3" :x "3nand" 1}
                {"x1" :x "x2" :x "x3" 0 "3nand" 1}]
               (singular-cubes gate-3-nand 1))))

      (testing "d-cubes"
        (is (= [{"x1" :d "x2" 1 "x3" 1 "3nand" :nd}
                {"x1" 1 "x2" :d "x3" 1 "3nand" :nd}
                {"x1" 1 "x2" 1 "x3" :d "3nand" :nd}
                {"x1" :nd "x2" 1 "x3" 1 "3nand" :d}
                {"x1" 1 "x2" :nd "x3" 1 "3nand" :d}
                {"x1" 1 "x2" 1 "x3" :nd "3nand" :d}]
               (d-cubes gate-3-nand))))

      (testing "fault-d-cubes"
        (is (= [{"x1" 0 "x2" :x "x3" :x "3nand" :d}
                {"x1" :x "x2" 0 "x3" :x "3nand" :d}
                {"x1" :x "x2" :x "x3" 0 "3nand" :d}]
               (fault-d-cubes gate-3-nand 0)))
        (is (= [{"x1" 1 "x2" 1 "x3" 1 "3nand" :nd}]
               (fault-d-cubes gate-3-nand 1)))))))
