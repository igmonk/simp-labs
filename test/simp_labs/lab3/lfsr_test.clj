(ns simp-labs.lab3.lfsr-test
  (:require [clojure.test :refer :all]
            [simp-labs.lab3.lfsr :refer :all]))

(deftest xor-test
  (testing "xor"
    (is (= 0 (xor [])))
    (is (= 1 (xor [1])))
    (is (= 0 (xor [0 0])))
    (is (= 0 (xor [1 1])))
    (is (= 1 (xor [1 0])))
    (is (= 1 (xor [0 1])))))

(deftest polynomial-x8+x7+x6+x3+x2+x+1-test
  (testing "polynomial-x8+x7+x6+x3+x2+x+1"
    (is (= 0 (polynomial-x8+x7+x6+x3+x2+x+1 1 1 1 1 1 1 1 1)))
    (is (= 0 (polynomial-x8+x7+x6+x3+x2+x+1 0 0 0 0 0 0 0 0)))
    (is (= 0 (polynomial-x8+x7+x6+x3+x2+x+1 1 1 1 0 0 1 1 1)))
    (is (= 0 (polynomial-x8+x7+x6+x3+x2+x+1 0 0 0 1 1 0 0 0)))

    (is (= 1 (polynomial-x8+x7+x6+x3+x2+x+1 0 0 0 0 0 0 0 1)))
    (is (= 1 (polynomial-x8+x7+x6+x3+x2+x+1 0 0 0 0 0 0 1 0)))
    (is (= 1 (polynomial-x8+x7+x6+x3+x2+x+1 0 0 0 0 0 1 0 0)))
    (is (= 0 (polynomial-x8+x7+x6+x3+x2+x+1 0 0 0 0 1 0 0 0)))
    (is (= 0 (polynomial-x8+x7+x6+x3+x2+x+1 0 0 0 1 0 0 0 0)))
    (is (= 1 (polynomial-x8+x7+x6+x3+x2+x+1 0 0 1 0 0 0 0 0)))
    (is (= 1 (polynomial-x8+x7+x6+x3+x2+x+1 0 1 0 0 0 0 0 0)))
    (is (= 1 (polynomial-x8+x7+x6+x3+x2+x+1 1 0 0 0 0 0 0 0)))

    (is (= 1 (polynomial-x8+x7+x6+x3+x2+x+1 1 1 1 0 0 1 1 0)))
    (is (= 1 (polynomial-x8+x7+x6+x3+x2+x+1 1 1 1 0 0 0 0 0)))))

(deftest polynomial-x7+x5+x2+x1+1-test
  (testing "polynomial-x7+x5+x2+x+1"
    (is (= 0 (polynomial-x7+x5+x2+x+1 1 1 1 1 1 1 1)))
    (is (= 0 (polynomial-x7+x5+x2+x+1 0 0 0 0 0 0 0)))
    (is (= 0 (polynomial-x7+x5+x2+x+1 1 1 0 0 1 0 1)))
    (is (= 0 (polynomial-x7+x5+x2+x+1 0 0 1 1 0 1 0)))

    (is (= 1 (polynomial-x7+x5+x2+x+1 0 0 0 0 0 0 1)))
    (is (= 0 (polynomial-x7+x5+x2+x+1 0 0 0 0 0 1 0)))
    (is (= 1 (polynomial-x7+x5+x2+x+1 0 0 0 0 1 0 0)))
    (is (= 0 (polynomial-x7+x5+x2+x+1 0 0 0 1 0 0 0)))
    (is (= 0 (polynomial-x7+x5+x2+x+1 0 0 1 0 0 0 0)))
    (is (= 1 (polynomial-x7+x5+x2+x+1 0 1 0 0 0 0 0)))
    (is (= 1 (polynomial-x7+x5+x2+x+1 1 0 0 0 0 0 0)))

    (is (= 1 (polynomial-x7+x5+x2+x+1 1 1 0 0 0 0 1)))))
