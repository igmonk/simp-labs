(ns simp-labs.common.util-test
  (:require [clojure.test :refer :all]
            [simp-labs.common.util :refer :all]))

(deftest not-empty?-test
  (testing "not-empty?"
    (let [test-data [[nil false]
                     [[] false]
                     [[1 2 3] true]]]
      (doseq [[coll expected] test-data]
        (let [actual (not-empty? coll)]
          (is (= expected actual)))))))

(deftest cartesian-product-test
  (testing "cartesian-product"
    (let [coll1 [111]
          coll2 [211 212]
          coll3 [311 312 313]
          expected [[111 211 311]
                    [111 211 312]
                    [111 211 313]
                    [111 212 311]
                    [111 212 312]
                    [111 212 313]]]
      (is (= expected (cartesian-product coll1 coll2 coll3))))))

(deftest merge-maps-test
  (testing "merge-maps"
    (is (= {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6}
           (merge-maps [{:a 1 :b 2}
                        {:c 3 :d 4}
                        {:e 5 :f 6}])))))

(deftest not-equal-test
  (testing "not-equal"
    (is (= [2 3 2 3] (not-equal 1 [1 2 3 1 2 3])))
    (is (= [] (not-equal 1 [1 1 1 1 1 1])))
    (is (= [] (not-equal 1 [])))
    (is (= [10 20 30] (not-equal 1 [10 20 30])))))

(deftest first-not-equal-test
  (testing "first-not-equal"
    (is (= 1 (first-not-equal 2 1 2 3 4 5)))
    (is (= 2 (first-not-equal 1 1 2 3 4 5)))
    (is (= 1 (first-not-equal 3 1 2 3 4 5)))))

(deftest n-not-equal-test
  (testing "n-not-equal"
    (is (= [:c] (n-not-equal 1 :a :c :b :a)))
    (is (= [:x :y] (n-not-equal 2 :a :x :y :z)))
    (is (= [:x] (n-not-equal 3 :a :a :a :a :x)))
    (is (= [] (n-not-equal 3 :a :a :a :a :a)))))

(deftest sort-maps-test
  (testing "sort-maps"
    (is (= [{:c 3 :b 2 :a 1}
            {:e 22 :f 33 :d 11}
            {:x 100 :y 200 :z 300}]
           [{:a 1 :b 2 :c 3}
            {:d 11 :e 22 :f 33}
            {:x 100 :y 200 :z 300}]))))
