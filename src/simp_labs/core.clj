(ns simp-labs.core
  (:gen-class)
  (:require [simp-labs.lab1.main :as lab1]
            [simp-labs.lab2.main :as lab2]
            [simp-labs.lab3.main :as lab3]
            [simp-labs.lab4.main :as lab4]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (lab1/define-test-sets)
  (lab2/define-test-cubes)
  (lab3/lfsr-test-sets)
  (lab4/run-ram-tests))
