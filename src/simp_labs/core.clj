(ns simp-labs.core
  (:gen-class)
  (:require [simp-labs.lab1.main :as lab1]
            [simp-labs.lab2.main :as lab2]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (lab1/define-test-sets)
  (lab2/define-test-cubes))
