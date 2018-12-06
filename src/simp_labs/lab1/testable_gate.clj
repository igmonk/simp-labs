(ns simp-labs.lab1.testable-gate
  (:require [simp-labs.common.gate :refer :all]
            [simp-labs.common.util :as util])
  (:import [simp_labs.common.gate GateIn
                                  GateNot
                                  Gate2Or
                                  Gate2And
                                  Gate2Nor
                                  Gate2Nand
                                  Gate3Nand
                                  Gate3Nor]))

(defprotocol TestableGate
  (inputs [gate output])
  (fixed-gate-inputs [gate active-gate])
  (activity-inputs [gate active-gate gate-path])
  (test-inputs [gate gate-path output]))

(defn combine-inputs [& inputs]
  (->> (filter util/not-empty? inputs)
       (apply util/cartesian-product)
       (map util/merge-maps)
       distinct))

(defn combine-input-gates [& gate-combinations]
  (->> (map #(apply combine-inputs %) gate-combinations)
       (apply concat)))

(extend-type GateIn
  TestableGate
  (inputs [{:keys [id]} output]
    [{id output}])
  (fixed-gate-inputs [_ _] [])
  (activity-inputs [gate _ gate-path]
    (activity-inputs (peek gate-path) gate (util/pop-silently gate-path)))
  (test-inputs [{:keys [id] :as gate} gate-path output]
    {id (combine-inputs (inputs gate output)
                        (activity-inputs (peek gate-path) gate (util/pop-silently gate-path)))}))

(extend-type GateNot
  TestableGate
  (inputs [{:keys [in]} output]
    (case output
      0 (inputs in 1)
      1 (inputs in 0)))
  (fixed-gate-inputs [_ _] [])
  (activity-inputs [gate _ gate-path]
    (activity-inputs (peek gate-path) gate (util/pop-silently gate-path)))
  (test-inputs [{:keys [id in] :as gate} gate-path output]
    (merge {id (combine-inputs (inputs gate output)
                               (activity-inputs (peek gate-path) gate (util/pop-silently gate-path)))}
           (test-inputs in (conj gate-path gate) output))))

(extend-type Gate2Or
  TestableGate
  (inputs [{:keys [in1 in2]} output]
    (case output
      0 (combine-input-gates [(inputs in1 0) (inputs in2 0)])
      1 (combine-input-gates [(inputs in1 0) (inputs in2 1)]
                             [(inputs in1 1) (inputs in2 0)]
                             [(inputs in1 1) (inputs in2 1)])))
  (fixed-gate-inputs [{:keys [in1 in2]} active-gate]
    (inputs (util/first-not-equal active-gate in1 in2) 0))
  (activity-inputs [gate active-gate gate-path]
    (combine-inputs (fixed-gate-inputs gate active-gate)
                    (activity-inputs (peek gate-path) gate (util/pop-silently gate-path))))
  (test-inputs [{:keys [id in1 in2] :as gate} gate-path output]
    (merge {id (combine-inputs (inputs gate output)
                               (activity-inputs (peek gate-path) gate (util/pop-silently gate-path)))}
           (test-inputs in1 (conj gate-path gate) output)
           (test-inputs in2 (conj gate-path gate) output))))

(extend-type Gate2And
  TestableGate
  (inputs [{:keys [in1 in2]} output]
    (case output
      0 (combine-input-gates [(inputs in1 0) (inputs in2 0)]
                             [(inputs in1 0) (inputs in2 1)]
                             [(inputs in1 1) (inputs in2 0)])
      1 (combine-input-gates [(inputs in1 1) (inputs in2 1)])))
  (fixed-gate-inputs [{:keys [in1 in2]} active-gate]
    (inputs (util/first-not-equal active-gate in1 in2) 1))
  (activity-inputs [gate active-gate gate-path]
    (combine-inputs (fixed-gate-inputs gate active-gate)
                    (activity-inputs (peek gate-path) gate (util/pop-silently gate-path))))
  (test-inputs [{:keys [id in1 in2] :as gate} gate-path output]
    (merge {id (combine-inputs (inputs gate output)
                               (activity-inputs (peek gate-path) gate (util/pop-silently gate-path)))}
           (test-inputs in1 (conj gate-path gate) output)
           (test-inputs in2 (conj gate-path gate) output))))

(extend-type Gate2Nor
  TestableGate
  (inputs [{:keys [in1 in2]} output]
    (case output
      0 (combine-input-gates [(inputs in1 0) (inputs in2 1)]
                             [(inputs in1 1) (inputs in2 0)]
                             [(inputs in1 1) (inputs in2 1)])
      1 (combine-input-gates [(inputs in1 0) (inputs in2 0)])))
  (fixed-gate-inputs [{:keys [in1 in2]} active-gate]
    (inputs (util/first-not-equal active-gate in1 in2) 0))
  (activity-inputs [gate active-gate gate-path]
    (combine-inputs (fixed-gate-inputs gate active-gate)
                    (activity-inputs (peek gate-path) gate (util/pop-silently gate-path))))
  (test-inputs [{:keys [id in1 in2] :as gate} gate-path output]
    (merge {id (combine-inputs (inputs gate output)
                               (activity-inputs (peek gate-path) gate (util/pop-silently gate-path)))}
           (test-inputs in1 (conj gate-path gate) output)
           (test-inputs in2 (conj gate-path gate) output))))

(extend-type Gate2Nand
  TestableGate
  (inputs [{:keys [in1 in2]} output]
    (case output
      0 (combine-input-gates [(inputs in1 1) (inputs in2 1)])
      1 (combine-input-gates [(inputs in1 0) (inputs in2 0)]
                             [(inputs in1 0) (inputs in2 1)]
                             [(inputs in1 1) (inputs in2 0)])))
  (fixed-gate-inputs [{:keys [in1 in2]} active-gate]
    (inputs (util/first-not-equal active-gate in1 in2) 1))
  (activity-inputs [gate active-gate gate-path]
    (combine-inputs (fixed-gate-inputs gate active-gate)
                    (activity-inputs (peek gate-path) gate (util/pop-silently gate-path))))
  (test-inputs [{:keys [id in1 in2] :as gate} gate-path output]
    (merge {id (combine-inputs (inputs gate output)
                               (activity-inputs (peek gate-path) gate (util/pop-silently gate-path)))}
           (test-inputs in1 (conj gate-path gate) output)
           (test-inputs in2 (conj gate-path gate) output))))

(extend-type Gate3Nand
  TestableGate
  (inputs [{:keys [in1 in2 in3]} output]
    (case output
      0 (combine-input-gates [(inputs in1 1) (inputs in2 1) (inputs in3 1)])
      1 (combine-input-gates [(inputs in1 0) (inputs in2 0) (inputs in3 0)]
                             [(inputs in1 0) (inputs in2 0) (inputs in3 1)]
                             [(inputs in1 0) (inputs in2 1) (inputs in3 0)]
                             [(inputs in1 0) (inputs in2 1) (inputs in3 1)]
                             [(inputs in1 1) (inputs in2 0) (inputs in3 0)]
                             [(inputs in1 1) (inputs in2 0) (inputs in3 1)]
                             [(inputs in1 1) (inputs in2 1) (inputs in3 0)])))
  (fixed-gate-inputs [{:keys [in1 in2 in3]} active-gate]
    (->> (util/n-not-equal 2 active-gate in1 in2 in3)
         (map #(inputs % 1))
         (apply combine-inputs)))
  (activity-inputs [gate active-gate gate-path]
    (combine-inputs (fixed-gate-inputs gate active-gate)
                    (activity-inputs (peek gate-path) gate (util/pop-silently gate-path))))
  (test-inputs [{:keys [id in1 in2 in3] :as gate} gate-path output]
    (merge {id (combine-inputs (inputs gate output)
                               (activity-inputs (peek gate-path) gate (util/pop-silently gate-path)))}
           (test-inputs in1 (conj gate-path gate) output)
           (test-inputs in2 (conj gate-path gate) output)
           (test-inputs in3 (conj gate-path gate) output))))

(extend-type Gate3Nor
  TestableGate
  (inputs [{:keys [in1 in2 in3]} output]
    (case output
      1 (combine-input-gates [(inputs in1 0) (inputs in2 0) (inputs in3 0)])
      0 (combine-input-gates [(inputs in1 0) (inputs in2 0) (inputs in3 1)]
                             [(inputs in1 0) (inputs in2 1) (inputs in3 0)]
                             [(inputs in1 0) (inputs in2 1) (inputs in3 1)]
                             [(inputs in1 1) (inputs in2 0) (inputs in3 0)]
                             [(inputs in1 1) (inputs in2 0) (inputs in3 1)]
                             [(inputs in1 1) (inputs in2 1) (inputs in3 0)]
                             [(inputs in1 1) (inputs in2 1) (inputs in3 1)])))
  (fixed-gate-inputs [{:keys [in1 in2 in3]} active-gate]
    (->> (util/n-not-equal 2 active-gate in1 in2 in3)
         (map #(inputs % 1))
         (apply combine-inputs)))
  (activity-inputs [gate active-gate gate-path]
    (combine-inputs (fixed-gate-inputs gate active-gate)
                    (activity-inputs (peek gate-path) gate (util/pop-silently gate-path))))
  (test-inputs [{:keys [id in1 in2 in3] :as gate} gate-path output]
    (merge {id (combine-inputs (inputs gate output)
                               (activity-inputs (peek gate-path) gate (util/pop-silently gate-path)))}
           (test-inputs in1 (conj gate-path gate) output)
           (test-inputs in2 (conj gate-path gate) output)
           (test-inputs in3 (conj gate-path gate) output))))

(extend-type nil
  TestableGate
  (inputs [_ _] [])
  (fixed-gate-inputs [_ _] [])
  (activity-inputs [_ _ _] [])
  (test-inputs [_ _ _] {}))
