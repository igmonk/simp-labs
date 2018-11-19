(ns simp-labs.lab2.d-testable-gate
  (:require [simp-labs.common.gate :refer :all]
            [simp-labs.common.util :as util])
  (:import [simp_labs.common.gate GateIn
                                  GateNot
                                  Gate2Or
                                  Gate2And
                                  Gate2Nor
                                  Gate2Nand
                                  Gate3Nand]))

(defprotocol DTestableGate
  (singular-cubes [gate output])
  (all-singular-cubes [gate])
  (d-cubes [gate])
  (fault-d-cubes [gate fault])
  (d-backward-intersection [gate cubes])
  (d-activation [gate active-gate fault-cubes gate-path])
  (test-cubes [gate gate-path fault init-cube]))

(def d-op-map
  {0 {0 0 1 :nd :x 0}
   1 {0 :d 1 1 :x 1}
   :x {0 0 1 1 :x :x}})

(defn d-op [value1 value2]
  (get-in d-op-map [value1 value2] value1))

(defn d-multi-op [values1 values2]
  (merge-with d-op values1 values2))

(defn d-cubes-from-singulars [singulars-1 singulars-2]
  (letfn [(apply-cubes-op [cubes-1 cubes-2]
            (for [cube-1 cubes-1
                  cube-2 cubes-2]
              (d-multi-op cube-1 cube-2)))]
    (concat (apply-cubes-op singulars-1 singulars-2)
            (apply-cubes-op singulars-2 singulars-1))))

(defn d-intersection [a b]
  (cond
    (or (= b a) (= b :x)) a
    (or (= a b) (= a :x)) b
    :else :na))

(defn d-cubes-intersection [a-cube b-cube]
  (merge-with d-intersection a-cube b-cube))

(defn d-multi-cubes-intersection [a-cubes b-cubes]
  (letfn [(apply-intersection [cubes-1 cubes-2]
            (for [cube-1 cubes-1
                  cube-2 cubes-2]
              (d-cubes-intersection cube-1 cube-2)))
          (valid-intersection [intersection]
            (not (some #(= :na %) (vals intersection))))]
    (->> (apply-intersection a-cubes b-cubes)
         (filter valid-intersection))))

(defn d-multi-cubes-intersection-first [a-cubes b-cubes]
  (->> (d-multi-cubes-intersection a-cubes b-cubes)
       first
       (conj [])))

(defn d-init [cubes init-cube]
  (-> (take (count cubes) (repeat init-cube))
      (d-multi-cubes-intersection cubes)))

(extend-type GateIn
  DTestableGate
  (singular-cubes [_ _] [])
  (all-singular-cubes [_] [])
  (d-cubes [_] [])
  (fault-d-cubes [_ _] [])
  (d-backward-intersection [_ cubes] cubes)
  (d-activation [_ _ _ _] [])
  (test-cubes [_ _ _ _] {}))

(extend-type GateNot
  DTestableGate
  (singular-cubes [{:keys [id in]} output]
    (case output
      0 [{(:id in) 1 id 0}]
      1 [{(:id in) 0 id 1}]))
  (all-singular-cubes [gate]
    (concat (singular-cubes gate 0)
            (singular-cubes gate 1)))
  (d-cubes [gate]
    (d-cubes-from-singulars (singular-cubes gate 0)
                            (singular-cubes gate 1)))
  (fault-d-cubes [{:keys [id in]} fault]
    (case fault
      0 [{(:id in) 0 id :d}]
      1 [{(:id in) 1 id :nd}]))
  (d-backward-intersection [_ cubes] cubes)
  (d-activation [gate _ cubes gate-path]
    (if-let [cc (d-multi-cubes-intersection-first cubes (d-cubes gate))]
      (d-activation (peek gate-path) gate cc (util/pop-silently gate-path))
      []))
  (test-cubes [{:keys [id in] :as gate} gate-path fault init-cube]
    (let [fault-cubes (d-init (fault-d-cubes gate fault) init-cube)
          activation-cubes (d-activation (peek gate-path) gate fault-cubes (util/pop-silently gate-path))]
      (merge {id (d-backward-intersection in activation-cubes)}
             (test-cubes in (conj gate-path gate) fault init-cube)))))

(extend-type Gate2Or
  DTestableGate
  (singular-cubes [{:keys [id in1 in2]} output]
    (let [id1 (:id in1)
          id2 (:id in2)]
      (case output
        0 [{id1 0 id2 0 id 0}]
        1 [{id1 1 id2 :x id 1}
           {id1 :x id2 1 id 1}])))
  (all-singular-cubes [gate]
    (concat (singular-cubes gate 0)
            (singular-cubes gate 1)))
  (d-cubes [gate]
    (d-cubes-from-singulars (singular-cubes gate 0)
                            (singular-cubes gate 1)))
  (fault-d-cubes [{:keys [id in1 in2]} fault]
    (let [id1 (:id in1)
          id2 (:id in2)]
      (case fault
        0 [{id1 1 id2 :x id :d}
           {id1 :x id2 1 id :d}]
        1 [{id1 0 id2 0 id :nd}])))
  (d-backward-intersection [{:keys [in1 in2] :as gate} cubes]
    (if-let [cc (d-multi-cubes-intersection-first cubes (all-singular-cubes gate))]
      (distinct
        (concat
          (d-backward-intersection in1 cc)
          (d-backward-intersection in2 cc)))
      []))
  (d-activation [{:keys [in1 in2] :as gate} active-gate cubes gate-path]
    (if-let [cc (d-multi-cubes-intersection-first cubes (d-cubes gate))]
      (->> (d-activation (peek gate-path) gate cc (util/pop-silently gate-path))
           (d-backward-intersection (util/first-not-equal active-gate in1 in2)))
      []))
  (test-cubes [{:keys [id in1 in2] :as gate} gate-path fault init-cube]
    (let [fault-cubes (d-init (fault-d-cubes gate fault) init-cube)
          activation-cubes (d-activation (peek gate-path) gate fault-cubes (util/pop-silently gate-path))]
      (merge {id (->> [in1 in2]
                      (map #(d-backward-intersection % activation-cubes))
                      (apply concat)
                      distinct)}
             (test-cubes in1 (conj gate-path gate) fault init-cube)
             (test-cubes in2 (conj gate-path gate) fault init-cube)))))

(extend-type Gate2And
  DTestableGate
  (singular-cubes [{:keys [id in1 in2]} output]
    (let [id1 (:id in1)
          id2 (:id in2)]
      (case output
        0 [{id1 0 id2 :x id 0}
           {id1 :x id2 0 id 0}]
        1 [{id1 1 id2 1 id 1}])))
  (all-singular-cubes [gate]
    (concat (singular-cubes gate 0)
            (singular-cubes gate 1)))
  (d-cubes [gate]
    (d-cubes-from-singulars (singular-cubes gate 0)
                            (singular-cubes gate 1)))
  (fault-d-cubes [{:keys [id in1 in2]} fault]
    (let [id1 (:id in1)
          id2 (:id in2)]
      (case fault
        0 [{id1 1 id2 1 id :d}]
        1 [{id1 0 id2 :x id :nd}
           {id1 :x id2 0 id :nd}])))
  (d-backward-intersection [{:keys [in1 in2] :as gate} cubes]
    (if-let [cc (d-multi-cubes-intersection-first cubes (all-singular-cubes gate))]
      (distinct
        (concat
          (d-backward-intersection in1 cc)
          (d-backward-intersection in2 cc)))
      []))
  (d-activation [{:keys [in1 in2] :as gate} active-gate cubes gate-path]
    (if-let [cc (d-multi-cubes-intersection-first cubes (d-cubes gate))]
      (->> (d-activation (peek gate-path) gate cc (util/pop-silently gate-path))
           (d-backward-intersection (util/first-not-equal active-gate in1 in2)))
      []))
  (test-cubes [{:keys [id in1 in2] :as gate} gate-path fault init-cube]
    (let [fault-cubes (d-init (fault-d-cubes gate fault) init-cube)
          activation-cubes (d-activation (peek gate-path) gate fault-cubes (util/pop-silently gate-path))]
      (merge {id (->> [in1 in2]
                      (map #(d-backward-intersection % activation-cubes))
                      (apply concat)
                      distinct)}
             (test-cubes in1 (conj gate-path gate) fault init-cube)
             (test-cubes in2 (conj gate-path gate) fault init-cube)))))

(extend-type Gate2Nor
  DTestableGate
  (singular-cubes [{:keys [id in1 in2]} output]
    (let [id1 (:id in1)
          id2 (:id in2)]
      (case output
        0 [{id1 1 id2 :x id 0}
           {id1 :x id2 1 id 0}]
        1 [{id1 0 id2 0 id 1}])))
  (all-singular-cubes [gate]
    (concat (singular-cubes gate 0)
            (singular-cubes gate 1)))
  (d-cubes [gate]
    (d-cubes-from-singulars (singular-cubes gate 0)
                            (singular-cubes gate 1)))
  (fault-d-cubes [{:keys [id in1 in2]} fault]
    (let [id1 (:id in1)
          id2 (:id in2)]
      (case fault
        0 [{id1 0 id2 0 id :d}]
        1 [{id1 1 id2 :x id :nd}
           {id1 :x id2 1 id :nd}])))
  (d-backward-intersection [{:keys [in1 in2] :as gate} cubes]
    (if-let [cc (d-multi-cubes-intersection-first cubes (all-singular-cubes gate))]
      (distinct
        (concat
          (d-backward-intersection in1 cc)
          (d-backward-intersection in2 cc)))
      []))
  (d-activation [{:keys [in1 in2] :as gate} active-gate cubes gate-path]
    (if-let [cc (d-multi-cubes-intersection-first cubes (d-cubes gate))]
      (->> (d-activation (peek gate-path) gate cc (util/pop-silently gate-path))
           (d-backward-intersection (util/first-not-equal active-gate in1 in2)))
      []))
  (test-cubes [{:keys [id in1 in2] :as gate} gate-path fault init-cube]
    (let [fault-cubes (d-init (fault-d-cubes gate fault) init-cube)
          activation-cubes (d-activation (peek gate-path) gate fault-cubes (util/pop-silently gate-path))]
      (merge {id (->> [in1 in2]
                      (map #(d-backward-intersection % activation-cubes))
                      (apply concat)
                      distinct)}
             (test-cubes in1 (conj gate-path gate) fault init-cube)
             (test-cubes in2 (conj gate-path gate) fault init-cube)))))

(extend-type Gate2Nand
  DTestableGate
  (singular-cubes [{:keys [id in1 in2]} output]
    (let [id1 (:id in1)
          id2 (:id in2)]
      (case output
        0 [{id1 1 id2 1 id 0}]
        1 [{id1 0 id2 :x id 1}
           {id1 :x id2 0 id 1}])))
  (all-singular-cubes [gate]
    (concat (singular-cubes gate 0)
            (singular-cubes gate 1)))
  (d-cubes [gate]
    (d-cubes-from-singulars (singular-cubes gate 0)
                            (singular-cubes gate 1)))
  (fault-d-cubes [{:keys [id in1 in2]} fault]
    (let [id1 (:id in1)
          id2 (:id in2)]
      (case fault
        0 [{id1 0 id2 :x id :d}
           {id1 :x id2 0 id :d}]
        1 [{id1 1 id2 1 id :nd}])))
  (d-backward-intersection [{:keys [in1 in2] :as gate} cubes]
    (if-let [cc (d-multi-cubes-intersection-first cubes (all-singular-cubes gate))]
      (distinct
        (concat
          (d-backward-intersection in1 cc)
          (d-backward-intersection in2 cc)))
      []))
  (d-activation [{:keys [in1 in2] :as gate} active-gate cubes gate-path]
    (if-let [cc (d-multi-cubes-intersection-first cubes (d-cubes gate))]
      (->> (d-activation (peek gate-path) gate cc (util/pop-silently gate-path))
           (d-backward-intersection (util/first-not-equal active-gate in1 in2)))
      []))
  (test-cubes [{:keys [id in1 in2] :as gate} gate-path fault init-cube]
    (let [fault-cubes (d-init (fault-d-cubes gate fault) init-cube)
          activation-cubes (d-activation (peek gate-path) gate fault-cubes (util/pop-silently gate-path))]
      (merge {id (->> [in1 in2]
                      (map #(d-backward-intersection % activation-cubes))
                      (apply concat)
                      distinct)}
             (test-cubes in1 (conj gate-path gate) fault init-cube)
             (test-cubes in2 (conj gate-path gate) fault init-cube)))))

(extend-type Gate3Nand
  DTestableGate
  (singular-cubes [{:keys [id in1 in2 in3]} output]
    (let [id1 (:id in1)
          id2 (:id in2)
          id3 (:id in3)]
      (case output
        0 [{id1 1 id2 1 id3 1 id 0}]
        1 [{id1 0 id2 :x id3 :x id 1}
           {id1 :x id2 0 id3 :x id 1}
           {id1 :x id2 :x id3 0 id 1}])))
  (all-singular-cubes [gate]
    (concat (singular-cubes gate 0)
            (singular-cubes gate 1)))
  (d-cubes [gate]
    (d-cubes-from-singulars (singular-cubes gate 0)
                            (singular-cubes gate 1)))
  (fault-d-cubes [{:keys [id in1 in2 in3]} fault]
    (let [id1 (:id in1)
          id2 (:id in2)
          id3 (:id in3)]
      (case fault
        0 [{id1 0 id2 :x id3 :x id :d}
           {id1 :x id2 0 id3 :x id :d}
           {id1 :x id2 :x id3 0 id :d}]
        1 [{id1 1 id2 1 id3 1 id :nd}])))
  (d-backward-intersection [{:keys [in1 in2 in3] :as gate} cubes]
    (if-let [cc (d-multi-cubes-intersection-first cubes (all-singular-cubes gate))]
      (distinct
        (concat
          (d-backward-intersection in1 cc)
          (d-backward-intersection in2 cc)
          (d-backward-intersection in3 cc)))
      []))
  (d-activation [{:keys [in1 in2 in3] :as gate} active-gate cubes gate-path]
    (if-let [cc (d-multi-cubes-intersection-first cubes (d-cubes gate))]
      (let [active-cubes (d-activation (peek gate-path) gate cc (util/pop-silently gate-path))]
        (->> (util/n-not-equal 2 active-gate in1 in2 in3)
             (map #(d-backward-intersection % active-cubes))
             (apply concat)
             distinct))
      []))
  (test-cubes [{:keys [id in1 in2 in3] :as gate} gate-path fault init-cube]
    (let [fault-cubes (d-init (fault-d-cubes gate fault) init-cube)
          activation-cubes (d-activation (peek gate-path) gate fault-cubes (util/pop-silently gate-path))]
      (merge {id (->> [in1 in2 in3]
                      (map #(d-backward-intersection % activation-cubes))
                      (apply concat)
                      distinct)}
             (test-cubes in1 (conj gate-path gate) fault init-cube)
             (test-cubes in2 (conj gate-path gate) fault init-cube)
             (test-cubes in3 (conj gate-path gate) fault init-cube)))))

(extend-type nil
  DTestableGate
  (singular-cubes [_ _] [])
  (all-singular-cubes [_] [])
  (d-cubes [_] [])
  (fault-d-cubes [_ _] [])
  (d-backward-intersection [_ _] [])
  (d-activation [_ _ fault-cubes _] fault-cubes)
  (test-cubes [_ _ _] {}))
