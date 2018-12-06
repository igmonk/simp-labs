(ns simp-labs.common.gate)

(defrecord GateIn [id])

(defrecord GateNot [id in])

(defrecord Gate2Or [id in1 in2])

(defrecord Gate2And [id in1 in2])

(defrecord Gate2Nor [id in1 in2])

(defrecord Gate2Nand [id in1 in2])

(defrecord Gate3Nand [id in1 in2 in3])

(defrecord Gate3Nor [id in1 in2 in3])
