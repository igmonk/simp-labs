(ns simp-labs.lab4.march-b
  (:require [simp-labs.lab4.ramable :refer :all]
            [simp-labs.lab4.ram-util :as ru]))

; March-B: ?(w0); ↑(r0,w1,r1,w0,r0,w1); ↑(r1,w0,w1); ↓(r1,w0,w1,w0); ↓(r0,w1,w0);

(defn check-cell-value [ram row col value]
  (when-not (= value (get-cell-value ram row col))
    (ru/throw-row-col row col)))

(defn check-cell-0 [ram row col]
  (check-cell-value ram row col 0))

(defn check-cell-1 [ram row col]
  (check-cell-value ram row col 1))

(defn run-m0 [ram]
  "M0: ?(w0)"
  (ru/write-all ram 0))

(defn run-m1 [ram]
  "M1: ↑(r0,w1,r1,w0,r0,w1)"
  (ru/do-all-2d
    ram
    (fn [row col _ _]
      (check-cell-0 ram row col)
      (set-cell-value ram row col 1)
      (check-cell-1 ram row col)
      (set-cell-value ram row col 0)
      (check-cell-0 ram row col)
      (set-cell-value ram row col 1))))

(defn run-m2 [ram]
  "M2: ↑(r1,w0,w1)"
  (ru/do-all-2d
    ram
    (fn [row col _ _]
      (check-cell-1 ram row col)
      (set-cell-value ram row col 0)
      (set-cell-value ram row col 1))))

(defn run-m3 [ram]
  "M3: ↓(r1,w0,w1,w0)"
  (ru/do-all-2d-backward
    ram
    (fn [row col _ _]
      (check-cell-1 ram row col)
      (set-cell-value ram row col 0)
      (set-cell-value ram row col 1)
      (set-cell-value ram row col 0))))

(defn run-m4 [ram]
  "M4: ↓(r0,w1,w0)"
  (ru/do-all-2d-backward
    ram
    (fn [row col _ _]
      (check-cell-0 ram row col)
      (set-cell-value ram row col 1)
      (set-cell-value ram row col 0))))

(defn run [ram]

  ; M0: ?(w0)
  (run-m0 ram)

  ; M1: ↑(r0,w1,r1,w0,r0,w1)
  (run-m1 ram)

  ; M2: ↑(r1,w0,w1)
  (run-m2 ram)

  ; M3: ↓(r1,w0,w1,w0)
  (run-m3 ram)

  ; M4: ↓(r0,w1,w0)
  (run-m4 ram)

  (prn (str "SUCCESS: March-B didn't find any RAM fault."))
  ram)
