(ns simp-labs.lab4.memory-cell
  (:import (clojure.lang Atom)))

(defrecord SimpleMemoryCell [^Atom value])

(defrecord ImmutableMemoryCell [^Atom value])

(defrecord CFinMemoryCell [^Atom value victim-cell])

(defrecord CFid0MemoryCell [^Atom value victim-cell])

(defrecord CFid1MemoryCell [^Atom value victim-cell])
