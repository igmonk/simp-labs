(ns simp-labs.lab4.memory-cell
  (:import (clojure.lang Atom)))

(defrecord SimpleMemoryCell [^Atom value])

(defrecord ImmutableMemoryCell [^Atom value])

(defrecord CFinMemoryCell [^Atom value victim-cell])

(defrecord CFin01MemoryCell [^Atom value victim-cell])

(defrecord CFin10MemoryCell [^Atom value victim-cell])

(defrecord CFid01MemoryCell0 [^Atom value victim-cell])

(defrecord CFid01MemoryCell1 [^Atom value victim-cell])

(defrecord CFid10MemoryCell0 [^Atom value victim-cell])

(defrecord CFid10MemoryCell1 [^Atom value victim-cell])
