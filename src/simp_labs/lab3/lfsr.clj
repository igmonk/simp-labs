(ns simp-labs.lab3.lfsr)

; Is going to be cached with memoize
(defn xor [inputs]
  "Returns 1 only when an odd number of inputs are true.
  Returns 0 otherwise."
  (let [n (count (filter #(= 1 %) inputs))]
    (if (odd? n) 1 0)))

(defn polynomial-x8+x7+x6+x3+x2+x+1 [x x2 x3 _ _ x6 x7 x8]
  "Returns result of XOR operation for x8+x7+x6+x3+x2+x+1 polynomial"
  (xor [x8 x7 x6 x3 x2 x]))

(defn polynomial-x7+x5+x2+x+1 [x x2 _ _ x5 _ x7]
  "Returns result of XOR operation for x7+x5+x2+x+1 polynomial"
  (xor [x7 x5 x2 x]))

(defn lfsr-seq-8 [x x2 x3 x4 x5 x6 x7 x8]
  (let [next-value (polynomial-x8+x7+x6+x3+x2+x+1 x x2 x3 x4 x5 x6 x7 x8)]
    (lazy-seq (cons [x x2 x3 x4 x5 x6 x7 x8]
                    (lfsr-seq-8 next-value x x2 x3 x4 x5 x6 x7)))))

(defn lfsr-seq-7 [x x2 x3 x4 x5 x6 x7]
  (let [next-value (polynomial-x7+x5+x2+x+1 x x2 x3 x4 x5 x6 x7)]
    (lazy-seq (cons [x x2 x3 x4 x5 x6 x7]
                    (lfsr-seq-7 next-value x x2 x3 x4 x5 x6)))))
