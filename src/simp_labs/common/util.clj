(ns simp-labs.common.util)

(defn not-empty? [coll]
  (not (empty? coll)))

(defn cartesian-product [& colls]
  (if (empty? colls)
    '(())
    (for [x (first colls)
          more (apply cartesian-product (rest colls))]
      (cons x more))))

(defn merge-maps [maps]
  (reduce merge maps))

(defn pop-silently [coll]
  (if (not (empty? coll))
    (pop coll)
    []))

(defn not-equal [x items]
  (filter #(not= x %) items))

(defn first-not-equal [x & items]
  (first (not-equal x items)))

(defn n-not-equal [n x & items]
  (take n (not-equal x items)))

(defn sort-maps [maps]
  (map #(into (sorted-map) %) maps))
