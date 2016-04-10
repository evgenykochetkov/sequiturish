(ns sequiturish.utils)

(defn abs [n] (max n (- n)))

(defn map-vals
  "Applies f to each value of m"
  [f m]
  (into (empty m) (for [[k v] m] [k (f v)])))
