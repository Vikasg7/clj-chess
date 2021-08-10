(ns clj-chess.utils)

(defn upper-case? [x]
  (Character/isUpperCase x))

(defn lower-case? [x]
  (Character/isLowerCase x))

(defn add-vec [a b]
  (mapv + a b))

(defn in-range? [u l v]
    (and (>= v u) (<= v l)))

(defn filter-by-val [mp val]
  (for [[k v] mp
        :when (= v val)]
    k))
