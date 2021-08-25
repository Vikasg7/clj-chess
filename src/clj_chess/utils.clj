(ns clj-chess.utils)

(defn upper-case? [x]
  (Character/isUpperCase x))

(defn lower-case? [x]
  (Character/isLowerCase x))

(defn upper-case [x]
  (Character/toUpperCase x))

(defn lower-case [x]
  (Character/toLowerCase x))

(defn is-letter? [x]
  (Character/isLetter x))

(defn add-vec [a b]
  (mapv + a b))

(defn in-range? [u l v]
    (and (>= v u) (<= v l)))

(defn where? [k v]
  (fn [mp]
    (when mp (= v (mp k)))))

(defn filter-keys [f mp]
  (for [[k v] mp
        :when (f v)]
    k))

(defn filter-first [f coll]
  (first (filter f coll)))

(def char-seq seq)

(defn debug [& vals]
  (apply println vals)
  vals)

(defn to-int
  ([v]
    (to-int (str v) v))
  ([v d]
    (try (Integer/parseInt v)
    (catch Exception e d))))
