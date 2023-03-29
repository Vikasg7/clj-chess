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
    (when mp (= v (k mp)))))

(defn filter-keys [f mp]
  (for [[k v] mp
        :when (f v)]
    k))

(defn filter-first [f coll]
  (first (filter f coll)))

(def char-seq seq)

(defn debug [val]
  (println val)
  val)

(defn to-int
  ([v]
    (to-int (str v) v))
  ([v d]
    (try (Integer/parseInt v)
    (catch Exception e d))))

(defn has-one? [coll]
  (= 1 (count coll)))

(defn poses-between
  ([[r f :as x] [rr ff :as y]]
    (let [offset (cond (= r rr) (if (< f ff) [0 1] [0 -1])
                       (= f ff) (if (< r rr) [1 0] [-1 0]))]
    (poses-between x y offset)))
  ([from-pos to-pos offset]
    (->> (iterate (partial add-vec offset) from-pos)
         (take-while (partial not= to-pos))
         (remove #{from-pos}))))
