(ns p-p-p-pokerface)

(defn rank [[r _]]
  (let [char-values {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit r)
      (Integer/valueOf (str r))
      (char-values r))))

(defn suit [[_ s]]
  (str s))

(defn n-of-a-kind? [n hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)
        counts (vals freqs)
        pairs (filter (fn [c] (= n c)) counts)]
    (< 0 (count pairs))))

(defn pair? [hand]
  (n-of-a-kind? 2 hand))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? 3 hand))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? 4 hand))

(defn flush? [hand]
  (let [suits (map suit hand)
        freqs (frequencies suits)]
    (= 1 (count freqs))))

(defn kind-counts? [expected-counts hand]
  (let [ranks (map rank hand)
        kind-counts (vals (frequencies ranks))]
    (= expected-counts (sort kind-counts))))

(defn full-house? [hand]
  (kind-counts? [2 3] hand))

(defn two-pairs? [hand]
  (or (kind-counts? [1 2 2] hand)
      (kind-counts? [1 4] hand)))

(defn straight? [hand]
  nil)

(defn straight-flush? [hand]
  nil)

(defn value [hand]
  nil)
