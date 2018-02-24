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
  (let [straight-up-to? (fn [ranks, highest] (= ranks (range (- highest 4) (+ highest 1))))
        ranks (map rank hand)
        low-ace-ranks (sort (replace {14 1} ranks))
        high-ace-ranks (sort ranks)]
    (or (straight-up-to? low-ace-ranks 5)
        (straight-up-to? high-ace-ranks 6)
        (straight-up-to? high-ace-ranks 7)
        (straight-up-to? high-ace-ranks 8)
        (straight-up-to? high-ace-ranks 9)
        (straight-up-to? high-ace-ranks 10)
        (straight-up-to? high-ace-ranks 11)
        (straight-up-to? high-ace-ranks 12)
        (straight-up-to? high-ace-ranks 13)
        (straight-up-to? high-ace-ranks 14))))

(defn straight-flush? [hand]
  (and (flush? hand)
       (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}
        hand-value (fn [[gets-value? value]] (if (gets-value? hand) value 0))
        value-candidates (map hand-value checkers)]
    (apply max value-candidates)))
