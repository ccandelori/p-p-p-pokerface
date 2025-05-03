(ns p-p-p-pokerface)

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)

(defn rank [card]
  (let [[first _] card]
    (if (Character/isDigit first)
      (Integer/valueOf (str first))
      (get {\T 10, \J 11, \Q 12, \K 13 \A 14} first))))

(defn suit [card]
  (let [[_ second] card] (str second)))

(defn pair? [hand]
  (< (count (set (map rank hand)))
     (count hand)))

(defn three-of-a-kind? [hand]
  (>= (apply max (vals (frequencies (map rank hand))))
      3))

(defn four-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand))))
     4))

(defn flush? [hand]
  (= 1 (count (set (map suit hand)))))

(defn full-house? [hand]
  (= (sort (vals (frequencies (map rank hand))))
     [2 3]))

(defn two-pairs? [hand]
  (= (sort (vals (frequencies (map rank hand))))
     [1 2 2]))

(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        low (first ranks)
        target (range low (+ low 5))
        ace-low? (= ranks [2 3 4 5 14])
        ranks-ace (sort (map #(if (= % 14) 1 %) ranks))] 
    (or 
     ace-low?
     (= ranks target)
     (= ranks-ace target))))

  (defn straight-flush? [hand]
    (and (straight? hand) (flush? hand)))

  (defn value [hand]
    (cond
      (straight-flush? hand) 8
      (four-of-a-kind? hand) 7
      (full-house? hand) 6
      (flush? hand) 5
      (straight? hand) 4
      (three-of-a-kind? hand) 3
      (two-pairs? hand) 2
      (pair? hand) 1
      :else 0))
