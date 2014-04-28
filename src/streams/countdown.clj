(ns streams.countdown)

(defn subseqs [[x & xs]]
  (if (nil? xs)
    (list (list x))
    (lazy-seq
      (let [xss (subseqs xs)]
        (concat
          xss
          (cons
            (list x)
            (map
              (partial cons x)
              xss)))))))

(defn value
  ([[op left right]] (apply op left right)))

(defn legal? [op left right]
  (condp = op
    '+ true
    '- (> left right)
    '* true
    '/ (zero? (mod left right))))

(defn add [x [ys zs]]
  [[(cons x ys) zs]
   [ys (cons x zs)]])

(defn unmerges [[x y & rest]]
  (if (nil? rest)
    [[[x] [y]] [[y] [x]]]
    (lazy-seq
      (let [xs (cons y rest)]
        (concat
          [[[x] xs] [xs [x]]]
          (mapcat (partial add x) (unmerges xs)))))))

(defn combine [[e1 v1] [e2 v2]]
  (for [op ['+ '- '* '/]
        :when (legal? op v1 v2)]
    [(list op e1 e2) (op v1 v2)]))

(defn make-exprs [xs]
  (println xs)
  (if (nil? (rest xs))
    [(first xs) (first xs)]
    (lazy-seq
      (for [[ys, zs] (unmerges xs)
            ev1      (make-exprs ys)
            ev2      (make-exprs zs)]
        (combine ev1 ev2)))))

(combine [26 26] [13 13])

(doseq [[ys zs] (take 10 (unmerges (subseqs [1, 3, 7, 10, 25, 50])))]
  (println ys)
  (println zs)
  (println)
  )

(value (list - 1 7))


(take 3 (make-exprs (subseqs [1, 3, 7, 10, 25, 50])))
