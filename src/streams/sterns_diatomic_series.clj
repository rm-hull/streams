(ns streams.sterns-diatomic-series)

(defn stern-brocot-seq
  "A002487: Stern's diatomic series (or Stern-Brocot sequence):

     a(0) = 0,
     a(1) = 1;
     for n > 0: a(2*n) = a(n), a(2*n+1) = a(n) + a(n+1).

   Also called fusc(n) [Dijkstra]."
  []
  (letfn [(seq0 [a b]
            (let [b1 (first b)
                  c  [(+ a b1) b1]]
              (lazy-cat c (seq0 b1 (rest (lazy-cat b c))))))]
    (lazy-cat [0 1 1] (seq0 1 [1]))))

(defn calkin-wilf-seq
  "Every positive rational number appears exactly once in the sequence."
  []
  (map
    (fn [[a b]] (/ a b))
    (partition 2 1 (stern-brocot-seq))))


(take 50 (stern-brocot-seq))
; => (0 1 1 2 1 3 2 3 1 4 3 5 2 5 3 4 1 5 4 7 3 8 5 7 2 7 5 8 3 7 4
;     5 1 6 5 9 4 11 7 10 3 11 8 13 5 12 7 9 2 9)


(println (take 50 (calkin-wilf-seq)))
; => (0 1 1/2 2 1/3 3/2 2/3 3 1/4 4/3 3/5 5/2 2/5 5/3 3/4 4 1/5 5/4
;     4/7 7/3 3/8 8/5 5/7 7/2 2/7 7/5 5/8 8/3 3/7 7/4 4/5 5 1/6 6/5
;     5/9 9/4 4/11 1 1/7 7/10 10/3 3/11 11/8 8/13 13/5 5/12 12/7 7/9
;     9/2 2/9 9/7)