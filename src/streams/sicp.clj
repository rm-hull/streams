(ns streams.sicp)

; SICP 3.5.3
(defn sqrt-improve [guess x]
  (/ (+ guess (/ x guess)) 2))

(defn sqrt-stream [x]
  (def guesses
    (cons 1.0 (lazy-seq (map #(sqrt-improve % x) guesses))))
  guesses)

; (take 100 (sqrt-stream 2))

(defn pi-summands [n]
  (lazy-seq
    (cons
      (/ 1 n)
      (lazy-seq (map - (pi-summands (+ n 2)))))))

(defn pi-stream []
  (->>
    (pi-summands 1)
    (reductions +)
    (map (partial * 4.0))))

; (take 100 (pi-stream))

(defn square [x] (* x x))

(defn euler-transform [s]
  (let [s0 (nth s 0)  ; s[n-1]
        s1 (nth s 1)  ; s[n]
        s2 (nth s 2)] ; s[n+1]
    (lazy-seq
      (cons
        (- s2 (/ (square (- s2 s1))
                 (+ s0 (* -2 s1) s2)))
        (euler-transform (next s))))))

; (take 100 (euler-transform (pi-stream)))

(defn make-tableau [transform s]
  (lazy-cat
    (cons
      s
      (make-tableau
        transform
        (transform s)))))

(defn accelerated-sequence [transform s]
  (map first (make-tableau transform s)))

; (take 9 (accelerated-sequence euler-transform (pi-stream)))
