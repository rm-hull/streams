(ns streams.newtons-method)

(defn square [x]
  (* x x))

(defn average [x y]
  (/ (+ x y) 2))

(defn improve [guess x]
  (average guess (/ x guess)))

(defn good-enough? [guess x]
  (< (Math/abs (- (square guess) x)) 0.0000001))

(defn fix [r]     ; fix point combinator
  ((fn [f] (f f))
   (fn [f]
     (r (fn [x] ((f f) x))))))

(defn sqrt [x]
  (letfn [(iter [func]
            (fn [guess]
              (if (good-enough? guess x)
                guess
                (func (improve guess x)))))]
    ((fix iter) 1.0)))

(doseq [x (range 1 10)]
  (println (str "√" x " = " (sqrt x))))
