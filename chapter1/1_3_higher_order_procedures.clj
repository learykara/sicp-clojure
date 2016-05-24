;; Section 1.3: Formualting Abstractions with Higher-Order Procedures

;; 1.3.1 Procedures as algorithms
(defn cube
  [x]
  (* x x x))

(defn sum-integers
  [a b]
  (if (> a b)
    0
    (+ a (sum-integers (+ a 1) b))))

(defn sum-cubes
  [a b]
  (if (> a b)
    0
    (+ (cube a) (sum-cubes (+ a 1) b))))

(defn pi-sum
  [a b]
  (if (> a b)
    0
    (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

;; Using shared procedure for sigma notation
(defn sum
  [term a next b]
  (if (> a b)
    0
    (+ (term a) (sum term (next a) next b))))

(defn inc
  [n]
  (+ n 1))

(defn sum-cubes
  [a b]
  (sum cube a inc b))

(defn identity
  [x]
  x)

(defn sum-integers
  [a b]
  (sum identity a inc b))

(defn pi-sum
  [a b]
  (defn pi-term
    [x]
    (/ 1.0 (* x (+ x 2))))
  (defn pi-next
    [x]
    (+ x 4))
  (sum pi-term a pi-next b))

(defn integral
  [f a b dx]
  (defn add-dx
    [x]
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))


;; 1.3.2 Constructing Procedures Using Lambda
(defn pi-sum
  [a b]
  (sum (fn [x] (/ 1.0 (* x (+ x 2))))
       a
       (fn [x] (+ x 4))
       b))

;; Using an auxiliary procedure to bind local vars:
(defn f
  [x y]
  (defn f-helper
    [a b]
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
            (- 1 y)))

;; Now with a lambda
(defn f-lambda
  [x y]
  ((fn [a b] (+ (* x (square a)) (* y b) (* a b)))
  (+ 1 (* x y))
  (- 1 y)))

;; Now with `let`
(defn f-let
  [x y]
  (let [
        a (+ 1 (* x y))
        b (- 1 y)]
    (+ (* x (square a))
       (* y b)
       (* a b))))

