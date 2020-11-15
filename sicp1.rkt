#lang sicp

(define (square x) (* x x))

(define (sum-of-squares x y) (+ (square x) (square y)))

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (max x y)
  (if (> x y) x y))

; This is a comment
; 1.2
;; (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

;1.3
(define (max-sum-of-squares x y z)
  (if (and (>= x y) (>= y z))
      (sum-of-squares x y)
      (if (and (>= x y) (>= z y))
          (sum-of-squares x z)
          (sum-of-squares y z))))

;1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))



(define (average x y)
  (/ (+ x y) 2))


(define (sqrt x)
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x)
                   x)))
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (sqrt-iter 1.0 x))

;1.6 If used in sqrt-iter instead of special form if will result in infinate method calls being made to evaluate (sqrt-iter (improve guessx) x), the third parameter passed to "if", due to applicative order execution
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;1.7
;; (sqrt 0.001)
;; (square (sqrt 0.001))
;(sqrt 900000000000000000)
;(square (sqrt 900000000000000000))

(define (sqrtt x)
  (define (sqrt-iter-diff guess lastguess x)
    (define (close-enough? guess lastguess x)
      (< (abs (- guess lastguess)) (/ x 10000000)))
    (define (improve guess x)
      (average guess (/ x guess)))
    (if (close-enough? guess lastguess x)
        guess
        (sqrt-iter-diff (improve guess x)
                        guess
                        x)))
  (sqrt-iter-diff 1.0 0 x))



;1.8
(define (cbrt x)
  (define (cube-iter guess lastguess x)
    (define (close-enough? guess lastguess x)
      (< (abs (- guess lastguess)) (/ x 10000000)))
    (define (cube-improve guess x)
      (/ (+ (/ x (square guess)) (* 2 guess)) 3))
    (if (close-enough? guess lastguess x)
        guess
        (cube-iter (cube-improve guess x)
                   guess
                   x)))
  (cube-iter 1.0 0 x))


;1.9
;(define (+ a b)
;  (if (= a 0)
;      b
;      (inc (+ (dec a) b))))
; recursive process
;; (+ 4 5)
;; (inc (+ (dec 4) 5))
;; (inc (+ 3 5))
;; (inc (inc (+ (dec 3) 5)))
;; (inc (inc (+ 2 5)))
;; (inc (inc (inc (+ dec(2) 5))))
;; (inc (inc (inc (+ 1 5))))
;; (inc (inc (inc (inc (+ dec(1) 5)))))
;; (inc (inc (inc (inc ( + 0 5)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; (9)


;(define (+ a b)
;  (if (= a 0)
;      b
;      (+ (dec a) (inc b))))
; iterative process
;; (+ 4 5)
;; (+ (dec 4) (inc 5))
;; (+ 3 6)
;; (+ (dec 3) (inc 6))
;; (+ 2 7)
;; (+ (dec 2) (inc 7))
;; (+ 1 8)
;; (+ (dec 1) (inc 8))
;; (+ 0 9)
;; (9)


;1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;; (A 2 4)
;; (A (- 2 1) (A 2 (- 4 1)))
;; (A 1 (A 2 3))
;; (A 1 (A 1 (A 2 2)))
;; (A 1 (A 1 (A 1 (A 2 1))))
;; (A 1 (A 1 (A 1 2)))
;; (A 1 (A 1 (A 0 (A 1 1))))
;; (A 1 (A 1 (A 0 2)))
;; (A 1 (A 1 (* 2 2)))
;; (A 1 (A 1 4))
;; (A 1 (A 0 (A 1 3)))
;; (A 1 (A 0 (A 0 (A 1 2))))
;; (A 1 (A 0 (A 0 (A 0 (A 1 1)))))
;; (A 1 (A 0 (A 0 (A 0 2))))
;; (A 1 (A 0 (A 0 (* 2 2))))
;; (A 1 (A 0 (A 0 4)))
;; (A 1 (A 0 (* 2 4)))
;; (A 1 (A 0 8))
;; (A 1 (* 2 8))
;; (A 1 16)



;; (f n) = 2n
;; (g n) = n^2
;; (h n) = ((n^2)^2)^2...   n^2 for n-1 times

;; (A 1 10)
;; (A (- 1 1) (A 1 (- 10 1)))
;; (A 0 (A 1 9))
;; (A 0 (A 0 (A 1 8)))
;; (A 0 (A 0 (A 0 (A 1 7))))
;; (A 0 (A 0 (A 0 (A 0 (A 1 6)))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (* 2 2)))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (* 2 4))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (* 2 8)))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (* 2 16))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 32)))))
;; (A 0 (A 0 (A 0 (A 0 (* 2 32)))))
;; (A 0 (A 0 (A 0 (A 0 64))))
;; (A 0 (A 0 (A 0 (* 2 64))))
;; (A 0 (A 0 (A 0 128)))
;; (A 0 (A 0 (* 2 128)))
;; (A 0 (A 0 256))
;; (A 0 (* 2 256))
;; (A 0 512)
;; (* 2 512)
;; ()

;; (A 2 3)
;; (A (- 2 1) (A 2 (- 3 1)))
;; (A 1 (A 2 2))
;; (A 1 (A 1 (A 2 (- 2 1))))
;; (A 1 (A 1 (A 2 1)))
;; (A 1 (A 1 2))
;; (A 1 (A (- 1 1) (A 1 (- 2 1))))
;; (A 1 (A 0 (A 1 1)))
;; (A 1 (A 0 2))
;; (A 1 (* 2 2))
;; (A 1 4)


;; (A (- 1 1) (A 1 (- n 1)))
;; (A 0 (A 0 (A 1 (- (- n 1) 1))))

;; (A 1 3)
;; (A (- 1 1) (A 1 (- 3 1)))
;; (A 0 (A 1 2))
;; (A 0 (A (- 1 1) (A 1 (- 2 1))))
;; (A 0 (A 0 (A 1 1)))
;; (A 0 (A 0 (2))

;; 1.2.2
(define (inefficient-fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (inefficient-fib (- n 1))
                 (inefficient-fib (- n 2))))))

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;; Excercise 1.11
;; f(n) = n if n < 3
;; f(n) = f(n-1) + 2f(n-2) + 3f(n-3) if n >= 3

(define (recfuncal n)
  (if (< n 3)
      n
      (+ (recfuncal (- n 1))
         (* 2 (recfuncal (- n 2)))
         (* 3 (recfuncal (- n 3))))))

(define (iterfuncal n)
  (funcal-iter 0 1 2 n))


(define (funcal-iter  a b c count)
  (if (= 0 count)
      a
      (funcal-iter b
                   c
                   (+ c (* 2 b) (* 3 a))
                   (- count 1))))

;; Excercie 1.12
;; 1
;; 11
;; 121
;; 1331
;; 14641


(define (calc-pascal r c)
  (cond ((= c 1) 1)
        ((= r c) 1)
        (else (+ (calc-pascal (- r 1) c) (calc-pascal (- r 1) (- c 1))))))



;; Excercie 1.13
;; phi = (1 + root(5))/2
;; (define (phi (/ (+ 1 (sqrt 5)) 2)))
;; (define sqrt5 (sqrt 5))
;; (define phi (/ (+ 1 sqrt5) 2))
;; (define psi (/ (- 1 sqrt5) 2))
;; See paper notes for proof

;; 1.14 paper notes

;; 1.15
(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;; a
(define (howManyPs angle count)
  (if (not (> (abs angle) 0.1))
      count
      (howManyPs (/ angle 3.0) (+ 1 count))))

;; (howManyPs 12.15 0)

;; b
;; (howManyPs 24.3 0)
;; (howManyPs 36.45 0)
;; (howManyPs 50 0)

;; Theta = log n (base 3) because angle is reduced at n/3

;;1.2.4 Exponentiation

;; (define (expt b n)
;;   (if (= n 0)
;;       1
;;       (* b (expt b (- n 1)))))

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))


;; 1.16 bove definition is recursive, design an iterative exponentiation process
;; Hint:(b^(n/2))^2 = (b^2)^(n/2) keep state var a such that ab^n is constant between states
;; at the beginign of the process a = 1

(define (fast-iter-expt b n)
  (fast-iter-expt-iter b n 1))

(define (fast-iter-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-iter-expt-iter (square b) (/ n 2) a))
        (else (fast-iter-expt-iter b (- n 1) (* a b)))))


;; 1.17
(define (mult a b)
  (define (double x)
    (+ x x))
  (define (half x)
    (/ x 2))
  (cond ((= b 0) 0)
        ((even? b) (mult (double a) (half b)))
        (else (+ a (mult a (- b 1))))))


(define (mult-it a b)
  (mult-iter a b 0))

(define (mult-iter a b n)
  (define (double x)
    (+ x x))
  (define (half x)
    (/ x 2))
  (cond ((= b 0) n)
        ((even? b) (mult-iter (double a) (half b) n))
        (else (mult-iter a (- b 1) (+ n a)))))
