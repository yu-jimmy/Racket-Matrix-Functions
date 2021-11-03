#lang racket

(require test-engine/racket-tests)

(provide dot-product-rec dot-product-map vector-add-rec vector-add-map add scalar-vector-mult scalar-mult transpose-rec transpose-map)

(define m1 '((1 0 2)
             (2 1 4)
             (-1 1 -1)))
(define m2 '((1 2 3)
             (4 5 6)
             (7 8 9)))
(define m3 '((2 2 5)
             (6 6 10)
             (6 9 8)))
(define m1tr '((1 2 -1)
               (0 1 1)
               (2 4 -1)))


;; (dot-product v1 v2) -> number?
;; v1, v2: list of numbers of the same size
;; returns the dot product of v1 and v2
;; a recursive version
(define (dot-product-rec v1 v2)
  (if (empty? v1)
      0
      (+ (* (first v1) (first v2)) (dot-product-rec (rest v1) (rest v2)))))

(define (dot-product-map v1 v2)
  (apply + (map * v1 v2)))


;; (vector-add-rec v1 v2) -> list?
;; v1, v2: list of numbers of the same size
;; return v1 + v2
;; a recursive version 
(define (vector-add-rec v1 v2)
  (if (empty? v1)
      '()
      (append (cons (+ (first v1) (first v2)) '()) (vector-add-rec (rest v1) (rest v2)))))

(define (vector-add-map v1 v2)
  (map + v1 v2))


;; (add m1 m2) -> list?
;; m1, m2: list of list of numbers of same dimensions
;; return m1 + m2 for matrices m1, m2
(define (add m1 m2)
  (map (lambda (l1 l2) (map + l1 l2)) m1 m2))


;; (scalar-vector-mult k v) -> list?
;; k: number
;; v: list of number
;; return the scalar multiplication kv
(define (scalar-vector-mult k v)
  (map (lambda (x) (* k x)) v))


;; (scalar-mult k m) -> list?
;; k: number
;; m: list of list of number
;; return the scalar multiplication km
(define (scalar-mult k m)
  (map (lambda (l1) (map (lambda (x) (* k x)) l1)) m))


;; (transpose m) -> list?
;; m: list of list of number, non-empty
;; return the transpose of matrix m
;; a recursive version 
(define (transpose-rec m)
  (if (empty? (car m))
      '()
      (if (empty? m)
          '()
          (cons (map car m) (transpose-rec (map cdr m))))))

(define (transpose-map m)
  (apply map list m))


;; (mult m1 m2) -> list?
;; m1, m2: list of list of number
;; return the matrix multiplication m1 x m2
;; no recursion: use map and the above transpose function
(define (mult m1 m2)
  (map (lambda (x1) (map (lambda (x2) (dot-product-map x1 x2)) (transpose-map m2))) m1))