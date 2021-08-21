#!/usr/bin/env racket
#lang racket
(require "gates.rkt")
(require "simulator.rkt")

(provide wire-or)
(provide wire-and)
(provide wire-not)
(provide wire-xor)
(provide wire-half-adder)
(provide wire-full-adder)
(provide wire-four-bit-ripple-carry-adder)
(provide wire-n-bit-ripple-carry-adder)
(provide ripple-carry-adder)

(define (half-adder-circuit a b s c)
  (and-gate a b c)
  (xor-gate a b s))

(define (full-adder-circuit a b cin s cout)
  (let ([wire-co1 (make-wire)]
	[wire-co2 (make-wire)]
	[wire-so1 (make-wire)])
  (half-adder-circuit a b wire-so1 wire-co2)
  (half-adder-circuit cin wire-so1 s wire-co1)
  (or-gate wire-co1 wire-co2 cout)))

(define (four-bit-ripple-carry-adder a1 a2 a3 a4 b1 b2 b3 b4 c1 c2 c3 c4 cin cout)
  (let ([wire-cout2 (make-wire)]
	[wire-cout3 (make-wire)]
	[wire-cout4 (make-wire)])
    (full-adder-circuit a4 b4 cin c4 wire-cout2)
    (full-adder-circuit a3 b3 wire-cout2 c3 wire-cout3)
    (full-adder-circuit a2 b2 wire-cout3 c2 wire-cout4)
    (full-adder-circuit a1 b1 wire-cout4 c1 cout)))

(define (n-bit-ripple-carry-adder a-list b-list cin)
  (define (n-bit-ripple-carry-adder-iter a-list b-list s-list cin cout)
    (let ([wire-sum (make-wire)])
      (if (null? a-list)
	(cons cin s-list)
	(begin
	  (full-adder-circuit (car a-list) (car b-list) cin wire-sum cout)
	  (n-bit-ripple-carry-adder-iter (cdr a-list) (cdr b-list) (cons wire-sum s-list) cout (make-wire))))))
  (n-bit-ripple-carry-adder-iter a-list b-list '() cin (make-wire)))

(define (wire-or a b)
  (let ([wire-a (make-wire)]
	[wire-b (make-wire)]
	[wire-c (make-wire)])
    (set-signal! wire-a a)
    (set-signal! wire-b b)
    (or-gate wire-a wire-b wire-c)
    (propagate)
    (get-signal wire-c)))

(define (wire-and a b)
  (let ([wire-a (make-wire)]
	[wire-b (make-wire)]
	[wire-c (make-wire)])
    (set-signal! wire-a a)
    (set-signal! wire-b b)
    (and-gate wire-a wire-b wire-c)
    (propagate)
    (get-signal wire-c)))

(define (wire-not a)
  (let ([wire-a (make-wire)]
	[wire-b (make-wire)])
    (set-signal! wire-a a)
    (inverter wire-a wire-b)
    (propagate)
    (get-signal wire-b)))

(define (wire-xor a b)
  (let ([wire-a (make-wire)]
	[wire-b (make-wire)]
	[wire-c (make-wire)])
    (set-signal! wire-a a)
    (set-signal! wire-b b)
    (xor-gate wire-a wire-b wire-c)
    (propagate)
    (get-signal wire-c)))

(define (wire-half-adder a b)
  (let ([wire-a (make-wire)]
	[wire-b (make-wire)]
	[wire-s (make-wire)]
	[wire-c (make-wire)])
    (set-signal! wire-a a)
    (set-signal! wire-b b)
    (half-adder-circuit wire-a wire-b wire-s wire-c)
    (propagate)
    (cons (get-signal wire-s) (get-signal wire-c))))

(define (wire-full-adder a b c)
  (let ([wire-a (make-wire)]
	[wire-b (make-wire)]
	[wire-cin (make-wire)]
	[wire-s (make-wire)]
	[wire-cout (make-wire)])
    (set-signal! wire-a a)
    (set-signal! wire-b b)
    (set-signal! wire-cin c)
    (full-adder-circuit wire-a wire-b wire-cin wire-s wire-cout)
    (propagate)
    (cons (get-signal wire-s) (get-signal wire-cout))))

(define (wire-four-bit-ripple-carry-adder a1 a2 a3 a4 b1 b2 b3 b4 cin)
  (let ([wire-a1 (make-wire)]
	[wire-a2 (make-wire)]
	[wire-a3 (make-wire)]
	[wire-a4 (make-wire)]
	[wire-b1 (make-wire)]
	[wire-b2 (make-wire)]
	[wire-b3 (make-wire)]
	[wire-b4 (make-wire)]
	[wire-c1 (make-wire)]
	[wire-c2 (make-wire)]
	[wire-c3 (make-wire)]
	[wire-c4 (make-wire)]
	[wire-cin (make-wire)]
	[wire-cout (make-wire)])
    (set-signal! wire-a1 a1)
    (set-signal! wire-a2 a2)
    (set-signal! wire-a3 a3)
    (set-signal! wire-a4 a4)
    (set-signal! wire-b1 b1)
    (set-signal! wire-b2 b2)
    (set-signal! wire-b3 b3)
    (set-signal! wire-b4 b4)
    (set-signal! wire-cin cin)
    (four-bit-ripple-carry-adder 
      wire-a1 wire-a2 wire-a3 wire-a4 
      wire-b1 wire-b2 wire-b3 wire-b4
      wire-c1 wire-c2 wire-c3 wire-c4
      wire-cin wire-cout)
    (propagate)
    (map get-signal (list wire-cout wire-c1 wire-c2 wire-c3 wire-c4))))

(define (list-wire-n-bit-ripple-carry-adder a-list b-list cin)
  (let ([s-list (n-bit-ripple-carry-adder a-list b-list cin)])
    (propagate)
    (map get-signal s-list)))

(define (wire-n-bit-ripple-carry-adder a1 a2 a3 a4 b1 b2 b3 b4 cin)
  (let ([wire-a1 (make-wire)]
	[wire-a2 (make-wire)]
	[wire-a3 (make-wire)]
	[wire-a4 (make-wire)]
	[wire-b1 (make-wire)]
	[wire-b2 (make-wire)]
	[wire-b3 (make-wire)]
	[wire-b4 (make-wire)]
	[wire-cin (make-wire)])
    (set-signal! wire-a1 a1)
    (set-signal! wire-a2 a2)
    (set-signal! wire-a3 a3)
    (set-signal! wire-a4 a4)
    (set-signal! wire-b1 b1)
    (set-signal! wire-b2 b2)
    (set-signal! wire-b3 b3)
    (set-signal! wire-b4 b4)
    (set-signal! wire-cin cin)
    (list-wire-n-bit-ripple-carry-adder 
      (list wire-a4 wire-a3 wire-a2 wire-a1)
      (list wire-b4 wire-b3 wire-b2 wire-b1)
      wire-cin)))

(define (ripple-carry-adder a b)
  (begin
    (define (get-binary-list integer)
      (map (lambda x (if (equal? x '(49)) 1 0))
	   (map char->integer (string->list (format "~b" integer)))))
    (define (pad-list list-in len)
      (if (< (length list-in) len)
	(pad-list (cons 0 list-in) len)
	list-in))
    (define (create-wire v)
      (let ([wire (make-wire)])
	(set-signal! wire v)
	wire))
    (define list-a (get-binary-list a))
    (define list-b (get-binary-list b))
    (define max-len (+ (max (length list-a) (length list-b)) 1))
    (define padded-list-a (pad-list list-a max-len))
    (define padded-list-b (pad-list list-b max-len))
    (define s-list
      (list-wire-n-bit-ripple-carry-adder
	(map create-wire (reverse padded-list-a))
	(map create-wire (reverse padded-list-b))
	(create-wire 0)))
    (string->number (string-join (map number->string s-list) "") 2)))
