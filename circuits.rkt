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

(define (half-adder-gate a b s c)
  (and-gate a b c)
  (xor-gate a b s))

(define (full-adder-gate a b cin s cout)
  (let ([wire-co1 (make-wire)]
	[wire-co2 (make-wire)]
	[wire-so1 (make-wire)])
  (half-adder-gate a b wire-so1 wire-co2)
  (half-adder-gate cin wire-so1 s wire-co1)
  (or-gate wire-co1 wire-co2 cout)))

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
    (half-adder-gate wire-a wire-b wire-s wire-c)
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
    (full-adder-gate wire-a wire-b wire-cin wire-s wire-cout)
    (propagate)
    (cons (get-signal wire-s) (get-signal wire-cout))))
