#!/usr/bin/env racket
#lang racket
(require rnrs/base-6)
(require rnrs/mutable-pairs-6)
(require "queue.rkt")
(require "simulator.rkt")

(provide inverter)
(provide and-gate)
(provide or-gate)
(provide xor-gate)

; From SICP 3.3.4 A Simulator for Digital Circuits
(define inverter-delay 1)
(define and-gate-delay 1)
(define or-gate-delay 1)
(define xor-gate-delay 1)

(define (logical-not s)
  (cond ((= s 0) 1)
	((= s 1) 0)
	(else (error "Invalid signal" s))))

(define (logical-and a b)
  (cond ((and (= a 0) (= b 0)) 0)
	((and (= a 1) (= b 0)) 0)
	((and (= a 0) (= b 1)) 0)
	((and (= a 1) (= b 1)) 1)
	(else (error "Invalid signal" a b))))

(define (logical-or a b)
  (cond ((and (= a 0) (= b 0)) 0)
	((and (= a 1) (= b 0)) 1)
	((and (= a 0) (= b 1)) 1)
	((and (= a 1) (= b 1)) 1)
	(else (error "Invalid signal" a b))))

(define (logical-xor a b)
  (cond ((and (= a 0) (= b 0)) 0)
	((and (= a 1) (= b 0)) 1)
	((and (= a 0) (= b 1)) 1)
	((and (= a 1) (= b 1)) 0)
	(else (error "Invalid signal" a b))))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
		   (λ ()
		      (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
	    (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
		   (λ ()
		      (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
	    (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
		   (λ ()
		      (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (xor-gate a1 a2 output)
  (define (xor-action-procedure)
    (let ((new-value
	    (logical-xor (get-signal a1) (get-signal a2))))
      (after-delay xor-gate-delay
		   (λ ()
		      (set-signal! output new-value)))))
  (add-action! a1 xor-action-procedure)
  (add-action! a2 xor-action-procedure)
  'ok)
