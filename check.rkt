#!/usr/bin/env racket
#lang racket

(require "circuits.rkt")
(provide check-simulation)

; This file contains assertions to verify if the circuit simulation is working as expected

(define (check-ripple-carry ripple-carry-circuit)
  (begin
      (unless (equal? (list 0 0 0 0 0)
		      (ripple-carry-circuit 
			0 0 0 0 ; A1 - A4
			0 0 0 0 ; B1 - B4
			0       ; Cin
			)) (error "Four bit ripple carry adder expected 0 output for this input"))
      (unless (equal? (list 0 0 0 0 1)
		      (ripple-carry-circuit 
			0 0 0 0 ; A1 - A4
			0 0 0 0 ; B1 - B4
			1       ; Cin
			)) (error "Four bit ripple carry adder expected 1 output for this input"))
      (unless (equal? (list 0 1 0 0 0)
		      (ripple-carry-circuit 
			0 1 0 0 ; A1 - A4
			0 1 0 0 ; B1 - B4
			0       ; Cin
			)) (error "Four bit ripple carry adder expected 8 output for this input"))
      (unless (equal? (list 0 1 0 0 1)
		      (ripple-carry-circuit 
			0 1 0 0 ; A1 - A4
			0 1 0 0 ; B1 - B4
			1       ; Cin
			)) (error "Four bit ripple carry adder expected 9 output for this input"))
      (unless (equal? (list 1 0 0 0 0)
		      (ripple-carry-circuit 
			1 0 0 0 ; A1 - A4
			1 0 0 0 ; B1 - B4
			0       ; Cin
			)) (error "Four bit ripple carry adder expected 0 output for this input with carry bit"))
      (unless (equal? (list 1 0 0 0 1)
		      (ripple-carry-circuit 
			1 0 0 0 ; A1 - A4
			1 0 0 0 ; B1 - B4
			1       ; Cin
			)) (error "Four bit ripple carry adder expected 1 output for this input with carry bit"))
      (unless (equal? (list 1 0 1 0 0)
		      (ripple-carry-circuit 
			1 0 1 0 ; A1 - A4
			1 0 1 0 ; B1 - B4
			0       ; Cin
			)) (error "Four bit ripple carry adder expected 4 output for this input with carry bit"))
      (unless (equal? (list 1 0 1 0 1)
		      (ripple-carry-circuit 
			1 0 1 0 ; A1 - A4
			1 0 1 0 ; B1 - B4
			1       ; Cin
			)) (error "Four bit ripple carry adder expected 5 output for this input with carry bit"))
      (unless (equal? (list 1 1 0 0 0)
		      (ripple-carry-circuit 
			1 1 0 0 ; A1 - A4
			1 1 0 0 ; B1 - B4
			0       ; Cin
			)) (error "Four bit ripple carry adder expected 8 output for this input with carry bit"))
      (unless (equal? (list 1 1 0 0 1)
		      (ripple-carry-circuit 
			1 1 0 0 ; A1 - A4
			1 1 0 0 ; B1 - B4
			1       ; Cin
			)) (error "Four bit ripple carry adder expected 9 output for this input with carry bit"))
      (unless (equal? (list 1 1 1 0 0)
		      (ripple-carry-circuit 
			1 1 1 0 ; A1 - A4
			1 1 1 0 ; B1 - B4
			0       ; Cin
			)) (error "Four bit ripple carry adder expected 12 output for this input with carry bit"))
      (unless (equal? (list 1 1 1 0 1)
		      (ripple-carry-circuit 
			1 1 1 0 ; A1 - A4
			1 1 1 0 ; B1 - B4
			1       ; Cin
			)) (error "Four bit ripple carry adder expected 13 output for this input with carry bit"))
      (unless (equal? (list 1 1 1 1 0)
		      (ripple-carry-circuit 
			1 1 1 1 ; A1 - A4
			1 1 1 1 ; B1 - B4
			0       ; Cin
			)) (error "Four bit ripple carry adder expected 14 output for this input with carry bit"))
      (unless (equal? (list 1 1 1 1 1)
		      (ripple-carry-circuit 
			1 1 1 1 ; A1 - A4
			1 1 1 1 ; B1 - B4
			1       ; Cin
			)) (error "Four bit ripple carry adder expected 15 output for this input with carry bit"))))


(define (check-bits)
  (begin
      (unless (= 0 (wire-or 0 0)) (error "OR with input 0 0 should output 0"))
      (unless (= 1 (wire-or 1 0)) (error "OR with input 1 0 should output 1"))
      (unless (= 1 (wire-or 0 1)) (error "OR with input 0 1 should output 1"))
      (unless (= 1 (wire-or 1 1)) (error "OR with input 1 1 should output 1"))

      (unless (= 0 (wire-and 0 0)) (error "AND with input 0 0 should output 0"))
      (unless (= 0 (wire-and 1 0)) (error "AND with input 1 0 should output 0"))
      (unless (= 0 (wire-and 0 1)) (error "AND with input 0 1 should output 0"))
      (unless (= 1 (wire-and 1 1)) (error "AND with input 1 1 should output 1"))

      (unless (= 1 (wire-not 0)) (error "NOT with input 0 should output 1"))
      (unless (= 0 (wire-not 1)) (error "NOT with input 1 should output 0"))

      (unless (= 0 (wire-xor 0 0)) (error "XOR with input 0 0 should output 0"))
      (unless (= 1 (wire-xor 1 0)) (error "XOR with input 1 0 should output 1"))
      (unless (= 1 (wire-xor 0 1)) (error "XOR with input 0 1 should output 1"))
      (unless (= 0 (wire-xor 1 1)) (error "XOR with input 1 1 should output 1"))

      (unless (= 0 (car (wire-half-adder 0 0))) (error "Half adder with input 0 0 should have sum 0"))
      (unless (= 1 (car (wire-half-adder 0 1))) (error "Half adder with input 0 1 should have sum 1"))
      (unless (= 1 (car (wire-half-adder 1 0))) (error "Half adder with input 1 0 should have sum 1"))
      (unless (= 0 (car (wire-half-adder 1 1))) (error "Half adder with input 1 1 should have sum 0"))
      (unless (= 0 (cdr (wire-half-adder 0 0))) (error "Half adder with input 0 0 should have carry 0"))
      (unless (= 0 (cdr (wire-half-adder 0 1))) (error "Half adder with input 0 1 should have carry 0"))
      (unless (= 0 (cdr (wire-half-adder 1 0))) (error "Half adder with input 1 0 should have carry 0"))
      (unless (= 1 (cdr (wire-half-adder 1 1))) (error "Half adder with input 1 1 should have carry 1"))

      (unless (= 0 (car (wire-full-adder 0 0 0))) (error "Full adder with input 0 0 0 should have sum 0"))
      (unless (= 1 (car (wire-full-adder 0 1 0))) (error "Full adder with input 0 1 0 should have sum 1"))
      (unless (= 1 (car (wire-full-adder 1 0 0))) (error "Full adder with input 1 0 0 should have sum 1"))
      (unless (= 0 (car (wire-full-adder 1 1 0))) (error "Full adder with input 1 1 0 should have sum 0"))
      (unless (= 1 (car (wire-full-adder 0 0 1))) (error "Full adder with input 0 0 1 should have sum 1"))
      (unless (= 0 (car (wire-full-adder 0 1 1))) (error "Full adder with input 0 1 1 should have sum 0"))
      (unless (= 0 (car (wire-full-adder 1 0 1))) (error "Full adder with input 1 0 1 should have sum 0"))
      (unless (= 1 (car (wire-full-adder 1 1 1))) (error "Full adder with input 1 1 1 should have sum 1"))

      (unless (= 0 (cdr (wire-full-adder 0 0 0))) (error "Full adder with input 0 0 0 should have cout 0"))
      (unless (= 0 (cdr (wire-full-adder 0 1 0))) (error "Full adder with input 0 1 0 should have cout 0"))
      (unless (= 0 (cdr (wire-full-adder 1 0 0))) (error "Full adder with input 1 0 0 should have cout 0"))
      (unless (= 1 (cdr (wire-full-adder 1 1 0))) (error "Full adder with input 1 1 0 should have cout 1"))
      (unless (= 0 (cdr (wire-full-adder 0 0 1))) (error "Full adder with input 0 0 1 should have cout 0"))
      (unless (= 1 (cdr (wire-full-adder 0 1 1))) (error "Full adder with input 0 1 1 should have cout 1"))
      (unless (= 1 (cdr (wire-full-adder 1 0 1))) (error "Full adder with input 1 0 1 should have cout 1"))
      (unless (= 1 (cdr (wire-full-adder 1 1 1))) (error "Full adder with input 1 1 1 should have cout 1"))

      (check-ripple-carry wire-four-bit-ripple-carry-adder)
      (check-ripple-carry wire-n-bit-ripple-carry-adder)
))

(define (check-numbers)
  (define (check-add a b)
    (let ([added (ripple-carry-adder a b)])
      (unless 
	(= (+ a b) added)
	(error 
	  (format "Expected ~a + ~a = ~a, but the ripple-carry-adder did ~a + ~a = ~a" 
		  a b (+ a b) a b added)))
      added))
  (map check-add (range (expt 2 10)) (range (expt 2 10))))

(define (check-simulation)
    (check-bits)
    (check-numbers)
    (displayln "Passed all checks!")
)
