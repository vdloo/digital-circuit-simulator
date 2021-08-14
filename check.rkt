#!/usr/bin/env racket
#lang racket

(require "circuits.rkt")
(provide check-simulation)

; This file contains assertions to verify if the circuit simulation is working as expected

(define (check-simulation)
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
))
