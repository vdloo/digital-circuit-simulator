#!/usr/bin/env racket
#lang racket

(display "Starting simulation\n")
(require "gates.rkt")
(require "check.rkt")

(check-simulation)
(display "Finished simulation\n")
