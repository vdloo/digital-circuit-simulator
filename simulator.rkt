#!/usr/bin/env racket
#lang racket
(require rnrs/base-6)
(require rnrs/mutable-pairs-6)
(require "queue.rkt")

(provide propagate)
(provide get-signal)
(provide set-signal!)
(provide make-wire)
(provide add-action!)
(provide set-signal!)
(provide after-delay)
(provide probe)
(provide the-agenda)

; From SICP 3.3.4 A Simulator for Digital Circuits
(define get-signal
  (λ (wire)
     (wire 'get-signal)))

(define set-signal!
  (λ (wire new-value)
     ((wire 'set-signal!) new-value)))

(define add-action!
  (λ (wire action-procedure)
     ((wire 'add-action!) action-procedure)))

(define call-each
  (λ (procedures)
     (if (null? procedures)
       'done
       (begin
	 ((car procedures))
	  (call-each (cdr procedures))))))

(define make-wire
  (λ ()
    (let ((signal-value 0) (action-procedures '()))
      (define set-my-signal! 
          (λ (new-value)
            (if (not (= signal-value new-value))
              (begin (set! signal-value new-value)
                     (call-each action-procedures))
              'one)))

      (define accept-action-procedure!
	(λ (proc)
          (set! action-procedures (cons proc action-procedures))
          (proc)))

      (define dispatch
	(λ (m)
          (cond ((eq? m 'get-signal) signal-value)
                ((eq? m 'set-signal!) set-my-signal!)
                ((eq? m 'add-action!) accept-action-procedure!)
                (else (error "Unknown operation -- WIRE" m)))))
        dispatch)))

(define make-time-segment
  (λ (time queue)
     (cons time queue)))

(define segment-time
  (λ (s)
     (car s)))

(define segment-queue
  (λ (s)
     (cdr s)))

(define make-agenda
  (λ ()
     (list 0)))

(define current-time
  (λ (agenda)
     (car agenda)))

(define set-current-time!
  (λ (agenda time)
     (set-car! agenda time)))

(define segments
  (λ (agenda)
     (cdr agenda)))

(define set-segments!
  (λ (agenda segments)
     (set-cdr! agenda segments)))

(define first-segment
  (λ (agenda)
     (car (segments agenda))))

(define rest-segments
  (λ (agenda)
     (cdr (segments agenda))))

(define empty-agenda?
  (λ (agenda)
     (null? (segments agenda))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
      (set-segments! agenda (rest-segments agenda))
      )))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
    (error "Agenda is empty -- FIRST-AGENDA-ITEM")
    (let ((first-seg (first-segment agenda)))
      (set-current-time! agenda (segment-time first-seg))
      (front-queue (segment-queue first-seg)))))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
	(< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
      (insert-queue! (segment-queue (car segments))
		     action)
      (let ((rest (cdr segments)))
	(if (belongs-before? rest)
	  (set-cdr!
	    segments
	    (cons (make-new-time-segment time action)
		  (cdr segments)))
	  (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
      (set-segments!
	agenda
	(cons (make-new-time-segment time action)
	      segments))
      (add-to-segments! segments))))

(define the-agenda (make-agenda))

(define after-delay
  (λ (delay action)
     (add-to-agenda! (+ delay (current-time the-agenda))
		      action
		      the-agenda)))
(define propagate
  (λ ()
     (if (empty-agenda? the-agenda)
       'done
       (let ((first-item (first-agenda-item the-agenda)))
	 (first-item)
	 (remove-first-agenda-item! the-agenda)
	 (propagate)))))

(define (probe name wire)
  (add-action! wire
       (λ ()
	  (newline)
	  (display name)
	  (display " ")
	  (display (current-time the-agenda))
	  (display " New-value = ")
	  (display (get-signal wire)))))
