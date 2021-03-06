#!/usr/bin/env racket
#lang racket
(require rnrs/base-6)
(require rnrs/mutable-pairs-6)
(provide front-ptr)
(provide rear-ptr)
(provide set-front-ptr!)
(provide set-rear-ptr!)
(provide empty-queue?)
(provide front-queue)
(provide insert-queue!)
(provide delete-queue!)
(provide make-queue)

; From SICP 3.3.2 Representing Queues
(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))

(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue))) 

(define (make-queue) (cons '() '()))
