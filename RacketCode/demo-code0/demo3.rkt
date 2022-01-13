#lang racket
(define (extract str)
  (substring str 4 7))
(extract "Hello World:)")