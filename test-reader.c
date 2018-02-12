#lang racket
; #lang reader "reader.rkt"

(require "utils.rkt")
(require "types.rkt")

(define x (c-label 'derp))

(destruct c-label x)
(displayln x-name)
