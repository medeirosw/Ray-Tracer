#lang typed/racket

;; CMSC 15100 Winter 2022
;; Project 2
;; plane.rkt module
;; Will Medeiros
;;
;; This module contains the implementation of the plane object
;;

;; load custom definitions
;;
(require "../include/cs151-core.rkt")

;; load image library definitions
;;
(require "../include/cs151-image.rkt")

;; load testing infrastructure
;;
(require typed/test-engine/racket-tests)

;; project modules
;;
(require "util.rkt")
(require "math-util.rkt")
(require "material.rkt")
(require "hit.rkt")
(require "object.rkt")

(: make-plane : Float3 Float3 Material -> Object)
;; (make-plane pt perp material) makes a plane object.  The plane
;; contains the point pt and its orientation is defined by the
;; vector perp, which is perpendicular to the plane.  The third
;; argument specifies the plane's surface material.
;; Note that the perpendicular vector does not have to be unit length.
(define (make-plane q perp mat)
  (local
    {(define n : Float3 (fl3-normalize perp))
     (: ht : Ray Float -> Hit-List)
     ;; hit-test for plane
     (define (ht ray mint)
       (local
         {(define d : Float3 (Ray-dir ray))}
         (if (< -.0001 (fl3-dot d n) .0001) (list (Hit 'OUT +inf.0 fl3-zero fl3-zero flat-black))
             (local
               {(define p : Float3 (Ray-origin ray))
                (define entry-check : Float (fl3-dot (fl3- q p) n))
                (define t : Float (/ entry-check (fl3-dot d n)))
                (define entry : Parity (if (< entry-check 0.0) 'IN 'OUT))}
               (if (<= mint t)
                   (if (symbol=? entry 'IN)
                       (list (Hit entry
                                  t
                                  (ray-point-at ray t)
                                  n
                                  mat)
                             (Hit 'OUT +inf.0 fl3-zero fl3-zero flat-black))
                       (list (Hit entry
                                  t
                                  (ray-point-at ray t)
                                  n
                                  mat)))
                       miss)))))}
(Object ht)))

;;;;;;;;;;
;; Exports
;;;;;;;;;;

(provide make-plane)
