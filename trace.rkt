#lang typed/racket

;; CMSC 15100 Winter 2022
;; Project 2
;; trace.rkt module
;; Will Medeiros
;;
;; Ray casting and recursive ray tracing
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
(require "color.rkt")
(require "camera.rkt")
(require "material.rkt")
(require "object.rkt")
(require "hit.rkt")

;; A (Scene obj bg) packages up the description of a scene, where obj
;; is an object representing the geometric objects in the scene and
;; bg is a function that computes a background color based on a ray
(define-struct Scene
  ([world : Object]
   [background : (Ray -> RGB)]))

(: cast-ray-in-world : Scene -> Ray -> RGB)
;; ray caster for testing purposes
(define (cast-ray-in-world scene)
  (match scene
    [(Scene world ray->background)
     (lambda ([ray : Ray])
       (match (first-entry (hit-test world ray 0.001))
         [(Some hit)
          (match (get-reflection ray hit)
            ['None (ray->background ray)]
            [(Some (Reflect-Info rgb _)) rgb])]
         ['None (ray->background ray)]))]))

(: trace-ray-in-world : Scene Natural -> Ray -> RGB)
;; Given a world and a maximum tracing depth, this function returns
;; a function that will recursively trace a ray through the world
;; to compute a color
(define (trace-ray-in-world scene max-depth)
  (lambda ([ray : Ray])
    (local
      {(: trace-ray-aux : Ray Natural -> RGB)
       ;; recursively calls the ray tracer on the reflection ray
       (define (trace-ray-aux ray n)
         (if (= n 0)
             rgb-black
             (match ((Object-hit-test (Scene-world scene)) ray 0.001)
               [(cons h hxr) (match (get-reflection ray h)
                               [(Some (Reflect-Info aten r)) (rgb* aten (trace-ray-aux r (- n 1)))]
                               ['None (get-emission ray h)])]
               [_ ((Scene-background scene) ray)])))}
      (trace-ray-aux ray max-depth))))

(: ray-tracer : Camera Scene Natural -> Image)
;; Given a camera, world object, and max depth, render a scene
;; using the given depth limit.
(define (ray-tracer cam scene maxd)
  (foreach-pixel cam
                 (make-pixel-renderer
                  (antialias-pixel->rgb cam
                                        (trace-ray-in-world scene maxd))
                  gamma-rgb->color)))

;;;;;;;;;;
;; Exports
;;;;;;;;;;

(provide Scene)

(provide cast-ray-in-world
         trace-ray-in-world
         ray-tracer)
