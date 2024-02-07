#lang typed/racket

;; CMSC 15100 Winter 2022
;; Project 2
;; examples.rkt
;;
;; Eyeball tests for ray casting and recursive ray tracing
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
(require "hit.rkt")
(require "object.rkt")
(require "sphere.rkt")
(require "trace.rkt")
(require "plane.rkt")
(require "csg.rkt")

;; test cameras
(define cam-400x200x1 (simple-camera 400 200 1 1.0))
(define cam-400x200x20 (simple-camera 400 200 20 1.0))
(define cam-400x200x100 (simple-camera 400 200 100 1.0))
(define cam-200x100x100 (simple-camera 200 100 100 1.0))

(define sphere-0
  (make-sphere (Float3 0.0 0.0 -2.0) 0.5 (flat-material (RGB 1.0 0.0 0.0))))
(define sphere-1 (make-sphere (Float3 0.0 0.0 -2.0) 0.5 normal-material))
(define sphere-2 (make-sphere (Float3 0.0 -100.5 -2.0) 100.0 normal-material))

;; tests for Step 1

"== eyeball test for camera"
(local
  {(define cam (make-camera 400 200 1
                            (Float3 2.0 1.0 1.0)
                            (Float3 0.0 0.0 0.0)
                            (Float3 0.0 1.0 0.0)
                            120.0))
   (define sphere-1 (make-sphere (Float3 0.0 -100.5 -1.0) 100.0
                                 (flat-material (RGB 0.8 0.9 0.0))))
   (define sphere-2 (make-sphere (Float3 1.0 0.0 -1.0) 0.5
                                 (flat-material (RGB 0.9 0.6 0.2))))
   (define sphere-3 (make-sphere (Float3 0.0 0.0 -1.0) 0.5
                                 (flat-material (RGB 0.9 0.3 0.3))))
   (define sphere-4 (make-sphere (Float3 -1.0 0.0 -1.0) 0.5
                                 (flat-material (RGB 0.3 0.3 0.9))))
   (define world (Scene
                  (list->object (list sphere-1 sphere-2 sphere-3 sphere-4))
                  ray->rgb))}
  (foreach-pixel cam
                 (make-pixel-renderer
                  (pixel->rgb cam (cast-ray-in-world world))
                  rgb->color)))

;; tests for Step 2

(: darker-ray->rgb : Ray -> RGB)
;; a darker background
(define (darker-ray->rgb ray)
  (match ray
    [(Ray _ dir)
     (local
       {(define t : Float (* 0.5 (+ 1.0 (Float3-y dir))))}
       (rgb-lerp rgb-white t (RGB 0.3 0.4 0.6)))]))

(: red-horizon : Ray -> RGB)
;; background that combines blue sky with orange at the horizon
(define (red-horizon ray)
  (local
    {(define t (Float3-y (Ray-dir ray)))}
    (rgb+ (RGB 0.1 0.1 0.1)
          (rgb-lerp (RGB 0.87 0.4 0.0) t (RGB 0.0 0.6 0.8)))))

"== eyeball test for emissive material"
(local
  {(define cam (simple-camera 400 200 200 1.0))
   (define sphere-1 (make-sphere (Float3 0.0 0.0 -2.0) 0.5
                                 (lambertian-material (RGB 0.8 0.3 0.3))))
   (define sphere-2 (make-sphere (Float3 0.0 -100.5 -2.0) 100.0
                                 (lambertian-material (RGB 0.8 0.8 0.0))))
   (define sphere-3 (make-sphere (Float3 1.0 0.0 -2.0) 0.5
                                 (diffuse-emissive-material (RGB 2.0 2.0 2.0))))
   (define sphere-4 (make-sphere (Float3 -1.0 0.0 -2.0) 0.5
                                 (metal-material (RGB 0.8 0.8 0.8) 0.1)))
   (define world (Scene
                  (list->object (list sphere-1 sphere-2 sphere-3 sphere-4))
                  darker-ray->rgb))}
  (ray-tracer cam world 10))

"== eyeball test for ray trace (100 samples): lambertian material and two spheres"
(define sphere-3 (make-sphere (Float3 0.0 0.0 -2.0) 0.5
                              (lambertian-material (rgb-gray 0.5))))
(define sphere-4 (make-sphere (Float3 0.0 -100.5 -2.0) 100.0
                              (lambertian-material (rgb-gray 0.5))))
(define world-2 (Scene (list->object (list sphere-3 sphere-4)) red-horizon))

(foreach-pixel cam-200x100x100
               (make-pixel-renderer
                (antialias-pixel->rgb cam-200x100x100
                                      (trace-ray-in-world world-2 5))
                gamma-rgb->color))

"== eyeball test for ray trace (100 samples): lambertian and metal; four spheres"
(define sphere-5 (make-sphere (Float3 0.0 0.0 -2.0) 0.5
                              (lambertian-material (RGB 0.8 0.3 0.3))))
(define sphere-6 (make-sphere (Float3 0.0 -100.5 -2.0) 100.0
                              (lambertian-material (RGB 0.8 0.8 0.0))))
(define sphere-7 (make-sphere (Float3 1.0 0.0 -2.0) 0.5
                              (metal-material (RGB 0.8 0.6 0.2) 1.0)))
(define sphere-8 (make-sphere (Float3 -1.0 0.0 -2.0) 0.5
                              (metal-material (RGB 0.8 0.8 0.8) 0.1)))
(define world-3 (Scene (list->object (list sphere-5 sphere-6 sphere-7 sphere-8)) red-horizon))
(foreach-pixel cam-200x100x100
               (make-pixel-renderer
                (antialias-pixel->rgb cam-200x100x100
                                      (trace-ray-in-world world-3 20))
                gamma-rgb->color))

(time (ray-tracer
       (make-camera 400 200 100 (Float3 0.0 0.25 2.0) fl3-zero (Float3 0.0 1.0 0.0) 120.0)
       (Scene
        (list->object
         (list
          (make-plane (Float3 0.0 0.0 -2.5)
                      (Float3 1.0 0.0 1.0)
                      (metal-material (RGB 0.9 0.9 0.9) 0.05))
          (make-sphere fl3-zero
                       0.5
                       (lambertian-material (RGB 0.8 0.3 0.3)))
          sphere-6))
        ray->rgb)
       10))

"== AA-100 ray-tracer plane test: two spheres and two planes"
(time (ray-tracer
       (make-camera 400 200 100 (Float3 0.0 0.25 2.0) fl3-zero (Float3 0.0 1.0 0.0) 120.0)
       (Scene
        (list->object
         (list
          (make-plane (Float3 0.0 0.0 -2.5)
                      (Float3 1.0 0.0 1.0)
                      (metal-material (RGB 0.9 0.9 0.9) 0.05))
          (make-plane (Float3 0.0 0.0 -2.5)
                      (Float3 -1.0 0.0 1.0)
                      (metal-material (RGB 0.9 0.9 0.9) 0.05))
          (make-sphere fl3-zero
                       0.5
                       (lambertian-material (RGB 0.8 0.3 0.3)))
          sphere-6))
        ray->rgb)
       10))

(define sphere1 (make-sphere (Float3 0.0 -0.25 -2.0) 0.5 normal-material))
(define sphere2 (make-sphere (Float3 0.0 0.25 -2.0) 0.5 normal-material))
(define plane1 (make-plane (Float3 0.0 -0.25 -2.0) (Float3 0.0 1.0 0.0) normal-material))
(define plane2 (make-plane (Float3 0.0 0.25 -2.0) (Float3 0.0 -1.0 0.0) normal-material))

(foreach-pixel
 cam-400x200x1
 (make-pixel-renderer
  (pixel->rgb
   cam-400x200x1
   (cast-ray-in-world
    (Scene
     (object-union
      (object-intersect plane1 sphere1)
      (object-intersect plane2 sphere2))
     ray->rgb)))
   rgb->color))

(foreach-pixel
 cam-400x200x1
 (make-pixel-renderer
  (pixel->rgb
   cam-400x200x1
   (cast-ray-in-world
    (Scene
     (object-subtract plane1 sphere1)
     ray->rgb)))
  rgb->color))

(foreach-pixel
 cam-400x200x1
 (make-pixel-renderer
  (pixel->rgb
   cam-400x200x1
   (cast-ray-in-world
    (Scene
     (object-subtract sphere1 plane1)
     ray->rgb)))
  rgb->color))