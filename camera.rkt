#lang typed/racket

;; CMSC 15100 Winter 2022
;; Project 2
;; camera.rkt module
;; Will Medeiros
;;
;; This module implements the Camera abstraction
;;

;; load custom definitions
;;
(require "../include/cs151-core.rkt")

;; load image library definitions
;;
(require "../include/cs151-image.rkt")

;; project modules
;;
(require "math-util.rkt")
(require "color.rkt")

;; the representation of a Camera for the ray tracer.
(define-struct Camera
  [(wid : Natural)           ;; width of image
   (ht : Natural)            ;; height of image
   (n-samples : Natural)     ;; number of samples per pixel
   (origin : Float3)         ;; where the camera is located
   (ulc : Float3)            ;; upper-left-corner of image plane
   (h-vec : Float3)          ;; horizontal pixel-wide vector parallel to image
   ;; pointing right
   (v-vec : Float3)])        ;; vertical pixel-wide vector parallel to image
;; pointing down

(: simple-camera : Natural Natural Natural Float -> Camera)
;; make a camera that is equivalent to a Project 1 camera (for
;; testing purposes)
(define (simple-camera wid ht ns flen)
  (local
    {(define pw : Float (/ 2.0 (->fl wid)))}
    (Camera wid ht ns
            fl3-zero
            (Float3 -1.0
                    (/ (->fl ht) (->fl wid))
                    (- flen))
            (Float3 pw 0.0 0.0)
            (Float3 0.0 (- pw) 0.0))))

(: make-camera : Natural Natural Natural Float3 Float3 Float3 Float -> Camera)
;; make a camera.  The arguments are (in order):
;;   - width of image
;;   - height of image
;;   - number of samples per pixel
;;   - origin of camera in the world
;;   - point that the camera is looking at
;;   - up vector
;;   - horizontal field of view (in degrees)
(define (make-camera wid ht ns pos look-at up fov)
  (local
    {(define dcam (fl3-normalize (fl3- look-at pos)))
     (define r (fl3-normalize (fl3-cross dcam up)))
     (define uprime (fl3-normalize (fl3-cross r dcam)))
     (define h (fl3-scale (/ 2.0 wid) r))
     (define v (fl3-scale (/ -2.0 wid) uprime))
     (define flen (/ 1.0 (tan (degrees->radians (/ fov 2.0)))))
     (define cimage (fl3+ pos (fl3-scale flen dcam)))
     (define ulc (fl3+ cimage (fl3- (fl3-scale (/ (->fl ht) (->fl wid)) uprime) r)))}
    (Camera wid ht ns pos ulc h v)))

;; A Pixel-Renderer is a function that takes the row and column of a pixel
;; and produces a Racket Image-Library Color
(define-type Pixel-Renderer (Natural Natural -> Color))

(: foreach-pixel : Camera Pixel-Renderer -> Image)
;; given a camera and a pixel renderer, generate an image.
;;
(define (foreach-pixel cam pixel-renderer)
  (match cam
    [(Camera wid ht _ _ _ _ _)
     (if (or (= wid 0) (= ht 0))
         empty-image
         (local
           {(: for-rows : Natural (Listof Color) -> (Listof Color))
            ;; iterate over the rows of the image from bottom to top
            (define (for-rows row pixels)
              (if (= 0 row)
                  pixels
                  (for-cols (- row 1) wid pixels)))
            (: for-cols :  Natural Natural (Listof Color) -> (Listof Color))
            ;; iterate over the columns of a row from right to left
            (define (for-cols row col pixels)
              (if (= 0 col)
                  (for-rows row pixels)
                  (for-cols
                   row
                   (- col 1)
                   (cons (pixel-renderer row (- col 1)) pixels))))}
           (color-list->bitmap
            (for-rows ht '())
            wid ht)))]))

(: make-pixel-renderer : (Natural Natural -> RGB) (RGB -> Color) -> Pixel-Renderer)
;; compose a function that maps pixel coordinates to RGB values with
;; an RGB to Image-Library Color converter
(define (make-pixel-renderer pixel->rgb rgb->color)
  (lambda ([row : Natural] [col : Natural]) (rgb->color (pixel->rgb row col))))

(: ray-for-pixel : Camera -> (Natural Natural -> Ray))
;; takes a camera and returns a function for generating a ray
;; for a pixel specified by its row and column
(define (ray-for-pixel cam)
  (local
    {(define up-left-pix (fl3+ (Camera-ulc cam)
                               (fl3-scale .5 (fl3+ (Camera-v-vec cam) (Camera-h-vec cam)))))}
    (lambda ([r : Natural] [c : Natural])
      (make-ray (Camera-origin cam) (fl3- (fl3+
                                           (fl3+ up-left-pix
                                                 (fl3-scale (->fl r) (Camera-v-vec cam)))
                                           (fl3-scale (->fl c) (Camera-h-vec cam)))
                                          (Camera-origin cam))))))

(: rays-for-pixel : Camera -> (Natural Natural -> (Listof Ray)))
;; given a camera, return a function that maps pixel coordinates in
;; the image plane to a list of rays from the camera through the pixel.
;; The number of rays is determined by the n-samples field of the
;; Camera.
(define (rays-for-pixel cam)
  (lambda ([r : Natural] [c : Natural])
    (local
      {(: rays-aux : Natural Natural Natural (Listof Ray) -> (Listof Ray))
       ;; accumulator for list of rays
       (define (rays-aux r c n acc)
         (if (= n 0)
             acc
             (rays-aux r
                       c
                       (- n 1)
                       (cons
                        (make-ray
                         (Camera-origin cam)
                         (fl3- (fl3+
                                (fl3+ (Camera-ulc cam)
                                      (fl3-scale (+ r (random)) (Camera-v-vec cam)))
                                (fl3-scale (+ c (random)) (Camera-h-vec cam)))
                               (Camera-origin cam)))
                        acc))))}
      (rays-aux r c (Camera-n-samples cam) '()))))

(: pixel->rgb : Camera (Ray -> RGB) -> Natural Natural -> RGB)
;; given a camera and a ray-tracing function, return a function that
;; traces the ray for the given pixel
(define (pixel->rgb cam ray->rgb)
  (lambda ([row : Natural] [col : Natural])
    (ray->rgb ((ray-for-pixel cam) row col))))

(: antialias-pixel->rgb : Camera (Ray -> RGB) -> Natural Natural -> RGB)
;; given a camera and a ray-tracing function, return a function that
;; traces a list of rays for the given pixel and returns their average
(define (antialias-pixel->rgb cam ray->rgb)
  (lambda ([row : Natural] [col : Natural])
    (local
      {(define scale : Float (if (= (Camera-n-samples cam) 0.0)
                                 1.0
                                 (/ 1.0 (Camera-n-samples cam))))}
      (rgb-scale scale (foldl
                        (lambda ([x : RGB] [y : RGB])
                          (rgb+ x y))
                        (RGB 0.0 0.0 0.0)
                        (map ray->rgb ((rays-for-pixel cam) row col)))))))

(: ray->rgb : Ray -> RGB)
;; a function for testing ray generation.  It maps a ray to a color in
;; the white-to-blue range based on the Y component of the ray's direction
;; vector.
(define (ray->rgb ray)
  (match ray
    [(Ray _ dir)
     (local
       {(define t : Float (* 0.5 (+ 1.0 (Float3-y dir))))}
       (rgb-lerp rgb-white t (RGB 0.5 0.7 1.0)))]))

;;;;;;;;;;
;; Exports
;;;;;;;;;;

(provide Camera)

(provide make-camera
         simple-camera
         foreach-pixel
         make-pixel-renderer
         pixel->rgb
         antialias-pixel->rgb
         ray->rgb)
