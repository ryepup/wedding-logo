(require 'vecto)
(require 'cl-ppcre)

(defpackage #:wedding
  (:use #:cl #:vecto))

(in-package #:wedding)

(defvar *r* 400)
(defvar *gear-height* 5)

(defun draw-outer-heart (&key (line-width 3))
  "draw the heart outline, a square with 2 half-circles"
  (with-graphics-state
    (set-line-width line-width)
    (move-to 0 0)
    (line-to 0 *r*)
    (arcn (/ *r* 2) *r* (/ *r* 2) pi 0)
    (arcn *r* (/ *r* 2) (/ *r* 2) (/ pi 2) (- (/ pi 2)))
    (line-to 0 0)
    (fill-and-stroke)))

(defgeneric gear-internal (style radius)
  (:documentation "draws the internals of a gear, everything
inside the teeth")
  (:method ((style (eql :simple)) radius)
    "a simple solid gear with a hole in the middle"
    (centered-circle-path 0 0 radius)
    (fill-path)
    (centered-circle-path 0 0 (/ radius 5))
    (set-rgb-fill 0 0 0)
    (fill-path)))

(defun spoked-gear-internal (radius spokes)
  "draws internals with spokes"
  ;;outer ring
  (centered-circle-path 0 0 radius)
  (fill-path)
  ;;clear the way
  (with-graphics-state
    (set-rgb-fill 0 0 0)
    (centered-circle-path 0 0 (* 0.8 radius))
    (fill-path))
  (with-graphics-state
    (let ((spoke-width (* 0.1 (/ (* pi (* 2 radius))
				 spokes))))
    (dotimes (i spokes)
      (rectangle (- (/ spoke-width 2)) 0
		 spoke-width radius)
      (fill-path)
      (rotate (/ (* pi 2)
		 spokes)))))

  (centered-circle-path 0 0 (* 0.4 radius))
  (fill-path)
  (with-graphics-state
    (set-rgb-fill 0 0 0)
    (centered-circle-path 0 0 (* 0.2 radius))
    (fill-path)))

(defmethod gear-internal :around (style radius)
  (with-graphics-state
    (call-next-method)))

(defun gear (x y radius teeth rotation &key (spokes (random 10)))
  "draw a gear"
  (with-graphics-state
    (let ((gw (/ (* pi (* 2 radius)) 2 teeth)))
      (translate x y)
      (rotate (* gw rotation))
      (with-graphics-state
	(dotimes (i teeth)
	  (rectangle (- (/ gw 2)) 0
		     gw radius)
	  (fill-path)
	  (rotate (/ (* 2 pi) teeth))))
      (let ((radius (- radius *gear-height*)))
	(format T "gear internal radius: ~a~%" radius)
	(if (< spokes 3)
	    (gear-internal :simple radius)
	    (spoked-gear-internal radius spokes)))

      (values radius gw))))

(defun save-mug-logo (&key (filename "heart-logo.png")
		  (font-file "Lucida Handwriting Italic.ttf")
		  )
  "draw the logo, saving to filename and using font-file for text"
  (with-canvas (:width (* 2 *r*)
		       :height (truncate
				(* 2.25 *r*)))
    (set-rgb-fill 1 1 1)
    (clear-canvas)
    (set-rgb-fill 0 0 0)
    (set-rgb-stroke 0 0 0)
    (with-graphics-state
      (translate *r* (- *r* (* (sqrt 2) (/ *r* 2))))
      (rotate (/ pi 4))
      (draw-outer-heart)
      (let ((*r* (* .985 *r*)))
	(loop for (x y r teeth rotation) in '((.1 .1 .1 16 0)
					      (.2 .35 .19 30 .25)
					      (.4 .83 .35 50 .25)
					      (.3 1.29 .14 20 0)
					      (.51 1.39 .11 14 .5)
					      (.73 1.22 .18 28 0)
					      (.87 .99 .11 15 .2)
					      (1.1 .65 .32 40 0)
					      (1.275 .26 .125 17 .1)
					      (1.05 .15 .15 19 .1)
					      (.8 .13 .12 14 .05)
					      (.6 .31 .17 24 0)
					      (.4 .15 .11 16 .15))
	      do
	   (gear (* x *r*)
		 (* y *r*)
		 (* r *r*)
		 teeth
		 rotation))))
    (let* ((font-size 50)
	   (label-height (aref (string-bounding-box
				"g" font-size
				(get-font font-file))
			       3)))
      (set-font (get-font font-file) font-size)
            (draw-centered-string *r*
				  (+ *r*
				     (* (sqrt 2) (/ *r* 2))
				     (* 3 label-height))
			    "Davis-Fullen Wedding")
      (draw-centered-string *r* (- *r*
				   (* (sqrt 2) (/ *r* 2))
				   (* 2 label-height))
			    "New Year's Eve 2008"))
    (save-png filename)))

(defun save-tattoo-logo (&key (filename "heart-tattoo.png"))
  "draw the logo in tattoo size, saving to filename"
  (let ((*r* 175)
	(*gear-height* 5))
    (with-canvas (:width (* 2 *r*)
			 :height (truncate
				  (* 2.25 *r*)))
      (set-rgb-fill 1 1 1)
      (clear-canvas)
      (set-rgb-fill 0 0 0)
      (set-rgb-stroke 0 0 0)
      (with-graphics-state
	(with-graphics-state
	  (translate *r* (- *r* (* (sqrt 2) (/ *r* 2))))
	  (rotate (/ pi 4))
	  (draw-outer-heart))
	(set-rgb-fill 1 1 1)
	(let ((*r* (* .985 *r*)))
	  (loop for (x y r teeth rotation) in '((1.08 1.5 .15 8 0)
						(.6 1.4 .35 18 0)
						(.87 .9 .24 14 0)
						(1 .55 .13 8 0)
						)
		do
	     (gear (* x *r*)
		   (* y *r*)
		   (* r *r*)
		   teeth
		   rotation))))
      (save-png filename))))






(defun make-samples (&optional (n 20))
  "make a bunch to try and get a good random set"
  (dotimes (i n)
    (save-logo :filename (format nil "heart_~a.png" i))))

