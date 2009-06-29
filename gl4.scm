(use gl)
(use gl.glut)
;(load "./gauche-gl/segment.scm")
;(load "./gauche-gl/frame.scm")
;(load "./gauche-gl/vector.scm")
;(load "./gauche-gl/painter.scm")

(define *earth-r* 6378000)
(define *moon-r* 1738000)
(define *display-scale* (/ 0.9 384000000))

(define (my-init progname)
  (glut-init-display-mode GLUT_RGBA)
  (glut-init-window-size 500 500)
  (glut-init-window-position 10 10)
  (glut-create-window (format "ICFP #~a" *scenario-id*))
  (gl-clear-color 0.0 0.0 0.0 0.0) ;;1.0 1.0 1.0 1.0))
  )

(define (my-keyboard key x y)
  (when (= key 27) (exit 0))

  ;; step
  (let ([ch (integer->char key)]
		[skip #f]
		[last-cs-x #f]
		[last-cs-y #f]
		[last-target-x #f]
		[last-target-y #f])
	;;(format #t "key=~a (~a)\n" key ch)
	(case ch
	  [(#\Space)
	   (hit-space)
	   ;;(moving)
	   (glut-post-redisplay)
	   ]
	  [(#\-)
	   (set! *display-scale* (/ *display-scale* 2))
	   (glut-post-redisplay)]
	  [(#\+ #\=)
	   (set! *display-scale* (* *display-scale* 2))
	   (glut-post-redisplay)]

	  [else #f]
	  ;;(vm-step vm 0 0) #f])
	  ;(begin (vm-step vm 0 0) #f) )
	  ;(unless skip
	  ;(moving)
	  ;(glut-post-redisplay))
	  )))

(define (my-display)
  (gl-clear GL_COLOR_BUFFER_BIT)
	;;;
  (gl-flush))

(define (my-reshape w h)
  )

(define (main args)
  (glut-init args)
  (my-init (car args))
  (glut-keyboard-func my-keyboard)
  (glut-reshape-func my-reshape)
  (glut-display-func my-display)

  (glut-main-loop)
  0)

;;;;;;;;;;;

(define (draw-cross x y hw)
  (let ([%x (* x *display-scale*)]
		[%y (* y *display-scale*)])
	(gl-begin GL_LINES)
	(gl-vertex (- %x hw) (- %y hw))
	(gl-vertex (+ %x hw) (+ %y hw))
	(gl-vertex (- %x hw) (+ %y hw))
	(gl-vertex (+ %x hw) (- %y hw))
	(gl-end)))

(define (draw-circle cx cy hw . args)
  (let ([%cx (* cx *display-scale*)]
		[%cy (* cy *display-scale*)]
		[fill? (not (null? args))])
	(gl-begin (if fill? GL_POLYGON GL_LINE_LOOP))
	(dotimes (i 100)
	  (let ([c-r (max (* hw *display-scale*) 0.01)]
			[c-theta (* 2/100 pi i)])
		(gl-vertex (+ %cx (* c-r (cos c-theta)))
				   (+ %cy (* c-r (sin c-theta))))
		))
	(gl-end)))

(define (draw-trace trace-list)
  (gl-begin GL_LINE_STRIP)
  (for-each (lambda (cs)
			  (let* ([cx (car cs)] [cy (cdr cs)]
					 [r (* (hypot cx cy) *display-scale*)]
					 [th (atan cy cx)]
					 [x (* r (cos th))] [y (* r (sin th))] )
				(gl-vertex x y)))
			trace-list)
  (gl-end))

(define (draw-char x y ch)
  (gl-raster-pos (- (* x *display-scale*) 0.02)
				 (- (* y *display-scale*) 0.02))
  (glut-bitmap-character GLUT_BITMAP_8_BY_13 (char->integer ch)))

(define (draw-string* x_ y_ str)
  (gl-raster-pos x_ y_)
  (for-each (cut glut-bitmap-character GLUT_BITMAP_8_BY_13 <>)
			(map char->integer (string->list str))))

