;(use gauche.uvector)
;(use binary.io)
(use srfi-1)

(use gl)
(use gl.glut)
(load "./gauche-gl/segment.scm")
(load "./gauche-gl/frame.scm")
(load "./gauche-gl/vector.scm")
(load "./gauche-gl/painter.scm")

(require "./vm")
(define vm (make-vm))

(vm-load vm "bin3.obf")
(define *scenario-id* 3004) ;; 自分は円
(define *mag* 6)

(vm-begin-tracing vm *scenario-id*)
(vm-set-configuration vm *scenario-id*)

(define target-r #f)
(define vcir1 #f) 
(define vcir2 #f)
(define delta-v1 #f)
(define delta-v2 #f)
(define rev? #f)

;(dotimes (i 2000) (vm-step vm 0 0))

;; t=0
(vm-step vm 0 0)
;; t=1
(let* ([cs-x1 (- (vm-oport vm 2))]
	   [cs-y1 (- (vm-oport vm 3))]
	   [cs-r1 (hypot cs-x1 cs-y1)]
	   [cs-th1 (atan cs-y1 cs-x1)]
	   ;[target-r1 (vm-oport vm 4)]
	   [target-x1 (+ cs-x1 (vm-oport vm 4))]
	   [target-y1 (+ cs-y1 (vm-oport vm 5))]
	   [target-r1 (hypot target-x1 target-y1)]
	   [target-th1 (atan target-y1 target-x1)]
	   )
  (vm-step vm 0 0)
  ;; t=2
  (let* ([cs-x2 (- (vm-oport vm 2))]
		 [cs-y2 (- (vm-oport vm 3))]
		 [cs-r2 (hypot cs-x2 cs-y2)]
		 [cs-th2 (atan cs-y2 cs-x2)]
		 ;;[target-r2 (vm-oport vm 4)]
		 [target-x2 (+ cs-x2 (vm-oport vm 4))]
		 [target-y2 (+ cs-y2 (vm-oport vm 5))]
		 [target-r2 (hypot target-x2 target-y2)]
		 [target-th2 (atan target-y2 target-x2)]
		 ;;
		 [delta-th (abs (- cs-th2 cs-th1))] ;; 1秒で進んだ角度
		 [delta (* cs-r2 delta-th)] ;; 1秒で進んだ距離
		 [dx (- cs-x2 cs-x1)]
		 [dy (- cs-y2 cs-y1)]
		 [dxx (hypot dx dy)]
		 [dxx-th (atan dy dx)]
		 )
;	(format #t "v) dx=~a delta=~a\n" dx delta)
;	(format #t "r) now:~a..~a target:~a\n" cs-r1 cs-r2 target-r1)
	(set! target-r target-r1)
	(set! vcir1 (sqrt (/ mu cs-r2)))
	(set! vcir2 (sqrt (/ mu target-r1)))
	(set! delta-v1 (* vcir1 (- (sqrt (/ (* 2 target-r1) (+ cs-r2 target-r1))) 1)))
;		   [delta-v1 (- (* vcir1 (sqrt (/ (* 2 target-r1) (+ cs-r2 target-r1)))) delta)]
	(set! delta-v2 (* vcir2 (- 1 (sqrt (/ (* 2 cs-r2) (+ cs-r2 target-r1))))))

	(format #t "Vcir) now:~a (delta=~a), target:~a\n" vcir1 delta vcir2)
	(format #t "delta-v) now:~a target:~a\n" delta-v1 delta-v2)

	))

;;
;; OpenGL
;;
(define earth-r 6378000.0)
(define cs-x earth-r)
(define cs-y 0)
(define cs-dx #f)
(define cs-dy #f)
(define cs-d-theta #f)
(define cs-r #f)
(define cs-theta #f)
(define cs-trace '())

(define target-x #f)
(define target-y #f)
(define target-r #f)
(define target-theta #f)
(define target-trace '())
;(define target-r #f)
(define target-r-min (* earth-r 50000))
(define target-r-max 0)

(define (my-init progname)
  (glut-init-display-mode GLUT_RGBA)
  (glut-init-window-size 500 500)
  (glut-init-window-position 10 10)
  (glut-create-window (format "ICFP #~a" *scenario-id*))
  (gl-clear-color 0.0 0.0 0.0 0.0) ;;1.0 1.0 1.0 1.0))
  )

(define (moving)
  (let ((last-x cs-x)
		(last-y cs-y))
;;	(format #t ">MOVING<\n")
	#;(format #t "last: (~a, ~a)\n" last-x last-y)
	;; サテライトから地球へのベクタなので両方ともマイナス
	(set! cs-x (- (vm-oport vm 2)))
	(set! cs-y (- (vm-oport vm 3)))
	(set! cs-r (hypot cs-x cs-y))
	(set! cs-theta (atan cs-y cs-x))
	(push! cs-trace (cons cs-x cs-y))

	(set! target-x (+ cs-x (vm-oport vm 4)))
	(set! target-y (+ cs-y (vm-oport vm 5)))
	(push! target-trace (cons target-x target-y))

	;;
	(when last-x
	  (set! cs-dx (- cs-x last-x))
	  (set! cs-dy (- cs-y last-y))
	  (set! cs-d-theta (atan cs-dy cs-dx))
	  #;(format #t "new: (~a, ~a) = last + Δ(~a, ~a), ~a, ~a\n" cs-x cs-y
			  cs-dx cs-dy (* 180/pi cs-d-theta) (hypot cs-dx cs-dy))
	  ))
  (set! target-x (+ cs-x (vm-oport vm 4)))
  (set! target-y (+ cs-y (vm-oport vm 5)))
  (set! target-r (hypot target-x target-y))
  (when (< target-r-max target-r) (set! target-r-max target-r))
  (when (> target-r-min target-r) (set! target-r-min target-r))
  (set! target-theta (atan target-y target-x))
  )

(define last-within? #f)
(define within-count 0)

(define *show-colon* #t)

(define (my-keyboard key x y)
  (when (= key 27) (exit 0))

  ;; step
  (let ([ch (integer->char key)]
		[skip #f]
		[last-cs-x #f]
		[last-cs-y #f]
		[last-target-x #f]
		[last-target-y #f]
		[last-target-r #f]
		[last-target-theta #f]
		[last-dist #f])
	(if cs-d-theta
		(let* ([r 100]
			   [dx (* r (cos cs-d-theta))]
			   [dy (* r (sin cs-d-theta))])
		  ;; (format #t "// r=~a th=~a dx=~a dy=~a\n" r (* 180/pi cs-d-theta) dx dy)
		  (case ch
			[(#\s)
			 (format #t "target-r: ~a .. ~a\n" target-r-min target-r-max)]
			
			[(#\Space)
			 (call/cc
			  (lambda (break)
				(let ((delta-vx 0) (delta-vy 0))
				  (dotimes (j 64)
					(dotimes (i 16)
					  ;;(unless skip-local
					  (vm-step vm 0 0)
					  ;(vm-step vm delta-vx delta-vy)
					  ;; (set! skip-local #f))
					  (let* ([score (vm-oport vm 0)]
							 [fuel (vm-oport vm 1)]
							 [cs-x (- (vm-oport vm 2))] [cs-y (- (vm-oport vm 3))]
							 [cs-r (hypot cs-x cs-y)]
							 [target-x (+ cs-x (vm-oport vm 4))] [target-y (+ cs-y (vm-oport vm 5))]
							 [target-r (hypot target-x target-y)]
							 [target-theta (atan target-y target-x)]
							 ;;[target-r (hypot target-x target-y)]
							 [dist (hypot (- target-x cs-x) (- target-y cs-y))]
							 [r-diff (- target-r cs-r)]
							 [v #f])
						(when (< 0.0 score)
						  (format #t "  !! SCORE = ~a\n" score)
						  (vm-end-tracing vm)
						  (break))
						(when (<= fuel 0.0)
						  (format #t "  !! FUEL = ~a\n" fuel)
						  (break))
						(when last-cs-x
						  (set! v (hypot (- cs-x last-cs-x) (- cs-y last-cs-y))))
						(if last-within?
							(inc! within-count)
							(set! within-count 1))
						(when (< (abs dist) 1000)
						  (format #t "!! TARGET WITHIN 1000m (dist=~a) ~a\n" dist within-count)
						  (set! last-within? #t))
						(let* ([cs-th (atan cs-y cs-x)]
							   [th (+ cs-th pi/2)])
						  (when last-cs-x
							(let* ([vx (- cs-x last-cs-x)]
								   [vy (- cs-y last-cs-y)]
								   [v-th (atan vy vx)]
								   [th-diff (- v-th cs-th)]
								   [ddist (- dist last-dist)]
								 
								   [tvx (- target-x last-target-x)]
								   [tvy (- target-y last-target-y)]
								   [tvth (- target-theta last-target-theta)]
								   [_dummy (when (> tvth pi) (set! tvth (- tvth pi pi)))]

								   [vh (hypot vx vy)]
								   [mag (round->exact 
										 (* (/ (hypot (- target-x cs-x) (- target-y cs-y))
											   vh)
											2.2))]
								   [tux (* target-r (cos (+ target-theta (* tvth mag))))]
								   [tuy (* target-r (sin (+ target-theta (* tvth mag))))]
								   [dirx (- tux cs-x)]
								   [diry (- tuy cs-y)]
								   
								   [g (/ mu cs-r cs-r)]
								   [anti-gx (* g (cos cs-th))]
								   [anti-gy (* g (sin cs-th))])
							  (when *show-colon*
								(format #t ": vc=(~a, ~a):~a\n: vt=(~a, ~a):~a\n: dist=(~a, ~a):~a\n"
										vx vy (hypot vx vy)
										tvx tvy (hypot tvx tvy)
										(- target-x cs-x) (- target-y cs-y) dist) )
							  (if (< dist 3000)
								  (let ([tux (* target-r (cos (+ target-theta tvth)))]
										[tuy (* target-r (sin (+ target-theta tvth)))]
										[dirx (- tux cs-x)]
										[diry (- tuy cs-y)]
										;; [dirx 0]
										;; [diry 0]
										[dvx (+ (- tvx vx) (/ dirx 7))]
										[dvy (+ (- tvy vy) (/ dirx 7))])
									;;(set! dvx (+ dvx anti-gx))
									;;(set! dvy (+ dvy anti-gy))
										;(when *show-colon*
									(if (> dist 300)
										(begin
										  (format #t ": dist = ~a\n: fire=(~a, ~d):~a\n"
												  dist dvx dvy (hypot dvx dvy)) ;)
										  ;(set! delta-vx dvx)
										  ;(set! delta-vy dvy)
										  (vm-step vm dvx dvy)
										  (dotimes (i 2) (vm-step vm 0 0))
										;(dotimes (i 1) (vm-step vm 0 0))
										  )
										(begin
										  (format #t ": dist = ~a\n: no fire\n" dist)
										;(vm-step vm dvx dvy)
										  (dotimes (i 2) (vm-step vm 0 0))
										  ;(set! delta-vx 0)
										  ;(set! delta-vy 0)
										  )))
								  (let* ([dvx (- (/ dirx mag) vx)]
										 [dvy (- (/ diry mag) vy)]
										 [dh (hypot dvx dvy)])
									(set! dvx (+ dvx anti-gx))
									(set! dvy (+ dvy anti-gy))
									(when *show-colon*
									  (format #t ": mag=~a, vh=~a\n: fire=(~a, ~d):~a\n"
											  mag vh
											  dvx dvy (hypot dvx dvy) ))
									(vm-step vm
											 (+ (/ dvx 2.2) anti-gx)
											 (+ (/ dvy 2.2) anti-gy));3) (/ dvy 3))
									(dotimes (i 2) (vm-step vm 0 0))
										; (set! skip-local #t)
									;(set! delta-vx (/ dvx 3))
									;(set! delta-vy (/ dvy 3))
									))
							  ;;(set! delta-v2 #f)
							  (break)
							  ))
						  ;;(set! last-within? #f))
						  (set! last-cs-x cs-x)
						  (set! last-cs-y cs-y)
						  (set! last-target-x target-x)
						  (set! last-target-y target-y)
						  (set! last-target-r target-r)
						  (set! last-target-theta target-theta)
						  (set! last-dist dist)
						  ))
					  (moving)
					  (glut-post-redisplay))
					(set! skip #t)
					))))]
			[else (vm-step vm 0 0)] ))
		(vm-step vm 0 0))
		  
	(unless skip
	  (moving)
	  (glut-post-redisplay)
	  )
		;;	(format #t "S:(~a, ~a) = (r:~a θ:~a)\n" cs-x cs-y cs-r (* 180/pi cs-theta))
	))

(define (my-display)
  (when (and cs-r target-r)
	(let* ([rmax (* earth-r *mag*)];(max earth-r cs-r target-r)]
		   [rate (/ 0.9 rmax)] ;; x * rate < 0.9
		   )
	  (gl-clear GL_COLOR_BUFFER_BIT)
	  ;; earth
	  (gl-color 0.1 0.3 1.0)
	  (gl-begin GL_POLYGON)
	  (dotimes (i 100)
		(let ([r (* earth-r rate)]
			  [theta (* 2/100 pi i)])
		  (gl-vertex (* r (cos theta))
					 (* r (sin theta)))
		  ))
	  (gl-end)
	  ;; 衛星の軌跡
	  (gl-color 0.7 0.7 0.7)
	  (gl-begin GL_LINE_STRIP)
	  (for-each (lambda (cs)
				  (let* ([cx (car cs)]
						 [cy (cdr cs)]
						 [r (* (hypot cx cy) rate)]
						 [th (atan cy cx)]
						 [x (* r (cos th))]
						 [y (* r (sin th))]
						 )
					(gl-vertex x y)))
				cs-trace)
	  (gl-end)
	  ;; 衛星を×で描画
	  (gl-color 1.0 1.0 1.0)
	  (gl-begin GL_LINES)
	  (let* ([r (* cs-r rate)]
			 [hw 0.02]
			 [x (* r (cos cs-theta))]
			 [y (* r (sin cs-theta))])
		(gl-vertex (- x hw) (- y hw))
		(gl-vertex (+ x hw) (+ y hw))
		(gl-vertex (- x hw) (+ y hw))
		(gl-vertex (+ x hw) (- y hw))
		)
	  (gl-end)
#|
	  ;; 目標軌道を赤っぽく
	  (gl-color 1.0 0.6 0.6)
	  (when target-r
		(gl-begin GL_LINE_LOOP)
		(dotimes (i 100)
		  (let ([r (* target-r rate)]
				[theta (* 2/100 pi i)])
			(gl-vertex (* r (cos theta))
					   (* r (sin theta)))
			)))
	  (gl-end)
|#
	  ;; 目標衛星の軌跡
	  (gl-color 1.0 0.6 0.6)
	  (gl-begin GL_LINE_STRIP)
	  (for-each (lambda (cs)
				  (let* ([cx (car cs)]
						 [cy (cdr cs)]
						 [r (* (hypot cx cy) rate)]
						 [th (atan cy cx)]
						 [x (* r (cos th))]
						 [y (* r (sin th))]
						 )
					(gl-vertex x y)))
				target-trace)
	  (gl-end)
	  ;; 目標衛星を○で描画
	  (gl-color 1.0 1.0 1.0)
	  (gl-begin GL_POLYGON)
	  (let* ([r (* target-r rate)]
			 [hw 0.02]
			 [x (* r (cos target-theta))]
			 [y (* r (sin target-theta))])
		(dotimes (i 100)
		  (let ([c-r (max (* 1000 rate) 0.01)]
				[c-theta (* 2/100 pi i)])
			(gl-vertex (+ x (* c-r (cos c-theta)))
					   (+ y (* c-r (sin c-theta))))
			)))
	  (gl-end)

	  (gl-flush)
	  )))
  
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

