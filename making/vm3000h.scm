;(use gauche.uvector)
;(use binary.io)
(use srfi-1)

(require "./vm")
(require "./gl")

(define (cycle-period semi-major-axis)
  (* 2 pi (sqrt (/ (expt semi-major-axis 3) mu))))

(define (tween a b ratio)
  (+ (* a (- 1 ratio)) (* b ratio)))

;(define *scenario-id* 3001) ;; homing mode
;(define *mag* 1.2)
;(define *scenario-id* 3002) ;; homing mode
;(define *mag* 0.2)
;(define *scenario-id* 3003) ;; homing mode
;(define *mag* 0.03)
(define *scenario-id* 3004) ;; homing mode

(define *mag* 1.0)

(define cs-perigee-x #f)
(define cs-perigee-y #f)
(define cs-perigee-r #f)
(define cs-apogee-x #f)
(define cs-apogee-y #f)
(define cs-apogee-r #f)
(define target-perigee-x #f)
(define target-perigee-y #f)
(define target-perigee-r 5000000000)
(define target-apogee-x #f)
(define target-apogee-y #f)
(define target-apogee-r 0)
(define target-perigee-decided #f)
(define target-apogee-decided #f)
(define target-c0x 0)
(define target-c0y 0)
(define target-c1x #f)
(define target-c1y #f)
(define target-semi-major-axis #f)
(define target-cycle-period #f)
(define cs-semi-major-axis #f)
(define cs-cycle-period #f)

(define middle-orbit-r #f)

(define vm (make-vm))
(vm-load vm "bin3.obf")
(vm-begin-tracing vm *scenario-id*)
(vm-set-configuration vm *scenario-id*)

(case *scenario-id*
  [(3001 3002 3003)
   ]
  [(3004)
   (set! *mag* 1.2)
   (set! cs-perigee-x 0) (set! cs-perigee-y 6457000)
   (set! cs-perigee-r (hypot cs-perigee-x cs-perigee-y))
   (set! cs-apogee-x 0) (set! cs-apogee-y -6457000)
   (set! cs-apogee-r (hypot cs-apogee-x cs-apogee-y))
   (set! target-perigee-x 0) (set! target-perigee-y 8357000)
   (set! target-perigee-r (hypot target-perigee-x target-perigee-y))
   (set! target-perigee-decided #t)
   (set! target-apogee-x 0) (set! target-apogee-y -12800000)
   (set! target-apogee-r (hypot target-apogee-x target-apogee-y))
   (set! target-apogee-decided #t)
   (set! target-c1x (+ target-apogee-x target-perigee-x))
   (set! target-c1y (+ target-apogee-y target-perigee-y))

   (set! cs-semi-major-axis (/ (+ cs-apogee-r cs-perigee-r) 2))
   (set! cs-cycle-period (cycle-period cs-semi-major-axis))
   (set! target-semi-major-axis (/ (+ target-perigee-r target-apogee-r) 2))
   (set! target-cycle-period (cycle-period target-semi-major-axis))

   (set! middle-orbit-r (tween cs-perigee-r target-perigee-r 3.832421875   ))
   (set! middle-orbit-r (tween cs-perigee-r target-perigee-r 3.8327887 ))
;   (set! middle-orbit-r (tween cs-perigee-r target-perigee-r 3.8327886 ))
   ]
  )

;; 初速と方向で軌道推測できるんじゃね？
;; あと、角速度は一定なんだよね

(set! *display-scale* (* *display-scale* *mag*))

(define target-r #f)
(define target-r* 8357000)
(define vcir1 #f) 
(define vcir2 #f)
(define vcir3 #f)
(define delta-v1 #f)
(define delta-v2 #f)
(define delta-v3 #f)
(define rev? #f)

;(dotimes (i 2000) (vm-step vm 0 0))

;; t=0

(vm-step vm 0 0) 
;; now t=1
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
  (format #t "t1出力: score=~a, fuel=~a, cs=(~a, ~a), target=(~a, ~a)\n"
		  (vm-oport vm 0) (vm-oport vm 1) cs-x1 cs-y1 target-x1 target-y1)

  (vm-step vm 0 0)
  ;; now t=2
  (let* ([cs-x2 (- (vm-oport vm 2))] [cs-y2 (- (vm-oport vm 3))]
		 [cs-r2 (hypot cs-x2 cs-y2)] [cs-th2 (atan cs-y2 cs-x2)]
		 ;;[target-r2 (vm-oport vm 4)]
		 [target-x2 (+ cs-x2 (vm-oport vm 4))] [target-y2 (+ cs-y2 (vm-oport vm 5))]
		 [target-r2 (hypot target-x2 target-y2)] [target-th2 (atan target-y2 target-x2)]
		 ;;
		 [delta-th (abs (- cs-th2 cs-th1))] ;; 1秒で進んだ角度
		 [delta (* cs-r2 delta-th)] ;; 1秒で進んだ距離
		 [dx (- cs-x2 cs-x1)] [dy (- cs-y2 cs-y1)]
		 [dxx (hypot dx dy)]
		 [dxx-th (atan dy dx)]
		 )
	(format #t "t2出力: score=~a, fuel=~a, cs=(~a, ~a), target=(~a, ~a)\n"
			(vm-oport vm 0) (vm-oport vm 1) cs-x2 cs-y2 target-x2 target-y2)
	(format #t "target ω = ~a, apogee exp = (~a, ~a), 周期 = [~a],\n"
			(* 180/pi (- target-th2 target-th1))
			target-apogee-x target-apogee-y
			target-cycle-period)

	(let ([cs-r* (- cs-r1 (- cs-r2 cs-r1))]
		  [cs-th* (- cs-th1 (- cs-th2 cs-th1))]
		  [target-r* (- target-r1 (- target-r2 target-r1))]
		  [target-th* (- target-th1 (- target-th2 target-th1))])
	  (format #t "t0推定: cs=(~a, ~a), target=(~a, ~a)\n"
			  (* cs-r* (cos cs-th*)) (* cs-r* (sin cs-th*))
			  (* target-r* (cos target-th*)) (* target-r* (sin target-th*)) ))
;	(format #t "v) dx=~a delta=~a\n" dx delta)
;	(format #t "r) now:~a..~a target:~a\n" cs-r1 cs-r2 target-r1)
;	(set! target-r target-r1)
	(set! vcir1 (sqrt (/ mu cs-r2)))
	(set! vcir2 (sqrt (/ mu middle-orbit-r)))
	(set! vcir3 (sqrt (/ mu target-perigee-r)))
;	(let ([delta-v1a (* vcir1 (- (sqrt (/ (* 2 middle-orbit-r) (+ cs-r2 middle-orbit-r))) 1))]
;		  [delta-v1b (* vcir2 (- 1 (sqrt (/ (* 2 cs-r2) (+ cs-r2 middle-orbit-r)))))]
;		  [delta-v2a (* vcir2 (- (sqrt (/ (* 2 target-perigee-r) (+ middle-orbit-r target-perigee-r))) 1))]
;		  [delta-v2b (* vcir3 (- 1 (sqrt (/ (* 2 middle-orbit-r) (+ middle-orbit-r target-perigee-r)))))])
;	  (set! delta-v1 delta-v1a)
;	  (set! delta-v2 (+ delta-v1a delta-v1b))
;	  (set! delta-v3 delta-v2b) )
	(let* ([r1 cs-r2]
		   [rm middle-orbit-r]
		   [r2p target-perigee-r]
		   [r2a target-apogee-r]
		   [a1 r1] [a2 r2p]
		   [a_h1 (/ (+ r1 rm) 2)]
		   [a_h2p (/ (+ rm r2p) 2)]
		   [a_h2a (/ (+ r2p r2a) 2)])

	  (set! delta-v1 (- (sqrt (* mu (- (/ 2 r1) (/ a_h1)))) ; h軌道1
						(sqrt (* mu (- (/ 2 r1) (/ a1)))))) ; スタート円
	  (set! delta-v2 (- (sqrt (* mu (- (/ 2 rm) (/ a_h2p)))) ; h軌道2
						(sqrt (* mu (- (/ 2 rm) (/ a_h1)))))) ; h軌道1
	  (set! delta-v3 (- ;;(sqrt (* mu (- (/ 2 r2) (/ a2))))
					  (sqrt (* mu (- (/ 2 r2p) (/ a_h2a))))  ; ゴール楕円
					  (sqrt (* mu (- (/ 2 r2p) (/ a_h2p)))))) ) ; h軌道2

	(format #t "Vcir) now:~a (delta=~a), middle:~a, target:~a\n" vcir1 delta vcir2 vcir3)
	(format #t "delta-v) now:~a middle:~a target:~a\n" delta-v1 delta-v2 delta-v3)

	(let* ([TH (* pi (sqrt (/ (expt (+ cs-r2 target-r*) 3) 8 mu)))]
		   [cs-omega (- cs-th2 cs-th1)]
		   [target-omega (- target-th2 target-th1)]
		   [TM (round->exact (/ (+ (- target-th2 cs-th2 pi) (* target-omega TH))
								(- cs-omega target-omega)))]
		   )
	  ;(when (< target-r2 cs-r2)
	  (when (< TM 0)
		(set! TM (round->exact (/ (+ (- target-th2 cs-th2 (* -3 pi)) (* target-omega TH))
								  (- cs-omega target-omega))))
		)
	  (format #t "TH = ~a, TM = ~a\n" TH TM)
	  (format #t "θ c = ~a, θ T = ~a\n" (* 180/pi cs-th2) (* 180/pi target-th2) )
	  (format #t "ω c = ~a, ω T = ~a\n" (* 180/pi cs-omega) (* 180/pi target-omega) )

#|
	  (dotimes (i (- (* cs-cycle-period 0) 2))
		(vm-step vm 0 0))
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
		(format #t "１周回ってみた: score=~a, fuel=~a, cs=(~a, ~a), target=(~a, ~a)\n"
				(vm-oport vm 0) (vm-oport vm 1) cs-x1 cs-y1 target-x1 target-y1)
		)
|#
	  ;;;
	  (let* ([th (- cs-th2 pi/2)]
			 [dvx (* delta-v1 (cos th))]
			 [dvy (* delta-v1 (sin th))])
		(vm-step vm dvx dvy)
		(format #t "  !! FIRE <1.>. Δ V=(~a ~a):~a fuel:~a\n"
				dvx dvy (hypot dvx dvy) (vm-oport vm 1))
		)


#|
	  (let1 TM (round->exact (/ pi delta-th))
		(dotimes (i (- TM 1)) (vm-step vm 0 0))
		)
	  ;; t = TM
	  (set! cs-x1 (- (vm-oport vm 2)))
	  (set! cs-y1 (- (vm-oport vm 3)))
	  (set! cs-r1 (hypot cs-x1 cs-y1))
	  (set! cs-th1 (atan cs-y1 cs-x1))
	  (set! target-x1 (+ cs-x1 (vm-oport vm 4)))
	  (set! target-y1 (+ cs-y1 (vm-oport vm 5)))
	  (set! target-r1 (hypot target-x1 target-y1))
	  (set! target-th1 (atan target-y1 target-x1))
	  (vm-step vm 0 0)
	  ;; t = TM+1
	  (set! cs-x2 (- (vm-oport vm 2)))
	  (set! cs-y2 (- (vm-oport vm 3)))
	  (set! cs-r2 (hypot cs-x2 cs-y2))
	  (set! cs-th2 (atan cs-y2 cs-x2))
		 ;;[target-r2 (vm-oport vm 4)]
	  (set! target-x2 (+ cs-x2 (vm-oport vm 4)))
	  (set! target-y2 (+ cs-y2 (vm-oport vm 5)))
	  (set! target-r2 (hypot target-x2 target-y2))
	  (set! target-th2 (atan target-y2 target-x2))
	  ;;
	  (set! delta-th (abs (- cs-th2 cs-th1))) ;; 1秒で進んだ角度
	  (set! delta (* cs-r2 delta-th)) ;; 1秒で進んだ距離
	  (set! dx (- cs-x2 cs-x1))
	  (set! dy (- cs-y2 cs-y1))
	  (set! dxx (hypot dx dy))
	  (set! dxx-th (atan dy dx))

	;; delta-v1
	  (let* ([th (+ cs-th2 pi/2)]
			 [dth (- dxx-th th)])
		(when (< dth (- pi)) (set! dth (+ dth pi pi)))
		(when (< pi dth) (set! dth (- dth pi pi)))
;	  (unless (<= pi/2 dth pi/2) (set! rev? #t) (set! th (- th pi)))
		(let ([dvx (* delta-v1 (cos th))]
			  [dvy (* delta-v1 (sin th))])
		  #;(format #t "th=~a: (~a, ~a):~a + (~a, ~a) => (~a, ~a):~a\n"
				  th
				  dx dy (hypot dx dy)
				  dvx dvy
				  (+ dx dvx) (+ dy dvy)
				  (hypot (+ dx dvx) (+ dy dvy)))
		  #;(format #t "th=~a: (~a, ~a):~a + (~a, ~a) => (~a, ~a):~a\n"
				  (+ th pi)
				  dx dy (hypot dx dy)
				  (- dvx) (- dvy)
				  (- dx dvx) (- dy dvy)
				  (hypot (- dx dvx) (- dy dvy)))

;		(format #t "  : (~a, ~a)\n" dvx dvy)
;		(set! dvx (+ dvx (* vcir1 (cos th))))
;		(set! dvy (+ dvy (* vcir1 (sin th))))
;		(format #t " => (~a, ~a)\n" dvx dvy)
;		(set! dvx (- dvx (* dxx (cos dxx-th))))
;		(set! dvy (- dvy (* dxx (sin dxx-th))))
;		(format #t " => (~a, ~a)\n" dvx dvy)
		  
		  (if (< cs-r2 target-r*)
			  (when (< (hypot (+ dx dvx) (+ dy dvy)) (hypot dx dy))
				(set! dvx (- dvx))
				(set! dvy (- dvy)))
			  (when (> (hypot (+ dx dvx) (+ dy dvy)) (hypot dx dy))
				(set! dvx (- dvx))
				(set! dvy (- dvy))) )
		  
;	  (let ([dvx (- (* vcir1 (cos th)) (* dx (cos dxx-th)))]
;			[dvy (- (* vcir1 (sin th)) (* dx (sin dxx-th)))])
;	  (let ([dvx (- (* dx (cos dxx-th)) (* vcir1 (cos th)))]
;			[dvy (- (* dx (sin dxx-th)) (* vcir1 (sin th)))])

;		  (format #t "  !! FIRE. Δ V = (~a ~a) ~a\n" dvx dvy rev?)
;		  (vm-step vm dvx dvy)
		  ))
|#
	  )))

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

;(define target-perigee-x #f)
;(define target-perigee-y #f)
;(define target-perigee-r (* earth-r 50000))
;(define target-perigee-decided #f)
;(define target-apogee-x #f)
;(define target-apogee-y #f)
;(define target-apogee-r 0)
;(define target-apogee-decided #f)

(define (moving)
  (let ((last-x cs-x)
		(last-y cs-y))
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
  (if (< target-apogee-r target-r)
	  (begin
		(set! target-apogee-r target-r)
		(set! target-apogee-x target-x)
		(set! target-apogee-y target-y))
	  (when (and (not target-apogee-decided) (> target-apogee-r target-r))
		(set! target-apogee-decided #t)
		(format #t "apogee (~a, ~a):~a decided at time:~d\n"
				target-apogee-x target-apogee-y target-apogee-r
				(vm-time vm) )
		))

  (if (> target-perigee-r target-r)
	  (begin
		(set! target-perigee-r target-r)
		(set! target-perigee-x target-x)
		(set! target-perigee-y target-y))
	  (when (and (not target-perigee-decided) (< target-perigee-r target-r))
		(set! target-perigee-decided #t)
		(format #t "perigee (~a, ~a):~a decided at time:~d\n"
				target-perigee-x target-perigee-y target-perigee-r
				(vm-time vm) )
		))
  (set! target-theta (atan target-y target-x))
  )

(define last-within? #f)
(define within-count 0)

(define last-cs-x #f)
(define last-cs-y #f)
(define last-target-x #f)
(define last-target-y #f)
(define last-target-theta #f)

(define (hit-space)
  (call/cc
   (lambda (break)
	 (dotimes (j 50)
	   (dotimes (i 20)
		 (vm-step vm 0 0)
		 (let* ([score (vm-oport vm 0)]
				[fuel (vm-oport vm 1)]
				[cs-x (- (vm-oport vm 2))]
				[cs-y (- (vm-oport vm 3))]
				[cs-r (hypot cs-x cs-y)]
				[target-x (+ cs-x (vm-oport vm 4))]
				[target-y (+ cs-y (vm-oport vm 5))]
				;;[target-r (hypot target-x target-y)]
				[dist (hypot (- target-x cs-x) (- target-y cs-y))]
				[middle-r-diff (- middle-orbit-r cs-r)]
				[target-r-diff (- target-perigee-r cs-r)]
				[v #f]
				)


;		   (unless (<= cs-apogee-y cs-y cs-perigee-y)
;			 (format #t "cs-y out of range? ... ~a outside of [~a .. ~a]\n"
;					 cs-y cs-apogee-y cs-perigee-y) )

		   (when (< 0.0 score)
			 (format #t "  !! SCORE = ~a\n" score)
			 (vm-end-tracing vm)
			 (break #t))
		   (when (<= fuel 0.0)
			 (format #t "  !! FUEL = ~a\n" fuel)
			 (break #t))
		   (when last-cs-x
			 (set! v (hypot (- cs-x last-cs-x) (- cs-y last-cs-y)))
			 #;(format #t "current v=~a\n" v)
			 )
		   ;;(format #t "/ ~a ~a\n" cs-r target-r)
		   (if (< (abs dist) 1000)
			   (begin
				 (format #t "!! TARGET WITHIN 1000m (dist=~a (~a, ~a)), #~d, score=~a, fuel=~a at ~a\n"
						 dist (vm-oport vm 4) (vm-oport vm 5)
						 within-count
						 score fuel
						 (vm-time vm))
				 (inc! within-count))
			   (set! within-count 0))

		   (let* ([cs-th (atan cs-y cs-x)]
				  [th (+ cs-th pi/2)])

			 (when last-cs-x
			   (let* ([vx (- cs-x last-cs-x)]
					  [vy (- cs-y last-cs-y)]
					  [v-th (atan vy vx)]
					  [th-diff (- v-th cs-th)]
					  
					  [tvx (- target-x last-target-x)]
					  [tvy (- target-y last-target-y)])
				 
				 (when (and delta-v2 (< (abs middle-r-diff) 1))
				   (let ([dvx (* delta-v2 (cos th))]
						 [dvy (* delta-v2 (sin th))])
;					 (set! dvy (+ dvy vy))
					 (vm-step vm (- dvx) (- dvy))
					 (format #t "  !! FIRE <2.>. Δ V=(~a, ~a):~a fuel:~a\n"
							 dvx dvy (hypot dvx dvy) (vm-oport vm 1))
					 (set! delta-v2 #f)
					 (break #t)))
				 
				 (when (and (not delta-v2) delta-v3 (< (abs target-r-diff) 0.9))
				   (let ([dvx (* delta-v3 (cos th))]
						 [dvy (* delta-v3 (sin th))])
					 (set! dvx (- dvx))
					 (set! dvy (- dvy))
;					 (let1 vcir3a
;						 (sqrt (* mu (- (/ 2 target-perigee-r)
;										(/ (/ (+ middle-orbit-r target-perigee-r) 2)))))
;					   (set! dvx (+ vcir3a (- dvx vx)))
;					   )
;					 (set! dvy (- dvy vy))
					 (vm-step vm dvx dvy)
					 (set! delta-v3 #f)

					 (format #t "  !! FIRE <3.>. Δ V=(~a, ~a):~a, dist=(~a, ~a):~a fuel:~a\n"
							 dvx dvy (hypot dvx dvy)
							 (vm-oport vm 4)
							 (vm-oport vm 5)
							 dist
							 (vm-oport vm 1))

					 (set! last-cs-x cs-x)
					 (set! last-cs-y cs-y)
					 (set! last-target-x target-x)
					 (set! last-target-y target-y)
					 ;(set! last-target-theta target-theta)
					 (let* ([cs-x (- (vm-oport vm 2))]
						    [cs-y (- (vm-oport vm 3))]
						    [target-x (+ cs-x (vm-oport vm 4))]
						    [target-y (+ cs-y (vm-oport vm 5))]
						    )
					   (format #t "  Sat.C. : (~a, ~a):(r=~a,theta=~a), v=(~a, ~a):~a\n"
							   cs-x cs-y (hypot cs-x cs-y) (* 180/pi (atan cs-y cs-x))
							   (- cs-x last-cs-x)
							   (- cs-y last-cs-y)
							   (hypot (- cs-x last-cs-x) (- cs-y last-cs-y))
							   )
					   (format #t "  Sat.T. : (~a, ~a):(r=~a,theta=~a), v=(~a, ~a):~a\n"
							   target-x target-y (hypot target-x target-y) (* 180/pi (atan target-y target-x))
							   (- target-x last-target-x)
							   (- target-y last-target-y)
							   (hypot (- target-x last-target-x) (- target-y last-target-y))
							   )
					   (set! last-cs-x cs-x)
					   (set! last-cs-y cs-y)
					   (set! last-target-x target-x)
					   (set! last-target-y target-y)
					   )
					 (set! last-target-theta (atan last-target-y last-target-x))

					 (break #t)))
				 
				 (unless delta-v3
				   ;; homing mode!
				   #|
					 (set! last-cs-x cs-x)
					 (set! last-cs-y cs-y)
					 (set! last-target-x target-x)
					 (set! last-target-y target-y)
					 (set! last-target-theta target-theta)
				   |#
					 (if (> (abs dist) 330)
;						 (vm-step vm (- tvx vx) (- tvy vy))
						 (vm-step vm 0 0)
;						 (vm-step vm 0 0)
						 )

					 (set! last-cs-x (- (vm-oport vm 2)))
					 (set! last-cs-y (- (vm-oport vm 3)))
					 (set! last-target-x (+ last-cs-x (vm-oport vm 4)))
					 (set! last-target-y (+ last-cs-y (vm-oport vm 5)))
					 (set! last-target-theta (atan last-target-y last-target-x))
				   #;(let* ([tvth (- target-theta last-target-theta)]
						  [tux (* target-r (cos (+ target-theta tvth)))]
						  [tuy (* target-r (sin (+ target-theta tvth)))]
						  [dirx (- tux cs-x)]
						  [diry (- tuy cs-y)]
						  [dvx (+ (- tvx vx) (/ dirx 7))]
						  [dvy (+ (- tvy vy) (/ dirx 7))])
					 ;;(vm-step vm (- tvx vx) (- tvy vy))
					 (if (> (abs dist) 330)
						 (begin
						   ;(let* ([g (/ mu cs-r cs-r)]
							;[anti-gx (* g (cos cs-th))]
							;[anti-gy (* g (sin cs-th))])
						   (format #t ": dist = ~a\n: fire=(~a, ~d):~a\n"
								   dist dvx dvy (hypot dvx dvy)) ;)
						   (vm-step vm dvx dvy)
						   (dotimes (i 2) (vm-step vm 0 0)) )
						 (begin
						   (format #t ": dist = ~a\n: no fire\n" dist)
						   (dotimes (i 2) (vm-step vm 0 0))
						   ))
					 (set! last-cs-x (- (vm-oport vm 2)))
					 (set! last-cs-y (- (vm-oport vm 3)))
					 (set! last-target-x (+ last-cs-x (vm-oport vm 4)))
					 (set! last-target-y (+ last-cs-y (vm-oport vm 5)))
					 (set! last-target-theta (atan last-target-y last-target-x))

					   #;(format #t "  !! homing fire. Δ V=(~a, ~a):~a, dist=(~a, ~a):~a fuel:~a\n"
							   (- tvx vx) (- tvy vy)
							   (hypot (- tvx vx) (- tvy vy))
							   (vm-oport vm 4)
							   (vm-oport vm 5)
							   (hypot (vm-oport vm 4) (vm-oport vm 5))
							   (vm-oport vm 1))
					   ;(break #t)
					 ))
					 ;(dotimes (i 4) (vm-step vm 0 0))
					 ;;(break #t)

				 )))

		   (set! last-cs-x cs-x)
		   (set! last-cs-y cs-y)
		   (set! last-target-x target-x)
		   (set! last-target-y target-y)
		   (set! last-target-theta target-theta)
		   ))
	   (moving)
	   (glut-post-redisplay))
	 #f)
   ))

(define (my-display)
  (when (and cs-r target-r)
	(gl-clear GL_COLOR_BUFFER_BIT)

	;; earth
	(gl-color 0.1 0.3 1.0)
	(draw-circle 0 0 earth-r 'fill)
	
	;; 衛星の軌跡
	(gl-color 0.7 0.7 0.7)
	(draw-trace cs-trace)
	
	;; 衛星を×で描画
	(gl-color 1.0 1.0 1.0)
	(let* ([x (* cs-r (cos cs-theta))]
		   [y (* cs-r (sin cs-theta))])
	  (draw-char x y #\C))
	  ;(draw-cross x y 0.02))

	;; 途中軌道
	(gl-color 0.3 0.8 0.1)
	(draw-circle 0 0 middle-orbit-r)

	;; 目標衛星の軌跡
	(gl-color 1.0 0.6 0.6)
	(draw-trace target-trace)

	;; 目標衛星を○で描画
	(gl-color 1.0 1.0 1.0)
	(let* ([x (* target-r (cos target-theta))]
		   [y (* target-r (sin target-theta))])
	  ;(draw-circle x y 1000))
	  (draw-char x y #\T))
	
	;; 目標衛星の軌道の焦点１（地球）を×で描画
	  ;; target-min, target-max
	(when (and target-perigee-decided target-apogee-decided)
	  ;; apogee    (when target-r-max-x
	  (gl-color 1.0 1.0 1.0)
	  (draw-cross target-perigee-x target-perigee-y 0.01)
	  (draw-cross target-apogee-x target-apogee-y 0.01)
	  (unless target-c1x
		(set! target-c1x (+ target-apogee-x target-perigee-x))
		(set! target-c1y (+ target-apogee-y target-perigee-y)) )
	  ;; focus #1
	  ;;(draw-cross target-c0x target-c0y 0.0166)
	  (draw-char target-c0x target-c0y #\F)
	  ;; focus #2
	  ;;(draw-cross target-c1x target-c1y 0.01)
	  (draw-char target-c1x target-c1y #\f)
	  )
	
	;;
	(gl-flush)))

