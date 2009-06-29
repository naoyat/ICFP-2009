;;
;; vm3s
;;

;(use gauche.uvector)
;(use binary.io)
(use srfi-1)

(require "./vm")
(require "./gl")

(define (cycle-period semi-major-axis)
  (* 2 pi (sqrt (/ (expt semi-major-axis 3) mu))))

(define (tween a b ratio)
  (+ (* a (- 1 ratio)) (* b ratio)))

(define (frem a b)  ;; (remainder a b)
  (if (< a 0)
	  (frem (+ a b) b)
	  (let1 quot (floor->exact (/ a b))
		(- a (* quot b)))))

(define (earth-gravity x y)
  (let* ([r (hypot x y)]
		 [th (atan y x)]
		 [g (/ mu r r)])
	(cons (* g (cos th))
		  (* g (sin th)) )))

;;
;; output stat
;;
										;(define *scenario-id* 3001) ;; homing mode
;(define *mag* 1.2)
;(define *scenario-id* 3002) ;; homing mode
;(define *mag* 0.2)
;(define *scenario-id* 3003) ;; homing mode
;(define *mag* 0.03)
;(define *scenario-id* 3004) ;; homing mode
;(define *scenario-id* 3004) ;; homing mode
;(define *scenario-id* 3001) ;; homing mode

;(define *mag* 0.5)

(define cs-perigee-x #f) (define cs-perigee-y #f) (define cs-perigee-r #f)
(define cs-apogee-x #f) (define cs-apogee-y #f) (define cs-apogee-r #f)
(define target-perigee-x #f) (define target-perigee-y #f) (define target-perigee-r 5000000000)
(define target-apogee-x #f) (define target-apogee-y #f) (define target-apogee-r 0)
(define target-perigee-decided #f)
(define target-apogee-decided #f)
(define target-c0x 0) (define target-c0y 0)
(define target-c1x #f) (define target-c1y #f)
(define target-semi-major-axis #f)
(define target-cycle-period #f)
(define cs-semi-major-axis #f)
(define cs-cycle-period #f)

(define middle-orbit-r #f)

(define vm (make-vm))
(vm-load vm "bin3.obf")
(vm-begin-tracing vm *scenario-id*)
(vm-set-configuration vm *scenario-id*)

;(set! *display-scale* (* *display-scale* *mag*))
(define g:cs-trace '())
(define g:target-trace '())


(define target-r #f)
;(define target-r* 8357000)
(define vcir1 #f) 
(define vcir2 #f)
(define vcir3 #f)
(define delta-v1 #f) (define fire1-at #f)
(define delta-v2 #f) (define fire2-at #f)
(define delta-v3 #f) (define fire3-at #f)
(define delta-v4 #f) (define fire4-at #f)
(define rev? #f)

;; t=0
;(let1 st0 (vm-stat vm)
;  (pp-stat st0))
(define stat #f)
(define last-stat #f)

(define (fire dvx dvy)
  (set! last-stat stat)
  (vm-step vm dvx dvy)
  (set! stat (vm-stat vm))
  (unless (= 0 (hypot dvx dvy))
	(format #t "~d)\n" (vm-time vm))
	(format #t "!! FIRE. Δ V=(~a ~a):~a fuel:~a\n" dvx dvy (hypot dvx dvy) (stat-fuel stat))
	)
  (push! g:cs-trace (cons (stat-cs-x stat) (stat-cs-y stat)))
  (push! g:target-trace (cons (stat-ts-x stat) (stat-ts-y stat)))
  )

(define (analyze)
  (define (gravity x y)
	(let* ([earth-r (hypot x y)]
;		   [earth-th (atan y x)]
		   [earth-g (/ mu earth-r earth-r)])
;	  (cons (* -1 earth-g (cos earth-th))
;			(* -1 earth-g (sin earth-th)) )))
	  (cons (* -1 earth-g (/ x earth-r)); (cos earth-th))
			(* -1 earth-g (/ y earth-r)); (sin earth-th)) )))
			)))

  (define (calc-v1 x0 y0 x1 y1)
	(let* ([dx (- x1 x0)]
		   [dy (- y1 y0)]
		   [g0 (gravity x0 y0)]
		   [g1 (gravity x1 y1)]
		   [v0x (- dx (/ (car g0) 2))]
		   [v0y (- dy (/ (cdr g0) 2))]
		   [v1x (+ v0x (/ (+ (car g0) (car g1)) 2))]
		   [v1y (+ v0y (/ (+ (cdr g0) (cdr g1)) 2))])
	  (cons v1x v1y)))

  (let ([cs-drive (make-vector 360 #f)] [cs-drive-cnt 0] [cs-rot-dir #f] [cs-fini #f]
		[ts-drive (make-vector 360 #f)] [ts-drive-cnt 0] [ts-rot-dir #f] [ts-fini #f]

		[c0x (stat-cs-x last-stat)] [c1x (stat-cs-x stat)]
		[c0y (stat-cs-y last-stat)] [c1y (stat-cs-y stat)]
		[t0x (stat-ts-x last-stat)] [t1x (stat-ts-x stat)]
		[t0y (stat-ts-y last-stat)] [t1y (stat-ts-y stat)])
	(let* ([c0th (atan c0y c0x)] [c0r (hypot c0x c0y)]
		   [t0th (atan t0y t0x)] [t0r (hypot t0x t0y)]
		   [c1th (atan c1y c1x)] [c1r (hypot c1x c1y)]
		   [t1th (atan t1y t1x)] [t1r (hypot t1x t1y)]

		   [cs-min-r-x c0x] [cs-max-r-x c0x]
		   [cs-min-r-y c0y] [cs-max-r-y c0y]
		   [cs-min-r c0r] [cs-max-r c0r]
		   [cs-min-r-t 1] [cs-max-r-t 1]

		   [ts-min-r-x t0x] [ts-max-r-x t0x]
		   [ts-min-r-y t0y] [ts-max-r-y t0y]
		   [ts-min-r t0r] [ts-max-r t0r]
		   [ts-min-r-t 1] [ts-max-r-t 1]

		   [cs-v1 (calc-v1 c0x c0y c1x c1y)]
		   [ts-v1 (calc-v1 t0x t0y t1x t1y)])

	  (set! cs-rot-dir (if (< c0th c1th) 1 -1))
	  (set! ts-rot-dir (if (< t0th t1th) 1 -1))
	  (unless (= cs-rot-dir ts-rot-dir)
		(format #t "CSとTSの回転方向が逆ですね...\n"))

	  (let ([cs-deg (floor->exact (rad->deg360 c0th))]
			[ts-deg (floor->exact (rad->deg360 t0th))])
		(vector-set! cs-drive cs-deg #t) (inc! cs-drive-cnt)
		(vector-set! ts-drive ts-deg #t) (inc! ts-drive-cnt) )

	  (when (< c1r cs-min-r)
		(set! cs-min-r c1r)
		(set! cs-min-r-x c1x)
		(set! cs-min-r-y c1y)
		(set! cs-min-r-t 2))
	  (when (> c1r cs-max-r)
		(set! cs-max-r c1r)
		(set! cs-max-r-x c1x)
		(set! cs-max-r-y c1y)
		(set! cs-max-r-t 2))
	  (when (< t1r ts-min-r)
		(set! ts-min-r t1r)
		(set! ts-min-r-x t1x)
		(set! ts-min-r-y t1y)
		(set! ts-min-r-t 2))
	  (when (> t1r ts-max-r)
		(set! ts-max-r t1r)
		(set! ts-max-r-x t1x)
		(set! ts-max-r-y t1y)
		(set! ts-max-r-t 2))

	  (let ([cs-deg (floor->exact (rad->deg360 c1th))]
			[ts-deg (floor->exact (rad->deg360 t1th))])
		(unless (vector-ref cs-drive cs-deg) (vector-set! cs-drive cs-deg #t) (inc! cs-drive-cnt))
		(unless (vector-ref ts-drive ts-deg) (vector-set! ts-drive ts-deg #t) (inc! ts-drive-cnt)) )
	  
	  (let loop ((t 3)
				 (cx c1x) (cy c1y) (cvx (car cs-v1)) (cvy (cdr cs-v1))
				 (tx t1x) (ty t1y) (tvx (car ts-v1)) (tvy (cdr ts-v1)) )
;		(format #t "~d) (~a,~a)(~a,~a), (~a,~a)(~a,~a)\n" t cx cy cvx cvy tx ty tvx tvy)
		(when (zero? (remainder t 100000)) (format #t "[~d] " t) (flush))
		(let ([gc1 (gravity cx cy)]
			  [gt1 (gravity tx ty)])
		  (let ([c2x (+ cx cvx (/ (car gc1) 2))]
				[c2y (+ cy cvy (/ (cdr gc1) 2))]
				[t2x (+ tx tvx (/ (car gt1) 2))]
				[t2y (+ ty tvy (/ (cdr gt1) 2))])
			(let ([c2r (hypot c2x c2y)]
				  [c2th (atan c2y c2x)]
				  [t2r (hypot t2x t2y)]
				  [t2th (atan t2y t2x)]
				  [gc2 (gravity c2x c2y)]
				  [gt2 (gravity t2x t2y)])
			  (let ([cs-deg (floor->exact (rad->deg360 c2th))]
					[ts-deg (floor->exact (rad->deg360 t2th))])
				(unless (vector-ref cs-drive cs-deg) (vector-set! cs-drive cs-deg #t) (inc! cs-drive-cnt))
				(unless (vector-ref ts-drive ts-deg) (vector-set! ts-drive ts-deg #t) (inc! ts-drive-cnt)) )

			  (when (< c2r cs-min-r)
				(set! cs-min-r c2r)
				(set! cs-min-r-x c2x)
				(set! cs-min-r-y c2y)
				(set! cs-min-r-t t))
			  (when (> c2r cs-max-r)
				(set! cs-max-r c2r)
				(set! cs-max-r-x c2x)
				(set! cs-max-r-y c2y)
				(set! cs-max-r-t t))
			  (when (< t2r ts-min-r)
				(set! ts-min-r t2r)
				(set! ts-min-r-x t2x)
				(set! ts-min-r-y t2y)
				(set! ts-min-r-t t))
			  (when (> t2r ts-max-r)
				(set! ts-max-r t2r)
				(set! ts-max-r-x t2x)
				(set! ts-max-r-y t2y)
				(set! ts-max-r-t t))

			  (unless cs-fini
				(let ([zero (* c0th cs-rot-dir)]
					  [now (* c2th cs-rot-dir)])
				  (when (and (= cs-drive-cnt 360)
							 (<= zero now (+ zero 1/pi)))
					(format #t "~d: CSが~d周回ったぽい\n" t cs-rot-dir)
					(set! cs-drive-cnt 361)
					(set! cs-fini #t))))
			  (unless ts-fini
				(let ([zero (* t0th ts-rot-dir)]
					  [now (* t2th ts-rot-dir)])
				  (when (and (= ts-drive-cnt 360)
							 (<= zero now (+ zero 1/pi)))
					(format #t "~d: TSが~d周回ったぽい\n" t ts-rot-dir)
					(set! ts-drive-cnt 361)
					(set! ts-fini #t))))
			  (if (and cs-fini ts-fini)
				  (begin
					;;;
					(let* ([cs-cycle
							(let1 a (/ (+ cs-min-r cs-max-r) 2)
							  (* 2 pi (sqrt (/ (expt a 3) mu))))]
						   [ts-cycle
							(let1 a (/ (+ ts-min-r ts-max-r) 2)
							  (* 2 pi (sqrt (/ (expt a 3) mu))))]
						   [t 2] ;;(vm-time vm)]
						   [next-cs-perigee
							(let1 time-after-perigee (frem (- t cs-min-r-t) cs-cycle)
							  (if (= time-after-perigee 0) t (+ t (- cs-cycle time-after-perigee))))]
						   [next-ts-perigee 
							(let1 time-after-perigee (frem (- t ts-min-r-t) ts-cycle)
							  (if (= time-after-perigee 0) t (+ t (- ts-cycle time-after-perigee))))]
						   )
					  
					  (format #t "t=~d\n" t)
					  (format #t "cs: perigee at t=~a,r=~a,theta=~a (x:~a, y:~a),\n"
							  cs-min-r-t (hypot cs-min-r-x cs-min-r-y) (* 180/pi (atan cs-min-r-y cs-min-r-x))
							  cs-min-r-x cs-min-r-y)
					  (format #t "     apogee at t=~a,r=~a,theta=~a (x:~a, y:~a),\n"
							  cs-max-r-t (hypot cs-max-r-x cs-max-r-y) (* 180/pi (atan cs-max-r-y cs-max-r-x))
							  cs-max-r-x cs-max-r-y)
					  (format #t "      cycle = ~a, next perigee at ~a\n" cs-cycle next-cs-perigee)
					  
					  (format #t "ts: perigee at t=~a,r=~a,theta=~a (x:~a, y:~a),\n"
							  ts-min-r-t (hypot ts-min-r-x ts-min-r-y) (* 180/pi (atan ts-min-r-y ts-min-r-x))
							  ts-min-r-x ts-min-r-y)
					  (format #t "     apogee at t=~a,r=~a,theta=~a (x:~a, y:~a),\n"
							  ts-max-r-t (hypot ts-max-r-x ts-max-r-y) (* 180/pi (atan ts-max-r-y ts-max-r-x))
							  ts-max-r-x ts-max-r-y)
					  (format #t "      cycle = ~a, next perigee at ~a\n" ts-cycle next-ts-perigee)
					  (newline)
					  
					  (let1 phi-diff (- (atan ts-min-r-y ts-min-r-x)
										(atan cs-min-r-y cs-min-r-x))
						(when (= ts-rot-dir -1) (set! phi-diff (- phi-diff))) ;; 3002
						(when (< phi-diff (- (/ 1/pi 5))) (set! phi-diff (+ phi-diff (* 2 pi))))
						(when (< phi-diff 1e-3) (set! phi-diff 0))
						
						(receive (rm addon)
							(find-middle-r cs-min-r ts-min-r phi-diff (- next-ts-perigee next-cs-perigee) ts-cycle)
						  (set! middle-orbit-r rm)
						  (format #t "middle-orbit-r = ~a, addon = ~d\n" middle-orbit-r addon)
						  
						  (let* ([middle-cycle (cycle-period middle-orbit-r)]
								 [goal-at (+ next-ts-perigee (* ts-cycle addon))])
							
							(format #t "TH1 = ~a, TH2 = ~a, middle-cycle = ~a\n"
									(/ (cycle-period (/ (+ cs-min-r middle-orbit-r) 2)) 2)
									(/ (cycle-period (/ (+ ts-min-r middle-orbit-r) 2)) 2)
									middle-cycle)
							
							(set! fire1-at next-cs-perigee)
							(set! fire2-at (+ next-cs-perigee (/ (cycle-period (/ (+ cs-min-r middle-orbit-r) 2)) 2)) )
										;			(set! fire3-at (round->exact (- goal-at (/ (cycle-period (/ (+ ts-min-r middle-orbit-r) 2)) 2)) ))
										;			(set! fire3-at (+ fire2-at (* (/ phi-diff (* 2 pi)) middle-cycle)))
							(set! fire3-at (- goal-at (/ (cycle-period (/ (+ ts-min-r middle-orbit-r) 2)) 2)) )
							(set! fire4-at goal-at)
							
							(set! fire1-at (round->exact fire1-at))
							(set! fire2-at (round->exact fire2-at))
							(set! fire3-at (round->exact fire3-at))
							(set! fire4-at (round->exact fire4-at))
							)))
					  
					  (set! vcir1 (sqrt (/ mu cs-min-r)))
					  (set! vcir2 (sqrt (/ mu middle-orbit-r)))
					  (set! vcir3 (sqrt (/ mu ts-min-r)))
					  
					  (let* ([a_cs (/ (+ cs-min-r cs-max-r) 2)]
							 [a_h1 (/ (+ cs-min-r middle-orbit-r) 2)]
							 [a_mid middle-orbit-r]
							 [a_h2 (/ (+ middle-orbit-r ts-min-r) 2)]
							 [a_ts (/ (+ ts-min-r ts-max-r) 2)])
						(set! delta-v1 (- (sqrt (* mu (- (/ 2 cs-min-r) (/ a_h1)))) ; h軌道1
										  (sqrt (* mu (- (/ 2 cs-min-r) (/ a_cs)))))) ; スタート円
						(set! delta-v2 (- (sqrt (* mu (- (/ 2 middle-orbit-r) (/ a_mid)))) ; mid円
										  (sqrt (* mu (- (/ 2 middle-orbit-r) (/ a_h1)))))) ; h軌道1
						(set! delta-v3 (- (sqrt (* mu (- (/ 2 middle-orbit-r) (/ a_h2)))) ; h軌道2
										  (sqrt (* mu (- (/ 2 middle-orbit-r) (/ a_mid)))))) ; mid円
						(set! delta-v4 (- (sqrt (* mu (- (/ 2 ts-min-r) (/ a_ts))))  ; ゴール楕円
										  (sqrt (* mu (- (/ 2 ts-min-r) (/ a_h2)))))) ; h軌道2
						)
					  
					  (set! *display-scale* (/ 0.9 (max cs-max-r ts-max-r *earth-r* middle-orbit-r)))
					  ))
				  (let ([cv2x (+ cvx (/ (+ (car gc1) (car gc2)) 2))]
						[cv2y (+ cvy (/ (+ (cdr gc1) (cdr gc2)) 2))]
						[tv2x (+ tvx (/ (+ (car gt1) (car gt2)) 2))]
						[tv2y (+ tvy (/ (+ (cdr gt1) (cdr gt2)) 2))])
					(loop (+ t 1)
						  c2x c2y cv2x cv2y
						  t2x t2y tv2x tv2y)
					)))))))))

;;@@@@@
;(print "now t=1")
;(pp-stat stat) ;; first stat
(define (rad->deg360 rad)
  (let1 deg (* 180/pi rad)
	(if (< deg 0) (+ 360 deg) deg)))

;;
;; コース選定のための情報とり
;;
(define (find-middle-r r1 r2 phi-diff duree addon-cycle)
  (define (half-cycle r1 r2) (/ (cycle-period (/ (+ r1 r2) 2)) 2))
  (define (calc rm)
	(+ (half-cycle r1 rm)
	   (* (/ phi-diff (* 2 pi)) (cycle-period rm))
	   (half-cycle rm r2)))
  (define (sub rlow rhi d)
;	(format #t "(sub ~a .. ~a d:~a\n" rlow rhi d)
	(let* ([rm (/ (+ rlow rhi) 2)]
		   [rmc (- (calc rm) d)]
		   [rdiff (- rhi rlow)]
		   )
	  (if (< rdiff 1) rm
		  (cond [(= 0 rmc) rm]
				[(< rmc 0) (sub rm rhi d)]
				[(< 0 rmc) (sub rlow rm d)]))))

  (format #t "(find-middle-r r1:~a r2:~a phi-diff:~a duree:~a addon-cycle:~a)\n"
		  r1 r2 phi-diff duree addon-cycle)
  (let ([rlow (min 6578000 r1)]
		[rhi (* r2 1000)])
	(let loop ((d duree) (addon 0))
	  (if (<= 3000000 d)
		(values #f #f)
		(let ([r1c (- (calc rlow) d)]
			  [r2c (- (calc rhi) d)])
		  (if (<= r1c 0 r2c)
			  (values (sub rlow rhi d) addon)
			  (loop (+ d addon-cycle) (+ addon 1))))))
	))
  ;(calc rlob)
	;(calc rhi)
; (format #t "find ~a in [~a..~a]...\n" duree r1 r2)
; (format #t "  r=~a: ~a\n" r1 (calc r1))
; (format #t "  r=~a: ~a\n" (/ (+ r1 r2) 2) (calc (/ (+ r1 r2) 2)))
; (format #t "  r=~a: ~a\n" r2 (calc r2))

(fire 0 0) ;; t = 0->1
(fire 0 0) ;; t = 1->2
(analyze)


; (define cs-perigee-x #f) (define cs-perigee-y #f) (define cs-perigee-r #f)
; (define cs-apogee-x #f) (define cs-apogee-y #f) (define cs-apogee-r #f)
;(set! stat (vm-stat vm))

;;@@@@@
;(pp-stat stat last-stat)



;;
;; OpenGL
;;

#;(define (moving)
  (push! g:cs-trace (cons (stat-cs-x stat) (stat-cs-y stat)))
  (push! g:target-trace (cons (stat-ts-x stat) (stat-ts-y stat)))
  )

(define last-within? #f)
(define within-count 0)

(define manoeuvre '())

(define (hit-space)
  (call/cc
   (lambda (break)
;	 (dotimes (j 32) ;(j 50)
;	   (dotimes (i 16) ;20)
	 (dotimes (j 50) ;(j 50)
	   (dotimes (i 100) ;20)
		 ;;@@@@@
		 (let ([t (vm-time vm)]
			   [fuel (stat-fuel stat)])
		   (when (<= fuel 0.0)
			 (format #t "  !! FUEL = ~a\n" fuel)
			 (break))

		   (cond [(< t fire1-at) (fire 0 0)]
				 [(= t fire1-at)
				  ;; fire1
				  (let* ([th (- (stat-cs-theta stat) pi/2)]
						 [dvx (* delta-v1 (cos th))]
						 [dvy (* delta-v1 (sin th))])
					(fire dvx dvy)
					#;(format #t "  !! FIRE <1.>. Δ V=(~a ~a):~a fuel:~a\n"
							dvx dvy (hypot dvx dvy) (stat-fuel stat))
					(break))]
				 [(< t fire2-at) (fire 0 0)]
				 [(= t fire2-at)
				  ;; fire2
				  (let* ([th (- (stat-cs-theta stat) pi/2)]
						 [power (if (= fire2-at fire3-at) (+ delta-v2 delta-v3) delta-v2)]
						 [dvx (* power (cos th))]
						 [dvy (* power (sin th))])
					(fire dvx dvy)
					#;(format #t "  !! FIRE <2.>. Δ V=(~a ~a):~a fuel:~a\n"
							dvx dvy (hypot dvx dvy) (stat-fuel stat))
					(break))]
				 [(< t fire3-at) (fire 0 0)]
				 [(= t fire3-at)
				  ;; fire3
				  (let* ([th (- (stat-cs-theta stat) pi/2)]
						 [dvx (* delta-v3 (cos th))]
						 [dvy (* delta-v3 (sin th))])
					(fire dvx dvy)
					#;(format #t "  !! FIRE <3.>. Δ V=(~a ~a):~a fuel:~a\n"
							dvx dvy (hypot dvx dvy) (stat-fuel stat))
					(break))]
				 [(< t fire4-at) (fire 0 0)]
				 [(= t fire4-at)
				  ;; fire4
				  (let* ([th (- (stat-cs-theta stat) pi/2)]
						 [dvx (* delta-v4 (cos th))]
						 [dvy (* delta-v4 (sin th))])
					(format #t "now fire4 begins.\n")
					(pp-stat stat last-stat)
					(fire dvx dvy)
					;;
					(fire 0 0)
					(pp-stat stat last-stat)
					#;(format #t "  !! FIRE <4.>. Δ V=(~a ~a):~a fuel:~a\n"
							dvx dvy (hypot dvx dvy) (stat-fuel stat))
					(break))]
				 [else ;; after fire
				  ;(fire 0 0)
				  
				  (unless (= 0.0 (stat-score stat))
					(format #t "  !! SCORE = ~a\n" (stat-score stat))
					(vm-end-tracing vm)
					(break #t))

				  ;; step1 (+-)
				  (let* ([cs-dir (stat-cs-theta stat)]
						 [cs-go-dir (- cs-dir pi/2)]
						 [cs-vx (- (stat-cs-x stat) (stat-cs-x last-stat))]
						 [cs-vy (- (stat-cs-y stat) (stat-cs-y last-stat))]
						 [cs-v (hypot cs-vx cs-vy)]
						 [cs-actual-dir (atan cs-vy cs-vx)]

						 [ts-dir (stat-ds-theta stat)]
						 [ts-go-dir (- ts-dir pi/2)]
						 [ts-vx (- (stat-ts-x stat) (stat-ts-x last-stat))]
						 [ts-vy (- (stat-ts-y stat) (stat-ts-y last-stat))]
						 [ts-v (hypot ts-vx ts-vy)]
						 [ts-actual-dir (atan ts-vy ts-vx)]

						 [ts-th0 (- ts-actual-dir ts-go-dir)]
						 [ts-g (/ mu (expt (stat-ts-r stat) 2))]
						 [ts-vx0 (* ts-v (cos ts-th0))]
						 [ts-vy0 (+ (* ts-v (sin ts-th0)) ts-g)]
						 [ts-v0 (hypot ts-vx0 ts-vy0)]

						 [dir (- ts-dir cs-go-dir)]
						 [dist (stat-dist stat)]
						 [dist-go (* dist (cos dir))]
						 [dist-up (* dist (sin dir))]
						 )
					(format #t "- my v:~a dir:~a (must be ~a)\n"
							cs-v
							(* 180/pi cs-actual-dir)
							(* 180/pi cs-go-dir))
					(format #t "- target v:~a dir:~a, dist:~a (go:~a, up:~a)\n"
							ts-v
							(* 180/pi dir) dist dist-go dist-up)
#;					(format #t " .. ts-th0=(~a - ~a)=~a, ts-v=(~a, ~a+~a)=~a\n"
							(* 180/pi ts-actual-dir)
							(* 180/pi ts-go-dir)
							(* 180/pi ts-th0)
							(* ts-v (cos ts-th0))
							(* ts-v (sin ts-th0)) ts-g
							ts-v0)
					(when (< 900 dist)
;					(when (< 500 dist)
					  (let ([dx (stat-ds-x stat)]
							[dy (stat-ds-y stat)]
							[vdiff (- ts-v cs-v)] 
							[vdiff0 (- ts-v0 cs-v)] 
							[g (/ mu (expt (stat-cs-r stat) 2))]
							)
						(format #t "firing...\n")
						(fire (+ (* (+ dist-go ) (cos cs-go-dir))
								 (* (+ dist-up ) (cos cs-dir))
								 )
							  (+ (* (+ dist-go ) (sin cs-go-dir))
								 (* (+ dist-up ) (sin cs-dir))
								 ))
						(let* ([cs-dir (stat-cs-theta stat)]
							   [cs-go-dir (- cs-dir pi/2)]
							   )
						  (fire (+ (* (- dist-go ) (cos cs-go-dir))
								   (* (- dist-up ) (cos cs-dir))
								   )
								(+ (* (- dist-go ) (sin cs-go-dir))
								   (* (- dist-up ) (sin cs-dir))
								   )))
						)
					  (fire 0 0)
						
				  ;; step2
;;				  (pp-stat stat last-stat)
					  (let* ([cs-x0 (stat-cs-x last-stat)] [cs-x1 (stat-cs-x stat)]
							 [cs-y0 (stat-cs-y last-stat)] [cs-y1 (stat-cs-y stat)]
							 [cs-g0 (earth-gravity cs-x0 cs-y0)]
							 [cs-g1 (earth-gravity cs-x1 cs-y1)]
							 [cs-vt0x (- cs-x1 cs-x0 (/ (car cs-g0) 2))]
							 [cs-vt0y (- cs-y1 cs-y0 (/ (cdr cs-g0) 2))]
							 [cs-vt1x (+ cs-vt0x (/ (car cs-g0) 2) (/ (car cs-g1) 2))]
							 [cs-vt1y (+ cs-vt0y (/ (cdr cs-g0) 2) (/ (cdr cs-g1) 2))]
							 
							 [ts-x0 (stat-ts-x last-stat)] [ts-x1 (stat-ts-x stat)]
							 [ts-y0 (stat-ts-y last-stat)] [ts-y1 (stat-ts-y stat)]
							 [ts-g0 (earth-gravity ts-x0 ts-y0)]
							 [ts-g1 (earth-gravity ts-x1 ts-y1)]
							 [ts-vt0x (- ts-x1 ts-x0 (/ (car ts-g0) 2))]
							 [ts-vt0y (- ts-y1 ts-y0 (/ (cdr ts-g0) 2))]
							 [ts-vt1x (+ ts-vt0x (/ (car ts-g0) 2) (/ (car ts-g1) 2))]
							 [ts-vt1y (+ ts-vt0y (/ (cdr ts-g0) 2) (/ (cdr ts-g1) 2))]
							 )
;						(when (< 900 (stat-dist stat))
;						(let ([dx (stat-ds-x stat)]
;							  [dy (stat-ds-y stat)]
;							  [vdiff (- ts-v cs-v)] 
;							  [vdiff0 (- ts-v0 cs-v)] 
;							  [g (/ mu (expt (stat-cs-r stat) 2))]
										;							  )
						(fire (- ts-vt1x cs-vt1x)
							  (- ts-vt1y cs-vt1y) )
						(fire 0 0)
						(break))
					  )
					)

				  (fire 0 0)
				  
				  (if (< (abs (stat-dist stat)) 1000)
					  (begin
						(format #t "!! TARGET WITHIN 1000m at ~a\n" (vm-time vm))
						;;(pp-stat stat)
						(inc! within-count))
					  (set! within-count 0))
				  
				  #t]);cond
		   ));dotimes i
	   (glut-post-redisplay)
	   );dotimes j
	 ));call/cc
  )

(define (my-display)
  (when stat; (and g:cs-x g:target-x)
	(gl-clear GL_COLOR_BUFFER_BIT)

	(let1 t (vm-time vm)
	  (gl-color 1 1 1)
	  (draw-string* -1 0.95 (format "t=~d d=~a" t (stat-dist stat)))

	  (gl-color 0.66 0.66 0)
										;	  (when (< t fire1-at) 
	  (draw-string* -1 0.88 (format "f1=~d" fire1-at)); )
										;	  (when (< t fire2-at) 
	  (draw-string* -1 0.83 (format "f2=~d" fire2-at)); )
										;	  (when (< t fire3-at) 
	  (draw-string* -1 0.78 (format "f3=~d" fire3-at)); )
										;	  (when (< t fire4-at) 
	  (draw-string* -1 0.73 (format "f4=~d" fire4-at)); )
	  )

	;; earth
	(gl-color 0.1 0.3 1.0)
	(draw-circle 0 0 *earth-r* 'fill)
	
	;; 衛星の軌跡
	(gl-color 0.7 0.7 0.7)
	(draw-trace g:cs-trace)
	
	;; 衛星を×で描画
	(gl-color 1.0 1.0 1.0)
	;(let* ([x (* g:cs-r (cos g:cs-theta))]
	;	   [y (* g:cs-r (sin g:cs-theta))])
	(draw-char (stat-cs-x stat) (stat-cs-y stat) #\C)
	  ;(draw-cross x y 0.02))

	;; 途中軌道
	(when middle-orbit-r
	  (gl-color 0.3 0.8 0.1)
	  (draw-circle 0 0 middle-orbit-r))

	;; 目標衛星の軌跡
	(gl-color 1.0 0.6 0.6)
	(draw-trace g:target-trace)

	;; 目標衛星を○で描画
	(gl-color 1.0 1.0 1.0)
	;(let* ([x (* g:target-r (cos g:target-theta))]
	;	   [y (* g:target-r (sin g:target-theta))])
	  ;(draw-circle x y 1000))
	(draw-char (stat-ts-x stat) (stat-ts-y stat) #\T);)
	
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

