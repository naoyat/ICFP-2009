(use gauche.uvector)
(use binary.io)
(use srfi-1)
(use math.const)

(define NOP 0)
(define *team-id* 514) ;; Ikoma

(define *dump-code-when-loading* #f)
(define *verbose* #f)

(define (hypot x y) (sqrt (+ (* x x) (* y y))))

(define mu (* 6.67428e-11 6.0e24))
(define mu-moon (* 6.67428e-11 7.347e22))

(define (make-vm)
  (let ([data (make-f64vector 16384 0.0)]
		[inst (make-u32vector 16384 NOP)]
		[inst* (make-vector 16384 #f)]
		[input-port (make-f64vector 16384 0.0)]
		[output-port (make-f64vector 16384 0.0)]
		[*d-op* #(- Add Sub Mult Div Output Phi)]
		[*s-op* #(Noop Cmpz Sqrt Copy Input)]
		[*cmpz-op* #(LTZ LEZ EQZ GEZ GTZ)] ;;< <= = >= >)
		[*cmpz-ht* (make-hash-table)]
		[status #f]
		[maxptr -1]
		[do-trace? #t]
		[trace-oport #f]
		[time 0]
		)

	(define (mem addr) (f64vector-ref data addr))
	(define (set-mem! addr val) (f64vector-set! data addr val))
	(define (iport addr) (f64vector-ref input-port addr))
	(define (set-iport! addr val) (f64vector-set! input-port addr val))
	(define (oport addr) (f64vector-ref output-port addr))
	(define (set-oport! addr val) (f64vector-set! output-port addr val))

	(define (reset)
	  (dotimes (i 16384)
		(f64vector-set! data i 0.0)
		(f64vector-set! input-port i 0.0)
		(f64vector-set! output-port i 0.0)
		(u32vector-set! inst i NOP)
		(vector-set! inst* i #f) )
	  (set! status #f)
	  (set! maxptr -1)
	  (set! time 0))

	(define (dump-data)
	  (let loop ((ad 0))
		(when (<= ad maxptr)
		  (let1 dat (f64vector-ref data ad)
			(format #t "[~d:~a] " ad dat)
			(loop (+ ad 1)))))
	  (newline))

	(define (dump-inst)
	  (let loop ((ad 0))
		(when (<= ad maxptr)
		  (let1 ins (vector-ref inst* ad)
			(format #t "~d: ~a\n" ad ins)
			(loop (+ ad 1))))))

;	(define (d-op proc dest r1 r2)
;	  (set-mem! dest (proc (mem r1) (mem r2))))
	(define (current-dvx) (iport #x2))
	(define (current-dvy) (iport #x3))
	(define (current-configuration) (floor->exact (iport #x3e80)))
	(define (set-dvx! val) (set-iport! #x2 val))
	(define (set-dvy! val) (set-iport! #x3 val))
	(define (set-configuration config)
	  (set-iport! #x3e80 config)
	  (when do-trace? (trace)) )


	;; tracer
	(define last-dvx #f)
	(define last-dvy #f)
	(define last-configuration #f)

	(define (trace-begin scenario-id)
	  (set! trace-oport (open-output-file (format "~d.osf" scenario-id)))
	  (write-u32 #xcafebabe trace-oport 'little-endian)
	  (write-u32 *team-id* trace-oport 'little-endian)
	  (write-u32 scenario-id trace-oport 'little-endian)
	  ;;
	  (when *verbose*
		(format #t "TRACE-BEGIN: 0xCAFEBABE, ~d, ~d\n" *team-id* scenario-id))
	  (set! do-trace? #t))

	(define (trace)
	  (when do-trace?
		(let ([dvx (current-dvx)]
			  [dvy (current-dvy)]
			  [config (current-configuration)]
			  [diffs '()])
		  (unless (and last-configuration (= last-configuration config))
			(push! diffs (cons #x3e80 config))
			(set! last-configuration config))
		  (unless (and last-dvy (= last-dvy dvy))
			(push! diffs (cons #x3 dvy))
			(set! last-dvy dvy))
		  (unless (and last-dvx (= last-dvx dvx))
			(push! diffs (cons #x2 dvx))
			(set! last-dvx dvx))
		  (unless (null? diffs)
			(when trace-oport
			  (write-u32 time trace-oport 'little-endian)
			  (write-u32 (length diffs) trace-oport 'little-endian)
			  (for-each (lambda (diff)
						  (write-u32 (car diff) trace-oport 'little-endian)
						  (write-f64 (cdr diff) trace-oport 'little-endian))
						diffs))
			(when *verbose*
			  (format #t "TRACE: t=~d, ~a\n" time diffs))
			))))
	(define (trace-end)
	  (when do-trace?
		(when trace-oport
		  (write-u32 time trace-oport 'little-endian)
		  (write-u32 0 trace-oport 'little-endian)
		  (close-output-port trace-oport)
		  (set! trace-oport #f))
		(when *verbose*
		  (format #t "TRACE-END: t=~d\n" time))
		(set! do-trace? #f)))

	(define (// y x) (if (= 0.0 x) 0.0 (/ y x)))

	(define (step dvx dvy)
	  (set-dvx! dvx) (set-dvy! dvy)
	  (when do-trace? (trace))
	  (when *verbose*
		(format #t "\n:mem:")
		(for-each (lambda (addr) (format #t " ~d=~a" addr (mem addr)))
				  (list 241 248 249 250 251 252 253 254 255 256 257 258 259 260 261 262 263 264 265) )
		(newline))
			  
	  #;(format #t "[Config#~d Δ V=(~a, ~a)]\n" (current-configuration) (current-dvx) (current-dvy))
	  (let loop ((pc 0))
		(when (<= pc maxptr);16384)
		  (let1 ins (vector-ref inst* pc)
			(when ins
			  (when *verbose*
				(let1 h (human (u32vector-ref inst pc) pc)
				  (when h (format #t "~a\n" h))))
;;			  (format #t "[~d] ~a\n" pc ins)
			  (case (car ins)
				[(Add) (let ([r1 (second ins)] [r2 (third ins)])
						 (set-mem! pc (+ (mem r1) (mem r2))))]
				[(Sub) (let ([r1 (second ins)] [r2 (third ins)])
						 (set-mem! pc (- (mem r1) (mem r2))))]
				[(Mult) (let ([r1 (second ins)] [r2 (third ins)])
						  (set-mem! pc (* (mem r1) (mem r2))))]
				[(Div) (let ([r1 (second ins)] [r2 (third ins)])
						 (set-mem! pc (// (mem r1) (mem r2))))]
				[(Phi) (let ([r1 (second ins)] [r2 (third ins)])
						 (set-mem! pc (if status (mem r1) (mem r2))))]
				[(Cmpz) (let* ([op (second ins)]
							   [op-proc (hash-table-get *cmpz-ht* (second ins))]
							   [r1 (third ins)])
						  (set! status (op-proc (mem r1) 0.0)))]
				[(Sqrt) (let1 r1 (second ins)
						  (when (>= (mem r1) 0.0)
							(set-mem! pc (sqrt (mem r1))))
						  ;; #<undef> if negative
						  )]
				[(Copy) (let1 r1 (second ins)
						  (set-mem! pc (mem r1)))]
				[(Noop)
				 (when *verbose*
				   (format #t "mem[~d] = (const) ~a\n" pc (mem pc)))
				 #f]
				[(Input) (let1 r1 (second ins)
				 ;;(format #t "// INPUT from ~x (= ~a)\n" (second ins) (iport (second ins)))
						   (set-mem! pc (iport r1)))]
				[(Output) (let ([r1 (second ins)] [r2 (third ins)])
				 ;;(format #t "// OUTPUT ~a := ~a\n" (second ins) (mem (third ins)))
							(set-oport! r1 (mem r2)))]
				[else
				 (format #t "INST ERROR: ~a\n" ins)]
				)
			  (when *verbose*
				(format #t " ... ~a=~a ...\n" pc (mem pc))
				)
			  (loop (+ pc 1)) ))))
	  (when *verbose*
		(let* ([sx (oport 2)]
			   [sy (oport 3)]
			   [r (hypot sx sy)]
			   [theta (atan sy sx)]) 
		  (format #t "[Score:~a Fuel:~a S:(~a, ~a) = (r:~a θ:~a), R:~a ... ]\n"
				  (oport 0)
				  (oport 1)
				  sx sy r theta
				  (oport 4)
				)))
	  ;; at last
	  (inc! time))

	(define (disasm inst)
	  (let1 op (bit-field inst 28 32)
		(if (zero? op)
			;; S-type
			(let ([op (bit-field inst 24 28)]
				  [imm (bit-field inst 14 24)]
				  [r1 (bit-field inst 0 14)])
			  (case op
				[(0)
				 '(Noop)]
				[(1)
				 (let1 cmpz-op (bit-field inst 21 24) ;; 20->21
				   (if (<= 0 cmpz-op 4)
					   (list 'Cmpz (vector-ref *cmpz-op* cmpz-op) r1)
					   (list 'unknown-Cmpz-op cmpz-op r1)))]
				[(2 3 4)
				 (list (vector-ref *s-op* op) r1)]
				[else
				 (list 'unknown-S-type-op imm r1)]))
			;; D-type
			(let ([r1 (bit-field inst 14 28)]
				  [r2 (bit-field inst 0 14)])
			  (if (<= 1 op 6)
				  (list (vector-ref *d-op* op) r1 r2)
				  (list 'unknown-D-type-op r1 r2)))
			)))

	(define status-humanstr #f)
	;(define const-humanstr (make-vector 16384 #f))
	(define (human ins pos)
	  (let* ([ins* (disasm ins)]
			 [op (first ins*)])
		(case op
		  [(Add)    (format "mem[~d] ←  mem[~d] + mem[~d]" pos (second ins*) (third ins*))]
		  [(Sub)    (format "mem[~d] ←  mem[~d] - mem[~d]" pos (second ins*) (third ins*))]
		  [(Mult)   (format "mem[~d] ←  mem[~d] * mem[~d]" pos (second ins*) (third ins*))]
		  [(Div)    (format "mem[~d] ←  mem[~d] / mem[~d]" pos (second ins*) (third ins*))]
		  [(Output) (format "OUT[~d] ←  mem[~d]" (second ins*) (third ins*))]
		  ;;[(Phi)    (format "mem[~d] ←  mem[status ? ~d : ~d]" pos (second ins*) (third ins*))]
		  [(Phi)    (format "mem[~d] ←  ~a? mem[~d] : mem[~d]" pos status-humanstr (second ins*) (third ins*))]
		  [(Noop)   #f];(format "//")]
		  [(Cmpz)
		   (let1 cmpz-c (case (second ins*)
						  [(LTZ) '<]
						  [(LEZ) '<=]
						  [(EQZ) '==]
						  [(GEZ) '>=]
						  [(GTZ) '>])
			 ;;(format "status ←  (mem[~d] ~a 0)" (third ins*) cmpz-c) ))
			 (set! status-humanstr
				   (format "(mem[~d] ~a 0)" (third ins*) cmpz-c) ))
		   #f]
		  [(Sqrt)   (format "mem[~d] ←  √mem[~d]" pos (second ins*))]
		  [(Copy)   (format "mem[~d] ←  mem[~d]" pos (second ins*))]
		  [(Input)  (format "mem[~d] ←  IN[~d]" pos (second ins*))]
		  ) ))

	(define (load-obf obf)
		(call-with-input-file obf
		  (lambda (iport)
			(let loop ((pos0 0))
			  (when (< pos0 16384)
				(let1 dat0 (read-f64 iport 'little-endian)
				  (unless (eof-object? dat0)
					(f64vector-set! data pos0 dat0)
					(let1 ins0 (read-u32 iport 'little-endian)
					  (unless (eof-object? ins0)
						(set! maxptr pos0)
						(u32vector-set! inst pos0 ins0)
						(vector-set! inst* pos0 (disasm ins0))
						;;(format #t "[~d] ~a ~x ~a\n" pos0 dat0 ins0 (disasm ins0))
						;;(format #t "~x ~a; ~a\n" ins0 (disasm ins0) (human ins0 pos0))
						(when *dump-code-when-loading*
						  (if (zero? ins0)
							  (format #t "mem[~d] = ~a\n" pos0 dat0);
							  (format #t "~a\n" (human ins0 pos0)) ))
						(let1 pos1 (+ pos0 1)
						  (let1 ins1 (read-u32 iport 'little-endian)
							(unless (eof-object? ins1)
							  (set! maxptr pos1)
							  (u32vector-set! inst pos1 ins1)
							  (vector-set! inst* pos1 (disasm ins1))
							  (let1 dat1 (read-f64 iport 'little-endian)
								(unless (eof-object? dat1)
								  (f64vector-set! data pos1 dat1)
								  ;;(format #t "[~d] ~a ~x ~a\n" pos1 dat1 ins1 (disasm ins1))
								  ;;(format #t "~x ~a; ~a\n" ins1 (disasm ins1) (human ins1 pos1))
								  (when *dump-code-when-loading*
									(if (zero? ins1)
										(format #t "mem[~d] = ~a\n" pos1 dat1);
										(format #t "~a\n" (human ins1 pos1)) ))
								  (loop (+ pos1 1))))))))))))) )))
#|
			  (read-block! data iport pos pos 'little-endian)
			  (read-block! inst iport pos pos 'little-endian)
			  (inc! pos)
			  (read-block! inst iport pos pos 'little-endian)
			  (read-block! data iport pos pos 'little-endian)
			  (loop (+ pos 2))
	|#
	(hash-table-put! *cmpz-ht* 'LTZ <)
	(hash-table-put! *cmpz-ht* 'LEZ <=)
	(hash-table-put! *cmpz-ht* 'EQZ =)
	(hash-table-put! *cmpz-ht* 'GEZ >=)
	(hash-table-put! *cmpz-ht* 'GTZ >)

	(lambda (m) ;; dispatcher
	  (case m
		[(reset) reset]
		[(load) load-obf]
		[(data) data]
		[(inst) inst*]
		[(dump-data) dump-data]
		[(dump-inst) dump-inst]
		;[(set-input-port) set-iport!]
		[(set-configuration) set-configuration]
		[(step) step]
		[(oport) oport]
		[(trace-begin) trace-begin]
		[(trace-end) trace-end]
		[(time) time]
		[else 'undefined]))
	))

(define (vm-reset vm) ([vm'reset]))
(define (vm-load vm obf) ([vm'load] obf))
(define (vm-data vm) [vm'dump-data])
(define (vm-inst vm) [vm'dump-inst])
(define (vm-dump-data vm) ([vm'dump-data]))
(define (vm-dump-inst vm) ([vm'dump-inst]))
;(define (vm-set-input-port vm addr value) ([vm'set-input-port] addr value))
(define (vm-set-configuration vm config)
  ([vm'set-configuration] config)) ;; #x3E80
;(define (vm-set-delta-v vm dvx dvy)
;  ([vm'set-input-port] 2 dvx)
;  ([vm'set-input-port] 3 dvy) )
(define (vm-step vm dvx dvy) ([vm'step] dvx dvy))
(define (vm-time vm) [vm'time])
(define (vm-oport vm addr) ([vm'oport] addr))
(define (vm-begin-tracing vm scenario-id) ([vm'trace-begin] scenario-id))
(define (vm-end-tracing vm) ([vm'trace-end]))

;;
;;
;; output stat
;;
(define (vm-stat vm)
  (let ([score (vm-oport vm 0)]
		[fuel-rest (vm-oport vm 1)]
		[earth-x-from-cs (vm-oport vm 2)]
		[earth-y-from-cs (vm-oport vm 3)]
		[gs-x-from-cs (vm-oport vm 4)]
		[gs-y-from-cs (vm-oport vm 5)]
		[gs-rest (vm-oport vm 6)] ;; fuel remaining on fuel station
		[targets (make-vector 11)]
		[moon-x-from-cs (vm-oport vm 100)]
		[moon-y-from-cs (vm-oport vm 101)]
		)
	(dotimes (k 11)
	  (let* ([tx-from-cs (vm-oport vm (+ (* 3 k) 7))]
			 [ty-from-cs (vm-oport vm (+ (* 3 k) 8))]
			 [tx (- tx-from-cs earth-x-from-cs)]
			 [ty (- ty-from-cs earth-y-from-cs)]
			 [collected? (if (= (vm-oport vm (+ (* 3 k) 9)) 1) #t #f)])
		(vector-set! targets k (list tx ty #f #f collected?)) ))
	(let* ([cs-x (- earth-x-from-cs)]
		   [cs-y (- earth-y-from-cs)]
		   [gs-x (- gs-x-from-cs earth-x-from-cs)]
		   [gs-y (- gs-y-from-cs earth-y-from-cs)]
		   [moon-x (- moon-x-from-cs earth-x-from-cs)]
		   [moon-y (- moon-y-from-cs earth-y-from-cs)]
		   [cs-r (hypot cs-x cs-y)]
		   [cs-th (atan cs-y cs-x)])
	  `(,score
		,fuel-rest
		(,cs-x ,cs-y ,cs-r ,cs-th)
		(,gs-x ,gs-y #f #f ,gs-rest)
		,targets
		(,moon-x ,moon-y))
	  )))
(define x-of first)
(define y-of second)
(define r-of third)
(define theta-of fourth)
(define (stat-score st) (first st))
(define (stat-fuel st) (second st))
(define (stat-cs-x st) (x-of (third st)))
(define (stat-cs-y st) (y-of (third st)))
(define (stat-cs-r st) (r-of (third st)))
(define (stat-cs-theta st) (theta-of (third st)))
(define (stat-gs-x st) (x-of (fourth st)))
(define (stat-gs-y st) (y-of (fourth st)))
(define (stat-gs-fuel st) (fifth (fourth st)))
(define (stat-ts-x k st) (x-of (vector-ref (fifth st) k)))
(define (stat-ts-y k st) (y-of (vector-ref (fifth st) k)))
;(define (stat-ts-r k st) (r-of (fifth st)))
;(define (stat-ts-theta k st) (theta-of (fifth st)))
(define (stat-ts-visited? k st) (fifth (vector-ref (fifth st) k)))
(define (stat-moon-x st) (x-of (sixth st)))
(define (stat-moon-y st) (y-of (sixth st)))
;(define (stat-ds-x st) (x-of (fifth st)))
;(define (stat-ds-y st) (y-of (fifth st)))
;(define (stat-ds-r st) (r-of (fifth st)))
;(define stat-dist stat-ds-r)
;(define (stat-ds-theta st) (theta-of (fifth st)))
(define (pp-stat st1 . args)
  (let1 st0 (if (null? args) #f (car args))
	(format #t "Sat.C. location: (~a, ~a) r=~a,theta=~a\n"
			(stat-cs-x st1) (stat-cs-y st1) (stat-cs-r st1) (stat-cs-theta st1) )
	(when st0
	  (let ([vx (- (stat-cs-x st1) (stat-cs-x st0))]
			[vy (- (stat-cs-y st1) (stat-cs-y st0))])
		(let ([vr (hypot vx vy)] [vtheta (atan vy vx)])
		  (format #t "       velocity: (~a, ~a) r=~a,theta=~a\n" vx vy vr vtheta) )))

;	(format #t "Sat.T. location: (~a, ~a) r=~a,theta=~a\n"
;			(stat-ts-x st1) (stat-ts-y st1) (stat-ts-r st1) (stat-ts-theta st1) )
;	(when st0
;	  (let ([vx (- (stat-ts-x st1) (stat-ts-x st0))]
;			[vy (- (stat-ts-y st1) (stat-ts-y st0))])
;		(let ([vr (hypot vx vy)] [vtheta (atan vy vx)])
;		  (format #t "       velocity: (~a, ~a) r=~a,theta=~a\n" vx vy vr vtheta) )))

;	(format #t "          delta: (~a, ~a) r=~a,theta=~a\n"
;			(stat-ds-x st1) (stat-ds-y st1) (stat-ds-r st1) (stat-ds-theta st1) )
	))

;;
