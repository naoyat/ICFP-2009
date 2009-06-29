(use gauche.uvector)
(use binary.io)
(use srfi-1)

(use gl)
(use gl.glut)
(load "./gauche-gl/segment.scm")
(load "./gauche-gl/frame.scm")
(load "./gauche-gl/vector.scm")
(load "./gauche-gl/painter.scm")

(use math.const)

(define NOP 0)
(define *team-id* 514) ;; Ikoma

(define *dump-code-when-loading* #f)
(define *verbose* #f)

(define (hypot x y) (sqrt (+ (* x x) (* y y))))

(define (cafebabe-writer filename scenario-id)
  (let1 oport (open-output-file filename)
	(write-u32 #xcafebabe oport 'little-endian)
	(write-u32 *team-id* oport 'little-endian)
	(write-u32 scenario-id oport 'little-endian)
	(lambda args
	  (if (null? args)
		  (close-output-port oport)
		  (let ([time-step (first args)]
				[count (second args)]
				[addr (third args)]
				[value (fourth args)])
			(write-u32 time-step oport 'little-endian)
			(write-u32 count oport 'little-endian)
			(write-u32 addr oport 'little-endian)
			(write-f64 value oport 'little-endian))))
	))

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
	  (format #t "TRACE-BEGIN: 0xCAFEBABE, ~d, ~d\n" *team-id* scenario-id)
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
			(format #t "TRACE: t=~d, ~a\n" time diffs)
			))))
	(define (trace-end)
	  (when do-trace?
		(when trace-oport
		  (write-u32 time trace-oport 'little-endian)
		  (write-u32 0 trace-oport 'little-endian)
		  (close-output-port trace-oport)
		  (set! trace-oport #f))
		(format #t "TRACE-END: t=~d\n" time)
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
				))
		)
	  (inc! time)
	  )

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
(define (vm-oport vm addr) ([vm'oport] addr))
(define (vm-begin-tracing vm scenario-id) ([vm'trace-begin] scenario-id))
(define (vm-end-tracing vm) ([vm'trace-end]))

(define vm (make-vm))

(vm-load vm "bin1.obf")
(define *scenario-id* 1004)

(vm-begin-tracing vm *scenario-id*)
(vm-set-configuration vm *scenario-id*)

(define mu (* 6.67428e-11 6.0e24))

(define target-r #f)
(define vcir1 #f) 
(define vcir2 #f)
(define delta-v1 #f)
(define delta-v2 #f)
(define rev? #f)


;; t=0
(vm-step vm 0 0)
;; t=1
(let* ([cs-x1 (- (vm-oport vm 2))]
	   [cs-y1 (- (vm-oport vm 3))]
	   [target-r1 (vm-oport vm 4)]
	   [cs-r1 (hypot cs-x1 cs-y1)]
	   [cs-th1 (atan cs-y1 cs-x1)]
	   )
  (vm-step vm 0 0)
  ;; t=2
  (let* ([cs-x2 (- (vm-oport vm 2))]
		 [cs-y2 (- (vm-oport vm 3))]
		 ;;[target-r2 (vm-oport vm 4)]
		 [cs-r2 (hypot cs-x2 cs-y2)]
		 [cs-th2 (atan cs-y2 cs-x2)]
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

	;; delta-v1
	(let* ([th (+ cs-th2 pi/2)]
		   [dth (- dxx-th th)])
	  (when (< dth (- pi)) (set! dth (+ dth pi pi)))
	  (when (< pi dth) (set! dth (- dth pi pi)))
	  (unless (<= pi/2 dth pi/2) (set! rev? #t) (set! th (- th pi)))
	  (let ([dvx (* delta-v1 (cos th))]
			[dvy (* delta-v1 (sin th))]
			)
		(format #t "th=~a: (~a, ~a):~a + (~a, ~a) => (~a, ~a):~a\n"
				th
				dx dy (hypot dx dy)
				dvx dvy
				(+ dx dvx) (+ dy dvy)
				(hypot (+ dx dvx) (+ dy dvy)))
		(format #t "th=~a: (~a, ~a):~a + (~a, ~a) => (~a, ~a):~a\n"
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

		(when (< (hypot (+ dx dvx) (+ dy dvy)) (hypot dx dy))
		  (set! dvx (- dvx))
		  (set! dvy (- dvy)))

;	  (let ([dvx (- (* vcir1 (cos th)) (* dx (cos dxx-th)))]
;			[dvy (- (* vcir1 (sin th)) (* dx (sin dxx-th)))])
;	  (let ([dvx (- (* dx (cos dxx-th)) (* vcir1 (cos th)))]
;			[dvy (- (* dx (sin dxx-th)) (* vcir1 (sin th)))])
		(format #t "  !! FIRE. Δ V = (~a ~a) ~a\n" dvx dvy rev?)
		(vm-step vm dvx dvy)))
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
;(define target-r #f)

(define (rerun)
  (vm-reset vm)
  (vm-load vm "bin1.obf")
  (vm-set-configuration vm 1001)

  (set! cs-dx #f)
  (set! cs-dy #f)
  (set! cs-d-theta #f)
  (set! cs-r #f)
  (set! cs-theta #f)
  (set! cs-trace '())
  (set! target-r #f)
  )

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
	#;(format #t "last: (~a, ~a)\n" last-x last-y)
	;; サテライトから地球へのベクタなので両方ともマイナス
	(set! cs-x (- (vm-oport vm 2)))
	(set! cs-y (- (vm-oport vm 3)))
	(set! cs-r (hypot cs-x cs-y))
	(set! cs-theta (atan cs-y cs-x))
	
	(push! cs-trace (cons cs-x cs-y))
	;;
	(when last-x
	  (set! cs-dx (- cs-x last-x))
	  (set! cs-dy (- cs-y last-y))
	  (set! cs-d-theta (atan cs-dy cs-dx))
	  #;(format #t "new: (~a, ~a) = last + Δ(~a, ~a), ~a, ~a\n" cs-x cs-y
			  cs-dx cs-dy (* 180/pi cs-d-theta) (hypot cs-dx cs-dy))
	  ))
  (set! target-r (vm-oport vm 4))
  )

(define last-within? #f)
(define within-count 0)

(define (my-keyboard key x y)
  (when (= key 27) (exit 0))

  ;; step
  (let ([ch (integer->char key)]
		[skip #f]
		[last-cs-x #f]
		[last-cs-y #f])
	;(format #t "key=~a\n" )
	(if cs-d-theta
		(let* ([r 100]
			   [dx (* r (cos cs-d-theta))]
			   [dy (* r (sin cs-d-theta))])
;		  (format #t "// r=~a th=~a dx=~a dy=~a\n" r (* 180/pi cs-d-theta) dx dy)
		  (case ch
			[(#\r) (rerun)
			 (vm-step vm 0 0)]
;			[(#\w) (vm-step vm 0 (- r))]
;			[(#\a) (vm-step vm (- r) 0)]
;			[(#\s) (vm-step vm r 0)]
;			[(#\z) (vm-step vm 0 r)]
			[(#\-) 
			 ;;(display '-) (flush)
			 (format #t "[-] (~a, ~a)\n" (- dx) (- dy))
			 (vm-step vm (- dx) (- dy))]
			[(#\= #\+)
			 ;;(display '+) (flush)
			 (format #t "[+] (~a, ~a)\n" dx dy)
			 (vm-step vm dx dy)]
			[(#\Space) ;#\8 #\*)
			 (call/cc
			  (lambda (break)
				(dotimes (j 64)
				  (dotimes (i 16)
					(vm-step vm 0 0)
					(let* ([score (vm-oport vm 0)]
						   [fuel (vm-oport vm 1)]
						   [cs-x (- (vm-oport vm 2))]
						   [cs-y (- (vm-oport vm 3))]
						   [cs-r (hypot cs-x cs-y)]
						   [r-diff (- target-r cs-r)]
						   [v #f]
						   )
					  (when (< 0.0 score)
						(format #t "  !! SCORE = ~a\n" score)
						(vm-end-tracing vm)
						(break))
					  (when (<= fuel 0.0)
						(format #t "  !! FUEL = ~a\n" fuel)
						(break))
					  (when last-cs-x
						(set! v (hypot (- cs-x last-cs-x) (- cs-y last-cs-y)))
						#;(format #t "current v=~a\n" v)
						)
										;(format #t "/ ~a ~a\n" cs-r target-r)
					  (if (< (abs r-diff) 1000)
						  (begin
							(if last-within?
								(inc! within-count)
								(set! within-count 1))
							(set! last-within? #t)
;							(format #t "!! TARGET WITHIN 1000m (~a), #~d, score=~a\n" r-diff within-count score)
							(let* ([cs-th (atan cs-y cs-x)]
								   [th (+ cs-th pi/2)]
								   [v-th #f]
								   [th-diff #f]
								   )
							  (when last-cs-x
								(set! v-th (atan (- cs-y last-cs-y) (- cs-x last-cs-x)))
								(set! th-diff (- v-th cs-th))
								(when rev? (set! th-diff (- th-diff)))
								(let1 slant (- pi/2 th-diff)
;								  (format #t "- rev=~a, th-diff=~a slant=~a\n" rev? (* 180/pi th-diff) slant)
								  ;;(when (and delta-v2 (< (abs slant) 1e-3))
								  (when (and delta-v2 (< (abs r-diff) 2))
;									(format #t " now v=(~a, ~a)\n" (- cs-x last-cs-x) (- cs-y last-cs-y))
									(set! delta-v2 (- vcir2 v))
									;; delta-v2
										;(when (< dth (- pi)) (set! dth (+ dth pi pi)))
										;(when (< pi dth) (set! dth (- dth pi pi)))
									(if rev? (set! th (- th pi)))
;									(let ([dvx (* delta-v2 (cos th))]
;										  [dvy (* delta-v2 (sin th))])
									(let ([dvx (- (* vcir2 (cos th)) (* v (cos v-th)))]
										  [dvy (- (* vcir2 (sin th)) (* v (sin v-th)))])
									  (format #t "  !! FIRE. Δ V = (~a ~a)\n" dvx dvy)
									  (vm-step vm dvx dvy)
									  (set! delta-v2 #f)
									  (break))))))
							)
						  (set! last-within? #f))
					  (set! last-cs-x cs-x)
					  (set! last-cs-y cs-y)
					  ))
				  (moving)
				  (glut-post-redisplay))
				(set! skip #t)
				))]
			[else (vm-step vm 0 0)] ))
		(vm-step vm 0 0))

	(unless skip
	  (moving)
	  (glut-post-redisplay)
	  )
;	(format #t "S:(~a, ~a) = (r:~a θ:~a)\n" cs-x cs-y cs-r (* 180/pi cs-theta))
	))

(define (my-display)
  (when (and cs-r target-r)
	(let* ([rmax (max earth-r cs-r target-r)]
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

