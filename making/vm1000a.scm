(use gauche.uvector)
(use binary.io)
(use srfi-1)

(define NOP 0)
(define *team-id* 514) ;; Ikoma

(define *dump-code-when-loading* #t)
(define *verbose* #f)

(define (cafebabe-writer filename scenario-id)
  (let1 oport (open-output-file filename)
	(write-u32 #xcafebabe oport 'little-endian)
	(write-u32 *team-id* oport 'little-endian)
	(write-u32 schenario-id oport 'little-endian)
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
	  (set! maxptr -1))

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
	(define (set-configuration config) (set-iport! #x3e80 config))

	(define (// y x) (if (= 0.0 x) 0.0 (/ y x)))

	(define (step dvx dvy)
	  (set-dvx! dvx) (set-dvy! dvy)
	  (format #t ":mem:")
	  (for-each (lambda (addr) (format #t " ~d=~a" addr (mem addr)))
				(list 241 248 249 250 251 252 253 254 255 256 257 258 259 260 261 262 263 264 265) )
	  (newline)
			  
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
				   (format #t "mem[~d] = (const) ~a\n" pc (mem pc))
				   )
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
	  (format #t "[~a ~a ~a ~a ~a ... ~a]\n"
			  (oport 0) (oport 1) (oport 2) (oport 3) (oport 4)
			  (oport 5) )
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
(define (vm-step vm dvx dvy)
  ([vm'step] dvx dvy))

(define vm (make-vm))

;(vm-load vm "bin1.obf")
;(vm-dump-data vm)
;(vm-run vm)
;(vm-dump-data vm)

(let repl ()
  (display "> ") (flush)
  (let1 line (read-line)
	(unless (eof-object? line)
	  (let1 s (string-split line " ")
		(case (string->symbol (first s))
		  [(reset)
		   (vm-reset vm)
		   (repl)]
		  [(quit exit bye end)]
		  [(load) ;; load [1-4]
		   (vm-reset vm)
		   (vm-load vm (string-append "bin" (second s) ".obf"))
		   (repl)]
;		  [(input)
;		   (vm-set-input-port vm (string->number (second s)) (string->number (third s)))
;		   (repl)]
		  [(config)
		   (vm-set-configuration vm (string->number (second s)))
		   (repl)]
		  [(step)
		   (let ([dvx (string->number (second s))]
				 [dvy (string->number (third s))])
			 ;(time (dotimes (i 10000) ;; 1step = 2.7/10000 sec
			 (vm-step vm dvx dvy)
			 ;))
			 (repl))]
;		  [(run)
;		   (vm-run vm)
;		   (repl)]
		  [(data)
		   (vm-dump-data vm)
		   (repl)]
		  [(list)
		   (vm-dump-inst vm)
		   (repl)]
		  [else
		   (format #t "UNKNOWN COMMAND: ~a\n" line)
		   (repl)])))))
