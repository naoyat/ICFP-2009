(use gauche.uvector)
(use binary.io)
(use srfi-1)

(define *team-id* 514) ;; Ikoma

(define (main args)
  (unless (null? (cdr args))
	(let* ([*scenario-id* (string->number (cadr args))]
		   [filename (format "~d.osf" *scenario-id*)])
	  (call/cc (lambda (break)
				 (call-with-input-file filename
				   (lambda (iport)
					 (define (u32) (read-u32 iport 'little-endian))
					 (define (f64) (read-f64 iport 'little-endian))
					 (let ([cafebabe (u32)]
						   [team-id (u32)]
						   [scenario-id (u32)])
					   (unless (and (= cafebabe #xcafebabe)
									(= team-id *team-id*)
									(= scenario-id *scenario-id*))
						 (format #t "invalid data.\n")
						 (break))
					   (let loop ()
						 (let1 time (u32)
						   (when (eof-object? time) (break))
						   (format #t "t=~d:\n" time)
						   (let1 count (u32)
							 (dotimes (i count)
							   (let ([addr (u32)]
									 [value (f64)])
								 (format #t "  port #~x ← ~d\n" addr value)
								 )))
						   (loop)))
					   )))));call/cc
	  0)))
