
;		 (let* ([cs-th (stat-cs-theta stat)]
;				[th (+ cs-th pi/2)]
;				[cs-r (stat-cs-r stat)]
;				[middle-r-diff (- middle-orbit-r cs-r)]
;				[target-r-diff (- target-perigee-r cs-r)]
				)

		   (when 
#;		 (when (and delta-v2 (< (abs middle-r-diff) 1))
;			   (format #t "@@@ ~a\n" (abs middle-r-diff))
		   (let ([dvx (* delta-v2 (cos th))]
				 [dvy (* delta-v2 (sin th))])
			 ;;;;;@@@@@
			 (fire (- dvx) (- dvy))
			 ;(format #t "  !! FIRE <2.>. Δ V=(~a, ~a):~a fuel:~a\n" dvx dvy (hypot dvx dvy) (vm-oport vm 1))
			 (set! delta-v2 #f)
			 (break #t)))
		 
#;		 (when (and (not delta-v2) delta-v3 (< (abs target-r-diff) 0.9))
		   (let ([dvx (* delta-v3 (cos th))]
				 [dvy (* delta-v3 (sin th))])
			 (set! dvx (- dvx))
			 (set! dvy (- dvy))

			 (fire dvx dvy) ;;;;;@@@@@
			 (set! delta-v3 #f)
			 
			 (format #t "  !! FIRE <3.>. Δ V=(~a, ~a):~a, dist=(~a, ~a):~a fuel:~a\n"
					 dvx dvy (hypot dvx dvy)
					 (stat-ds-x stat) (stat-ds-y stat) (stat-ds-r stat)
					 (stat-fuel stat))
			 (break #t)))
		 
#;		 (unless delta-v3
		   (when (> (abs (stat-ds-r stat)) 300)
			 (let* ([vx (- (stat-cs-x stat) (stat-cs-x last-stat))]
					[vy (- (stat-cs-y stat) (stat-cs-y last-stat))]
					[v-th (atan vy vx)]
					[th-diff (- v-th (stat-cs-theta stat))]
						  
										;						[tvx (- target-x last-target-x)]
										;						[tvy (- target-y last-target-y)]
										;						  [g (/ mu target-r target-r)]
					[g 0]
					[g-th (- (stat-ts-theta stat))]
					[gx (* g (cos g-th))]
					[gy (* g (sin g-th))]
					[tvx (- (- (stat-ts-x stat) gx) (stat-ts-x last-stat))]
					[tvy (- (- (stat-ts-y stat) gy) (stat-ts-y last-stat))]
					[tv_r (hypot tvx tvy)]
					[tv_th (atan tvy tvx)]
					
					[target-th (stat-ts-theta stat)]
					[cs-th (stat-cs-theta stat)]
					
					[tv_th_ (+ tv_th (- cs-th target-th))]
					[tvx_ (* tv_r (cos tv_th_))]
					[tvy_ (* tv_r (sin tv_th_))]
					
;						  [c+x (+ cs-x tvx_)]
;						  [c+y (+ cs-y tvy_)]
;						  [c+th (atan c+y c+x)]
;						  [g+th (- c+th)]
;						  [g+x (* g (cos g+th))]
;						  [g+y (* g (sin g+th))]
;						  [tvx+ (- tvx_ g+x)]
;						  [tvy+ (- tvy_ g+y)]
					)
			   (pp-stat stat last-stat)

			   (fire (- tvx_ vx) (- tvy_ vy)) ;;;;@@@@@
			   (format #t "... with this delta V, (~a, ~a):~a => (~a, ~a):~a\n"
					   vx vy (hypot vx vy)
					   tvx_ tvy_ (hypot tvx_ tvy_) )
			   
			   (break #t)
			   ))); unless delta-v3
