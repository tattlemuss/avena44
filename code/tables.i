
					rsreset
randrange_seed		rs.w	1		; random seed
randrange_min		rs.w	1		; minimum value
randrange_max		rs.w	1		; minimum value
randrange_SIZE		rs.b	1

DECLARE_RANDRANGE	macro 			;(min,max)
			        dc.w	0,\1,\2
			        endm

					rsreset
twitcher_rangeval	rs.b	randrange_SIZE		; chooses value in range
twitcher_rangetime	rs.b	randrange_SIZE		; chooses time in range
twitcher_time		rs.w	1					; current countdown timer
twitcher_value		rs.w	1					; current output value
twitcher_SIZE		rs.b	1

					rsreset
decrementer_val:	rs.w	1
decrementer_sub:	rs.w	1
					rs.b	1					
					
; setup a twitcher
DECLARE_TWITCHER	macro 			;(min,max,tickmin,tickmax)
					DECLARE_RANDRANGE	\1,\2
					DECLARE_RANDRANGE	\3,\4
					dc.w		1		;timer
					dc.w		0		;value
					endm
