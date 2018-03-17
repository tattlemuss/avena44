; Debug settings

SHOW_TIMINGS		equ	 	0		; 1 == border colours
ALLOW_KEYBOARD		equ		0
ENABLE_CONSOLE		equ		0

; -----------------------------------------------------
; TIMING COLOURS
; -----------------------------------------------------

	if SHOW_TIMINGS
		xref debug_set_colour
		macro 	DBG_COL
			move.l	d0,-(a7)
			move.w	\1,d0
			jsr		debug_set_colour
			move.l	(a7)+,d0
		endm
	else
		macro 	DBG_COL
		endm
	endif

; -----------------------------------------------------
; CONSOLE OUTPUT
; -----------------------------------------------------

	if ENABLE_CONSOLE
		xref debug_string
		xref debug_hex
		xref debug_hex_word
		macro 	CON_STRING
			section data
.mystring\@:	dc.b	\1,13,10,0
				even
			section text
			pea		(a0)
			lea		.mystring\@,a0
			jsr		debug_string
			move.l	(a7)+,a0
		endm
		macro 	CON_HEX
			move.l	d0,-(a7)
			move.l	\1,d0
			jsr		debug_hex
			move.l	(a7)+,d0
		endm
		macro 	CON_WORD
			move.l	d0,-(a7)
			move.l	\1,d0
			jsr		debug_hex_word
			move.l	(a7)+,d0
		endm
	else
		macro 	CON_STRING
		endm
		macro 	CON_HEX
		endm
		macro 	CON_WORD
		endm		
	endif
