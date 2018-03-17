;	getting the coding boots on
;
;	started 14th june 2014
;
	opt	d+,s+

; External references

	if	SHOW_TIMINGS
; -------------------------------------------------------------------
debug_set_colour:
	; only show timings if LEFT SHIFT is pressed
	cmp.b	#$2a,$fffffc02.w
	bne.s	.skip
	move.w	d0,$ffff8240.w
.skip:
	rts
	endif

	if	ENABLE_CONSOLE
; -------------------------------------------------------------------
debug_string:
	movem.l	d0-d7/a0-a6,-(a7)
.loop:
	moveq	#0,d0
	move.b	(a0)+,d0
	beq.s	.done
	pea		(a0)
	move.w	d0,-(a7)		; char
	move.w	#2,-(a7)		; device
	move.w	#3,-(a7)		; xbios 3, bconout
	trap	#13
	addq.l	#6,a7
	move.l	(a7)+,a0
	bra.s	.loop
.done
	movem.l	(a7)+,d0-d7/a0-a6
	rts

; -------------------------------------------------------------------
debug_hex_word:
	movem.l	d0-d7/a0-a6,-(a7)
	moveq	#4-1,d2
	swap	d0
	bra.s	debug_hex_shared
debug_hex:
	movem.l	d0-d7/a0-a6,-(a7)
	moveq	#8-1,d2
debug_hex_shared:
	rol.l	#4,d0			; get the top-most 4 bits
	move.l	d0,-(a7)
	move.w	d0,d1
	and.w	#$f,d1
	move.b	.hex_table(pc,d1.w),d1
	move.w	d1,-(a7)		; char
	move.w	#2,-(a7)		; device
	move.w	#3,-(a7)		; xbios 3, bconout
	trap	#13
	addq.l	#6,a7
	move.l	(a7)+,d0
	dbf		d2,debug_hex_shared
	lea		.carriage_return(pc),a0
	bsr		debug_string
	movem.l	(a7)+,d0-d7/a0-a6
	rts
.hex_table			dc.b	'0123456789abcdef'	
.carriage_return	dc.b	13,10,0
	even

	endif

	ifne	KEYBOARD
debug_save_pi1:
	movem.l	d0-d7/a0-a6,-(a7)
	move.w	#0,-(a7)			;attr
	pea	pi1_path(pc)
	move.w	#60,-(a7)			;fcreate
	trap	#1
	addq.l	#8,a7
	tst.w	d0
	bmi	.fail
	move.w	d0,d7				;d7 handle

	lea	header(pc),a0
	move.l	$ffff8240.w,2(a0)
	move.l	$ffff8244.w,6(a0)
	move.l	$ffff8248.w,10(a0)
	move.l	$ffff824c.w,14(a0)
	move.l	$ffff8250.w,18(a0)
	move.l	$ffff8254.w,22(a0)
	move.l	$ffff8258.w,26(a0)
	move.l	$ffff825c.w,30(a0)

	move.l	a0,-(a7)			;buffer
	move.l	#34,-(a7)			;count
	move.w	d7,-(a7)			;handle
	move.w	#$40,-(a7)			;fwrite
	trap	#1	
	lea	12(a7),a7

	move.l	front_buffer(pc),-(a7)		;buffer
	move.l	#32000,-(a7)			;count
	move.w	d7,-(a7)			;handle
	move.w	#$40,-(a7)			;fwrite
	trap	#1	
	lea	12(a7),a7

	move.w	d7,-(a7)			;handle
	move.w	#$3e,-(a7)			;CLOSE
	trap	#1
	addq.l	#4,a7
.fail:
	lea	pi1_path_number(pc),a0
	addq.b	#1,(a0)				;next filename
	movem.l	(a7)+,d0-d7/a0-a6
	rts

header:	ds.b	34

pi1_path:
	dc.b	"\\SCREEN"
pi1_path_number:
	dc.b	"0"
	dc.b	".PI1",0
	even

	endif	
