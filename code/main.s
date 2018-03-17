	opt	d+,s+

        ifeq    DEBUG
        opt     p+
        endif
	opt	o1+
	opt	o2+
	opt	o3+
	opt	o4+
	opt	o5+
	;opt	o6+
	opt	o7+
	opt	o10+
	opt	o11+
	opt	o12+
	opt	oc+
	opt	og+
	;opt	oj+	not with -p

	;opt	ow+
;oc Enable optimizations to CLR (refer to -opt-clr).
;od Enable optimization of divisions into shifts (refer to -opt-div).
;of Enable immediate ;-opt-fconst).
;og Enable generic vasm optimizations. All optimizations which cannot be controlled by another option.
;oj Enable branch to jump optimizations (refer to -opt-brajmp).
;ol Enable shift optimizations to ADD (refer to -opt-lsl).
;om Enable MOVEM optimizations (refer to -opt-movem).
;op Enable optimizations to PEA (refer to -opt-pea).
;os Optimize for speed before optimizing for size (refer to -opt-speed).
;ot Enable optimizations to ST (refer to -opt-st).
;ox Enable optimization of multiplic
; -----------------------------------------------------------------------------
; m4 macros
m4_changecom
m4_define(`A6REF',`($1)-base_a6(a6)')
m4_define(`TEXTREF',`($1-text_end)+(bss_start-base_a6)(a6)')

m4_define(`PCREF',`$1(pc)')
m4_define(`A6REFLONG',`move.l a6,$2
        add.l #($1)-base_a6,$2')

m4_include(defines.s)
	include sound2/defines_music.s
	include	debug.i

; -----------------------------------------------------------------------------
;	DEFINES
; -----------------------------------------------------------------------------
LINES			equ	200
BYTES_PER_LINE      	equ    	160
PADDED_LINES		equ	208

MAX_POINTS		equ	64
MAX_POLYS		equ	16
MAX_OBJECTS		equ	16

BYTES_PER_SCREEN	equ	PADDED_LINES*BYTES_PER_LINE
BITPLANE_BLUR		equ	1

HALFTONE_STRIDE		equ	208			;full screen (16 multiple)
NUM_HALFTONE_LINES	equ	HALFTONE_STRIDE*17	;16 levels+black

TILE_X			equ	20
TILE_Y			equ	208/16+1
BYTES_PER_TILE		equ	16*2
NUM_TILES		equ	64
NUM_HALFTONE_TILES	equ	256
NUM_PATTERNS		equ	4			;count of different patterns
;TILE_SHAPE_COUNT	equ	MESH_AVENA_N

TOTAL_TILE_SIZE_MASK 	= (MESH_COUNT*BYTES_PER_TILE*NUM_TILES)-1
TILESET_MASK    	= (BYTES_PER_TILE*NUM_TILES)-1

CLIP_VAL_MIN_X		equ	0
CLIP_VAL_MAX_X		equ	+319*8	; stop going off end of table
CLIP_VAL_MIN_Y		equ	0
CLIP_VAL_MAX_Y		equ	+200

; -----------------------------------------------------------------------------
SINE_LOOP_BITS		equ	9			; Number of bits in a quadrant
QUADRANT_COUNT		equ	(1<<SINE_LOOP_BITS)	; Number of entries in one quad
SINE_COUNT		equ	(1<<(SINE_LOOP_BITS+2))	; Number of entries in whole table
FIXED_POINT_PLACE	equ	14
THREE_FIXED		equ	3<<14			; This gives max precision in 16 bits to hold "3" (2:14)
SINE_SIZE		equ	SINE_COUNT*2		; Size of whole table in bytes
SINE_MASK		equ	SINE_SIZE-2		; Mask for byte offset when looping
COS_OFFSET		equ	2*QUADRANT_COUNT	; Offset in bytes to second quadrant

; -----------------------------------------------------------------------------
; DARTS DEFINES
TRI_EXT			equ	$700

; TODO test these
; Size optimisation flags (for future tests)
USE_MOVEQ		equ	0		;move.w	seems to pack smaller than moveq!!
USE_JMP_A6		equ	0		;might pack smaller than bsr

; Keep it consistent to improve packer perf
PUSH_ALL		MACRO
			movem.l	d0-d7/a0-a5,-(a7)
		        ENDM

POP_ALL			MACRO
			movem.l	(a7)+,d0-d7/a0-a5
		        ENDM

RESTORE_ALL		MACRO
			movem.l	(a7),d0-d7/a0-a5
		        ENDM

; -----------------------------------------------------------------------------
BYTES_PER_INDEX	equ		6
; -------------------------------------------------------------------------------
DEF_TRI		macro
		dc.b		3,\1*6,\2*6,\3*6			; num verts, NV * vert offsets
		endm

; -------------------------------------------------------------------------------
DEF_QUAD	macro
		dc.b		4,\1*6,\2*6,\3*6,\4*6			; num verts, NV * vert offsets
		endm

; -------------------------------------------------------------------------------
DEF_PENT	macro
		dc.b		5,\1*6,\2*6,\3*6,\4*6,\5*6		; num verts, NV * vert offsets
		endm
; -------------------------------------------------------------------------------
DEF_HEX		macro
		dc.b		6,\1*6,\2*6,\3*6,\4*6,\5*6,\6*6		; num verts, NV * vert offsets
		endm
; -------------------------------------------------------------------------------
;			MESH
; -------------------------------------------------------------------------------
			rsreset
_mesh_vert_count	rs.w	1
_mesh_poly_count	rs.w	1
;_mesh_poly_descs	rs.w	1
			
; -------------------------------------------------------------------------------
;			MATRIX
; -------------------------------------------------------------------------------
			rsreset
_matrix43_11		rs.w	1
_matrix43_12		rs.w	1
_matrix43_13		rs.w	1
_matrix43_21		rs.w	1
_matrix43_22		rs.w	1
_matrix43_23		rs.w	1
_matrix43_31		rs.w	1
_matrix43_32		rs.w	1
_matrix43_33		rs.w	1
_matrix43_tx		rs.w	1
_matrix43_ty		rs.w	1
_matrix43_tz		rs.w	1
_matrix43_size		rs.w	1

; -------------------------------------------------------------------------------
;			OBJECT
; -------------------------------------------------------------------------------
			rsreset
_object_rotation_add_x	rs.w	1
_object_rotation_add_z	rs.w	1
_object_rotation_x	rs.w	1	;will have mask applied
_object_rotation_z	rs.w	1	;will have mask applied
_object_translation_x	rs.w	1
_object_translation_y	rs.w	1
_object_translation_z	rs.w	1
_object_mesh_index	rs.w	1	;offset into mesh_desc_table
_object_mesh_dither	rs.w	1	;offset into dither_desc_table
_object_size		rs.b	1

	include	tables.i
	include	tri.i

start:
	; supervisor
	clr.l	-(a7)
	move.w	#$20,-(a7)
	trap	#1

	; now in supervisor mode
	lea	PCREF(stack),a7
	lea	PCREF(base_a6),a6			; TODO could use stack in theory

; -----------------------------------------------------------------------------
;	INIT FUNCTIONS
; -----------------------------------------------------------------------------
system_init:

; -----------------------------------------------------------------------------
;	SINE TABLE
; -----------------------------------------------------------------------------
; NOTE: assumes 512 entries (9 bits)

;	Our target approximation is z/2 * (3 - z*z)

;
;	halfz = i << (14-(9-2)-1)
;	# Calc (3 - z*z)
;   	# starting i is 7 bits (effectively 8 with sign)
;	zmul = i #<< (16-X)
;	zsqr = (zmul * zmul) << 0		# 32-18 = 14 bit range
;	s = halfz * (three - zsqr)		# now 28-bit range again
;
;	# Shift down to range
;	s = s >> (X*4-16+1)			# +1 because of sign

_init_sine_table:
	lea	A6REF(sine_table+COS_OFFSET),a0; a0 - quadrant 0 (working backwards)
	move.l	a0,a1				; a1 - quadrant 1 (working fwds)
	lea	COS_OFFSET*2(a1),a2		; a2 - quadrant 2 (working backwards)
	move.l	a2,a3				; a3 - quadrant 3 (working fwds)
	; Change of approach.
	; Keep at 16 bits precision unless stated.
	move.w	#16+2,d4			; d4 - shift amount after 1st mulu
	move.w	#(QUADRANT_COUNT-1)<<(16-SINE_LOOP_BITS),d0	; d0 - z (count backwards)
.loop:
	move.w	d0,d2				; d0 = z
	;NOTE: all calcs guarantee positive values, so use unsigned
	mulu.w	d2,d2				; d2 = z*z (fixed point 32)
	lsr.l	d4,d2				; d2 = z*z (fixed point 14)
	move.w	#THREE_FIXED,d3			; d3 = 3.0 (fixed point 14) (0xc000)
	sub.w	d2,d3				; d3 = 3.0 - z*z (fixed point 14)
	move.w	d0,d1				; d1 - Z (fixed point 16)
	mulu.w	d1,d3				; d3 = z * (3 - z*z)  (30-bit range)
	; Now at 30-bit range, but the d3 term is already *2, so effectively 31 bits
	; we want 15 bits, so just swap
	swap	d3
	;lsr.l	d5,d3				; shift
	; mirror into 4 quadrants. Can this be improved?
	add.w	d3,-(a0)
	add.w	d3,(a1)+
	sub.w	d3,-(a2)
	sub.w	d3,(a3)+
	sub.w	#1<<(16-SINE_LOOP_BITS),d0
	bne.s	.loop

; -----------------------------------------------------------------------------
;	INIT TILES
; -----------------------------------------------------------------------------
	lea	PCREF(grid_values_1),a0
	move.l	a0,A6REF(tile_grid_1)
	lea	PCREF(grid_values_2),a0
	move.l	a0,A6REF(tile_grid_2)


; -----------------------------------------------------------------------------
;	EDGE MASKS
; -----------------------------------------------------------------------------
	moveq	#-8,d2			;stride	NOTE: reused

	; init LR masks
	moveq	#-8,d3			;stride (again)
	neg.w	d3			;(it's negated)
_init_edge_masks:
	lea	PCREF(mask_buffers+320*8),a0
	move.w	#319,d0				; d0 = counterf
.loop:
	; We put this data in backwards

	; now the mask parts
	move.w	d0,d1
	and.w	#$f,d1				;d1 = shift
	move.l	#$7fff8000,d2			;mask 0xffff
	lsr.l	d1,d2
	clr.w	-(a0)
	not.w	d2
	move.w	d2,-(a0)			; 0xffff >> (mod)

	; offset
	move.w	d0,d2
	lsr.w	#4,d2
	mulu.w	d3,d2				; *stride (usually 8)
	move.w	d2,-(a0)			; offset

	moveq	#-1,d2				;mask 0xffff
	lsr.w	d1,d2
	not.w	d2
	move.w	d2,-(a0)			; 0xffff >> (mod)
	dbf	d0,.loop

; -----------------------------------------------------------------------------
;	REPT CODE OFFSETS
; -----------------------------------------------------------------------------

;NEWSEC
	; Fill out the move.w x(a0),d0 loops so they pack better
	lea	PCREF(system_copy_increment),a1
	; TODO move these into data
	moveq	#8,d2				; d2 - increment

	lea	PCREF(effect_clear_1plane_fill),a0	; a0 - buffer to clear
	moveq	#20-1,d0			; d0 - counter (minus 1)
	move.w	#0,d1				; d1 - start value
	;move.w	#8,d2				; d2 - increment
	move.w	#4,d3				; d3 - amount to add to a0
	jsr	(a1)

	; Init the dither table at position 64
	;lea	A6REF(dither_desc_table+64*2),a0; a0 - buffer to clear
	;move.w	#64-1,d0			; d0 - counter (minus 1)
	;move.w	#0,d1				; d1 - start value
	;move.w	#1,d2				; d2 - increment
	;move.w	#2,d3				; d3 - amount to add to a0
	;jsr	(a1)

	;Set the offsets in poly_render
	lea	PCREF(poly_render_fill_rept),a0
	moveq	#20-1,d0		;count
	move.w	#20*8,d1		;starting value
	moveq	#-8,d2			;increment
	move.w	#4,d3			;stride
	bsr	system_copy_increment


	;NOTE This is needed first for the 3d render-to-texture to work!
	bsr	init_halftone_tiles_2

; -----------------------------------------------------------------------------
;	TILE GRAPHICS
; -----------------------------------------------------------------------------
	; Effects inits
	lea	PCREF(clip_setup_tiles),a1
	bsr	clip_setup

TILE_3D_START_SIZE	equ	NUM_TILES*3

	;move.w	#512/8,A6REF(camera_rot_z)
	; Set up objects
	move.w	#1,A6REF(world_object_count)

	; make sure starfish mesh exists
	move.l	#((600)<<16)|1100,A6REF(radius_2)
	bsr	script_update_starfish_shared

	; Loop through all the objects?
	clr.w	A6REF(world_objects+_object_mesh_index)
	move.w	#HEX_EXT*2,A6REF(world_camera_matrix+_matrix43_tz)	; distance
	move.w	#MESH_COUNT-1,d7
	A6REFLONG(tiles_3d,a0)
.loopmesh:
	clr.w	A6REF(camera_rot_z)		; reset rotation
	; ... and then size
	move.w	#TILE_3D_START_SIZE,A6REF(design_persp_scale_w)		; reset size
	moveq	#NUM_TILES-1,d0
.looptile:
	sub.w	#TILE_3D_START_SIZE/(NUM_TILES),A6REF(design_persp_scale_w)	; shrink
	PUSH_ALL
	move.l	a0,A6REF(poly_render_base)
	bsr	world_render
	POP_ALL
	add.w	#BYTES_PER_TILE,a0
	dbf	d0,.looptile

	add.w	#4,A6REF(world_objects+_object_mesh_index)	; next mesh
	dbf	d7,.loopmesh
	
; -----------------------------------------------------------------------------
;	SCREEN ADDRESSES
; -----------------------------------------------------------------------------

	; calc screen base
	move.l	a6,a2
	add.l	#screen_base-base_a6,a2
	move.l	a2,d2
	clr.b	d2
	move.l	d2,a2				; round to 256 bytes
	move.l	a2,A6REF(back_buffer)
	add.l	#BYTES_PER_SCREEN,a2
	move.l	a2,A6REF(front_buffer)
	move.l	a2,A6REF(active_bitplane_base)		; NO CHECK not needed except for 1st frame
	add.l	#BYTES_PER_SCREEN,a2

	CON_STRING	"\nbuffer A: "
	CON_HEX		back_buffer
	CON_STRING	"\nbuffer B: "
	CON_HEX		front_buffer

	; Effects inits
	lea	PCREF(clip_setup_screen),a1
	bsr	clip_setup

	; Low-res, we can burn some bytes here
	clr.w	-(a7)
	move.l	A6REF(back_buffer),-(a7)
	move.l	A6REF(front_buffer),-(a7)
	move.w	#5,-(a7)
	trap	#14          ; Call XBIOS
	lea	12(a7),a7

; -----------------------------------------------------------------------------
;	INTERRUPT INIT
; -----------------------------------------------------------------------------
_system_mfp_init:
	; set vbl, interrupts
	lea	PCREF(system_timer_vbl),a0
	move.l	a0,$70.w

	; Timer B!
	bclr	#3,$fffffa17.w			; AEI automatic end-of-interrupt
	; Disable all other interrupts

	ifeq	KEYBOARD
	clr.l 	$fffffa12.w			; Int mask set A/B
	else
	clr.b	$484.w				; keyclick of
	endif

	bsr	music_init
	move.w	#$2300,sr

; -----------------------------------------------------------------------------
;	MAIN LOOP
; -----------------------------------------------------------------------------

; -----------------------------------------------------------------------------
;	DESIGN SCRIPT UPDATE
; -----------------------------------------------------------------------------
design_loop:
	lea	PCREF(new_main_script),a0
main_loop:
; -----------------------------------------------------------------------------
;	DEBUG INIT FUNCTIONS
; -----------------------------------------------------------------------------

	ifne	KEYBOARD
	PUSH_ALL
keytest:
	; Key checks
	move.w	#11,-(a7)
	trap	#1
	addq.l	#2,a7
	tst.l	d0
	beq	.no_key

	; Read it
	move.w	#7,-(sp)     ; Offset 0
	trap    #1           ; GEMDOS
	addq.l	#2,sp        ; Correct stack

	lea	base_a6(pc),a6
	tst.b	d0
	beq.s	.no_key

	not.w	$ffff8240.w
	cmp.b	#'f',d0		; "F" - fast
	bne.s	.not_fast
	eor.w	#5,A6REF(debug_speed)
.not_fast:
	cmp.b	#'1',d0
	bne.s	.no_next
	move.l	A6REF(vbl_counter),d1
	move.l	d1,A6REF(script_counter)
.no_next

	cmp.b	#'s',d0
	bne.s	.no_shot

	bsr	debug_save_pi1
.no_shot:

	cmp.b	#'p',d0
	bne.s	.no_palette
	add.w	#16*2,A6REF(debug_palette_offset)
	cmp.w	#(palette_end-palette_start),A6REF(debug_palette_offset)
	bne.s	.no_palette
	clr.w	A6REF(debug_palette_offset)
.no_palette:

	cmp.b	#'c',d0
	bne.s	.no_cycle
	add.w	#4,A6REF(palette_shift)
.no_cycle:
	cmp.b	#'t',d0
	bne.s	.no_time
	not.w	A6REF(debug_show_time)
.no_time:
.no_key:
	POP_ALL
	endif	; DEBUG

	; "Script" handling
	; Wait for next vbl
	move.l	A6REF(vbl_counter),d1		; d1 - current vbl counter
	cmp.l	A6REF(script_counter),d1
	blt.s	.cont
.force_run_script:
	bsr	run_script			; script pointer is a0, updated
	ext.l	d0				; get return code
	beq	design_loop			; exit(0) == repeat
	add.l	d0,A6REF(script_counter)	; update next expected time
.cont:	
.skip_normal_script:
	PUSH_ALL
		
	; Update the one-poly plane (at half-rate)
	move.l	A6REF(back_buffer),a0
	if	BITPLANE_BLUR
	move.w	A6REF(frame_counter),d0
	and.w	#6,d0				; ignore bottom bit (use same bitplane on consecutive frames for buffer flip!)
	add.w	d0,a0
	endif
	move.l	a0,A6REF(poly_render_base)
	move.l	A6REF(active_bitplane_base),A6REF(active_bitplane_base_old)
	move.l	a0,A6REF(active_bitplane_base)

	; Frame-independent updates
	move.w	A6REF(last_vbl_frame_count),d6
	bra	.bottom
.frame_inc:
	; Update constant movements
	lea	A6REF(increment_start),a0
	lea	A6REF(to_increment_start),a1
	move.w	#(increment_end-increment_start)/2-1,d7
.inc:	move.w	(a0)+,d0
	add.w	d0,(a1)+
	dbf 	d7,.inc
.bottom:	
	dbf	d6,.frame_inc
	
; -----------------------------------------------------------------------------
;	EFFECT LAYER CALLING
; -----------------------------------------------------------------------------
	; Try each effect function in turn, according to the mask
	move.w	A6REF(active_effects_mask),d0
	lea	PCREF(render_funcs),a0
.fx_loop:
	move.w	(a0)+,d1			; read fx offset
	asr.w	d0
	bcc.s	.no_fx
	move.l	d0,-(a7)
	move.l	a0,-(a7)
	jsr	-2(a0,d1.w)			; -2 since we've just stepped
	move.l	(a7)+,a0
	move.l	(a7)+,d0			; resets d0 empty flag
.no_fx:
	bne.s	.fx_loop	

	; FLIP BUFFERS
	; set VIDEL for new "front" buffer
	movem.l A6REF(back_buffer),d2/d3
	exg.l	d2,d3
	movem.l	d2/d3,A6REF(back_buffer)
	lsr.w	#8,d3
	move.l	d3,$ffff8200.w
	addq.w	#1,A6REF(frame_counter)

	ifne	KEYBOARD
	tst.w	A6REF(debug_show_time)
	beq.s	.no_timeshow
	eor.w	#$700,$ffff8240.w
.no_timeshow:
	endif

	; WAIT VBL
	move.l	A6REF(vbl_counter),d0
.vbl_loop:
	cmp.l	A6REF(vbl_counter),d0
	beq.s	.vbl_loop

	; UPDATE FRAME COUNT
	move.l	A6REF(last_vbl_frame),d1
	move.l	d0,A6REF(last_vbl_frame)
	sub.l	d1,d0				; d0 = vbl diff
	move.w	d0,A6REF(last_vbl_frame_count)

	POP_ALL					;so design updates ok

	; LOOP
	bra	main_loop
	
; -----------------------------------------------------------------------------
;	DEBUGGING
; -----------------------------------------------------------------------------
	include	debug.s

render_funcs:
	dc.w	effect_tiledrop_random-*
	dc.w	effect_tiledrop_bass-*
	dc.w	effect_tile_circle-*
	dc.w	effect_clear_1plane-*
	dc.w	effect_render_tile-*
	dc.w	effect_render_script-*
	dc.w	world_render-*

; -----------------------------------------------------------------------------
; a2 - buffer to clear
; d0 - counter (minus 1)
;NEWSEC
system_memset_0:
	moveq.l #0,d1
.copy1:	move.l  d1,(a2)+
	dbf	d0,.copy1
	rts

; -----------------------------------------------------------------------------
; a0 - buffer to clear
; d0 - counter (minus 1)
; d1 - start value
; d2 - increment
; d3 - amount to add to a0
;NEWSEC
system_copy_increment:
.loop:	move.w	d1,(a0)
	add.w	d2,d1
	add.w	d3,a0
	dbf	d0,.loop
	rts

; -----------------------------------------------------------------------------
;	INTERNAL FUNCTIONS
; -----------------------------------------------------------------------------
;NEWSEC
system_timer_vbl:
	; Set 50Hz
	if	SET_50HZ
	move.b  #2,$ffff820a.w      ; 50Hz please!
	else
	clr.b	$ffff820a.w
	endif

	PUSH_ALL
        ifne    DEBUG
        lea     base_a6(pc),a6
        endif

	ifne	KEYBOARD
	move.w	A6REF(debug_speed),d0
.music_speed
	move.w	d0,-(a7)
	addq.l	#1,A6REF(vbl_counter)
	bsr	music_update
	move.w	(a7)+,d0
	dbf	d0,.music_speed
	else

	; Final version
	addq.l	#1,A6REF(vbl_counter)
	bsr	music_update
	endif

	;Write to shifter
	lea	$ffff8240.w,a0
	lea	PCREF(palette_lookup),a2

	ifne	KEYBOARD
	add.w	A6REF(debug_palette_offset),a2
	endif
	
	moveq	#16-1,d7
	moveq	#0,d0
	moveq	#0,d1
apply_colour:
	move.w	A6REF(frame_counter),d3
	subq.w	#1,d3
	; When we are on bitplane 3, we want to use the table as-is
	; When we are on bitplane 2, we want to shift up by 1...
	and.w	#6,d3		; calc shift amount
	lsr.w	d3		; 0-6 to 0-3
	eor.w	#3,d3

	move.w	A6REF(palette_shift),d4
.write_loop:
	move.b	d1,d2		; $00,$11... to $ff
	rol.b	d3,d2		; rotate around
	and.w	#$f,d2		; mask out
	add.w	d2,d2
	move.w	(a2,d2.w),d2

	ror.w	d4,d2		; palette shift
	move.w	d2,(a0)+	; get the palette

	add.w	#$11,d1
	dbf	d7,.write_loop

	;Check for palette cycle
        cmp.w   #FX_COUNTDOWN_VAL-1,TEXTREF(track_a_hi_data+track_fx_countdown_w)
	bne.s	.no_drum

	; There are 4 palettes
	; 0,32,64 (bottom 5 bits empty)
	; These are 4 shifts
	; 0,4,8,16 (bottom 2 bits empty)
	bsr	general_rand
	move.w	d0,d1

	and.w	#%1100,d1	;d1 == shift
	move.w	d1,A6REF(palette_shift)
	and.w	#%1100000,d0	;d0 == offset
	move.w	d0,A6REF(debug_palette_offset)
.no_drum:
	POP_ALL
	rte
	
; -----------------------------------------------------------------------------
;	RANDOM NUMBERS
; -----------------------------------------------------------------------------
; -----------------------------------------------------------------------------
;NEWSEC
general_rand:
	lea	A6REF(rand_seed),a0
get_rand_int:
	move.w	(a0),d0		; fetch seed
	; This actually generates quite good results now!
	mulu.w	#36969,d0
	add.w	#1,d0
	rol.w	#3,d0
	move.w	d0,(a0)
	and.l	#$ffff,d0	; clear top
	rts

; -----------------------------------------------------------------------------
;NEWSEC
randrange:
	bsr	get_rand_int			;d0 = new random value
	move.w	randrange_max(a0),d1
	sub.w	randrange_min(a0),d1		;d1=range
	and.w	#$7fff,d0
	muls.w	d1,d0				;NOTE: random value is 0-0xffff,effectively unsigned 0..1
	add.l	d0,d0
	swap	d0
	add.w	randrange_min(a0),d0
	rts

; -----------------------------------------------------------------------------
	;section	text
; -----------------------------------------------------------------------------

; -----------------------------------------------------------------------------
; -----------------------------------------------------------------------------
;
; IMPLEMENTATION NOTES;
;
; We store a matrix as rows, so use column vectors for simplicity, like so
;
; (1 2 3) . (x) = (x')  1x + 2y + 3z
; (4 5 6)   (y)   (y')
; (7 8 9)   (z)   (z')
;
; In theory we could use row vectors, then flip the matrices when storing.
;
; Matrix Operation order: gf(x) means "apply f first, then g"
;
; BACKFACE CULLING
;
; One method of implementing back-face culling is by discarding all tricamera_rot_xs where
; the dot product of their surface normal and the camera-to-tricamera_rot_x vector is greater
; than or equal to zero.

; -----------------------------------------------------------------------------
;NEWSEC
poly_persp_transform:
	; a0 - output buffer
	; a1 - input buffer
	; d0 - number of points
	; viewport_offset_x/y -- what to add
	; design_persp_scale_w
	; totally naive impl
	subq.w 	#1,d0
	moveq	#0,d4
	move.w	PCREF(design_persp_scale_w),d4	;d4 = divisor
	swap	d4
	movem.w	A6REF(viewport_offset_x),d6/d7
.loop:
	movem.w	(a1)+,d1/d2/d3			;d1 = x, d2 = y, d3 = z
	move.w	d3,(a0)+			;copy Z coord for later rejection check
	cmp.w	#200,d3				;NO CHECK use persp value?
	bgt.s	.no_clip_z
	move.w	#200,d3
.no_clip_z:
	move.l	d4,d5
	divu.w	d3,d5				;d4 = "size/z"

	; Apply it
	muls.w	d5,d1
	asl.l	#3,d1				;NO CHECK for 8-byte table size
	swap	d1
	muls.w	d5,d2
	swap	d2
	; apply offset
	add.w	d6,d1
	add.w	d7,d2
	move.w	d1,(a0)+
	move.w	d2,(a0)+
	dbf	d0,.loop
	rts

; -----------------------------------------------------------------------------
;NEWSEC
poly_get_sin_cos:
	; d0 = input camera_rot_x
	; d1 = output sinX
	; d2 = output cosX
	;add.w	d0,d0
	and.w	#SINE_MASK,d0
	lea	PCREF(sine_table),a1

	; rot2d = cos     0	sin
	;	  0	  1	0
	;         -sin    0	cos

	move.w	(a1,d0.w),d1			; d1 = sinX
	add.w	#COS_OFFSET,d0
	and.w	#SINE_MASK,d0
	move.w	(a1,d0.w),d2			; d2 = cosX
	rts

; -----------------------------------------------------------------------------
;NEWSEC
;poly_create_rot3d_matrix_y:
;	; Creates matrix rotated round Y
;	; a0 - created matrix
;	; d0 - camera_rot_x (0-$1fff)
;	bsr	poly_get_sin_cos
;	moveq	#0,d3				; d3 = 0
;
;	; row 0
;	move.w	d2,(a0)+			; cosX
;	move.w	d3,(a0)+			; 0
;	move.w	d1,(a0)+			; sinX
;
;	; row 1
;	move.w	d3,(a0)+			; 0
;	move.w	#$7fff,(a0)+
;	move.w	d3,(a0)+			; 0
;
;	; row 2
;	neg.w	d1
;	move.w	d1,(a0)+			; -sinX
;	move.w	d3,(a0)+			; 0
;	move.w	d2,(a0)+			; cosX
;	rts
;
; -----------------------------------------------------------------------------
;NEWSEC
poly_create_rot3d_matrix_z:
	; Creates matrix rotated round Z
	; a0 - created matrix
	; d0 - camera_rot_x (0-$1fff)
	bsr	poly_get_sin_cos
	moveq	#0,d3				; d3 = 0

	; row 0
	move.w	d2,(a0)+			; cosX
	move.w	d1,(a0)+			; sinX
	move.w	d3,(a0)+			; 0

	; row 1
	neg.w	d1
	move.w	d1,(a0)+			; -sinX
	move.w	d2,(a0)+			; cosX
	move.w	d3,(a0)+			; 0

	; row 2
	move.w	d3,(a0)+			; 0
	move.w	d3,(a0)+			; 0
	move.w	#$7fff,(a0)+
	rts

; -----------------------------------------------------------------------------
;NEWSEC
poly_create_rot3d_matrix_x:
	; Creates matrix rotated round x
	; a0 - created matrix (incremented)
	; d0 - camera_rot_x (0-$1fff)
	bsr	poly_get_sin_cos
	moveq	#0,d3				; d3 = 0

	; row 0
	move.w	#$7fff,(a0)+			; 1
	move.w	d3,(a0)+			; 0
	move.w	d3,(a0)+			; 0

	; row 1
	move.w	d3,(a0)+			; 0
	move.w	d2,(a0)+			; cosX
	move.w	d1,(a0)+			; sinX

	; row 2
	move.w	d3,(a0)+			; 0
	neg.w	d1
	move.w	d1,(a0)+			; -sinX
	move.w	d2,(a0)+			; cosX
	rts

; -----------------------------------------------------------------------------
;NEWSEC
;a0 - (unaffected)  output
;a1 - trashed
;a2 - trashed
;a4 - (incremented) input X/Z rotation angles
poly_create_xz_matrix:
	pea	(a0)
	move.w	(a4)+,d0
	lea	A6REF(tmp_matrix33_1),a0
	bsr	poly_create_rot3d_matrix_x
	
	move.w	(a4)+,d0
	lea	A6REF(tmp_matrix33_2),a0
	bsr	poly_create_rot3d_matrix_z
	move.l	(a7)+,a0
	
	;apply the result
	lea	A6REF(tmp_matrix33_1),a1
	lea	A6REF(tmp_matrix33_2),a2
	bra	poly_mul_matrix33			;a0 is now unaffected
		

; -----------------------------------------------------------------------------
;NEWSEC
poly_rotate_points_3d:
	PUSH_ALL
	; a0 - output buffer
	; a1 - input buffer
	; a2 - matrix 3x3
	; a3 - add x,y,z
	; d0 - number of points
	; totally naive impl
	subq.w 	#1,d0
.rot_loop:
	move.l	a2,a4
	movem.w (a1)+,d1/d2/d3			; d1 = x_input, d2 = y_input, d3 = z_input

	rept	2
	move.w 	d1,d4
	move.w 	d2,d5
	move.w 	d3,d6
	muls.w 	(a4)+,d4
	muls.w 	(a4)+,d5
	muls.w 	(a4)+,d6
	add.l 	d4,d5
	add.l	d5,d6
	add.l	d6,d6				;compensate for $7fff = 1
	swap	d6
	add.w	(a3)+,d6
	move.w 	d6,(a0)+
	endr

	; Z has additional offset
	; Last entry can trash d1/d2/d3
	muls.w 	(a4)+,d1
	muls.w 	(a4)+,d2
	muls.w 	(a4)+,d3
	add.l 	d1,d2
	add.l	d2,d3
	add.l	d3,d3				;compensate for $7fff = 1
	swap	d3
	add.w	(a3)+,d3
	move.w 	d3,(a0)+
	subq.l	#6,a3
	dbf	d0,.rot_loop
	POP_ALL
	rts

; -----------------------------------------------------------------------------
;NEWSEC
poly_mul_matrix33:
	move.w	#3,d0
	lea	PCREF(empty_translate),a3
	bsr	poly_rotate_points_3d
	rts
	
; -----------------------------------------------------------------------------
; 4x3 matrix multiplication.
; a0 - output -- incremented - matrix43 result
; a1 - input  -- unaffected -- LHS is the FIRST transform.
; a2 - input  -- unaffected -- RHS is the SECOND transform.
; a3 - trashed (used for translation)
; d0 - trashed
; This is equivalent to:-
; [a1 b1 c1  0][a2 b2 c2  0] = [a1.a2 + b1.d2 + c1.g2         a1.b2 + b1.e2 + c1.h2  a1.c2 + b1.f2 + c1.i2  0]
; [d1 e1 f1  0][d2 e2 f2  0]   [  ..                          ..                     ..                     0]
; [g1 h1 i1  0][g2 h2 i2  0]   [  ..                          ..                     ..                     0]
; [x1 y1 z1  1][x2 y2 z2  1]   [x1.a2 + y1.d2 + z1.g2 + x2 <-- SPECIAL CASE
; so it's like a 4x3 matrix multiply, plus x2,y2,z2 added to the last part.
matrix43_mul:
	; do 3 "rows" first for the rotation. The implicit final "0" in each row is skipped.
	; the 4th row involves the LHS matrix's translation
	move.w	#3,d0
	lea	A6REF(matrix43_mul_dummy_trans),a3
	bsr	matrix43_transform_internal		;a0 and a1 a incremented
	
	; Then add the translation offsets for the translation row.
	move.w	#1,d0
	lea	_matrix43_tx(a2),a3
	bra	matrix43_transform_internal
	
; -----------------------------------------------------------------------------
; matrix43 transform
;NEWSEC
matrix43_transform:
	; a0 - output buffer		<-- INCREMENTED
	; a1 - LHS (row3)		<-- INCREMENTED
	; a2 - RHS (matrix43)		<-- static
	; a3 - trashed (used for translation)
	; d0 - number of points
	; totally naive impl
	lea	_matrix43_tx(a2),a3
matrix43_transform_internal:
	subq.w 	#1,d0
.loop:	
	; Read the LHS row
	movem.w (a1)+,d1/d2/d3			; d1 = x_input, d2 = y_input, d3 = z_input

	;X - col 1
	move.w 	d1,d4
	move.w 	d2,d5
	move.w 	d3,d6
	muls.w 	_matrix43_11(a2),d4
	muls.w 	_matrix43_21(a2),d5
	muls.w 	_matrix43_31(a2),d6
	add.l 	d4,d5
	add.l	d5,d6
	add.l	d6,d6				;compensate for $7fff = 1
	swap	d6
	add.w	0(a3),d6
	move.w 	d6,(a0)+

	;Y - col 2
	move.w 	d1,d4
	move.w 	d2,d5
	move.w 	d3,d6
	muls.w 	_matrix43_12(a2),d4
	muls.w 	_matrix43_22(a2),d5
	muls.w 	_matrix43_32(a2),d6
	add.l 	d4,d5
	add.l	d5,d6
	add.l	d6,d6
	swap	d6
	add.w	2(a3),d6
	move.w 	d6,(a0)+

	;Z - col 3
	; Last entry can trash d1/d2/d3
	muls.w 	_matrix43_13(a2),d1
	muls.w 	_matrix43_23(a2),d2
	muls.w 	_matrix43_33(a2),d3
	add.l 	d1,d2
	add.l	d2,d3
	add.l	d3,d3
	swap	d3
	add.w	4(a3),d3
	move.w 	d3,(a0)+
	dbf	d0,.loop
	rts

; -----------------------------------------------------------------------------
;NEWSEC
poly_scan_convert:
	; input a0 = poly_ structure
	; input a1 = array of poly vertices (with last one duplicated)
	; input d0 = number of poly vertices
	move.w	d0,d7				; save vert count
	subq.w	#2,d0				; 1+1 since we skip the first
	ble.s	.done

	move.l	a1,a2				; a2 = saved_start_address

	; Find top and bottom Y values
	move.l	(a1)+,d5			; d2 = smallest_y
	move.w	d5,d6				; d5 = biggest_y
.cmp_loop:
	move.l	(a1)+,d3			; d3 = xxx yyy
	cmp.w	d3,d5				; old-new. if < 0, d5<d3
	ble.s	.not_small
	move.w	d3,d5				; d5 = new smallest_y
.not_small:
	cmp.w	d3,d6				;old-new
	bge.s	.not_big
	move.w	d3,d6
.not_big:
	dbf	d0,.cmp_loop

	; d5 now smallest value
	; d6 now biggest value
	sub.w	d5,d6
	move.w	d6,poly_height_w(a0)
	move.w	d5,poly_top_w(a0)

	; Current register status:
	; d5 = smallest_y
	; d6 = height (can be trashed)
	; d7 = saved vertcount
	; a0 = output poly scan
	; a2 = saved_start_address
	subq.w	#1,d7
.loop1
	; clockwise poly means that next vertex Y must be bigger or same
	movem.w (a2)+,d1/d3			; d1 = x1, d3 = y1
	movem.w (a2),d2/d4			; d2 = x2, d4 = y2
	lea	poly_roffs(a0),a1
	cmp.w	d3,d4
	beq.s	.skip_fill
	bge.s	.rhs
.lhs:
	exg	d1,d2				; swap points
	exg	d3,d4
	lea	poly_loffs(a0),a1		; use LHS buffer
.rhs:
	sub.w	d3,d4				; d4 = y2 - y1 (should be +ve)
	; input: d3 = y top
	; input: d4 = y offset (mu)
	; d1/d2 = x1, x2
	sub.w	d5,d3				; sub top value
	add.w	d3,d3				; double top Y offset
	add.w	d3,a1				; a1 = output buffer
	bsr	poly_fill_gradient
.skip_fill:
	dbf	d7,.loop1
.done:
	rts					; Should only get here if poly is flat

; -----------------------------------------------------------------------------
;NEWSEC
poly_fill_gradient:
	; fill bits of the scan buffer top-to-bottom
	; d1 - X1
	; d2 - X2
	; d4 - y2 - y1	(MUST NOT BE 0)
	; a1 - output buffer
	move.l	#$8000,d0		       	; use div rather than table
	divu.w	d4,d0
	sub.w	d1,d2			       	; d2 = xdiff			e.g. 380
	muls.w	d0,d2			       	; d2 = $7fff*xdiff/height	e.g. $0000f331 for above
	add.l	d2,d2			       	; handle signed mul
	moveq	#0,d3			       	; d3 = xfrac
	move.w	d2,d0			     	; d0 = gfrac
	swap	d2			    	; d2 = gwhole
	subq.w	#1,d4
.loop:
	move.w	d1,(a1)+		       	; 4 bytes each
	add.w	d0,d3			       	; add frac
	addx.w	d2,d1			       	; add whole
	dbf	d4,.loop		       	; TODO SLOW!
	rts

; -----------------------------------------------------------------------------
;NEWSEC
poly_render:
	; a0 = poly_ structure
	; a1 = L buffer
	; a2 = R buffer
	lea	PCREF(tiles_halftone),a4
	move.w	PCREF(poly_dither_offset),d0
	add.w	A6REF(base_dither_offset),d0	;for whole-world shading
	mulu.w	#HALFTONE_STRIDE*2,d0
	add.w	d0,a4

	move.w	poly_top_w(a0),d0		; d0 = top
	move.w	poly_height_w(a0),d7		; d7 = size
	beq	poly_render_done
	subq.w	#1,d7

	DBG_COL	#$300				; dark red - render
	lea	PCREF(mask_buffers),a3		; offset to centre...
	add.w	d0,a4				;offset into "top"
	add.w	d0,a4

	lea	poly_loffs(a0),a1
	lea	poly_roffs(a0),a2

	; calc screen pos
	move.l	A6REF(poly_render_base),a0

	mulu.w	A6REF(viewport_bytesperline),d0
	add.l	d0,a0				; offset top
	move.w	#$fff8,d1			; d1 mask for bottom 3 bits so we can point to lookup table
poly_render_row_loop:
	; This section here is a BIG time sink. Might be able to
	; use blitter to do shift/lookup, since they are just mask + shift...

	; NOTE: the outer edges take much more time than the "fill" bits,
	; and are constant!
	move.w	(a4)+,d0			; fetch dither mask
	move.w	(a1)+,d2			; d2 = LL
	move.w	(a2)+,d3			; d2 = RR
	cmp.w	d2,d3
	ble	poly_render_safety_clip		; safe clipping
	
	and.w	d1,d3				; d2 = masked RR
	and.w	d1,d2				; d2 = masked LL
	movem.w	2(a3,d3.w),d5/d6		; d5 = ROFF | d6 = RMASK
	movem.w	(a3,d2.w),d3/d4			; d3 = LMASK | d4 = LOFF
	lea	(a0,d4.w),a5			; a5 = a0+left screen addr
	sub.w	d5,d4				; d4 = LOFF-ROFF * 8 <-= this is negative!
	bge.s	poly_render_same_word		; handle same word case
	;bgt.s	.jmp_table_end			; safety code...!

	;proper masking
	and.w	d3,(a5)				; SOME OF THE WORST CODE EVER
	not.w	d3
	and.w	d6,(a0,d5.w)
	not.w	d6

	and.w	d0,d3				; masking
	and.w	d0,d6				; masking
	or.w	d3,(a5)				; draw leftmask
	or.w	d6,(a0,d5.w)			; draw right mask

	;DBG_COL	#$000
poly_render_jump_adjust:
	;This instruction is reserved to adjust from the *8 value in the tables
	;to the *4 value needed for the jump table	
	asr	#1,d4
	jmp	poly_render_jmp_table_end+4(pc,d4.w)	; jump table - offset
poly_render_fill_rept	equ	*+2
	;; WRITE LOOP
	rept	20
	move.w	d0,8(a5)			; 4 bytes each
	endr
poly_render_jmp_table_end:
poly_render_safety_clip:
	add.w	A6REF(viewport_bytesperline),a0
	dbf	d7,poly_render_row_loop
poly_render_done:
	rts
poly_render_same_word:
	or.w	d6,d3				; combine masks
	and.w	d3,(a5)
	not.w	d3
	and.w	d0,d3				; apply dither
	or.w	d3,(a5)
	add.w	A6REF(viewport_bytesperline),a0
	dbf	d7,poly_render_row_loop
	rts

; -----------------------------------------------------------------------------
; Render a series of objects in turn.
;NEWSEC
world_render:
	;Update the camera
	lea	A6REF(camera_rot_x),a4
	lea	A6REF(world_camera_matrix),a0				; a0 result
	bsr	poly_create_xz_matrix

	lea	A6REF(world_objects),a4
	move.w	A6REF(world_object_count),d7
	subq.w	#1,d7
.object_loop:
	PUSH_ALL
	; Update rotation
	move.w	(a4)+,d0
	move.w	(a4)+,d1
	add.w	d0,(a4)
	add.w	d1,2(a4)

	; Generate a local object matrix from the object's XZ rotations and translations
	; a4 - rotation X,Z (incremented)
	; a0 - output
	lea	PCREF(local_object_matrix),a0
	bsr	poly_create_xz_matrix
	move.w	(a4)+,_matrix43_tx(a0)				;copy translation
	move.w	(a4)+,_matrix43_ty(a0)
	move.w	(a4)+,_matrix43_tz(a0)

	; Create a local->camera matrix by combining world_object_matrices with world_camera_matrix
	move.l	a0,a1					; a1 LHS matrix -- local->world
	lea	world_camera_matrix(pc),a2		; a2 RHS matrix -- world->camera
	lea	A6REF(local_camera_matrix),a0		; a0 output
	bsr	matrix43_mul
	
	; Transform local object verts into "camera" space with final local->world->camera matrix
	; TODO reuse registers
	move.w	(a4)+,d0				; d0 == mesh description index
	move.w	(a4)+,d2				; d2 == shading index

	; We now use the same index for mesh and vertices
	lea	PCREF(mesh_desc_table),a1
	move.w	(a1,d0.w),d1				; lookup offset
	lea	(a1,d1.w),a5				; a5 == mesh pointer	
	move.w	2(a1,d0.w),d1				; lookup offset
	add.w	d1,a1					; a1 == vertex data

	; Can deref a4 here since we've pushed
	lea	PCREF(dither_desc_table),a4
	add.w	d2,a4					; a4 == dither data (doubled)
	add.w	d2,a4					; a4 == dither data

	lea 	PCREF(eyespace_verts),a0		; a0 output buffer
	lea	A6REF(local_camera_matrix),a2		; a2 input matrix
	move.w	_mesh_vert_count(a5),d0			; d0 vert count
	bsr	matrix43_transform			; apply
	bsr	mesh_posttransform_render
	POP_ALL

	lea	_object_size(a4),a4			; next object
	dbf	d7,.object_loop
	rts
	
;NEWSEC
mesh_posttransform_render:
; a5 == input object structure
; eyespace_verts -- final rotated points in camera space
	DBG_COL	#$006				; mid blue - persp transform

	; transform 3d to 2d
	lea	PCREF(screen_space_xyverts),a0	; a0 output buffer (xformed points)
	lea 	PCREF(eyespace_verts),a1	; a1 rotate input verts
	move.w	_mesh_vert_count(a5),d0		; d0 vert count
	bsr	poly_persp_transform

	DBG_COL	#$005				; dark blue -- rotate normals

	; Do poly loop
	lea	_mesh_poly_count(a5),a0		; a0 - start of poly data
	move.w	(a0)+,d7			; d7 poly count, followed by polydescs
	subq.w	#1,d7                           ; NO CHECK remove

; a4 - dither values
; eyespace_verts -- 3d point block
; screen_space_xyverts -- persp transformed points
tri_poly_loop:
	DBG_COL	#$004				; dark blue -- backface/setup
	move.w	(a4)+,A6REF(poly_dither_offset)	; read dither
	move.w	d7,-(a7)
	lea	PCREF(eyespace_verts),a1
        moveq   #0,d6
	move.b	(a0)+,d6			; d6 vert count
	move.w	d6,d7				; save point count

	; Unpack the transformed verts into a buffer
	lea	PCREF(screen_space_xyverts),a1
	lea	PCREF(final_poly),a2
	move.l	a2,a3
	subq.w	#1,d6
	moveq	#-1,d5				; d5 -- total Z-coord mask
.copy_loop:
        moveq   #0,d0
	move.b	(a0)+,d0			; vertex index
	and.w	0(a1,d0.w),d5                   ; update Z-coord mask
	move.l	2(a1,d0.w),(a2)+		; copy XY
	dbf	d6,.copy_loop

	;Z clip
	;"and" all the points to see if they all have the top bit set
	and.w	#$8000,d5			;check -ve bit
	bne.s	tri_behind_camera

	; Copy the first ever value to the end for looping
	move.l	(a3),(a2)+
_cross_product:
	; Perform 2d cross product
	movem.w	(a3),d0/d1/d2/d3/d4/d5		;x0,y0,x1,y1,x2,y2
	sub.w	d2,d0
	sub.w	d3,d1
	sub.w	d2,d4
	sub.w	d3,d5
	muls	d0,d5				;am
	muls.w	d1,d4				;bl
	cmp.l	d4,d5
	bge	tri_backfaced

	; CLIPPER GOES HERE
	DBG_COL	#$500				; light red -- scan convert
	move.w	d7,d0

	; At this point the poly is set up
	PUSH_ALL
	bsr	poly_clip_and_render
	POP_ALL
tri_behind_camera:
tri_backfaced:
_tri_continue:
	move.w	(a7)+,d7
	dbf	d7,tri_poly_loop
	rts

; -----------------------------------------------------------------------------
; Process just a single poly, already put in final_poly
; input: final_poly -- poly data
; d0 -- point count
;NEWSEC
poly_clip_and_render:

	; clip process
	; TODO optimise?
	lea	A6REF(final_poly),a0
	lea	A6REF(poly_clip_buf_1),a1
	lea	PCREF(test_clip_max_x),a2
	lea	PCREF(clip_max_x),a3
	bsr	tri_clipper

	lea	A6REF(poly_clip_buf_1),a0
	lea	A6REF(poly_clip_buf_2),a1
	lea	PCREF(test_clip_min_x),a2
	lea	PCREF(clip_min_x),a3
	bsr	tri_clipper

	lea	A6REF(poly_clip_buf_2),a0
	lea	A6REF(poly_clip_buf_1),a1
	lea	PCREF(test_clip_min_y),a2
	lea	PCREF(clip_min_y),a3
	bsr	tri_clipper

	lea	A6REF(poly_clip_buf_1),a0
	lea	A6REF(poly_clip_buf_2),a1
	lea	PCREF(test_clip_max_y),a2
	lea	PCREF(clip_max_y),a3
	bsr	tri_clipper

	tst.w	d0
	beq.s	.no_render

	lea	A6REF(poly_clip_buf_2),a1
	; d0  (points) returned from clipper

	; Scan convert + render
	; build scans
	lea	A6REF(poly_scanline_data),a0
	bsr	poly_scan_convert

	DBG_COL	#$400				; dark red - render
	; Render to screen
	lea	A6REF(poly_scanline_data),a0
	bsr	poly_render
.no_render:
	rts

; -----------------------------------------------------------------------------
;NEWSEC
tri_clipper:
	tst.w	d0
	ble	tri_clipper_complete
	; a0 = input buffer
	; a1 = output buffer
	; a2 = outside test func
	; a3 = clip func
	; d0 = num points, OUTPUT num new points
	; The clipper works iterately and shuttles between buffers
	; Let's do a test where the only clip is against X = (something)

	; NOTE: we do not decrement d0 since we jump to the end of the loop
	moveq	#0,d1
	; Prefetch the first point
	move.l	a1,a4				;save start location of output buffer
	move.l	(a0)+,d7			;d7 = X1:Y1
	jsr	(a2)				;d5 = outside
	bra	.shift_round
	; d6 is now inside value
.inside_loop:
	; Is the current point inside?
	tst.b	d4				;outside?
	bmi.s	.p1_outside

	move.l	d6,(a1)+			;copy last_point
	addq.w	#1,d1
.p1_outside:
	move.l	(a0)+,d7			;d7 = point X2:Y2
	jsr	(a2)				;d5 = outside
	; are we crossing the clip edge?
	eor.b	d5,d4				;d4 = outside^last_outside
	bpl.s	.no_clip
	movem.l	d5/d7,-(a7)
	jsr	(a3)				;generate clipped point

	move.w	d7,(a1)+			;output points
	move.w	d5,(a1)+
	movem.l	(a7)+,d5/d7			;restore current points
	addq.w	#1,d1
.no_clip:
	; Shuffle current pos to last pos
.shift_round:
	move.l	d7,d6				;d6 = last_point X1:Y2 (prev point)
	move.b	d5,d4				;d4 = last_outside
	dbf	d0,.inside_loop

	; Copy the first ever value to the end for looping
	move.l	(a4),(a1)+
	move.w	d1,d0
tri_clipper_complete:
	rts

;NEWSEC
test_clip_max_x:
	; check d7 for "outside" flag
	; returns d5
	move.w	A6REF(viewport_clip_max_x),d5
	swap	d5
	cmp.l	d5,d7
	sge 	d5
	rts
;NEWSEC
test_clip_min_x:
	; check d7 for "outside" flag
	; returns d5
	moveq	#0,d5
	move.w	A6REF(viewport_clip_min_x),d5
	swap	d5
	cmp.l	d5,d7
	slt 	d5
	rts
;NEWSEC
test_clip_max_y:
	; check d7 for "outside" flag
	; returns d5
	cmp.w	A6REF(viewport_clip_max_y),d7
	sge 	d5
	rts
;NEWSEC
test_clip_min_y:
	; check d7 for "outside" flag
	; returns d5
	cmp.w	A6REF(viewport_clip_min_y),d7
	slt 	d5
	rts
;NEWSEC
clip_max_x:
	move.w	A6REF(viewport_clip_max_x),d5
	bra	clip_x
clip_min_x:
	; d6 - X1:Y1
	; d7 - X2:Y2
	move.w	A6REF(viewport_clip_min_x),d5
clip_x:
	swap	d6
	swap	d7
	bsr	clip_y
	exg	d5,d7				;swap values
	rts
clip_min_y:
	move.w	A6REF(viewport_clip_min_y),d5
	bra	clip_y
clip_max_y:
	move.w	A6REF(viewport_clip_max_y),d5
	; FALLS THROUGH
clip_y:
	cmp.w	d6,d7
	bge.s	.no_swap
	exg	d6,d7
.no_swap
	; d5 - boundary value
 	; d6 - Y1:X1
 	; d7 - Y2:X2
	move.w	d7,d4
	sub.w	d6,d4				;d4 = x2 - x1
	move.w	d5,d3				;d3 = (clip value)
	sub.w	d6,d3				;d3 = CLIP - x1
	swap	d6				;d6 = y2
	swap	d7				;d7 = y1
	sub.w	d6,d7				;d7 = y2 - y1 (diff)
	; we want diff * (CLIP - x1) / (x2 - x1)
	muls	d3,d7
	divs 	d4,d7
	add.w	d6,d7				;d7 = interpolated Y
	; output d5 - clip value
	; output d7 - interpolated value
	rts

; -----------------------------------------------------------------------------
; Resets clip values to normal screen
;NEWSEC
clip_setup:
	move.w	(a1)+,A6REF(viewport_clip_min_x)
	move.w	(a1)+,A6REF(viewport_clip_min_y)
	move.w	(a1)+,A6REF(viewport_clip_max_x)
	move.w	(a1)+,A6REF(viewport_clip_max_y)
	move.w	(a1)+,A6REF(viewport_bytesperline)
	move.w	(a1)+,A6REF(viewport_offset_x)
	move.w	(a1)+,A6REF(viewport_offset_y)
	rts

; -----------------------------------------------------------------------------
;	TILE
; -----------------------------------------------------------------------------
;NEWSEC
init_halftone_tiles_2:
	lea	halftone_table(pc),a1
	lea	A6REF(tiles_halftone),a0
.new_pattern:
	move.b	(a1)+,d2		;d2 add per tile
	bmi.s	.done
	move.b	(a1)+,d3		;d3 rotate per line

	moveq	#16-1,d7		;16 reps, one per pixel fill

	moveq	#-1,d5 			;do one page to start with
	moveq	#0,d0			;d0 start shift amount
.pattern:
	add.w	#HALFTONE_STRIDE,d5	; do more this time, up to 16*HALFTONE_STRIDE
	move.w	d5,d6			; reset loop

	add.b	d2,d0			; next pos in pattern
	moveq	#1,d1			; create OR mask
	rol.w	d0,d1			; shift into place

	move.l	a0,a2			; reset current write pos
.word:	or.w	d1,(a2)+
	rol.w	d3,d1			; rotate mask
	dbf	d6,.word
	dbf	d7,.pattern

	lea	HALFTONE_STRIDE*2(a2),a0 ; update to next pattern location, skipping black
	bra.s	.new_pattern
.done:	rts
; NO CHECK expand output area!

halftone_table:
	dc.b	3,5			; good for greyscale
	dc.b	1,1
	dc.b	1,0
	dc.b	1,-1			; this is cool!
	dc.b	-1,-1

; -----------------------------------------------------------------------------

; -----------------------------------------------------------------------------
;	EFFECT ROUTINES
; -----------------------------------------------------------------------------
;NEWSEC
effect_clear_1plane:
	; Get the garbage data start
	;lea	tiles_halftone+160*16*2(pc),a1
	move.w 	#199,d7				; d7 row
	move.l	A6REF(poly_render_base),a0

effect_clear_1plane_fill	equ	*+4
.row_loop:
	;move.w	(a1)+,d5
	moveq	#0,d5
	rept 	20
	move.w 	d5,8(a0)
	endr
	lea	BYTES_PER_LINE(a0),a0
	dbf 	d7,.row_loop
	rts

; -----------------------------------------------------------------------------
;NEWSEC
tile_offsets:	dc.w	-TILE_Y*2,+TILE_Y*2+2,-2
tile_offsets2:	dc.w	-2,-4,0


effect_render_tile:
	movem.l	A6REF(tile_grid_1),a2/a3	; a2 == grid 1, a3 = grid 2
	; Swap
	exg.l	a2,a3
	movem.l	a2/a3,A6REF(tile_grid_1)

	; smooth the grid using the opposite entry
	move.w	#(TILE_Y*TILE_X)-1,d0
	move.w	A6REF(tile_damp),d4
	move.w	A6REF(tile_offset_offset),d5
	lea	tile_offsets(pc),a0
	movem.w	(a0,d5.w),d5/d6/d7
.smooth:
	move.w	(a3,d5.w),d1
	add.w	(a3,d6.w),d1
	add.w	(a3,d7.w),d1
	add.w	(a3)+,d1
	asr.w	#2,d1			; (sum of 4 neighbours) / 2

	sub.w	d4,d1
	spl	d3			; damping
	ext.w	d3
	and.w	d3,d1

	move.w	d1,(a2)+
	dbf	d0,.smooth
	
	; render
	move.l	A6REF(tile_grid_1),a2		; a2 == grid 1
	move.l	A6REF(poly_render_base),a0		; a0 screen base

	A6REFLONG((tiles_3d+(NUM_TILES-1)*(BYTES_PER_TILE)),a1)	; This is the last tile in the set

	move.w	tile_render_index(pc),d0
	mulu.w	#BYTES_PER_TILE*NUM_TILES/4,d0	; /4 is to compensate for indices being 0,4,8 etc
	add.l	d0,a1

	move.w	#TILE_X-1,d0
	move.l	#(-160*8)<<16,d1		; offset 1 : offset 0
	moveq	#-BYTES_PER_TILE,d2		; d2 - mask
.xloop:
	lea	(a0,d1.w),a3			; a3 - top of column
	moveq	#TILE_Y-1-1,d3
.yloop:
	; Lookup grid value
	move.l	a1,a4
	move.w	(a2)+,d4
	bge.s	.no_clip
	clr.w	d4
.no_clip:
	and.w	d2,d4				; mask
	sub.w	d4,a4

	; copy
o	set	0
	rept	16
	move.w	(a4)+,o(a3)
o	set	o+160
	endr
	lea	BYTES_PER_LINE*16(a3),a3
	dbf	d3,.yloop

	; On the last row, clear the value
	; to stop leakage
	clr.w	(a2)+
	swap	d1				; use other offset
	addq.l	#8,a0				; next column
	dbf	d0,.xloop
	rts

;NEWSEC
effect_tiledrop_random:
	move.w	A6REF(frame_counter),d0
	and.w	#%111,d0
	bne.s	.skip_drop

	bsr	general_rand
	divu.w	#TILE_Y*TILE_X,d0
	swap	d0
	add.w	d0,d0
	move.l	A6REF(tile_grid_1),a2
	lea	(a2,d0.w),a4

	; Do a random level, quite dark
	bsr	general_rand
	and.w	#BYTES_PER_TILE*128-1,d0
	add.w	#-BYTES_PER_TILE*32,d0
	or.w	d0,(a4)
.skip_drop:
	rts

;NEWSEC
effect_tiledrop_bass:
	; Read from the track data
	move.l	A6REF(tile_grid_1),a2
	bsr	general_rand
	and.w	#BYTES_PER_TILE*128-1,d0
	add.w	#-BYTES_PER_TILE*16,d0
	move.w	PCREF(track_d_data+track_curr_period),d1
	lsl.w	#1+2,d1
	or.w	d0,(a2,d1.w)
	rts


;NEWSEC
effect_tile_circle:
	move.w	#FX_COUNTDOWN_VAL,d0
	sub.w	track_a_hi_data+track_fx_countdown_w(pc),d0	; drum track
	move.w	d0,A6REF(tile_circle_rad)

	add.w	#13,A6REF(tile_circle_angle)
        lea     A6REF(tile_circle_rad),a0		;input scalar
	clr.w	d0
        lea     tile_circle_positions(pc),a1
        move.w  #0,d1		;stride-4
        move.w  A6REF(tile_circle_angle),d2    		; angle
        move.w  #SINE_SIZE/TILE_CIRCLE_COUNT,d3         ; angle stride
        move.w  #TILE_CIRCLE_COUNT-1,d7
        bsr     circle_calc

	move.w	#BYTES_PER_TILE*10,d3

	; Now transform into tile space (16x16)
        lea     tile_circle_positions(pc),a4
	move.l	A6REF(tile_grid_1),a2
	move.w	#TILE_CIRCLE_COUNT-1,d7
.object_loop2:
	movem.w	(a4)+,d1/d2
	add.w	#TILE_X/2,d1
	add.w	#TILE_Y/2,d2
	cmp.w	#TILE_X,d1
	bcc.s	.no		; treat -ve as offscreen
	cmp.w	#TILE_Y,d2
	bcc.s	.no		; treat -ve as offscreen
	mulu.w	#TILE_Y,d1
	add.w	d1,d2
	add.w	d2,d2
	move.w	d3,(a2,d2.w)	; write from object
.no:
	dbf	d7,.object_loop2
	rts

; -----------------------------------------------------------------------------
;NEWSEC
; a0 = script position
; returns a0 = next script position
run_script:
	lea	.script_yield(pc),a5
	jmp	(a0)			;jump to function, will jsr (a5) back
					;pushing the next instruction after that instruction to the stack...
.script_yield:
	move.l	(a7)+,a0		;fetch "return" address from stack as next to run
	rts

; -----------------------------------------------------------------------------
; NEWSEC
script_rout_set_obj:
; Fill object field with a value
; a0 - cmd data (offset - value - add)
	lea	A6REF(world_objects),a2
	move.l	(a7)+,a4		;get args
	movem.w	(a4)+,d0/d1/d2
					;d0 = write fieldl
					;d1 = value
					;d2 = add val 
	move.w	A6REF(world_object_count),d7
	subq.w	#1,d7
.loop:	move.w	d1,(a2,d0.w)
	lea	_object_size(a2),a2
	add.w	d2,d1
	dbf	d7,.loop
	pea	(a4)			;back to before
	rts

; -----------------------------------------------------------------------------
;NEWSEC
script_rout_rand_obj:
; field, min, max
; trashes a0!
	lea	A6REF(world_objects),a2
	move.l	(a7)+,a4		;get args
	movem.w	(a4)+,d0/d1/d2
				;d0 = write fieldl
				;d1 = min
				;d2 = mxa val 
	move.w	d1,A6REF(rand_tpm+randrange_max)
	move.w	d2,A6REF(rand_tpm+randrange_min)
	add.w	d0,a2			;select write field	
	
	lea	A6REF(rand_tpm),a0
	move.w	A6REF(world_object_count),d7
	subq.w	#1,d7
.rand_loop:
	bsr	randrange
	move.w	d0,(a2)
	lea	_object_size(a2),a2	;next obj
	dbf	d7,.rand_loop
	pea	(a4)			;back to before
	rts
	
; -----------------------------------------------------------------------------
; NEWSEC
script_rout_reset:	
	lea	A6REF(world_objects),a2
	move.w	#_object_size*MAX_OBJECTS/4-1,d0
	bra	system_memset_0

	;d7 - counter-1
	;d1 - sine pos
	;d2 - sine offset
	;d3 - scalar
	;d4 - stride
	;d5 - add value
	;d6 - trashed
	;a1 - trashed
	;a2 - buffer
; NEWSEC
scaled_sine_common:
	lea	A6REF(sine_table),a1
.sine_loop:
	and.w	#SINE_MASK,d1
	move.w	(a1,d1.w),d6		;get sine
	muls.w	d3,d6			;apply scale
	add.l	d6,d6
	swap	d6
	add.w	d5,d6
	move.w	d6,(a2)
	add.w	d2,d1			;offset pos
	add.w	d4,a2			;next obj
	dbf	d7,.sine_loop
	rts

; NEWSEC
circle_calc:
	;d0 - input stride - 2
	;d1 - output stride - 4
	;d2 - sine pos
	;d3 - sine offset
	;d7 - counter-1

	;a0 - input scalar array
	;a1 - output array (xy)
	;a2 - trashed
	lea	A6REF(sine_table),a2
.loop:
	move.w	d2,d4
	add.w	#COS_OFFSET,d4
	and.w	#SINE_MASK,d4		;TODO this breaks addition
	move.w	(a2,d4.w),d4		;scale cos
	muls.w	(a0),d4
	add.l	d4,d4
	swap	d4
	move.w	d4,(a1)+

	move.w	d2,d4
	and.w	#SINE_MASK,d4		;TODO this breaks addition
	move.w	(a2,d4.w),d4		;scale cos
	muls.w	(a0),d4
	add.l	d4,d4
	swap	d4
	move.w	d4,(a1)+

	add.w	d3,d2			;increment angle
	add.w	d0,a0			;increment input
	add.w	d1,a1			;increment output
	dbf	d7,.loop
	rts

; -----------------------------------------------------------------------------
;NEWSEC
effect_render_script:
	; move the objects
	move.w	A6REF(current_script),d0
	lea	script_base(pc,d0.w),a0
	; execute it
	jmp	(a0)

script_base:

; -----------------------------------------------------------------------------
;NEWSEC
effect_update_spikeball:
	; Reset all spikes when kick is sounded
;	tst.w	instrument_trigger_table+INSTRUMENT_KIK
;	beq.s	.no_reset
;	clr.w	instrument_trigger_table+INSTRUMENT_KIK
;	bsr	effect_init_spikeball
;.no_reset:

	; pick a random spike to reset
	if	1
	;TODO: use instruments
	;move.w	A6REF(frame_counter),d0
	;and.w	#1,d0
	;bne.s	.no_new

	;reset a line
	move.w	A6REF(last_spike),d0
	add.w	#_object_size,d0
	
	; keep in range
	move.w	A6REF(world_object_count),d1
	mulu.w	#_object_size,d1
	cmp.w	d1,d0				;TODO might be able to use mask
	ble.s	.ok
	clr.w	d0				;loop
.ok:	lea	A6REF(world_objects),a2
	move.w	d0,A6REF(last_spike)
	add.w	d0,a2
	bsr	effect_build_spike
.no_new:

	endif
	rts

; -----------------------------------------------------------------------------
;NEWSEC
effect_build_spike:
	;a2 - spike data
	; Generate the first point (the tip)
	; This is the only point that must be scaled
	bsr	general_rand
	move.w	d0,_object_rotation_x(a2)
	bsr	general_rand
	move.w	d0,_object_rotation_z(a2)
	rts
		
; -----------------------------------------------------------------------------
;NEWSEC
;	opt	p+
m4_sinclude(sound2/player.s)

m4_sinclude(scripts.s)
        ;section text
	
;	opt	p-

; -----------------------------------------------------------------------------
        ;section text
	even
; -----------------------------------------------------------------------------
;NEWSEC
;mesh descriptions
mesh_desc_table:
	; These need to be first for for the random effects
	dc.w		avena_a_mesh_desc-mesh_desc_table,	avena_a_verts-mesh_desc_table
	dc.w		avena_v_mesh_desc-mesh_desc_table,	avena_v_verts-mesh_desc_table
	dc.w		avena_e_mesh_desc-mesh_desc_table,	avena_e_verts-mesh_desc_table
	dc.w		avena_n_mesh_desc-mesh_desc_table,	avena_n_verts-mesh_desc_table
	dc.w		hex_mesh_desc-mesh_desc_table,		hex_mesh_verts-mesh_desc_table
	dc.w		starfish_mesh_desc-mesh_desc_table,	starfish_mesh_verts-mesh_desc_table
	dc.w		avena_4_mesh_desc-mesh_desc_table,	avena_4_verts-mesh_desc_table

	dc.w		dart_mesh_desc-mesh_desc_table,		dart_verts-mesh_desc_table
	dc.w		tri_mesh_desc-mesh_desc_table,		spike_baseline_verts-mesh_desc_table

	dc.w		dodec_mesh_desc-mesh_desc_table,	dodec_mesh_verts-mesh_desc_table

;NEWSEC
avena_a_mesh_desc:
	dc.w		11						; 8 vert count
	dc.w		4						; 10 poly count
	DEF_TRI		0,1,2
	DEF_HEX		1,10,9,6,4,2
	DEF_HEX		0,2,3,5,8,7
	DEF_QUAD	3,4,6,5
        even	

; -----------------------------------------------------------------------------
;NEWSEC
avena_a_verts:
	LETTER_VERT		-221+178,	-191+54,	0	;0
	LETTER_VERT		-221+271,	-191+54,	0	;1
	LETTER_VERT		-221+221,	-191+144,	0	;2
	
	LETTER_VERT		-221+192,	-191+225,	0	;3
	LETTER_VERT		-221+250,	-191+225,	0	;4
	
	LETTER_VERT		-221+168,	-191+290,	0	;5
	LETTER_VERT		-221+273,	-191+290,	0	;6

	LETTER_VERT		-221+61,	-191+329,	0	;7
	LETTER_VERT		-221+154,	-191+329,	0	;8
	LETTER_VERT		-221+286,	-191+329,	0	;9
	LETTER_VERT		-221+382,	-191+329,	0	;10

; -----------------------------------------------------------------------------
;NEWSEC
avena_v_mesh_desc:
	dc.w		7						; 8 vert count
	dc.w		3						; 10 poly count
	DEF_QUAD	0,1,4,5
	DEF_QUAD	2,3,6,4
	DEF_TRI		4,6,5
        even	

; -----------------------------------------------------------------------------
;NEWSEC
avena_v_verts:
	LETTER_VERT		-532+387,	-191+54,	0	;0
	LETTER_VERT		-532+475,	-191+54,	0	;1
	LETTER_VERT		-532+590,	-191+54,	0	;2
	LETTER_VERT		-532+682,	-191+54,	0	;2
	LETTER_VERT		-532+532,	-191+235,	0	;2
	LETTER_VERT		-532+483,	-191+329,	0	;2
	LETTER_VERT		-532+575,	-191+329,	0	;2

; -----------------------------------------------------------------------------
;NEWSEC
avena_e_mesh_desc:
	dc.w		12						; 8 vert count
	dc.w		4						; 10 poly count
	DEF_QUAD	0,1,2,3
	DEF_HEX		0,3,4,7,8,11
	DEF_QUAD	4,5,6,7
	DEF_QUAD	8,9,10,11


; -----------------------------------------------------------------------------
;NEWSEC
avena_e_verts:
	LETTER_VERT		-816+701,	-191+54,	0	;0
	LETTER_VERT		-816+933,	-191+54,	0	;1
	LETTER_VERT		-816+933,	-191+122,	0	;2
	LETTER_VERT		-816+791,	-191+122,	0	;3
	LETTER_VERT		-816+791,	-191+157,	0	;4
	LETTER_VERT		-816+920,	-191+157,	0	;5
	LETTER_VERT		-816+920,	-191+225,	0	;6
	LETTER_VERT		-816+791,	-191+225,	0	;7
	LETTER_VERT		-816+791,	-191+261,	0	;8
	LETTER_VERT		-816+933,	-191+261,	0	;9
	LETTER_VERT		-816+933,	-191+329,	0	;10
	LETTER_VERT		-816+701,	-191+329,	0	;11

; -----------------------------------------------------------------------------
;NEWSEC
avena_n_mesh_desc:
	dc.w		10						; 8 vert count
	dc.w		3						; 10 poly count
	DEF_PENT	0,1,5,7,6
	DEF_QUAD	1,4,8,5
	DEF_PENT	2,3,9,8,4
        even

; -----------------------------------------------------------------------------
;NEWSEC
avena_n_verts:
	LETTER_VERT		-1109+972,	-191+54,	0	;0
	LETTER_VERT		-1109+1048,	-191+54,	0	;1
	LETTER_VERT		-1109+1158,	-191+54,	0	;2
	LETTER_VERT		-1109+1240,	-191+54,	0	;3
	LETTER_VERT		-1109+1158,	-191+196,	0	;4
	LETTER_VERT		-1109+1055,	-191+187,	0	;5
	LETTER_VERT		-1109+972,	-191+329,	0	;6
	LETTER_VERT		-1109+1055,	-191+329,	0	;7
	LETTER_VERT		-1109+1164,	-191+329,	0	;8
	LETTER_VERT		-1109+1240,	-191+329,	0	;9

; -----------------------------------------------------------------------------
;NEWSEC
avena_4_mesh_desc:
	dc.w		11						; 8 vert count
	dc.w		3						; 10 poly count
	DEF_PENT	0,1,2,3,4
	DEF_QUAD	2,9,10,3
	DEF_QUAD	5,6,7,8
        even

; -----------------------------------------------------------------------------
;NEWSEC
avena_4_verts:
	LETTER2_VERT		-477+491,-463+317,0	;0
	LETTER2_VERT		-477+591,-463+317,0	;1
	LETTER2_VERT		-477+456,-463+476,0	;2
	LETTER2_VERT		-477+330,-463+548,0	;3
	LETTER2_VERT		-477+342,-463+488,0	;4
	LETTER2_VERT		-477+503,-463+433,0	;5
	LETTER2_VERT		-477+588,-463+433,0	;6
	LETTER2_VERT		-477+556,-463+596,0	;7
	LETTER2_VERT		-477+466,-463+596,0	;8
	LETTER2_VERT		-477+617,-463+476,0	;9
	LETTER2_VERT		-477+603,-463+548,0	;9

; -----------------------------------------------------------------------------
;NEWSEC
tri_mesh_desc:
	dc.w		3						; 8 vert count
	dc.w		2						; 10 poly count
	DEF_TRI		0,1,2
	DEF_TRI		2,1,0
        even

;NEWSEC
dart_mesh_desc:
	dc.w		3						; 8 vert count
	dc.w		2						; 10 poly count
	DEF_TRI		0,1,2
	DEF_TRI		2,1,0
        even

; -----------------------------------------------------------------------------
;NEWSEC
starfish_mesh_desc:
	dc.w		5*4						; 8 vert count
	dc.w		5*2						; 10 poly count

DEF_STAR macro
	dc.b		4,(\1+0)*6,(\1+1)*6,(\1+3)*6,(\1+2)*6			; num verts, NV * vert offsets
	endm
	;
	DEF_STAR	0
	DEF_STAR	2

	DEF_STAR	4
	DEF_STAR	6

	DEF_STAR	8
	DEF_STAR	10

	DEF_STAR	12
	DEF_STAR	14

	DEF_QUAD	16,17,19,18
	DEF_QUAD	18,19,1,0	;this one wraps round
        even
; -------------------------------------------------------------------------------
        
;NEWSEC
hex_mesh_desc:
	dc.w		6						; 8 vert count
	dc.w		1						; 10 poly count
	dc.b		6
	dc.b		5*6,4*6,3*6
	dc.b		2*6,1*6,0*6
        even

; -------------------------------------------------------------------------------
;NEWSEC
dodec_mesh_desc:
	dc.w		20						; 8 vert count
	dc.w		12						; 10 poly count

	;bottom
	DEF_PENT	0,8,4,14,12
	DEF_PENT	0,16,2,10,8
	DEF_PENT	8,10,6,18,4
	DEF_PENT	4,18,19,5,14
	DEF_PENT	14,5,9,1,12
	DEF_PENT	12,1,17,16,0

	;top
	DEF_PENT	3,11,7,15,13
	DEF_PENT	13,2,16,17,3
	DEF_PENT	3,17,1,9,11
	DEF_PENT	11,9,5,19,7
	DEF_PENT	7,19,18,6,15
	DEF_PENT	15,6,10,2,13
        even
	
; -----------------------------------------------------------------------------
;NEWSEC
DART_LENGTH		= $400
dart_verts:
	; Points down Z, flat along x==0
	dc.w		0,	0,		DART_LENGTH*3		; 0
	dc.w		0,	-DART_LENGTH,	-DART_LENGTH		; 1
	dc.w		0,	+DART_LENGTH,	-DART_LENGTH		; 2

;NEWSEC
spike_baseline_verts:
	dc.w		0,	0,	$2800			; 0
	dc.w		0,	-$600,	$000			; 1
	dc.w		0,	+$600,	$000			; 2
	
; -------------------------------------------------------------------------------
;NEWSEC
hex_mesh_verts:
	dc.w		-HEX_EXT,	0,		0		; 0
	dc.w		-HEX_EXT_COS,	+HEX_EXT_SIN,	0		; 1
	dc.w		+HEX_EXT_COS,	+HEX_EXT_SIN,	0		; 2
	dc.w		+HEX_EXT,	0,		0		; 3
	dc.w		+HEX_EXT_COS,	-HEX_EXT_SIN,	0		; 4
	dc.w		-HEX_EXT_COS,	-HEX_EXT_SIN,	0		; 5

; -----------------------------------------------------------------------------
;/*
; https://en.wikipedia.org/wiki/Regular_dodecahedron#Cartesian_coordinates
;    (±1, ±1, ±1)
;    (0, ±1/ϕ, ±ϕ)
;    (±1/ϕ, ±ϕ, 0)
;    (±ϕ, 0, ±1/ϕ)
;
;where ϕ = (1 + √5) / 2 (golden ratio)
;*/

;NEWSEC

dodec_ONE		equ		1000
dodec_PHI		equ		1618
dodec_1_PHI		equ		618
dodec_mesh_verts:
;    (±1, ±1, ±1) orange
	dc.w		-dodec_ONE,	-dodec_ONE,	-dodec_ONE	; 0
	dc.w		-dodec_ONE,	-dodec_ONE,	+dodec_ONE	; 1
	dc.w		-dodec_ONE,	+dodec_ONE,	-dodec_ONE	; 2
	dc.w		-dodec_ONE,	+dodec_ONE,	+dodec_ONE	; 3
	dc.w		+dodec_ONE,	-dodec_ONE,	-dodec_ONE	; 4
	dc.w		+dodec_ONE,	-dodec_ONE,	+dodec_ONE	; 5
	dc.w		+dodec_ONE,	+dodec_ONE,	-dodec_ONE	; 6
	dc.w		+dodec_ONE,	+dodec_ONE,	+dodec_ONE	; 7

;    (0, ±1/ϕ, ±ϕ) green
	dc.w		0,		-dodec_1_PHI,	-dodec_PHI	; 8
	dc.w		0,		-dodec_1_PHI,	+dodec_PHI	; 9
	dc.w		0,		+dodec_1_PHI,	-dodec_PHI	; 10
	dc.w		0,		+dodec_1_PHI,	+dodec_PHI	; 11
;    (±1/ϕ, ±ϕ, 0) blue
	dc.w		-dodec_1_PHI,	-dodec_PHI,	0		; 12
	dc.w		-dodec_1_PHI,	+dodec_PHI,	0		; 13
	dc.w		+dodec_1_PHI,	-dodec_PHI,	0		; 14
	dc.w		+dodec_1_PHI,	+dodec_PHI,	0		; 15
;    (±ϕ, 0, ±1/ϕ) pink
	dc.w		-dodec_PHI,	0,		-dodec_1_PHI	; 16
	dc.w		-dodec_PHI,	0,		+dodec_1_PHI	; 17
	dc.w		+dodec_PHI,	0,		-dodec_1_PHI	; 18
	dc.w		+dodec_PHI,	0,		+dodec_1_PHI	; 19

;NEWSEC

		;ARGB
col0	equ	$0500
col1	equ	$0240
col2	equ	$4032
col3	equ	$3005
palette_start:
palette_lookup:	
	; Fried
MTPAL	MACRO
	dc.w	((\1/$20)<<8)+((\2/$20)<<4)+((\3/$20))
	ENDM

	if 0
	; Fried's palette is a bit too nunty.
           MTPAL 0,0,0
           MTPAL 30,51,82
           MTPAL 35,57,15
           MTPAL 48,80,52
           MTPAL 185,75,48
           MTPAL 114,85,104
           MTPAL 118,90,36
           MTPAL 86,95,62
           MTPAL 140,102,0
           MTPAL 154,125,37
           MTPAL 156,128,7
           MTPAL 162,138,23
           MTPAL 223,136,22
           MTPAL 191,140,47
           MTPAL 193,143,16
           MTPAL 179,145,28
	endif

;One:
	dc.w	$0000
	dc.w	$6410
	dc.w	$0330
	dc.w	$4740

	dc.w	$4031
	dc.w	$5441
	dc.w	$3351
	dc.w	$6760

	dc.w	$2025
	dc.w	$4425
	dc.w	$3354
	dc.w	$6652

	dc.w	$3046
	dc.w	$7466
	dc.w	$5377
	dc.w	$6776

;Two:

	dc.w	$0000
	dc.w	$3014
	dc.w	$1023
	dc.w	$2324
	dc.w	$4032
	dc.w	$4334
	dc.w	$3433
	dc.w	$5634
	dc.w	$2042
	dc.w	$3332
	dc.w	$2343
	dc.w	$5643
	dc.w	$4441
	dc.w	$6641
	dc.w	$5742
	dc.w	$7763

;Three:
	dc.w	$0000
	dc.w	$1202
	dc.w	$5310
	dc.w	$4412
	dc.w	$2230
	dc.w	$2322
	dc.w	$4430
	dc.w	$5740
	dc.w	$4024
	dc.w	$3214
	dc.w	$5334
	dc.w	$5744
	dc.w	$5133
	dc.w	$5554
	dc.w	$6653
	dc.w	$6775

;Four:
	dc.w	$0000
	dc.w	$5003
	dc.w	$5201
	dc.w	$6123
	dc.w	$5320
	dc.w	$5134
	dc.w	$5223
	dc.w	$3436
	dc.w	$5540
	dc.w	$4254
	dc.w	$5433
	dc.w	$4445
	dc.w	$4543
	dc.w	$3455
	dc.w	$3554
	dc.w	$4667
      	;include	"colours.s"

	; Original
		; this is a table of where the 
		; We need to reverse the 
            dc.w	0			
            dc.w	0	      +col3	
            dc.w                 +col2		
            dc.w                 +col2+col3	
            dc.w	     col1		
            dc.w	     col1     +col3
            dc.w	     col1+col2	
            dc.w	     col1+col2+col3
            dc.w	col0			
            dc.w	col0	      +col3	
            dc.w	0	 +col2		
            dc.w	col0	 +col2+col3
 	    dc.w	col0+col1
 	    dc.w	col0+col1     +col3
 	    dc.w	col0+col1+col2
 	    dc.w	col0+col1+col2+col3
palette_end:
	    even


; -----------------------------------------------------------------------------
; -----------------------------------------------------------------------------
; CLIPPING/RENDER SETUP
; -----------------------------------------------------------------------------

;NEWSEC
clip_setup_screen:
	dc.w	CLIP_VAL_MIN_X	;viewport_clip_min_x
	dc.w	CLIP_VAL_MIN_Y	;viewport_clip_min_y
	dc.w	CLIP_VAL_MAX_X	;viewport_clip_max_x
	dc.w	CLIP_VAL_MAX_Y	;viewport_clip_max_y
	dc.w	BYTES_PER_LINE	;viewport_bytesperline
	dc.w	160*8		;viewport_offset_x
	dc.w	100		;viewport_offset_y
;NEWSEC
clip_setup_tiles:
	dc.w	0		;viewport_clip_min_x
	dc.w	0		;viewport_clip_min_y
	dc.w	15*8		;viewport_clip_max_x
	dc.w	16		;viewport_clip_max_y
	dc.w	2		;viewport_bytesperline
	dc.w	8*8		;viewport_offset_x
	dc.w	8		;viewport_offset_y
	
; -----------------------------------------------------------------------------

			;section	text
text_end		equ	*
			section bss
bss_start		equ	*

; -----------------------------------------------------------------------------
; TRI
			even
poly_scanline_data	ds.b		poly_SIZE

tmp_matrix33_1		ds.w		9
tmp_matrix33_2		ds.w		9

local_object_matrix:	ds.b		_matrix43_size	; temp when converting from object XZ to local->world
local_camera_matrix:	ds.b		_matrix43_size	; final local->camera transform

tmp_matrix:	ds.b	_matrix43_size			; NO CHECK matrix for tile rotation
	
poly_dither_offset	ds.w		1		; how far into tiles_halftone we index for each poly render
; -----------------------------------------------------------------------------
;	3D OBJECT DATA
; -----------------------------------------------------------------------------
eyespace_verts:		ds.w 		MAX_POINTS*3	; output from rotate
screen_space_xyverts:	ds.w		MAX_POINTS*2	; output from perspective xform

final_poly:		ds.w		MAX_POINTS*2	; base input
poly_clip_buf_1:	ds.w		MAX_POINTS*2	; clip output 1
poly_clip_buf_2:	ds.w		MAX_POINTS*2	; clip output 2

; -----------------------------------------------------------------------------
;	"DESIGN" DATA (twiddled by script)
; -----------------------------------------------------------------------------
; This is a static pointer in a6 so we can use position independent code
base_a6			equ		*		; relative offset base

design_buffer		equ 		*

; -----------------------------------------------------------------------------
; DESIGN START
; -----------------------------------------------------------------------------
world_object_count:	ds.w		1		; number of world objects to render
active_effects_mask:	ds.w		1		; Mask of effects to render in turn

;_matrix43_size	; camera world position/rotation
world_camera_matrix:	ds.w		9
to_increment_start:
world_translation_x:	ds.w		1		; KEEP THESE BEHIND THE MATRIX!
world_translation_y:	ds.w		1
world_translation_z:	ds.w		1
camera_rot_x		ds.w		1
camera_rot_z		ds.w		1
design_persp_scale_w:	ds.w		1
base_dither_offset:	ds.w		1		; push into halftone tables
tile_damp:		ds.w		1
tile_offset_offset:	ds.w		1
world_z_sine:		ds.w		1

; SPEEDS
increment_start:
world_translation_x_add:	ds.w		1		; KEEP THESE BEHIND THE MATRIX!
world_translation_y_add:	ds.w		1
world_translation_z_add:	ds.w		1
camera_rot_x_add:		ds.w		1
camera_rot_z_add:		ds.w		1
design_persp_scale_w_add:	ds.w		1
base_dither_offset_add:		ds.w		1		; push into halftone tables
tile_damp_add:			ds.w		1
tile_offset_offset_add:		ds.w		1
world_z_sine_add:		ds.w		1
increment_end:

current_script:		ds.w		1

			ifne KEYBOARD
debug_speed		ds.w		1
debug_show_time		ds.w		1
			endif

; -----------------------------------------------------------------------------
; Effect vars
last_spike:		ds.w		1		; offset of last updated spike

viewport_clip_min_x:	ds.w		1
viewport_clip_min_y:	ds.w		1
viewport_clip_max_x:	ds.w		1
viewport_clip_max_y:	ds.w		1
viewport_bytesperline:	ds.w		1
viewport_offset_x	ds.w		1		; offset to apply after persp transform
viewport_offset_y	ds.w		1
			even

tile_anim_offset:	ds.w		1		;base offset into tiles_3d

; UPDATING THINGS
tile_circle_angle:	ds.w		1
tile_circle_rad:	ds.w		1		;current radius for tile circle
dither_anim     	ds.w    	1

; Star radii
radius_1     		ds.w		1	;       600	     ;in
radius_3        	ds.w		1	;	900	     ;in
radius_2        	ds.w       	1	;	1200         ;out (updated by music)
radius_4        	ds.w       	1	;	4000         ;out

; tile graphics index, as a 0,1,2,3,4
tile_render_index:	ds.w		1

; -----------------------------------------------------------------------------
			even
rand_seed:		ds.w		1
frame_counter:		ds.w		1		; general counter
poly_render_base:	ds.l		1		; bitplane offset for world_render
active_bitplane_base:		ds.l	1		; render target for final image
active_bitplane_base_old:	ds.l	1		; previous render target for final image
tile_grid_1:		ds.l		1
tile_grid_2:		ds.l		1
base_tile_offset	ds.w		1		; which tileset to pick
mask_buffers:		ds.w		320*4		; LMASK | OFFSET | RMASK | 0
empty_translate		ds.w		3		; three zero values for translate matrix
rand_tpm:		ds.b		randrange_SIZE	; temporary for dynamic range gen

; SYSTEM
; TODO: can fill in with (a0)+
back_buffer:		ds.l		1		; MUST BE FIRST for swap to work
front_buffer:		ds.l		1

vbl_counter:		ds.l		1
script_counter:		ds.l		1		; expected vbl count for next script update
last_vbl_frame:		ds.l		1		; frame counter after last frame flip
last_vbl_frame_count:	ds.w		1
debug_palette_offset	ds.w		1

palette_shift:		ds.w		1		; cycle colours
			even
			
; -----------------------------------------------------------------------------
;	"WORLD" DATA
; -----------------------------------------------------------------------------
world_objects:		ds.b		MAX_OBJECTS*_object_size
tile_circle_positions:	ds.w	TILE_CIRCLE_COUNT*2

;-------------------------------------------------------------------------------
matrix43_mul_dummy_trans:
			ds.w	3

; -----------------------------------------------------------------------------
sine_table:		ds.w		SINE_COUNT
cos_table		equ		sine_table+COS_OFFSET

			ds.b		2048		; Stack space (we push a lot)
stack:

; These are always 0 at the moment
starfish_mesh_verts:	ds.w		3*4*STARFISH_TURNS*4	; 4 points per "turn"
dither_desc_table:	ds.w		64*2			; 0-63 is zero, 64-127 user defined

; -----------------------------------------------------------------------------
; Actual offsets into tile gfx

			ds.w		TILE_Y+2	; slop
grid_values_1:		ds.w		TILE_X*TILE_Y
			ds.w		TILE_Y+2	; slop
grid_values_2:		ds.w		TILE_X*TILE_Y
			ds.w		TILE_Y+2	; slop

tiles_halftone:		ds.w		NUM_HALFTONE_LINES+HALFTONE_STRIDE*17*NUM_PATTERNS

tiles_3d:		ds.w		16*NUM_TILES*MESH_COUNT		; rendered tiles

								; halftone tiles plus slop for "black"

			ds.b		256		 	; alignment slop
			
			ds.b		8*BYTES_PER_LINE	; for 2d tile alignment
screen_base:		ds.b		PADDED_LINES*BYTES_PER_LINE	; space for 2 screens
			ds.b		PADDED_LINES*BYTES_PER_LINE
