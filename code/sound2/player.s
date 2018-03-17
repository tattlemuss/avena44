; TODO
; extra commands
; multi track

SID		=	1

;$FF8800|byte |Read data/Register select				 |R/W
;       |     |0 Channel A Freq Low     	 BIT 7 6 5 4 3 2 1 0|
;       |     |1 Channel A Freq High    		 BIT 3 2 1 0|
;       |     |2 Channel B Freq Low     	 BIT 7 6 5 4 3 2 1 0|
;       |     |3 Channel B Freq High    		 BIT 3 2 1 0|
;       |     |4 Channel C Freq Low     	 BIT 7 6 5 4 3 2 1 0|
;       |     |5 Channel C Freq High    		 BIT 3 2 1 0|
;       |     |6 Noise Freq     		     BIT 5 4 3 2 1 0|
;       |     |7 Mixer Control  		 BIT 7 6 5 4 3 2 1 0|
;       |     |  Port B IN/OUT (1=Output) -----------' | | | | | | ||
;       |     |  Port A IN/OUT ------------------------' | | | | | ||
;       |     |  Channel C Noise (1=Off) ----------------' | | | | ||
;       |     |  Channel B Noise --------------------------' | | | ||
;       |     |  Channel A Noise ----------------------------' | | ||
;       |     |  Channel C Tone (0=On) ------------------------' | ||
;       |     |  Channel B Tone ---------------------------------' ||
;       |     |  Channel A Tone -----------------------------------'|
;       |     |8 Channel A Amplitude Control           BIT 4 3 2 1 0|
;       |     |  Fixed/Variable Level (0=Fixed) -----------' | | | ||
;       |     |  Amplitude level control --------------------+-+-+-'|
;       |     |9 Channel B Amplitude Control           BIT 4 3 2 1 0|
;       |     |  Fixed/Variable Level ---------------------' | | | ||
;       |     |  Amplitude level control --------------------+-+-+-'|
;       |     |10 Channel C Amplitude Control          BIT 4 3 2 1 0|
;       |     |  Fixed/Variable Level ---------------------' | | | ||
;       |     |  Amplitude level control --------------------+-+-+-'|
;       |     |11 Envelope Period High  	 BIT 7 6 5 4 3 2 1 0|
;       |     |12 Envelope Period Low   	 BIT 7 6 5 4 3 2 1 0|
;       |     |13 Envelope Shape			 BIT 3 2 1 0|
;       |     |  Continue -----------------------------------' | | ||
;       |     |  Attack ---------------------------------------' | ||
;       |     |  Alternate --------------------------------------' ||
;       |     |  Hold ---------------------------------------------'|
;       |     |   00xx - \____________________________________      |
;       |     |   01xx - /|___________________________________      |
;       |     |   1000 - \|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\      |
;       |     |   1001 - \____________________________________      |
;       |     |   1010 - \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\      |
;       |     |   1011 - \|-----------------------------------      |
;       |     |   1100 - /|/|/|/|/|/|/|/|/|/|/|/|/|/|/|/|/|/|/      |
;       |     |   1101 - /------------------------------------      |
;       |     |   1110 - /\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/      |
;       |     |   1111 - /|___________________________________      |
;

;-------------------------------------------------------------------------------
; Register 7 Mixer control

			opt	p+

;-------------------------------------------------------------------------------
;NEWSEC
music_init:
	;generate freqtab by smearing notes down an octave
	lea	freq_table_notes(pc),a0
	lea	12*2(a0),a1			;offset by octave
	moveq	#12*8-1,d7
.smear_octaves:
	move.w	(a0)+,d1
	lsr.w	d1				;shift up an octave
	move.w	d1,(a1)+
	dbf	d7,.smear_octaves

	;reset track positions
	move.w	#NUM_TRACKS-1,d7
	lea	music_start_positions(pc),a1
	lea	track_data(pc),a5
.init_track:
	move.w	(a1)+,d1
	lea	(a1,d1.w),a2
	move.l	a2,track_tune_ptr_l(a5)
	move.w	#1,track_step_w(a5)
	add.w	#track_sizeof,a5
	dbf	d7,.init_track

	if	SID
	lea	sid_interrupt(pc),a0
	move.l	a0,$134.w
	endif
	rts


;NEWSEC
	even
music_start_positions:
	dc.w	songdata_buzzer-*-2
	dc.w	songdata_c_lo-*-2
	dc.w	songdata_c_hi-*-2
	dc.w	songdata_b-*-2
	dc.w	songdata_a_lo-*-2
	dc.w	songdata_a_hi-*-2

;-------------------------------------------------------------------------------
; SOUND PLAYER
;NEWSEC
music_update_table:
	dc.b	0,8		; buzzer (shift out of way)
	dc.b	3,0		; track C lo
	dc.b	3,1		; track C hi
	dc.b	6,1		; track B lo
	dc.b	9,0		; track A lo
	dc.b	9,0		; track A hi
	dc.b	-1


;NEWSEC
	even
; NOTE: needs to be near player
			ds.b	1		; hack so period for channel B is on even boundary
register_values:	ds.b	NUM_YM_REGISTERS

			even
music_update:
	lea	track_data(pc),a5		;this shouldn't write to mixer
	lea	music_update_table(pc),a0
	moveq	#-1,d7
.music_loop:
	moveq	#0,d0
	move.b	(a0)+,d0			; d0 = track offset
	bmi.s	.done
	lea	register_values(pc,d0.w),a1	; a1 - track data
	lea	$ffff8800.w,a3
	bsr	music_update_track		; do it, write in values
	move.b	(a0)+,d0			; shift amount
	rol.w	d0,d7				; shift mixer reg
	add.w	#track_sizeof,a5
	bra.s	.music_loop
.done:	
	;---- TRACK END ----
	lea	register_values(pc),a1
	and.b	#%111111,d7			;don't nobble port registers
	move.b	d7,12(a1)			;write final mixer

	; Write Registers to YM
	move.w	sr,-(a7)
	move.w	#$2700,sr			;stop interrupts else it might happen with register-set!
	lea	registers(pc),a0
	moveq	#NUM_YM_REGISTERS-1,d0
.write: 
	move.b	(a0)+,(a3)
	move.b	(a1)+,2(a3)
	dbf	d0,.write
	move.w	(a7)+,sr

	if 	SID
	; Set up timer A
	move.w	(register_values+reg_period_offset)(pc),d0
	rol.w	#8,d0
	moveq	#0,d1
	cmp.w	#$80,d0
	ble.s	.kill_sid			;too high for SID

; The formula is
;    mfp_clock = 2457600.0
;    cpu_clock = 8021248.0	roughly, based on Hatari
;    ym_clock = cpu_clock / 4 / 16.0  # sound runs at this rate, approx 125332
; clock ratio = mfp_clock / ym_clock  (approx 19.60871)
;
; mfp_period = ym_period * clock_ratio
; mfp_divisor * mfp_data = ym_period * clock_ratio
; mfp_data = ym_period * [clock_ratio / mfp_divisor]
;
; So our our ratio is [clock_ratio / mfp_divisor]
;
; clock_ratio / mfp_timer_divisor / 2		(2 for up/down of volume over the cycle)
;
; e.g. for 100 timer,   19.6 / 2 / 100 			~ ~0.098
; For integer multiplication with swap: 0.204 * 65536 = 6425

; Results:
;6425.38512112	timer = 100
;10039.6642517  timer = 64
;6425.38512112*2	timer = 50

	; Turn Timer A on
	or.b	#$20,$FFFFFA07.w		;|byte |Interrupt Enable A
	or.b	#$20,$FFFFFA13.w		;|byte |Interrupt MASK A
	mulu.w	#6425*2,d0
	swap	d0
	move.b	d0,$fffffa1f.w			;timer A data

;	move.b	#%0110,$fffffa19.w		;timer A, divide by 100
;	move.b	#%0101,$fffffa19.w		;timer A, divide by 64
	move.b	#%0100,$fffffa19.w		;timer A, divide by 50
	; Poke the new volume into the timer code
	move.b	(register_values+reg_volume_offset)(pc),TEXTREF(sid_interrupt+4)
	rts

.kill_sid:
	; we need to kill the timer and write the volume direct
	clr.b	$fffffa19.w			;clear timer A control (no timer)
	; Direct volume write
	move.b	#YM_REG_VOLUME_B,$ffff8800.w
	move.b	(register_values+reg_volume_offset)(pc),$ffff8802.w
	endif
	rts

	if 	SID
sid_interrupt:
	move.l	#$09000000,$ffff8800.w		;wiggle channel B volume
	add.l	#sid_interrupt_2-sid_interrupt,$134.w
	rte
sid_interrupt_2:
	move.l	#$09000000,$ffff8800.w		;wiggle channel B volume
	sub.l	#sid_interrupt_2-sid_interrupt,$134.w
	rte
	endif

;-------------------------------------------------------------------------------
music_control_byte:
	; d1 - signed command byte (.w)
	jmp	.jump_table(pc,d1.w)
	bra.s	music_command_RETURN	; -10
	bra.s	music_command_GOSUB	; -8
	bra.s	music_command_SETREG	; -6
	bra.s	music_command_LOCAL	; -4
	bra.s	music_command_INST	; -2
.jump_table:

music_command_SETREG:
	move.b	(a2)+,(a3)
	move.b	(a2)+,$ffff8802.w
	bra.s	music_fetch_next
music_command_LOCAL:
	moveq	#0,d1
	move.b	(a2)+,d1			;offset
	move.b	(a2)+,(a5,d1.w)			;set value
	bra.s	music_fetch_next
music_command_INST:
	moveq	#0,d1
	move.b	(a2)+,d1			;get offset
	lea	instruments(pc),a4
	move.l	(a4,d1.w),track_inst_decay(a5)	;copy instrument data 
	bra.s	music_fetch_next
music_command_GOSUB:
	move.b	(a2)+,d1			;get offset hi
	lsl.w	#8,d1
	move.b	(a2)+,d1			;get offset lo
	move.l	a2,track_return_addr_l(a5)	;save return address after the gosub
	lea	songdata_base(pc),a2
	add.w	d1,a2
	bra.s	music_fetch_next
music_command_RETURN:
	move.l	track_return_addr_l(a5),a2	;get return address
	bra.s	music_fetch_next

music_update_track:
	;---- step checking
	subq.w	#1,track_step_w(a5)
	bne.s	music_update_instrument
	move.l	track_tune_ptr_l(a5),a2		;a2 = current tune pos

music_fetch_next:
	; fetch next control byte
	move.b	(a2)+,d1			;d1 = note/command
	ext.w	d1
	bmi.s	music_control_byte

	beq.s	.skip_trigger			;special case for zero-note
	; This is a note
	add.b	track_transpose_b(a5),d1		;apply TRANSP
	add.w	d1,d1				; NO CHECK can we keep this in a byte?

	; Trigger new note
	lea	freq_table(pc),a4
	move.w	(a4,d1.w),track_curr_period(a5)	; save pitch

	; Start envelope and trigger waveshape
	move.b	track_start_volume_b(a5),track_volume_b(a5)
	move.b	track_inst_length(a5),track_inst_countdown(a5)

	move.w	#FX_COUNTDOWN_VAL,track_fx_countdown_w(a5)
.skip_trigger:
	; Fetch time
	moveq	#0,d1				;can we remove this?
	move.b	(a2)+,d1
	asl.w	#1,d1				;2 ticks per step
	move.w	d1,track_step_w(a5)		;save counter
	move.l	a2,track_tune_ptr_l(a5)		;store tune, even if we skip the note

music_update_instrument:
	tst.b	track_inst_countdown(a5)	;is instrument completed/
	beq.s	.no_write			;don't overwrite previous
	subq.b	#1,track_inst_countdown(a5)

	and.b	track_mixer_mask(a5),d7		;turn on square/noise

	; 1) VOLUME
	move.b	track_volume_b(a5),d0		;bits 7-0
	lsr.b	#3,d0				;bits 4-0, use top bit for forcing buzzer
	move.b	d0,(a1)+			;volume

	move.w	track_curr_period(a5),d0
	move.b	d0,(a1)+			;set lo TODO if 4 regs can do move.w
	lsr.w	#8,d0
	move.b	d0,(a1)+			;set hi

	; update period + volume
	move.b	track_volume_b(a5),d0
	sub.b	track_inst_decay(a5),d0		;apply decay
	cmp.b	track_sustain_b(a5),d0
	; We need to detect if it's gone below track_sustain_b
	; *in signed space*, but this breaks buzzer where the
	; top bit is set
	bcc.s	.no_vol_clip
	move.b	track_sustain_b(a5),d0
.no_vol_clip:
	move.b	d0,track_volume_b(a5)

	; Pitch slide
	moveq	#0,d1
	move.b	track_inst_slide(a5),d1
	add.w	d1,track_curr_period(a5)
.no_write:
	; FX anim
	tst.w	track_fx_countdown_w(a5)
	beq.s	.no_dec
	sub.w	#1,track_fx_countdown_w(a5)
.no_dec:
	;write pitch to square regs TODO just write direct?
	;calc final square
	rts

;-------------------------------------------------------------------------------
;NEWSEC
registers:
	; buzzer track first
	dc.b	YM_REG_DUMMY
	dc.b	YM_ENV_PERIOD_LO	; A buzzer lo
	dc.b	YM_ENV_PERIOD_HI	; A buzzer hi

	dc.b	YM_REG_VOLUME_C		; C volume
	dc.b	YM_REG_PERIOD_LO_C	; C period lo
	dc.b	YM_REG_PERIOD_HI_C	; C period hi
reg_volume_offset	equ	*-registers
	if	SID
	dc.b	YM_REG_DUMMY		; B volume (this is done by SID, fallback for when SID is off)
	else
	dc.b	YM_REG_VOLUME_B
	endif	
reg_period_offset	equ	*-registers
	dc.b	YM_REG_PERIOD_LO_B	; B period lo
	dc.b	YM_REG_PERIOD_HI_B	; B period hi

	dc.b	YM_REG_VOLUME_A		; A volume
	dc.b	YM_REG_PERIOD_LO_A	; A period lo
	dc.b	YM_REG_PERIOD_HI_A	; A period hi

	dc.b	YM_REG_MIXER	 	; mixer (last)

NUM_YM_REGISTERS	equ	*-registers

	even
;NEWSEC

;-------------------------------------------------------------------------------
;NEWSEC
freq_table:
	dc.w	0			; dummy
freq_table_notes:
	;base octave values
	dc.w	3832	; C -1, 32.7Hz, raw: 3832.4
	dc.w	3617	; C#-1, 34.6Hz, raw: 3617.3
	dc.w	3414	; D -1, 36.7Hz, raw: 3414.3
	dc.w	3223	; D#-1, 38.9Hz, raw: 3222.7
	dc.w	3042	; E -1, 41.2Hz, raw: 3041.8
	dc.w	2871	; F -1, 43.7Hz, raw: 2871.1
	dc.w	2710	; F#-1, 46.2Hz, raw: 2709.9
	dc.w	2558	; G -1, 49.0Hz, raw: 2557.8
	dc.w	2414	; G#-1, 51.9Hz, raw: 2414.3
	dc.w	2279	; A -1, 55.0Hz, raw: 2278.8
	dc.w	2151	; A#-1, 58.3Hz, raw: 2150.9
	dc.w	2030	; B -1, 61.7Hz, raw: 2030.1
	ds.w	12*8

;NEWSEC
track_data:
track_d_data:			ds.b	track_sizeof		; buzzer
track_c_lo_data:		ds.b	track_sizeof
track_c_hi_data:		ds.b	track_sizeof
track_b_data:			ds.b	track_sizeof
track_a_lo_data:		ds.b	track_sizeof		; track A lo pri
track_a_hi_data:		ds.b	track_sizeof		; track A hi pri
NUM_TRACKS			=	(*-track_data)/track_sizeof


		even
songdata_base	equ	*

;NEWSEC
; This track is solely to set the buzzer values
songdata_buzzer:
		INST	INSTRUMENT_BASS_BUZZ
		TRANSP	0,6		; buzzer needs to be low
.loop:
		GOSUB	bass_shared
		JUMP	.loop
;NEWSEC
songdata_a_lo:
		; Square bass track
		TRANSP	0,0
		GOSUB	silent_64

		; Run with just buzzer first
		INST	INSTRUMENT_BASS_BUZZ
		LOCAL	track_start_volume_b,0|128	; adds buzzer!
		SETREG	YM_REG_ENV_SHAPE,$e
		GOSUB	bass_shared
		GOSUB	bass_shared
		GOSUB	bass_shared

		; Now add sqaure
		INST	INSTRUMENT_BASS			; has decay
		LOCAL	track_start_volume_b,(8*12)|128	; adds buzzer!
.loop:		
		GOSUB	bass_shared
		JUMP	.loop

;NEWSEC
songdata_a_hi:
		GOSUB	silent_64
		GOSUB	silent_64
		GOSUB	silent_64
		GOSUB	silent_64

		GOSUB	silent_64
		GOSUB	silent_64
		GOSUB	silent_64
		;GOSUB	silent_64

		;GOSUB	silent_64
		;GOSUB	silent_64

		INST	INSTRUMENT_SNARE
		LOCAL	track_start_volume_b,127
		LOCAL	track_transpose_b,32
		DUMMY	4
.loop
		; drum track
		NOTERAW	0,8
		NOTERAW	0,8
		NOTERAW	0,8
		NOTERAW	0,3
		NOTERAW	0,1+4

		;DUMMY	4
		NOTERAW	0,8
		NOTERAW	0,8
		NOTERAW	0,8
		NOTERAW	0,2
		NOTERAW	0,1
		NOTERAW	0,1+4
		JUMP	.loop

;NEWSEC
songdata_b:
		SETREG	YM_REG_NOISE_PERIOD,4	; soft noise
		GOSUB	silent_64
		GOSUB	silent_64
.loop:
		GOSUB	silent_64
		INST	INSTRUMENT_LEAD		; lead

		;repeating motif 1
		LOCAL	track_start_volume_b,8*12
		GOSUB	motif_0
		GOSUB	motif_0

		LOCAL	track_start_volume_b,8*13
		GOSUB	motif_0
		GOSUB	motif_0

		LOCAL	track_start_volume_b,8*14
		GOSUB	motif_0
		GOSUB	motif_1
		
		GOSUB	motif_1
		;repeating motif 2
		LOCAL	track_start_volume_b,8*15
		TRANSP	N_AS,2
		NOTE	0,0,2
		NOTE	3,1,2
		NOTE	2,1,2
		NOTE	3,1,2

		NOTE	0,1,2
		NOTE	3,1,2
		NOTE	2,1,2
		NOTE	3,1,2

		NOTE	0,1,2
		NOTE	3,1,2
		NOTE	2,1,2
		NOTE	3,1,2

		NOTE	5,1,2
		NOTE	7,1,2
		NOTE	10,1,2

		; New pattern here
		LOCAL	track_sustain_b,14*8

		NOTE	12,1,16			; last note should be on a 64-beat
		LOCAL	track_sustain_b,12*8

		;NOTE	12,1,8			; last note should be on a 64-beat
		DUMMY	8
		LOCAL	track_sustain_b,1	; fall away
		LOCAL	track_inst_countdown,128	; keep the note alive

		dc.b	0,160	; 40 * 4
		;DUMMY	40
		;DUMMY	8
		;DUMMY	16
		;DUMMY	16

		; ... but it seems to be early?
		GOSUB	silent_64
		GOSUB	silent_64

		; Melody part 1
		INST	INSTRUMENT_LEAD2		; reset lead
		LOCAL	track_start_volume_b,8*15
		LOCAL	track_sustain_b,8*12		; emphasize the SID

		DUMMY	2
		TRANSP	0,2
		rept	2
		NOTE	N_AS,0,12
		NOTE	N_C,1,2		;c
		NOTE	N_CS,1,2	;e
		NOTE	N_DS,1,4	;10
		NOTE	N_CS,1,2	;14
		NOTE	N_C,1,2		;16
		NOTE	N_CS,1,4	;18
		NOTE	N_C,1,2		;1c
		NOTE	N_AS,0,2
		NOTE	N_GS,0,12	;20

		NOTE	N_AS,0,2	;2c
		NOTE	N_C,1,2
		NOTE	N_CS,1,4
		NOTE	N_C,1,2
		NOTE	N_AS,0,2
		NOTE	N_C,1,4		;38
		NOTE	N_A,0,4		;3c
		endr

		SETREG	YM_REG_NOISE_PERIOD,$14	; harsh noise

		; Sparkly bit at end?
		INST	INSTRUMENT_WHISTLE
		LOCAL	track_start_volume_b,8*14
		LOCAL	track_inst_length,2
		;dc.b	0,1		; tiny delay
		TRANSP	0,7

		rept	64/8*2
		NOTE	N_AS,0,1
		NOTE	N_G,0,1
		NOTE	N_GS,0,1
		NOTE	N_FS,0,1

		NOTE	N_F,0,1
		NOTE	N_E,0,1
		NOTE	N_DS,0,1
		NOTE	N_CS,0,1
		;NOTE	N_B,0,1
		endr

		JUMP	.loop

;NEWSEC
songdata_c_lo:
		;Parper
		GOSUB	silent_64
		GOSUB	silent_64
.loop:		
		INST	INSTRUMENT_BASS2		; more like a parp

		LOCAL	track_start_volume_b,8*13
		TRANSP	N_AS,3
		NOTERAW	0,1
		NOTERAW	0,3
		DUMMY	4
		
		;LOCAL	track_start_volume_b,8*11
		;TRANSP	N_AS,oct
		;NOTERAW	0,1
		;NOTERAW	0,3

		;LOCAL	track_start_volume_b,8*12
		;TRANSP	N_AS,1
		;NOTE	0,0,1
		;NOTE	3,0,1
		;NOTE	2,0,1
		;NOTE	3,0,1

		JUMP	.loop

;NEWSEC
songdata_c_hi:
		LOCAL	track_start_volume_b,8*12		; not too loud!
		INST	INSTRUMENT_HIHAT	; hihat

		TRANSP	0,8

		; THIS IS 7 NOTES LONG
		NOTE	N_AS,0,1
		NOTE	N_GS,0,1
		NOTE	N_FS,0,1
		NOTE	N_E,0,1
		NOTE	N_DS,0,1
		NOTE	N_CS,0,1
		NOTE	N_B,0,1
		JUMP	songdata_c_hi

;NEWSEC
motif_0:
		; This is the motif without the rise at the end
		TRANSP	N_AS,1
		NOTE	0,1,2
		NOTE	3,1,2
		NOTE	2,1,2
		NOTE	3,1,2

		NOTE	0,1,2
		NOTE	3,1,2
		NOTE	2,1,2
		NOTE	3,1,2

		NOTE	0,1,2
		NOTE	3,1,2
		NOTE	2,1,2
		NOTE	3,1,2

		NOTE	0,1,2
		NOTE	3,1,2
		NOTE	2,1,2
		NOTE	3,1,2
		RETURN

;NEWSEC
motif_1:
		TRANSP	N_AS,1
		NOTE	0,1,2
		NOTE	3,1,2
		NOTE	2,1,2
		NOTE	3,1,2

		NOTE	0,1,2
		NOTE	3,1,2
		NOTE	2,1,2
		NOTE	3,1,2

		NOTE	0,1,2
		NOTE	3,1,2
		NOTE	2,1,2
		NOTE	3,1,2

		NOTE	5,1,2
		NOTE	3,1,2
		NOTE	2,1,2
		NOTE	3,1,2
		RETURN

;NEWSEC
bass_shared:
		; This is shared, but with octave transposes
		rept	8
		NOTE	N_AS,0,2
		endr
		rept	8
		NOTE	N_FS,0,2
		endr
		rept	8
		NOTE	N_GS,0,2
		endr
		rept	8
		NOTE	N_G,0,2
		endr
		RETURN

;NEWSEC
silent_64:	
		DUMMY	32
silent_32:
		DUMMY	32
		RETURN

	even

;NEWSEC
	even
instruments:
	;	DECAY 	SLIDE	CHANNELS	LEN
	dc.b	1,	0,	SQUARE,		255	; Lead
	dc.b	6,	255,	SQUARE&NOISE,	7	; Snare
	dc.b	4,	0,	SQUARE,		16	; Bass Square
	dc.b	255,	0,	SQUARE,		1	; Hihat/SpaceCadet
	dc.b	0,	0,	SQUARE,		3	; Whistle
	dc.b	2,	0,	SQUARE,		32	; Parp
	dc.b	1,	0,	SQUARE,		128	; Lead
	dc.b	0,	0,	SILENT,		255	; Bass Buzz	RECONCILE
