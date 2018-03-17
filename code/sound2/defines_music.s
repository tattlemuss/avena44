; Flag buzzer on volume step
BUZZER_VOL	equ		$10

YM_REG_PERIOD_LO_A	equ	0
YM_REG_PERIOD_HI_A	equ	1
YM_REG_PERIOD_LO_B	equ	2
YM_REG_PERIOD_HI_B	equ	3
YM_REG_PERIOD_LO_C	equ	4
YM_REG_PERIOD_HI_C	equ	5
YM_REG_NOISE_PERIOD	equ	6
YM_REG_MIXER		equ	7
YM_REG_VOLUME_A		equ	8
YM_REG_VOLUME_B		equ	9
YM_REG_VOLUME_C		equ	10
YM_ENV_PERIOD_LO	equ	11	;??!
YM_ENV_PERIOD_HI	equ	12	;??!

YM_REG_ENV_SHAPE	equ	13
YM_REG_DUMMY		equ	15	;15 Port B (Parallel port)

FX_COUNTDOWN_VAL	equ	50

;-------------------------------------------------------------------------------
			rsreset
track_inst_decay	rs.b	1		; FROM INSTRUMENT
track_inst_slide	rs.b	1		; FROM INSTRUMENT
track_mixer_mask	rs.b	1		; FROM INSTRUMENT
track_inst_length	rs.b	1		; FROM INSTRUMENT

track_curr_period	rs.w	1		; current period value
track_step_w	 	rs.w	1		; time to next instrument step
track_fx_countdown_w 	rs.w	1		; countdown for anim fx

track_pad		rs.b	1
track_volume_b		rs.b	1		; current volume
track_start_volume_b	rs.b	1		; volume at note start
track_inst_countdown	rs.b	1		; counts down instrument length
track_transpose_b	rs.b	1
track_sustain_b		rs.b	1		; min value for volume

track_tune_ptr_l	rs.l	1		; next control byte/note
track_return_addr_l	rs.l	1		; addr to pop after GOSUB/RTS
track_sizeof		rs.b	1

SQUARE	equ	$fe	; turn off bit 0
NOISE	equ	$f7	; turn off bit 3
SILENT	equ	$ff	; turn off nothing (both off)

INSTRUMENT_LEAD		=	0
INSTRUMENT_SNARE	=	1
INSTRUMENT_BASS		=	2
INSTRUMENT_HIHAT	=	3
INSTRUMENT_WHISTLE	=	4
INSTRUMENT_BASS2	=	5
INSTRUMENT_LEAD2	=	6
INSTRUMENT_BASS_BUZZ	=	7


N_C		=	0
N_CS		=	1
N_D		=	2
N_DS		=	3
N_E		=	4
N_F		=	5
N_FS		=	6
N_G		=	7
N_GS		=	8
N_A		=	9
N_AS		=	10
N_B		=	11


COMMAND_RETURN	=	-10
COMMAND_GOSUB	=	-8
COMMAND_SETREG	=	-6
COMMAND_LOCAL	=	-4
COMMAND_INST 	=	-2

TICKS_PER_STEP	=	2

NOTE		macro
		dc.b	\1+((\2)*12)+1,(\3)*8/TICKS_PER_STEP	; NOTE +1 for dummy note
		endm

DUMMY		macro
		dc.b	0,(\1)*4				; 0 interpreted as "skip"
		endm

NOTERAW		macro
		dc.b	(\1)+1,(\2)*8/TICKS_PER_STEP		; NOTE +1 for dummy data again
		endm

JUMP		macro
		; NOTE this is actually gosub now!
		dc.b	COMMAND_GOSUB			; command byte
		dc.b	((\1)-songdata_base)/256
		dc.b	((\1)-songdata_base)&255
		endm

SETREG		macro
		dc.b	COMMAND_SETREG			; command byte
		dc.b	\1,\2
		endm

LOCAL		macro
		dc.b	COMMAND_LOCAL			; command byte
		dc.b	\1,\2
		endm

TRANSP		macro					; Special local var poke
		dc.b	COMMAND_LOCAL
		dc.b	track_transpose_b,\1+((\2)*12)	; NOTE: -1 for dummy data compensation not needed!
		endm

INST		macro
		dc.b	COMMAND_INST			; command byte
		dc.b	(\1)*4
		endm

GOSUB		macro
		dc.b	COMMAND_GOSUB
		dc.b	((\1)-songdata_base)/256
		dc.b	((\1)-songdata_base)&255
		endm

RETURN 		macro
		dc.b	COMMAND_RETURN
		endm
