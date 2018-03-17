
fx_tiledrop_random  =       1
fx_tiledrop_bass    =       2
fx_tile_circle      =       4
fx_clear            =       8
fx_render_tile      =	    16 ;before or after?
fx_script           =       32
fx_world_render     =       64
; post?

                        rsreset 
MESH_AVENA_A		rs.l    1
MESH_AVENA_V		rs.l    1
MESH_AVENA_E		rs.l    1
MESH_AVENA_N		rs.l    1
MESH_HEX 		rs.l    1
MESH_STARFISH		rs.l    1
MESH_AVENA_4		rs.l    1

MESH_DART		rs.l    1
MESH_SPIKE 		rs.l    1
MESH_DODEC 		rs.l    1
;MESH_TRI_Y_AXIS         rs.l    1

MESH_END                rs.l    1

MESH_COUNT              =       7

YIELD           MACRO
                move.w  #\1,d0
                jsr     (a5)
                ENDM

RETURN          MACRO
                jsr     (a5)
                ENDM

SET             MACRO
                move.w  \1,((\2)-base_a6)(A6)
                ENDM

CLEAR           MACRO
                clr.w   ((\1)-base_a6)(A6)
                ENDM

SET_OBJ         MACRO
                JSR     TEXTREF(script_rout_set_obj)
                dc.w    \1,\2,\3
                ENDM

SET_ONE         MACRO
                move.w  #\2,A6REF(world_objects+\1+(_object_size*\3))
                ENDM

RAND_OBJ        MACRO
                bsr     script_rout_rand_obj
                dc.w    \1,\2,\3                ; field min max
                ENDM

SCRIPT          MACRO
                move.w  #\1-script_base,current_script-base_a6(A6)
                ENDM

EFFECTS         MACRO
                move.w  #\1,active_effects_mask-base_a6(A6)
                ENDM

RESET_OBJ       MACRO
                bsr     script_rout_reset               ; loop cleanup
                ENDM

TILE_CIRCLE_COUNT	=	8
SQUARE_EXT		equ		$2600/2
SQUARE_EXT2		equ		$2900/2
NORMAL_SIZE		equ		$3000

; -----------------------------------------------------------------------------
; Max is 221 so 221*8 == 
LETTER_VERT	macro
		dc.w	(\1)*8,(\2)*8,(\3)*8
		endm

; -----------------------------------------------------------------------------
; Max is 221 so 221*8 == 
LETTER2_VERT	macro
		dc.w	(\1)*8,(\2)*8,(\3)*8
		endm


; sin 60 = 0.866
; cos 60 = 0.5
HEX_EXT			equ		221*8
HEX_EXT_SIN		equ		HEX_EXT*866/1000
HEX_EXT_COS		equ		HEX_EXT*500/1000
