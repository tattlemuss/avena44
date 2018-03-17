

;NEWSEC
new_main_script:
        CLEAR   camera_rot_x
        CLEAR   camera_rot_z
        SET     #1,world_object_count
        SET     #MESH_HEX,tile_render_index
        bsr     script_rout_reset               ; loop cleanup

        ;bra     script_3d_parts
;#----------------------------------------------------------------
;#	TILES + MUSIC
;#----------------------------------------------------------------
        if      1
        SET     #BYTES_PER_TILE*1/3,tile_damp
        SET     #MESH_STARFISH/4,base_tile_offset
        EFFECTS fx_render_tile|fx_tiledrop_random
        YIELD   64*8
        EFFECTS fx_render_tile|fx_tiledrop_random|fx_tiledrop_bass
        YIELD   64*8*2
        
;#----------------------------------------------------------------
;#	AVENA LETTERS
;#----------------------------------------------------------------
        SET     #18000,world_translation_z
        ; Avena letters
        RESET_OBJ
        ;SET     #1,world_object_count
        CLEAR   tile_damp
        EFFECTS fx_render_tile|fx_tiledrop_random|fx_tiledrop_bass|fx_world_render

        ; make the letters black!
        SET     #16,base_dither_offset                  ; full black?

        SET     #5000,design_persp_scale_w
        SET     #-2,design_persp_scale_w_add            ; slowly recede

        SET_ONE _object_mesh_index,MESH_AVENA_A,0
        YIELD   8*16
        SET_ONE _object_mesh_index,MESH_AVENA_V,0
        YIELD   8*16
        SET_ONE _object_mesh_index,MESH_AVENA_E,0
        YIELD   8*16
        SET_ONE _object_mesh_index,MESH_AVENA_N,0
        YIELD   8*16

        SET     #MESH_STARFISH,tile_render_index

        SET     #5000-1800,design_persp_scale_w
        SET     #1,camera_rot_z_add
        SET_ONE _object_mesh_index,MESH_AVENA_A,0
        YIELD   8*16
        SET_ONE _object_mesh_index,MESH_AVENA_V,0
        YIELD   8*16
        SET_ONE _object_mesh_index,MESH_AVENA_E,0
        YIELD   8*16
        SET_ONE _object_mesh_index,MESH_AVENA_N,0
        YIELD   8*16

        SET     #5000-1800*2,design_persp_scale_w
        SET     #-1,design_persp_scale_w_add           ; slowly recede
        CLEAR   camera_rot_z_add
        CLEAR   camera_rot_z
        SET_ONE _object_mesh_index,MESH_AVENA_A,0
        YIELD   8*16
        SET_ONE _object_mesh_index,MESH_AVENA_V,0
        YIELD   8*16
        SET_ONE _object_mesh_index,MESH_AVENA_E,0
        YIELD   8*16
        SET_ONE _object_mesh_index,MESH_AVENA_N,0
        YIELD   8*16
        SET_ONE _object_mesh_index,MESH_AVENA_A,0
        YIELD   8*32

        ; "44"
        SET     #MESH_AVENA_4,tile_render_index
        SET     #+2,design_persp_scale_w_add           ; slowly zoom
        ;SET     #5000-1800*2,design_persp_scale_w
        SET     #2,world_object_count
        ;SET     #4,base_dither_offset
        SET_ONE _object_mesh_index,MESH_AVENA_4,0
        SET_ONE _object_mesh_index,MESH_AVENA_4,1
        SET_ONE _object_translation_x,-1200,0
        SET_ONE _object_translation_x,+1200,1

        YIELD   8*32
        ;SET     #4,base_dither_offset
        ;YIELD   8*16

main_script_loop:
        CLEAR   camera_rot_x
        CLEAR   camera_rot_z
        CLEAR   camera_rot_x_add
        CLEAR   camera_rot_z_add

        SCRIPT  rand_obj_script             ; set random object/tile
        CLEAR   base_dither_offset                  ; back to white
        CLEAR   design_persp_scale_w_add            ; slowly recede

;#----------------------------------------------------------------
;#	DRUM SPLASH
;#----------------------------------------------------------------
        ; Go nutso here with the tiles!
        SET     #MESH_HEX,tile_render_index
        EFFECTS fx_render_tile|fx_tiledrop_random|fx_tile_circle
        SET     #BYTES_PER_TILE*1/3,tile_damp
        YIELD   8*64
        endif

;#----------------------------------------------------------------
;#	STARFISH
;#----------------------------------------------------------------

        SET     #2,world_object_count
        bsr     script_rout_reset
        SET     #16000,world_translation_z
        SET_OBJ _object_mesh_index,MESH_STARFISH,0
        SET_OBJ _object_mesh_dither,64,0
        SET_OBJ _object_rotation_x,0,SINE_COUNT/2*2       ; flip 180 for the second one

        SCRIPT  script_update_starfish
        EFFECTS fx_clear|fx_world_render|fx_script

        SET     #1024,design_persp_scale_w
        SET     #2,camera_rot_z_add
        SET     #20,world_z_sine_add
        YIELD   64*8
        SET     #5,camera_rot_x_add
        YIELD   64*8

;#---------------------------------------------------------------
;#	STARS
;#----------------------------------------------------------------
        SET	#12,world_object_count
        RESET_OBJ
        SET	#256,design_persp_scale_w
        SET	#$3800,world_translation_z		; should be more than below 
        SET	#-4,camera_rot_z_add

        ; star positions
        RAND_OBJ	_object_translation_x,-$2000,$2000
        RAND_OBJ	_object_translation_y,-$2000,$2000
        RAND_OBJ	_object_translation_z,-$2000,$2000
        SET_OBJ	        _object_mesh_index,MESH_DART,0
        SCRIPT          starfield_script

        ; Try turning the clear on and off
        EFFECTS         fx_clear|fx_world_render|fx_script
        YIELD           48*8
        EFFECTS         fx_world_render|fx_script
        YIELD           16*8

;#----------------------------------------------------------------
        SET	        #-14,camera_rot_x_add                     ;extra swirl
        EFFECTS         fx_clear|fx_world_render|fx_script
        YIELD           16*8
        EFFECTS         fx_world_render|fx_script
        YIELD           16*8
        EFFECTS         fx_clear|fx_world_render|fx_script
        YIELD           16*8
        EFFECTS         fx_world_render|fx_script
        YIELD           16*8

;#----------------------------------------------------------------
;#	DRUM SPLASH 2
;#----------------------------------------------------------------
        ; Go nutso here with the tiles!
        SET     #6,tile_offset_offset
        EFFECTS fx_render_tile|fx_tiledrop_random|fx_tile_circle
        SET     #BYTES_PER_TILE*0/3,tile_damp
        YIELD   8*64

;#----------------------------------------------------------------
;#	DODEC
;#----------------------------------------------------------------
script_3d_parts:
        RESET_OBJ
        SET	#1,world_object_count
        SET	#180,design_persp_scale_w
        SET	#2000,world_translation_z
        SET	#-5,camera_rot_x_add
        SET	#-2,camera_rot_z_add
        SET_OBJ	_object_mesh_index, MESH_DODEC, 0		;dodec
        SET_OBJ _object_mesh_dither,64,0
        SCRIPT	dodec_script
        EFFECTS fx_world_render|fx_script                       ;script + world render
        YIELD   512

;#----------------------------------------------------------------
;#	SPIKEBALL
;#----------------------------------------------------------------
        CLEAR	 camera_rot_x_add
        SET	 #12,world_object_count
        SET	 #180,design_persp_scale_w

        RESET_OBJ
        SET_OBJ	 _object_mesh_index, MESH_SPIKE, 0

        ; Randomise rotation of spikes
        RAND_OBJ        _object_rotation_add_x, -$10, $10
        RAND_OBJ	_object_rotation_add_z, -$10, $10
        SCRIPT          spikeball_script
        EFFECTS	        fx_clear|fx_world_render|fx_script
        YIELD   512*2

;#---------------------------------------------------------------
;#	RANDOM TRIANGLES
;#----------------------------------------------------------------
        CLEAR   tile_offset_offset
        SET	#200,design_persp_scale_w
        SET	#12,world_object_count
        SET	#$3800,world_translation_z		; should be more than below 
        SET	#-4,camera_rot_z_add

        SCRIPT          rand_obj_script
        SET_OBJ	        _object_mesh_index,MESH_DART,0
        EFFECTS         fx_render_tile|fx_script|fx_world_render|fx_tiledrop_random
        ; Hack some positions here
        YIELD   512*2
        
;#----------------------------------------------------------------
;#	LOOP
;#----------------------------------------------------------------
        ; Loop handling

        bra     main_script_loop

;NEWSEC
; This dithers the polygons in the mesh
; TODO: complete

dodec_script:
        ; THIS COULD JUST BE RANDOM
        lea     dither_desc_table+64*2(pc),a2
        move.w  dither_anim(pc),d1
        move.w  #1500,d2                        ;sine offset
        move.w  #8,d3                           ;scale
        move.w  #2,d4                           ;stride
        move.w  #8,d5                           ;add
        move.w  #32-1,d7                        ;count
        bsr     scaled_sine_common
        add.w   #60,A6REF(dither_anim)

        ;change the pattern on the drum
        tst.w   TEXTREF(track_a_hi_data+track_fx_countdown_w)
        beq.s   .ok
        clr.w   TEXTREF(track_a_hi_data+track_fx_countdown_w)

        ; Randomize rotation
        ; TODO this doesn't seem very random...
        bsr     general_rand
        and.w   #15,d0
        sub.w   #8,d0
        move.w  d0,A6REF(camera_rot_z_add)

        bsr     general_rand
        move.w  d0,A6REF(camera_rot_x)

        ; Next texture
        sub.w   #1*17,A6REF(base_dither_offset)
        bpl.s   .ok
        add.w   #4*17,A6REF(base_dither_offset)
.ok
        rts

;NEWSEC
starfield_script:
	lea	A6REF(world_objects),a2
	move.w	A6REF(world_object_count),d7
	subq.w	#1,d7
.scroll_loop:
        move.w  _object_translation_z(a2),d0
        add.w   #600+$2000,d0                    ; convert to unsigned :(
        and.w   #$3fff,d0
        sub.w   #$2000,d0
        move.w  d0,_object_translation_z(a2)
	lea	_object_size(a2),a2	;next obj
	dbf	d7,.scroll_loop		
        rts

;NEWSEC
rand_obj_script:
        ;change the pattern on the drum
        tst.w   TEXTREF(track_a_hi_data+track_fx_countdown_w)
        beq.s   .nochange
        clr.w   TEXTREF(track_a_hi_data+track_fx_countdown_w)
        ; star positions
        RAND_OBJ	_object_translation_x,-$2000,$2000
        RAND_OBJ	_object_translation_y,-$2000,$2000
        RAND_OBJ	_object_translation_z,-$2000,$2000
        RAND_OBJ	_object_rotation_add_z,-$20,$20

        ; next tile 0-3
        add.w           #4,A6REF(tile_render_index)
        and.w           #%1100,A6REF(tile_render_index)
.nochange:
        rts

; -----------------------------------------------------------------------------
;NEWSEC
STARFISH_TURNS		=	5
STARFISH_ANGLE_STEP	=	SINE_SIZE/STARFISH_TURNS*1

script_update_starfish:
        bsr     script_sine_z
	;generate the ring values
	;TODO: these could be somewhere else
	;move.w	A6REF(starfish_anim),d0
	;add.w	#$10,A6REF(starfish_anim)

        ; We want something in the range 1000-4000
        ; So scale by (3000/75)
	move.w	track_c_lo_data+track_fx_countdown_w(pc),d0	; spangle 0-50 (small variance)
        muls.w  #(1000/75),d0
        add.w   #1500,d0
        move.w  d0,A6REF(radius_4)

	move.w	track_a_hi_data+track_fx_countdown_w(pc),d0	; drum 0-50 (big variance)
        muls.w  #(3000/75),d0
        add.w   #1700,d0
        move.w  d0,A6REF(radius_2)

	move.l	#((600)<<16)|900,A6REF(radius_1)

script_update_starfish_shared:
        ; Use the ring generator
        ; Static values:
	clr.w	d0                                      ;input stride (all constant)
        move.w  #4*6-4,d1		                ;output stride-4
        move.w  #STARFISH_ANGLE_STEP,d3                ;angle stride

        lea     PCREF(radius_1),a0	        	;input scalar
        lea     starfish_mesh_verts(pc),a1
        move.w  #0,d2                     	        ; angle
        move.w  #STARFISH_TURNS-1,d7
        jsr     TEXTREF(circle_calc)

        lea     PCREF(radius_2),a0	        	;input scalar
        lea     starfish_mesh_verts+6(pc),a1
        ;move.w  #0,d2                     	        ; angle
        move.w  #STARFISH_TURNS-1,d7
        jsr     TEXTREF(circle_calc)

        lea     PCREF(radius_3),a0	        	;input scalar
        lea     starfish_mesh_verts+12(pc),a1
        move.w  #STARFISH_ANGLE_STEP/2,d2    	        ; angle
        move.w  #STARFISH_TURNS-1,d7
        jsr     TEXTREF(circle_calc)

        lea     PCREF(radius_4),a0	        	;input scalar
        lea     starfish_mesh_verts+18(pc),a1
        ;move.w  #STARFISH_ANGLE_STEP/2,d2    	        ; angle
        move.w  #STARFISH_TURNS-1,d7
        jsr     TEXTREF(circle_calc)

        ;change the pattern on the *bass*
        tst.w   TEXTREF(track_a_lo_data+track_fx_countdown_w)
        beq   .ok
        clr.w   TEXTREF(track_a_lo_data+track_fx_countdown_w)

        ; Generate random 
        lea     dither_desc_table+64*2(pc),a2
        move.w  #16-1,d7
.loop:  bsr     general_rand
        and.w   #16*1-1,d0
        move.w  d0,(a2)+
        dbf     d7,.loop
.ok:
        rts

spikeball_script:
        bsr     script_sine_z
        bsr     effect_update_spikeball
        rts

script_sine_z:
        move.w  A6REF(world_z_sine),d0
        bsr     poly_get_sin_cos
        ; d1 is now sine
        asr.w   #3,d1           ;now 65K -> 8K
        add.w   #17000,d1
        move.w  d1,A6REF(world_translation_z)
        rts