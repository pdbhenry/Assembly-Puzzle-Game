SECTION "player", ROM0

include "include/gb/constants.inc"

move_eyes::
	cp 4					;If a=0-3, eyes are normal
	jr c, move_eyes_normal
	sub 4
	add a						;Sub 4, then double.
	ld b, a
	ld a, 16
	add b						;This translates to tile number.
	jr change_eye_sprites
move_eyes_normal:
	ld a, 5
change_eye_sprites:
	call lcd_wait
	ld hl, $C416				;Left eye tile number addr
	ld [hl], a
	inc a
	ld hl, $C41A				;Right eye tile number addr
	ld [hl], a
	
	ret


;----------------------------------------------------------------------------

;Updates Sirloin's sprites' positions based on spr_x and spr_y
update_sprite_pos::
	ld a, [spr_y]
	ld hl, $C400
	ld [hl], a
	ld hl, $C404
	ld [hl], a
	add 8
	ld hl, $C408
	ld [hl], a
	ld hl, $C40C
	ld [hl], a
	
	ld a, [spr_x]
	ld hl, $C401
	ld [hl], a
	ld hl, $C409
	ld [hl], a
	add 8
	ld hl, $C405
	ld [hl], a
	ld hl, $C40D
	ld [hl], a
	
	ret


;Called when switching gravity. b is stored with original orientation
;With that, we know where Goblin will be facing (started with gravity down,
;Goblin will be facing up on a left or right wall). 'moving' is altered accordingly
wall_transfer::
	ld a, TURNING_DELAY
	ld [turning_corner], a	
	ld l, a					;If set, we are mid-anim.
	
	ld a, b
	cp 0
	jr z, wall_transfer_down
	cp 1
	jr z, wall_transfer_up
	cp 2
	jr z, wall_transfer_right
	jr wall_transfer_left
	
wall_transfer_down:
	ld a, %11101011						;Up
	jr change_sprite_setup
wall_transfer_up:
	ld a, %11100111						;Down
	jr change_sprite_setup
wall_transfer_right:
	ld a, %11101101						;Left
	jr change_sprite_setup
wall_transfer_left:
	ld a, %11101110						;Right

change_sprite_setup:
	ld [facing], a
	ld h, 0
	
;if h=0, not using walking leg sprites. if h=1, we are turning a corner
change_sprite::							;Filtering by orientation
	ld de, $0100						;d matters in change_sprite_2 and
										;informs how we jump in tile numbers
	ld a, [facing]						;e tells us if tiles are flipped. (init to 0)
	ld b, a
	ld a, [ori]
	cp 0
	jr z, change_sprite_down
	cp 1
	jr z, change_sprite_up
	cp 2
	jr z, change_sprite_right
	jr change_sprite_left
	
	
change_sprite_down:						;Filtering by direction being faced
	bit 0, b
	jp z, change_sprite_down_right
	jp change_sprite_down_left
change_sprite_up:
	bit 0, b
	jp z, change_sprite_up_right
	jp change_sprite_up_left
change_sprite_right:
	bit 3, b 
	jp z, change_sprite_right_down
	jp change_sprite_right_up
change_sprite_left:
	bit 3, b 
	jp z, change_sprite_left_down
	jp change_sprite_left_up
	
	
change_sprite_down_right:				;Setting sprite tile order & attribs.
	ld a, l
	or a
	jp nz, change_sprite_corner_down_right
	
	ld b, SIR_TL
	ld c, SIR_BL
	ld a, h
	cp 0
	jp z, change_sprite_2
	ld c, SIR_WALK_BL
	jp change_sprite_2
change_sprite_down_left:
	ld a, l
	or a
	jp nz, change_sprite_corner_down_left
	
	ld b, SIR_TR
	ld c, SIR_BR
	ld d, -1
	ld e, %00100000
	ld a, h
	cp 0
	jp z, change_sprite_2
	ld c, SIR_WALK_BR
	jp change_sprite_2
change_sprite_up_right:
	ld a, l
	or a
	jp nz, change_sprite_corner_up_right
	
	ld b, SIR_BL
	ld c, SIR_TL
	ld e, %01000000
	ld a, h
	cp 0
	jp z, change_sprite_2
	ld b, SIR_WALK_BL
	jp change_sprite_2
change_sprite_up_left:
	ld a, l
	or a
	jp nz, change_sprite_corner_up_left
	
	ld b, SIR_BR
	ld c, SIR_TR
	ld d, -1
	ld e, %01100000
	ld a, h
	cp 0
	jp z, change_sprite_2
	ld b, SIR_WALK_BR
	jp change_sprite_2
change_sprite_right_up:
	ld a, l
	or a
	jp nz, change_sprite_corner_right_up
	
	ld b, SIR_SIDE_TL
	ld c, SIR_SIDE_BL
	ld a, h
	cp 0
	jp z, change_sprite_2
	ld d, 4
	jp change_sprite_2
change_sprite_right_down:
	ld a, l
	or a
	jp nz, change_sprite_corner_right_down
	
	ld b, SIR_SIDE_BL
	ld c, SIR_SIDE_TL
	ld e, %01000000
	ld a, h
	cp 0
	jp z, change_sprite_2
	ld d, 3
	jp change_sprite_2
change_sprite_left_up:
	ld a, l
	or a
	jp nz, change_sprite_corner_left_up
	
	ld b, SIR_SIDE_TR
	ld c, SIR_SIDE_BR
	ld d, -1
	ld e, %00100000
	ld a, h
	cp 0
	jp z, change_sprite_2
	ld b, SIR_SIDE_WALK_TR			;Tile $0A
	ld c, SIR_SIDE_WALK_BR			;Tile $0B
	ld d, -4
	jp change_sprite_2
change_sprite_left_down:
	ld a, l
	or a
	jp nz, change_sprite_corner_left_down
	
	ld b, SIR_SIDE_BR
	ld c, SIR_SIDE_TR
	ld d, -1
	ld e, %01100000
	ld a, h
	cp 0
	jp z, change_sprite_2
	ld b, SIR_SIDE_WALK_BR
	ld c, SIR_SIDE_WALK_TR
	ld d, -3
	jp change_sprite_2
	
	
change_sprite_corner_down_right:
	ld b, SIR_SIDE_TURN_BR
	ld c, SIR_SIDE_TURN_TR
	ld d, -1
	ld e, %01100000
	jr change_sprite_2
change_sprite_corner_down_left:
	ld b, SIR_SIDE_TURN_BL
	ld c, SIR_SIDE_TURN_TL
	ld e, %01000000
	jr change_sprite_2
change_sprite_corner_up_right:
	ld b, SIR_SIDE_TURN_TR
	ld c, SIR_SIDE_TURN_BR
	ld d, -1
	ld e, %00100000
	jr change_sprite_2
change_sprite_corner_up_left:
	ld b, SIR_SIDE_TURN_TL
	ld c, SIR_SIDE_TURN_BL
	jr change_sprite_2
change_sprite_corner_right_up:
	ld b, SIR_TURN_TL
	ld c, SIR_TURN_BL	
	jr change_sprite_2
change_sprite_corner_right_down:
	ld b, SIR_TURN_BL	
	ld c, SIR_TURN_TL
	ld e, %01000000 
	jr change_sprite_2
change_sprite_corner_left_up:
	ld b, SIR_TURN_TR	
	ld c, SIR_TURN_BR
	ld d, -1
	ld e, %00100000
	jr change_sprite_2
change_sprite_corner_left_down:
	ld b, SIR_TURN_BR
	ld c, SIR_TURN_TR
	ld d, -1
	ld e, %01100000
	
change_sprite_2:
	ld hl, $C402
	ld a, b
	ldi [hl], a
	;ld a, [hl]						;Keeping priority
	;and %10000000
	;or e
	;ld e, a
	ld [hl], e
	
	add a, d
	
	ld hl, $C406
	ldi [hl], a
	ld [hl], e
	
	
	ld a, d							;|
	cp 4 							;|
	jr z, dec_tile_num_step
	cp 3
	jr z, inc_tile_num_step
	cp -3
	jr z, dec_tile_num_step
	cp -4
	jr z, inc_tile_num_step
	jr change_sprite_3
dec_tile_num_step:					;Jank way to handle left/right orientation
	dec a							;weirdness. Look at spriteNotes.txt to see the
	jr change_sprite_3				;non-constant left orientation number jumps when walking.
inc_tile_num_step:					;Jump -3 then -4, or -4 then -3
	inc a							
change_sprite_3:					;|
	ld d, a							;|
	
	
	ld hl, $C40A
	ld a, c
	ldi [hl], a
	ld [hl], e
	
	add a, d
	
	ld hl, $C40E
	ldi [hl], a
	ld [hl], e
	
	ret
	
change_pose::
	ld a, [facing]						;e tells us if tiles are flipped. (init to 0)
	ld b, a
	ld a, [ori]
	
	ld d, 1
	ld e, 0
	
	cp 1
	jr z, change_pose_up
	cp 2
	jr z, change_pose_right
	cp 3
	jr z, change_pose_left

	
change_pose_down:						;Filtering by direction being faced
	ld a, h
	
	bit 0, b
	jp z, change_pose_down_right
	jp change_pose_down_left
change_pose_up:
	ld a, h
	
	bit 0, b
	jp z, change_pose_up_right
	jp change_pose_up_left
change_pose_right:
	ld a, h
	
	bit 3, b 
	jp z, change_pose_right_down
	jp change_pose_right_up
change_pose_left:
	ld a, h
	
	bit 3, b 
	jp z, change_pose_left_down
	jp change_pose_left_up


change_pose_down_right:	
	ld b, a;$14
	add 2
	ld c, a;$16
	
	jr change_pose_2
change_pose_down_left:
	inc a
	ld b, a;$15
	add 2
	ld c, a;$17
	ld d, -1
	ld e, %00100000
	
	jr change_pose_2
change_pose_up_right:
	add 2
	ld b, a;$16
	sub 2
	ld c, a;$14
	ld e, %01000000
	
	jr change_pose_2
change_pose_up_left:
	add 3
	ld b, a;$17
	sub 2
	ld c, a;$15
	ld d, -1
	ld e, %01100000

	jr change_pose_2
change_pose_right_down:
	add 6
	ld b, a;$1A
	sub 2
	ld c, a;$18
	ld d, 1
	ld e, %01000000
	
	jr change_pose_2
change_pose_right_up:
	add 4
	ld b, a;$18
	add 2
	ld c, a;$1A
	ld d, 1
	
	jr change_pose_2
change_pose_left_down:
	add 7
	ld b, a;$1B
	sub 2
	ld c, a;$19
	ld d, -1
	ld e, %01100000
	
	jr change_pose_2
change_pose_left_up:
	add 5
	ld b, a;$19
	add 2
	ld c, a;$1B
	ld d, -1
	ld e, %00100000
	
change_pose_2:
	ld hl, $C402
	ld a, b
	ldi [hl], a
	ld [hl], e
	
	add a, d
	
	ld hl, $C406
	ldi [hl], a
	ld [hl], e
	
	ld hl, $C40A
	ld a, c
	ldi [hl], a
	ld [hl], e
	
	add a, d
	
	ld hl, $C40E
	ldi [hl], a
	ld [hl], e
	
	ret


;----------------------------------------------------------------------------

become_ball::
	ld a, SIR_BALL
	ld e, 0
	ld hl, $C402
	ldi [hl], a
	ld [hl], e
	
	ld a, [spr_y]
	add 4
	ld [spr_y], a
	ld a, [spr_x]
	add 4
	ld [spr_x], a
	
	ld a, $7F					;Set all the three other Sirloin sprites to a blank tile (transparent)
	ld hl, $C406
	ldi [hl], a
	ld hl, $C40A
	ldi [hl], a
	ld hl, $C40E
	ldi [hl], a
	ret


unball::
	ld a, [spr_y]
	sub 4
	ld [spr_y], a
	ld a, [spr_x]
	sub 4
	ld [spr_x], a
	ld hl, 0
	call change_sprite
	ret

;----------------------------------------------------------------------------

change_goblin_pal::
	ld b, 1
	ld c, 0
	ld a, [power]
	;if a==0
	;	ld hl, GBPalGoblin
	;elseif a==1
	;	ld hl, GBPalGoblinFire
	;elseif a==2
	;	ld hl, GBPalGoblinIce
	;endif
	
	or a
	jr nz, change_goblin_pal_ice
	ld hl, GBPalGoblin
	jr change_goblin_pal_fin
change_goblin_pal_ice:
	cp 1
	jr nz, change_goblin_pal_fire
	ld hl, GBPalGoblinIce
	jr change_goblin_pal_fin
change_goblin_pal_fire:
	cp 2
	jr nz, change_goblin_pal_earth
	ld hl, GBPalGoblinFire
	jr change_goblin_pal_fin
change_goblin_pal_earth:
	ld hl, GBPalGoblinEarth
	
change_goblin_pal_fin:
	call SetGBCPalettes
	ret
	
	
;----------------------------------------------------------------------------

;If a sprite's priority is lowered, they are only visible when overlapping BG tiles'
;first color. This could be used to make the frame of frame blocks only overlap Sirloin.
lower_priority::
	push hl
		ld d, %10000000
		ld hl, $C403
		ld a, [hl]
		or d
		ld [hl], a
		
		ld hl, $C407
		ld a, [hl]
		or d
		ld [hl], a

		ld hl, $C40B
		ld a, [hl]
		or d
		ld [hl], a

		ld hl, $C40F
		ld a, [hl]
		or d
		ld [hl], a
	pop hl
	ret


;----------------------------------------------------------------------------


raise_priority::
	push hl
		ld d, %01111111
		ld hl, $C403
		ld a, [hl]
		and d
		ld [hl], a
		
		ld hl, $C407
		ld a, [hl]
		and d
		ld [hl], a

		ld hl, $C40B
		ld a, [hl]
		and d
		ld [hl], a

		ld hl, $C40F
		ld a, [hl]
		and d
		ld [hl], a
	pop hl
	
	ret