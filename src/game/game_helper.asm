SECTION "game_helper", ROM0

include "include/gb/constants.inc"

;Called after get_dir_input, this function assigns vals to variables that depend on the orientation/face of Sirloin.
;These vars are used in later processes of the game loop.
ori_face_var_setup::
	ld a, [ori]
	cp 2					;Check if on ground or ceiling
	jp nc, .on_wall
	
	ld a, [moving]			;If player inputs diagonal direction, ignore up/down
	or %00001100
	ld [moving], a
	ld [facing_temp], a		;Now that moving is settled, we can set facing to the non-diagonal input


	ld hl, new_player_x
	bit BUTTON_RIGHT, b
	jr nz, .check_left
	
	inc [hl]				;Setting vars for generally moving right (ground/ceiling)
	ld a, 2
	ld [new_ori], a
	xor a
	ld [block_front], a
	ld a, $2
	ld [block_front+1], a	;Holds the val to add to get block in front (used for ramps)
	ld a, 1					;Storing the values spr_x/spr_y change by if a move does happen from the current input
	ld [move_xy], a
	xor a
	ld [move_xy+1], a
	;ld a, -2				;Value added to ramp tile address to get the tile "in front" of it	
	;ld [ramp_front], a		;(this applies to rampable_1. It can be complimented for rampable_2's val)
	
	ld a, [ori]
	or a
	jr nz, .oufr
	
;ori down, facing right
.odfr:
	xor a
	ld [block_below], a
	ld a, $40				;64 in decimal
	ld [block_below+1], a	;Holds the val to add to get block below (used for ramps)
	
	ld a, RAMP_BR_TILE
	ld [rampable_1], a		;Load rampable_1 with the ramp Sirloin could ascend at this ori and moving this direction
	ld a, RAMP_BL_TILE		;Load rampable_2 with the ramp Sirloin could descend if in front+below him	
	ld [rampable_2], a
	
	ret
	
.oufr:
	ld a, $FF
	ld [block_below], a
	ld a, $C0				;-64 in decimal
	ld [block_below+1], a	;Holds the val to add to get block below (used for ramps)
	
	ld a, RAMP_TR_TILE
	ld [rampable_1], a		;Load rampable_1 with the ramp Sirloin could ascend at this ori and moving this direction
	ld a, RAMP_TL_TILE		;Load rampable_2 with the ramp Sirloin could descend if in front+below him	
	ld [rampable_2], a
	;rampable_3 & 4 (maybe just set them at moving_check_above
	
	ret
	
.check_left:
	bit BUTTON_LEFT, b
	ld a, $66
	ret nz
	
	dec [hl]
	ld a, 3
	ld [new_ori], a
	ld a, -1
	ld [move_xy], a
	xor a
	ld [move_xy+1], a
	ld a, $FF
	ld [block_front], a
	ld a, -2
	ld [block_front+1], a
	;ld a, 2						
	;ld [ramp_front], a		
	
	ld a, [ori]
	or a
	jr nz, .oufl
	
.odfl:
	xor a
	ld [block_below], a
	ld a, $40				;64 in decimal
	ld [block_below+1], a	;Holds the val to add to get block below (used for ramps)
	
	ld a, RAMP_BL_TILE
	ld [rampable_1], a		;Load rampable_1 with the ramp Sirloin could ascend if at this ori and moving this direction
	ld a, RAMP_BR_TILE
	ld [rampable_2], a
	
	ret
	
.oufl:
	ld a, $FF
	ld [block_below], a
	ld a, $C0				;-64 in decimal
	ld [block_below+1], a	;Holds the val to add to get block below (used for ramps)
	
	ld a, RAMP_TL_TILE
	ld [rampable_1], a		;Load rampable_1 with the ramp Sirloin could ascend if at this ori and moving this direction
	ld a, RAMP_TR_TILE
	ld [rampable_2], a	

	ret
	

.on_wall:
	ld a, [moving]			;If player inputs diagonal direction, ignore left/right
	or %00000011
	ld [moving], a
	ld [facing_temp], a		;Now that moving is settled, we can set facing to the non-diagonal input
	
	ld hl, new_player_y
	bit BUTTON_UP, b
	jr nz, .check_down
	
	dec [hl]
	ld a, 1
	ld [new_ori], a
	ld a, $FF
	ld [block_front], a
	ld a, $C0
	ld [block_front+1], a
	xor a
	ld [move_xy], a
	dec a
	ld [move_xy+1], a	
	;ld a, 64				
	;ld [ramp_front], a	
	
	ld a, [ori]
	cp 2
	jr nz, .olfu
	
.orfu:	
	xor a
	ld [block_below], a
	ld a, $2				
	ld [block_below+1], a	;Holds the val to add to get block below (used for ramps)
	
	ld a, RAMP_TR_TILE
	ld [rampable_1], a		;Load rampable_1 with the ramp Sirloin could ascend if at this ori and moving this direction
	ld a, RAMP_BR_TILE
	ld [rampable_2], a		
	
	ret
	
.olfu:
	ld a, $FF
	ld [block_below], a
	ld a, $FE				;-2 in decimal
	ld [block_below+1], a	;Holds the val to add to get block below (used for ramps)
	
	ld a, RAMP_TL_TILE
	ld [rampable_1], a		;Load rampable_1 with the ramp Sirloin could ascend if at this ori and moving this direction
	ld a, RAMP_BL_TILE
	ld [rampable_2], a
	
	ret
	
	
.check_down:
	bit BUTTON_DOWN, b
	ld a, $66
	ret nz
	
	inc [hl]
	ld a, 0
	ld [new_ori], a
	ld [block_front], a
	ld a, $64
	ld [block_front+1], a
	xor a
	ld [move_xy], a
	inc a
	ld [move_xy+1], a
	;ld a, -64
	;ld [ramp_front], a

	ld a, [ori]
	cp 2
	jr nz, .olfd
	
.orfd:	
	xor a
	ld [block_below], a
	ld a, $2				
	ld [block_below+1], a	
	
	ld a, RAMP_BR_TILE
	ld [rampable_1], a		
	ld a, RAMP_TR_TILE
	ld [rampable_2], a
	ret
	
.olfd:
	ld a, $FF
	ld [block_below], a
	ld a, $FE				
	ld [block_below+1], a	
	
	ld a, RAMP_BL_TILE
	ld [rampable_1], a		
	ld a, RAMP_TL_TILE
	ld [rampable_2], a
	ret
	
	
;Used to add map addr and direction of push to push_origins
;hl comes with map addr
;bc comes with dir val
;Destroys de
add_push_origins::
	ld a, [push_origins_ind]
	ld de, push_origins				;We load the starting position and the direction
	add e
	ld e, a
	
	ld a, h
	ld [de], a
	inc e
	ld a, l
	ld [de], a
	inc e
	ld a, b
	ld [de], a
	inc e
	ld a, c
	ld [de], a

	ret
	
	

rem_ramping_blocks::					;Removes block that is ramping
	push hl
		ld a, [ramping_blocks_ind]
		cp 0
		jr z, .end
		
		ld de, ramping_blocks
.loop:								;Store ramping block's pal and tiles
		ld a, [de]					;First, change ramp block to look filled.
		ld h, a
		inc e
		ld a, [de]
		inc e
		ld l, a
		call lcd_wait
		ld a, [hl]
		
		add $10						;Based on our tilemap, the filled ramp will always be have tile
		call lcd_wait				;numbers $10 ahead of the non-filled ramp
		ldi [hl], a
		inc a
		ld [hl], a
		inc a
	
		ld bc, 31
		add hl, bc
		
		call lcd_wait
		ldi [hl], a
		inc a
		ld [hl], a
		
		ld a, [de]
		ld h, a
		inc e
		ld a, [de]
		ld l, a
		inc e
		inc e						;Get past dest addr
		inc e
		;inc e						;
		
		call get_tile_pal
		ld [de], a
		inc e
		
		ld a, 0
		call set_block_pal
		
		call lcd_wait
		ld a, [hl]
		ld [hl], 0					;Setting the tiles to default bg block
		ld [de], a
		inc l
		inc e
		call lcd_wait
		ld a, [hl]
		ld [hl], 0
		ld [de], a
		inc e
		
		
		add hl, bc

		call lcd_wait
		ld a, [hl]
		ld [hl], 2
		ld [de], a
		inc l
		inc e
		call lcd_wait
		ld a, [hl]
		ld [hl], 2
		ld [de], a
		inc e
		
		ld bc, ramping_blocks
		ld a, e	
		sub c						;de - ramping_blocks = curr ind we're at
		ld b, a
		ld a, [ramping_blocks_ind]
		cp b
		jr nz, .loop
		
.end:
	pop hl
	ret
	
	
	
ret_ramping_blocks::					;Placing a ramping block in its new position
	ld a, [ramping_blocks_ind]
	or a
	ret z
	
	push hl
		ld de, ramping_blocks
.loop:								;Store ramping block's pal and tiles
		ld a, [de]					;First, change ramp block to look filled.
		ld h, a
		inc e
		ld a, [de]
		inc e
		ld l, a
		call lcd_wait
		ld a, [hl]
		
		sub $10						;Based on our tilemap, the filled ramp will always be have tile
		call lcd_wait				;numbers $10 ahead of the non-filled ramp
		ldi [hl], a
		inc a
		ld [hl], a
		inc a
	
		ld bc, 31
		add hl, bc
		
		call lcd_wait
		ldi [hl], a
		inc a
		ld [hl], a
		
		inc e
		inc e
		ld a, [de]
		ld h, a
		inc e
		ld a, [de]
		ld l, a
		inc e
		
		;inc e						;
		
		ld a, [de]
		call set_block_pal
		inc e
		
		ld a, [de]
		call lcd_wait
		ld [hl], a					;Setting the tiles to default bg block
		inc l
		inc e
		call lcd_wait
		ld a, [de]
		ld [hl], a
		inc e
		
		ld bc, 31
		add hl, bc

		call lcd_wait
		ld a, [de]
		ld [hl], a
		inc l
		inc e
		call lcd_wait
		ld a, [de]
		ld [hl], a
		inc e
		
		ld bc, ramping_blocks
		ld a, e	
		sub c						;de - ramping_blocks = curr ind we're at
		ld b, a
		
		ld a, [ramping_blocks_ind]
		cp b
		jr nz, .loop
		
.end:
	pop hl
	ret
	

;Checks if the next block we're pushing has been visited (is in the current push). If so,
;the push is cancelled (returning with a=$FF). If not, new visited addr is stored
;hl comes with curr block map addr
;Destroys de
add_visited::
	push hl
	push bc
		ld d, h
		ld e, l
		ld hl, visited
		ld a, [visited_ind]
		or a
		jr z, checked_all_visited
		ld b, a
check_visited_loop:
		ldi a, [hl]
		cp d
		jr nz, .no_match_inc
		ldi a, [hl]
		cp e
		jr nz, .no_match
		;xor a
		;ld [visited_ind], a
		ld a, $FF
		jr add_visited_end
.no_match_inc:
		inc l
.no_match:
		dec b
		jr nz, check_visited_loop
checked_all_visited:
		ld a, [visited_ind]			;In the case that the curr block is newly visited, 
		inc a						;add it to the arr and inc ind
		ld [visited_ind], a
		dec a
		sla a
		ld hl, visited
		add l
		ld l, a
		ld a, d
		ldi [hl], a
		ld [hl], e
add_visited_end:
	pop bc
	pop hl
	ret


;bc holds direction of push
;de hold rampable_3/4
turn_dir::
	call turn_dir_helper
	cp d
	ret nz
	
pushing_ramp:
	call get_action_dirs
	ld a, [de]
	ld [action_dir_vert], a
	inc e
	ld a, [de]
	ld [action_dir_hor], a	
	ld a, $FF
	ret
	


turn_dir_2::
	push bc
		cp c							;First we figure out which action dir aligns with the dir of our push.
		ld c, 0
		jr nz, .hor						;We load a with the other action dir. That's the one we want to compare
		ld a, [action_dir_hor]			;with the upcoming ramp
		ld c, 1
.hor:
		ld b, a
	
		ld a, [push_tile]
		cp d
		jr z, .rampable
		cp e
		jr nz, .not_rampable
.rampable:
		call get_action_dirs
		ld a, c
		add e
		ld e, a
		jr nc, .nc
		inc d
.nc:
		ld a, [de]
		cp b
		jr nz, .opposite
;In this case, ramps are opposite only on one axis (Ex: TL into TR |/ -> \|)
;Neither will ramp, they'll just push like blocks
		xor a
		ld [action_dir_vert], a
	pop bc
	
	add hl, bc							;Move to next block here, since we are jp to check_block_push_no_add
	ld a, [blocks_pushed]
	inc a
	ld [blocks_pushed], a
	ret
	
;In the case the complete opposite ramps are pushed into each other (Ex: TL into BR |/ -> /|)
;They will just combine into a standard wood block. The push ends there
.opposite:
		ld de, block_10					;WOOD BLOCK, subject to change
		call replace_block
		ld a, 1
		call set_block_pal
	pop bc
	ld a, b
	cpl
	ld b, a
	ld a, c
	cpl
	inc a
	ld c, a
	add hl, bc	
	ld de, block_00						;EMPTY BLOCK, subject to change
	call replace_block
	
	ld a, [blocks_pushed]
	dec a
	ld [blocks_pushed], a
	xor a
	ld [action_dir_vert], a
	ld a, $FF							;ld a with FF to say we need to jp to start_block_push
	ret
	
;In the case that we are pushing a ramp into a block or a ramp that is not rampable from this dir,
;we need the pushed ramp to turn this current block/ramp
.not_rampable:
	ld d, 0
	ld e, b
	ld a, b
	cp $C0
	jr c, .pos
	ld d, $FF
.pos:
	pop bc
	
store_turning_block_2:
	;Storing $FF in position of ramp so we know not to animate it
	ld bc, ramping_blocks
	ld a, [ramping_blocks_ind]
	add c
	ld c, a
	ld a, $FF
	ld [bc], a
	inc c
	ld [bc], a
	inc c
	
	ld a, h					;Store that position
	ld [bc], a
	inc c
	ld a, l
	ld [bc], a
	inc c
	
.skip_orig_block:
	add hl, de					;Get position that block will end up in after ramp
		
	ld bc, ramping_blocks
	ld a, [ramping_blocks_ind]
	add 4
	add c
	ld c, a
	ld a, h					;Store that position in both the block's data and
	ld [bc], a				;as the starting point of the next push
	inc c
	ld a, l
	ld [bc], a
	
	;!!! Maybe we can just store all non-linear push data in one array (what block is being erased, what direction of push)
	ld a, [ramping_blocks_ind]	;We've only filled 4 bytes, but keep space of 9 for later (pal, tiles)
	add 11
	ld [ramping_blocks_ind], a
	
	ld a, [total_segs_pushed]	;Storing and resetting (?) blocks_pushed.
	ld bc, blocks_pushed_arr
	add c
	ld c, a
	ld a, [blocks_pushed]
	
	ld [bc], a	
	ld a, [total_segs_pushed]
	inc a
	ld [total_segs_pushed], a
	
	ld a, [push_origins_ind]
	add 4
	ld [push_origins_ind], a
		
	xor a						
	ld [blocks_pushed], a
	ld [action_dir_vert], a
	
	ld a, 1
	ld [turn_switch], a
	
	ld b, d
	ld c, e
	
	ld a, c
.check_up:
	cp UP_PUSH
	jr nz, .check_down
	ld a, RAMP_TL_TILE
	ld [rampable_3], a
	ld a, RAMP_TR_TILE
	ld [rampable_4], a
	ret
.check_down:
	cp DOWN_PUSH
	jr nz, .check_right
	ld a, RAMP_BL_TILE
	ld [rampable_3], a
	ld a, RAMP_BR_TILE
	ld [rampable_4], a
	ret
.check_right:
	cp RIGHT_PUSH
	jr nz, .check_left
	ld a, RAMP_TR_TILE
	ld [rampable_3], a
	ld a, RAMP_BR_TILE
	ld [rampable_4], a
	ret
.check_left:
	ld a, RAMP_BL_TILE
	ld [rampable_3], a
	ld a, RAMP_TL_TILE
	ld [rampable_4], a
	ret
	
	
;Given the ramp's tile number, returns with de holding corresponding addr of action dir vals
;Destroys de
get_action_dirs::
	sub RAMP_TILE_MIN
	sra a
	ld de, BR_ACTION_DIRS
	add e
	ld e, a
	ret
	
	
	
	
turn_dir_helper::
	cp d								;If a==d, we are ramping in a positive direction
	jr nz, turn_dir_neg

turn_dir_pos::
	ld a, RAMP_BR_TILE					;Setting ramps that would turn a block in the new direction we're headed
	ld [rampable_4], a
	
	bit 1, c							;If bit 1,c==0, we are jumping down/up and thus must be
	jr z, .pos_ramp_64					;turning right by ramp (since we know we're ramping positively)
	ld de, 64							;If jumping right/left, we are turning down since we're ramping
	ld a, RAMP_BL_TILE					;positively
	ld [rampable_3], a
	ret									;Originally "jr .store_turning_block"
		
.pos_ramp_64:
	ld de, 2
	ld a, RAMP_TR_TILE
	ld [rampable_3], a
	ret
		
turn_dir_neg::
	cp e
	jp z, .cont						;We've determined curr block is a ramp, but not one where the slope is facing
	ld d, a							;the dir of the push, so it is getting pushed. ld d, a as a way to check this
	ret								;specific ret
	
.cont:
	ld a, RAMP_TL_TILE
	ld [rampable_3], a

	bit 1, c							;If bit 1,c==0, we are jumping down/up and thus must be
	jr z, .neg_ramp_64					;turning right by ramp (since we know we're ramping positively)
;
	ld de, -64							;If jumping right/left, we are turning down since we're ramping positively
	ld a, RAMP_TR_TILE
	ld [rampable_4], a
	ret
		
.neg_ramp_64:
	ld de, -2
	ld a, RAMP_BL_TILE
	ld [rampable_4], a
	ret



;Before a jump or fall occurs, we check if it's valid. For example, if Sirloin would jump, get turned by multiple ramps,
;and hit a block that can't be pushed, the jump shouldn't go through (maybe we would animate it down the road).
;If valid, we fill out the spr anims in moving_arr.
project_jump_or_fall::
	ld a, [new_player_x]		;In case there's no ramps, we just have temps to revert
	ld [temp], a				;new_player_x/y
	ld a, [new_player_y]
	ld [temp_2], a
	
	ld de, moving_arr
	ld a, 2
	ld [moving_arr_ind], a
	xor a						;Vertical jump is indicated by 0 at first ind of moving_arr
	ld [de], a
	
	push hl
	
	ld a, [falling]
	or a
	ld a, [ori]
	jr z, .no_fall
	
;If we are falling into ramps, we change the val of ori. If we are falling down, we simulate
;as if we are actually jumping from up ori. Just so we get in the correct sections .up, .right, .left
	bit 0, a
	jr z, .set
	res 0, a
	jr .no_fall
.set:
	set 0, a
.no_fall:
	ld [jump_ori], a 
	or a
	jr nz, .up
	
	inc e
	ld a, -16
	ld [de], a
	ld bc, -64
	ld a, RAMP_TL_TILE
	ld [rampable_3], a					
	ld a, RAMP_TR_TILE	
	ld [rampable_4], a
	ld a, [new_player_y]
	dec a
	ld [new_player_y], a
	jr .loop
.up:
	cp 1
	jr nz, .right
	
	inc e
	ld a, 16
	ld [de], a
	ld bc, 64
	ld a, RAMP_BL_TILE
	ld [rampable_3], a					
	ld a, RAMP_BR_TILE	
	ld [rampable_4], a
	ld a, 16
	ld [de], a
	ld a, [new_player_y]
	inc a
	ld [new_player_y], a
	jr .loop
.right:
	cp 2
	ld a, 1						;Horizontal jump is indicated by 1 at first ind of moving_arr
	ld [de], a
	jr nz, .left
	
	inc e
	ld a, -16
	ld [de], a
	ld bc, -2
	ld a, RAMP_TL_TILE
	ld [rampable_3], a					
	ld a, RAMP_BL_TILE	
	ld [rampable_4], a
	ld a, [new_player_x]
	dec a
	ld [new_player_x], a
	jr .loop
.left:
	inc e
	ld a, 16
	ld [de], a
	ld bc, 2
	ld a, RAMP_TR_TILE
	ld [rampable_3], a					
	ld a, RAMP_BR_TILE	
	ld [rampable_4], a
	ld a, 16
	ld [de], a
	ld a, [new_player_x]
	inc a
	ld [new_player_x], a
	
.loop:
	add hl, bc
	call lcd_wait
	ld a, [hl]
	
	cp RAMP_TILE_MIN					;Making sure curr block is a ramp. If not, skip past this.
	jp c, .not_rampable
	cp RAMP_TILE_MAX
	jp nc, .not_rampable

	ld b, a
	ld a, [rampable_3]
	ld d, a
	ld a, [rampable_4]
	ld e, a	
	ld a, b
	
	call turn_dir_helper
	cp d								;Special case of turn_dir_helper
	ld a, b
	jr z, .not_rampable
	
	ld a, e
	bit 1, a
	ld b, d
	ld c, e
	jr z, .dir_64	
	sla a								;If 2/-2, multiply by 8 to get 16/-16 sprite move
	sla a
	sla a
	ld de, moving_arr
	push af
		ld a, [new_player_y]			;While we have a pushed, take a moment to update y temp
		ld [temp_2], a
		
		ld a, [moving_arr_ind]
		add e
		ld e, a
	pop af
	ld [de], a
	bit 7, a
	ld a, [new_player_x]
	ld [temp], a
	jr nz, .neg_2
	inc a
	ld [new_player_x], a
	ld a, 3								;We are being pushed right by the ramp. If then ends in pushing a block,
	ld [jump_ori], a					;it would be as if our orientation is left. Hence the val of 3 for push_ori
	jp .inc
.neg_2:
	dec a
	ld [new_player_x], a
	ld a, 2
	ld [jump_ori], a					
	jp .inc
	
.dir_64:	
	sra a								;If 64/-64, divide by 4 to get 16/-16 sprite move
	sra a
	ld de, moving_arr
	push af
		ld a, [new_player_x]			;While we have a pushed, take a moment to update x temp
		ld [temp], a
		
		ld a, [moving_arr_ind]
		add e
		ld e, a
	pop af
	ld [de], a
	bit 7, a
	ld a, [new_player_y]
	ld [temp_2], a
	jr nz, .neg_64
	inc a
	ld [new_player_y], a
	ld a, 1
	ld [jump_ori], a
	jr .inc
.neg_64:
	dec a
	ld [new_player_y], a
	ld a, 0
	ld [jump_ori], a	
	
.inc:
	ld a, [moving_arr_ind]
	inc a
	ld [moving_arr_ind], a
	jp .loop
	
	
.not_rampable:
	pop hl
	cp PASSABLE_TILE
	jr nc, .not_rampable_2
	ld a, [falling]
	or a
	jr z, .not_rampable_2
	ld a, [moving_arr_ind]
	cp 2
	jr z, .not_rampable_2
	
	ld a, [new_player_x]
	ld [player_x], a
	ld a, [new_player_y]
	ld [player_y], a
	ld a, $FF
	jr .end_seq
	
.not_rampable_2:
	ld a, [temp]						;Back up our pos by one step, so we're at the last ramp
	ld [new_player_x], a
	ld a, [temp_2]
	ld [new_player_y], a
	
	ld a, [moving_arr_ind]
	cp 2
	ld a, $FF							;If no ramping happened, ld a with FF to show
	jr z, .no_seq						;that we are not jumping out of ramps
			
.end_seq:			
	inc e
	ld [de], a
	ld a, 1
	ld [moving_arr_ind], a									
	jr .finish_projection

.no_seq:
	ld [moving_arr], a
.finish_projection:
	ret
	
;A ramp's action directions are the two directions which it's ramp half are facing and, if pushed in an action direction, 
;have the potential to ramp what it's being pushed into (or get combined with another ramp).
;If pushed in an action direction (Ex: UP) into a ramp where only its corresponding action direction is opposite (Ex: DOWN), no
;ramping happens, the push continues in a straight direction
;If pushed in an action direction into a ramp with totally opposite action directions, the ramps combine
BR_ACTION_DIRS::
	DB UP_PUSH, LEFT_PUSH
BL_ACTION_DIRS::
	DB UP_PUSH, RIGHT_PUSH
TR_ACTION_DIRS::
	DB DOWN_PUSH, LEFT_PUSH
TL_ACTION_DIRS::
	DB DOWN_PUSH, RIGHT_PUSH
