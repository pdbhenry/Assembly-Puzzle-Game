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