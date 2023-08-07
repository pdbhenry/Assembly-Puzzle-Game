SECTION "sprite_anims_vars", WRAM0[$C100]
anim_space: DS $90
open_anim_spot:: DB		;Gives the low byte of C1XX addr of first 
								;available spot for anim vars to be kept
								;First set to C110
ongoing_anims:: DB		;Gives the # of ongoing anims. Max of 9
run_count:: DB			;Holds anims we have yet to process in a call of run_anims

;anim_sprite_id		equ $C060	;Tells us what anim this is (0=none, 1=moving frost)
;anim_length		equ $C061	;Tells us how many delays anim lasts
;anim_spr_x			equ $C062	;X and Y pos of the sprite
;anim_spr_y			equ $C063	
;anim_direction_x	equ $C064	;If anim is moving, this tells us the dir (-1, 0, or 1)
;anim_direction_y	equ $C065
;anim_dest_block_hi	equ $C066	;Block that anim may affect once it finishes
;anim_dest_block_lo	equ $C067

;anim_step			equ $C068
;anim_delay			equ $C069	;Describes time between switching to a new frame in anim

ANIM_DELAY_RAPID	equ $2
ANIM_DELAY_FAST		equ $4
ANIM_DELAY_MED		equ $8

MOVING_ANIMS		equ 3		;Anims with ID 1-2 are moving
POWER_ANIMS			equ 4		;Anims with ID 1-3 are powers

FROST_ANIM_ID		equ 1
FLAME_ANIM_ID		equ 2
EARTH_ANIM_ID		equ 3	

;FROST_ANIM_TILE		equ $2401
;FLAME_ANIM_TILE		equ $2802
;EARTH_ANIM_TILE		equ $3002	
;SOIL_ANIM_PAL		equ $2C03	


SECTION "sprite_anims", ROM0

include "include/gb/constants.inc"

run_anims::
	ld a, [ongoing_anims]
	or a
	ret z
	ld [run_count], a
	ld hl, $C110
	
run_anims_loop:
	ld a, [hl]			;If sprite_id != 0, anim is playing & we can't create another
	or a
	jp z, run_anims_next
	
	ld a, l
	add 8
	ld l, a
	ld a, [hl]			;Checking anim_step
	dec a
	jr z, run_anims_2
	ld [hl], a
	jp run_anims_count
run_anims_2:
	inc l
	ld a, [hl]			;anim_delay
	dec l
	ld [hl], a			;Reset anim_step

	ld a, l
	sub 7
	ld l, a
	
	ld a, [hl]			;anim_length
	dec a
	jr z, run_anims_end
	ld [hl], a			;anim_length
	
	dec hl
	ld a, [hl]
	cp EARTH_ANIM_ID	;There may be more linear anims, but for now we can just check for EARTH_ANIM.
	jr nz, run_anims_3	;If more are made, we can have a var in each anim that tells if it's mirror or linear
	call progress_sprite
	jr run_anims_4
run_anims_3:
	call mirror_sprite	
run_anims_4:
	;dec hl
	ld a, [hl]			;anim_sprite_id
	cp MOVING_ANIMS
	jp nc, run_anims_count
	
	call move_anim_spr
	jp run_anims_count
	
run_anims_end:
	dec hl
	ld a, [hl]			;anim_sprite_id
	cp FROST_ANIM_ID
	jr z, run_anims_end_frost
	cp FLAME_ANIM_ID
	jr z, run_anims_end_flame
	cp EARTH_ANIM_ID
	jr z, run_anims_end_earth
	jp run_anims_clear
	
run_anims_end_frost:
	xor a
	ld [casting], a
	push hl
		ld hl, 0
		call change_sprite
		ld hl, frost_ding_sfx
		call play_wave_seq_init
	pop hl
	
	
	ld a, l
	add 6
	ld l, a
	ldi a, [hl]			;anim_dest_block_hi
	ld d, a
	ld a, [hl]			;anim_dest_block_lo
	ld e, a
	
	ld a, [block_push_addr_hi]
	cp d
	jr nz, run_anims_end_frost_2
	ld a, [block_push_addr_lo]
	cp e
	jr nz, run_anims_end_frost_2
	jP run_anims_clear
	
run_anims_end_frost_2:
	push de				;Exchanging de and hl
		ld d,h
		ld e,l
	pop  hl
	call lcd_wait
	ld a, [hl]
	cp TRUE_PASSABLE_TILE
	jp c, run_anims_pre_clear
	ld a, ICE_PAL
	call set_block_pal
	jp run_anims_pre_clear
	
run_anims_end_flame:			;Identical to frost
	xor a
	ld [casting], a
	push hl
		ld hl, 0
		call change_sprite
	pop hl
	
	ld a, l
	add 6
	ld l, a
	ldi a, [hl]			;anim_dest_block_hi
	ld d, a
	ld a, [hl]			;anim_dest_block_lo
	ld e, a
	
	ld a, [block_push_addr_hi]
	cp d
	jr nz, run_anims_end_flame_2
	ld a, [block_push_addr_lo]
	cp e
	jr nz, run_anims_end_flame_2
	jp run_anims_clear
	
run_anims_end_flame_2:
	push de				;Exchanging de and hl
		ld d,h
		ld e,l
	pop  hl
	call lcd_wait
	ld a, [hl]
	cp TRUE_PASSABLE_TILE
	jr c, run_anims_pre_clear
	cp IMPASSABLE_TILE
	jr nz, run_anims_end_flame_wood
	ld a, HOT_PAL
	call set_block_pal
	jr run_anims_pre_clear
	
run_anims_end_earth:
	xor a
	ld [earth_casting], a
	push hl
		ld hl, 0
		call change_sprite
	pop hl
	
	ld d,h						;Anim addr goes to de
	ld e,l
	ld a, l
	add 6
	ld l, a
	ldi a, [hl]
	push af
		ld a, [hl]
		ld l, a					;anim_dest_block_lo
	pop af
	ld h, a						;anim_dest_block_hi
	
	push de
		ld a, WOOD_PAL
		ld [earth_curr_pal], a	;Give created block pal 1 by default
		ld a, [earth_making]
		res 7, a
		ld b, WOOD_BLOCK
		cp 1
		jr nz, run_anims_end_earth_2
		ld b, SOIL_BLOCK
		ld a, SOIL_PAL			;Soil pal
		ld [earth_curr_pal], a
run_anims_end_earth_2:
		ld a, b
		call get_block_0x_addr
		call replace_block
		
		ld a, [earth_below_pal]
		cp ICE_PAL
		jr z, run_anims_end_earth_3
		cp SLIME_PAL
		jr z, run_anims_end_earth_3
		ld a, [earth_curr_pal]
		
run_anims_end_earth_3:
		call set_block_pal
	pop de
	jr run_anims_pre_clear

	
run_anims_end_flame_wood:
	push af						;Preserve the tile value
		call get_tile_pal
		cp ICE_PAL
		jr z, run_anims_end_flame_wood_2
		cp SLIME_PAL
		jr nz, run_anims_end_flame_wood_no_coating
run_anims_end_flame_wood_2:
	pop af
	cp WOOD_TILE
	ld a, WOOD_PAL
	jr z, run_anims_end_flame_wood_3
	ld a, SOIL_PAL
run_anims_end_flame_wood_3:
	call set_block_pal
	jr run_anims_pre_clear
	
run_anims_end_flame_wood_no_coating:
	pop af
	cp WOOD_TILE
	jr nz, run_anims_pre_clear	;If this is a soil block, it doesn't burn away
	
	push de
		xor a
		call get_block_0x_addr
		call replace_block
	pop de
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
run_anims_pre_clear:
	push de				;de is holding our C1X0 addr. Give it to hl
	pop hl
run_anims_clear:
	call clear_anim
	
run_anims_count:
	ld a, [run_count]
	dec a
	ld [run_count], a
	ret z
run_anims_next:
	ld bc, $10
	
	ld a, l				;Get hl back to C1X0
	and %11110000
	ld l, a
	add hl, bc
	jp run_anims_loop
	

;a holds sprite id
;b holds anim length
;de holds dest block addr
create_anim:
	ldi [hl], a				;id
	ld [hl], b				;anim length
	
	ld a, [facing]
	ld bc, 0
	bit 0, a
	jr nz, create_anim_2
	inc b					;Facing right
	inc b
create_anim_2:
	bit 1, a
	jr nz, create_anim_3
	dec b					;Facing left
	dec b
create_anim_3:
	bit 2, a
	jr nz, create_anim_4
	dec c					;Facing up
	dec c
create_anim_4:	
	bit 3, a
	jr nz, create_anim_5
	inc c					;Facing down
	inc c

create_anim_5:
	ld a, l
	add 3
	ld l, a					;Getting hl to anim_direction_x
	
	ld a, b
	ldi [hl], a				;Setting dir X
	ld a, c
	ldi [hl], a				;Setting dir Y
	
	ld a, d					;Destination block
	ldi [hl], a
	ld a, e
	ldi [hl], a

update_anim_stack:
	push hl					;Preserve hl at destination block (it's close to anim_step)
		ld a, l
		and %11110000
		ld l, a
		ld bc, $10
update_anim_stack_loop:			;Find the next available anim spot
		add hl, bc
		ld a, [hl]
		or a
		jr nz, update_anim_stack_loop
		
		ld a, l
		ld [open_anim_spot], a
		ld a, [ongoing_anims]
		inc a
		ld [ongoing_anims], a	;Inc # of ongoing anims
	pop hl
	ret

create_frost_anim::
	ld h, $C1
	ld a, [open_anim_spot]
	ld l, a
	
	inc l
	inc l			;anim_spr_x
	ld a, [spr_x]	;Fill in x&y based on player's pos 
	ldi [hl], a		;(which isn't the case for all anims)
	ld a, [spr_y]
	ld [hl], a
	
	ld a, [open_anim_spot]
	ld l, a			;Reset hl back to anim_sprite_id addr before calling create_anim
	
	ld a, 1					;Sprite id 1
	ld b, 9					;Anim length 9
	call create_anim	;Outputs with hl at C1X8, anim_step
			
	ld a, ANIM_DELAY_RAPID
	ldi [hl], a			;anim_step
	ld [hl], a			;anim_delay
	
	ld b, FROST_ANIM_TILE		;b (sprite #) is $14, c (palette) is 1 for frost
	ld c, FROST_SPR_PAL
	call setup_anim_sprite
	
	ld a, 1
	ld [casting], a
	ld h, $14
	call change_pose
	ld hl, frost_sfx
	call play_wave_adv_init
	ret
	
create_flame_anim::
	ld h, $C1
	ld a, [open_anim_spot]
	ld l, a
	
	inc l
	inc l			;anim_spr_x
	ld a, [spr_x]	;Fill in x&y based on player's pos 
	ldi [hl], a		;(which isn't the case for all anims)
	ld a, [spr_y]
	ld [hl], a
	
	ld a, [open_anim_spot]
	ld l, a			;Reset hl back to anim_sprite_id addr before calling create_anim
	
	ld a, 2					;Sprite id 2
	ld b, 9					;Anim length 9
	call create_anim	;Outputs with hl at C1X8, anim_step
	
	ld a, ANIM_DELAY_RAPID
	ldi [hl], a			;anim_step
	ld [hl], a			;anim_delay
	
	ld b, FLAME_ANIM_TILE		;b (sprite #) is $14, c (palette)
	ld c, FLAME_SPR_PAL
	call setup_anim_sprite
	
	ld a, 1
	ld [casting], a
	ld h, $14
	call change_pose
	ret
	
create_earth_anim::
	ld h, $C1
	ld a, [open_anim_spot]
	ld l, a
	
	inc l
	inc l
	ld a, [earth_addr_hi]
	ld d, a
	ld a, [earth_addr_lo]
	ld e, a
	
	push de					;Preserve de's earth addr
	push hl					;Preserve the anim addr of hl
	push de
	pop hl					;Put de's addr in hl			
		call get_spr_pos
	pop hl					
		ld a, d
		ldi [hl], a		
		ld a, e
		ld [hl], a
	pop de
	
	ld a, [open_anim_spot]
	ld l, a
	
	ld a, 3
	ld b, 2
	call create_anim
	
	ld a, ANIM_DELAY_RAPID
	ldi [hl], a
	ld [hl], a
	
	ld b, EARTH_ANIM_TILE
	ld c, EARTH_SPR_PAL
	call setup_anim_sprite
	
	ret
	
	
;de comes loaded with x,y pos of soil block breaking
create_soil_anim::
	ld h, $C1
	ld a, [open_anim_spot]
	ld l, a
	inc l
	inc l			;anim_spr_x
	
	ld a, d			
	ldi [hl], a
	ld a, e
	ld [hl], a
	
	ld a, [open_anim_spot]
	ld l, a			;Reset hl back to anim_sprite_id addr before calling create_anim
	
	ld a, 4					;Sprite id 4
	ld b, 5					;Anim length 7
	call create_anim		;Outputs with hl at C1X8, anim_step
	
	ld a, ANIM_DELAY_MED
	ldi [hl], a			;anim_step
	ld [hl], a			;anim_delay
	
	ld b, SOIL_ANIM_TILE
	ld c, SOIL_SPR_PAL
	call setup_anim_sprite
	ret
	

;b comes with 1st value of sprites (frost uses $14,$15,$16,$17 so comes with $14)
;c comes with sprite attributes val
setup_anim_sprite:
	ld a, l				;Get hl to C1X0
	and %11110000
	ld l, a
	
	push hl
	pop de
	inc e
	inc e
	inc e				;anim_spr_y
	
	;Set the 4 sprites
	ld h, $C4			;hl is at C1X0. By changing the 1 to 4, we'll have sprite addr C4X0.
	ld a, [de]			;anim_spr_y
	ldi [hl], a
	dec e
	ld a, [de]			;anim_spr_x
	ldi [hl], a
	ld a, b
	ldi [hl], a
	ld a, c
	ldi [hl], a
	
	inc e
	ld a, [de]			;anim_spr_y
	ldi [hl], a
	dec e
	ld a, [de]			;anim_spr_x
	add 8
	ldi [hl], a
	ld a, b
	inc a
	ldi [hl], a
	ld a, c
	ldi [hl], a
	
	inc e
	ld a, [de]
	add 8
	ldi [hl], a
	dec e
	ld a, [de]
	ldi [hl], a
	ld a, b
	add 2
	ldi [hl], a
	ld a, c
	ldi [hl], a
	
	inc e
	ld a, [de]
	add 8
	ldi [hl], a
	dec e
	ld a, [de]
	add 8
	ldi [hl], a
	ld a, b
	add 3
	ldi [hl], a
	ld a, c
	ld [hl], a
	
	ret
	
	
	
clear_anim:
	ld a, l		;Zeroing lowest nibble to get to anim_sprite_id addr (C1X0)
	and %11110000
	ld l, a
	ld b, a		;Save sprite_id addr
	
	ld a, [hl]
	cp POWER_ANIMS
	jr nc, clear_anim_2
	xor a
	ld [using_power], a
clear_anim_2:
	ld [hl], 0	;Set ID to 0 to show no anim is happening	;anim_sprite_id
	
	ld a, [ongoing_anims]	;Dec ongoing anims
	dec a
	ld [ongoing_anims], a
	
	ld a, [open_anim_spot]
	cp b
	jr c, clear_anim_3
	ld a, b
	ld [open_anim_spot], a	;If cleared anim spot is lower than open_anim_spot, replace it
	
clear_anim_3:
	ld h, $C4
	inc l
	inc l		;Add 2 to get to sprite #
	
	ld bc, 4
	ld [hl], $40			;Fill all 4 four sprites with a blank sprite
	add hl, bc				;Ex of addrs are C412, C416, C41A, C41E
	ld [hl], $40
	add hl, bc
	ld [hl], $40
	add hl, bc
	ld [hl], $40
	
	ret
	
mirror_sprite:
	push hl
		ld h, $C4
		ld a, l
		and %11110000
		add 2
		ld l, a				;We need hl at C4X2, C4X6, C4XA, C4XE
		call mirror_helper
		ld a, l
		add 3				;Add 3 b/c mirror_helper adds 1 to hl already
		ld l, a
		call mirror_helper
		ld a, l
		add 3
		ld l, a
		call mirror_helper
		ld a, l
		add 3
		ld l, a
		call mirror_helper
	pop hl
	ret
	
;Swaps left sprites with right sprites and compliments X-flip attribute
mirror_helper:
	ld a, [hl]
	ld b, a
	inc a
	and %00000001
	res 0, b
	add b
	ldi [hl], a
	ld a, [hl]
	add %00100000
	and %00100011		;Bit 0 is 1 since these sprites use palette 1
	ld [hl], a
	ret
	

progress_sprite:
	push hl
		ld h, $C4
		ld a, l
		and %11110000
		add 2
		ld l, a				;We need hl at C4X2, C4X6, C4XA, C4XE
		call progress_helper
		ld a, l
		add 4				;Add 3 b/c mirror_helper adds 1 to hl already
		ld l, a
		call progress_helper
		ld a, l
		add 4
		ld l, a
		call progress_helper
		ld a, l
		add 4
		ld l, a
		call progress_helper
	pop hl
	ret
	
;Adds 4 to each tile number, progressing the sprite to the next frame of its anim
progress_helper:
	ld a, [hl]
	add 4
	ld [hl], a
	ret
	
move_anim_spr:
	ld a, l
	add 4
	ld l, a				;anim_direction_x
	
	ldi a, [hl]
	ld b, a
	ld a, [hl]
	ld c, a
	
	push hl
		ld de, 3
		ld h, $C4
		ld a, l
		and %11110000
		ld l, a				;C4X0
		call move_anim_spr_helper
		add hl, de			;C4X4
		call move_anim_spr_helper
		add hl, de			;C4X8
		call move_anim_spr_helper
		add hl, de			;C4XC
		call move_anim_spr_helper
	pop hl
	ret
	
move_anim_spr_helper:
	ld a, [hl]
	add c
	ldi [hl], a
	ld a, [hl]
	add b
	ld [hl], a
	ret