SECTION "game_load", ROM0

include "include/gb/constants.inc"

load_game_data::
	xor a
	ld [fade_state], a
	ld [ori], a
	ld [moving], a
	ld [moved], a
	ld [falling], a
	ld [rocketing], a
	ld [rocket_push], a
	ld [palette_map_addr_lo], a
	ld [block_push_addr_hi], a
	ld [turning_corner], a
	;ld [ice_block_count], a
	;ld [ice_slide_delay], a
	ld [using_power], a
	ld [earth_making], a
	ld [earth_below_pal], a
	ld [earth_curr_pal], a
	ld [mid_air], a
	ld [slime_block_hi], a
	ld [slime_block_lo], a
	ld [slimed_hi], a
	ld [slimed_lo], a
	ld [casting], a
	ld [earth_casting], a
	ld [ramping_blocks_ind], a	;Must be init to 0 for ret_ramping_blocks to work
	ld [push_origins_ind], a
	ld [total_segs_pushed], a	;;Must be init to 0 since we use it to determine if a push is happening
	ld [turn_switch], a
	ld [visited_ind], a
	
	ld d, 4
	ld hl, ICE_MEM_START
	ld bc, ICE_SLIDE_VARS
load_game_data_slide_spots:		;Loop to set C2XX open_slide_spots to 0 
	ld [hl], a
	add hl, bc
	dec d
	jr nz, load_game_data_slide_spots
	
	ld [ongoing_slides], a
	ld a, ICE_MEM_START_LO		;$04 currently
	ld [open_slide_spot], a
	
	;xor a
	;ld [loose_below_switch], a	;From loose_block.asm
	;ld [fall_arr_ind], a	
	;ld [any_fallers], a
	;ld [squashed], a
	;inc a
	;ld [curr_vram_ind], a
	;ld a, $FF
	;ld [$D200], a
	;ld [$D300], a
	;ld a, $9C
	;ld [curr_vram_section], a
	
	xor a
	ld bc, $10					;From sprite_anims.asm
	ld hl, $C110
	ld d, 9
load_game_data_anim_spots:		;Loop to set C1X0 addrs to 0
	ld [hl], a
	add hl, bc
	dec d
	jr nz, load_game_data_anim_spots

	ld [ongoing_anims], a		
	ld a, $10
	ld [open_anim_spot], a
	
	
	call power_setup
	call change_goblin_pal

	ld a, %11101110
	ld [facing], a
	
	;ld de, GBPalSoil
	;ld c, 0*2
	;call SetHighlight			;For some reason this set our ROMX to ROM0.
	
	
	;;Setting BG Palette 3 to Frost palette
	;ld b, 0
	;ld c, 3*8
	;ld hl, GBPalFrost
	;call SetGBCPalettes
	
	;Setting OBJ Palette 1 to Frost palette
	ld b, 1
	ld c, 1*8
	ld hl, GBPalFrost
	call SetGBCPalettes
	
	;Setting OBJ Palette 1 to Fire palette
	ld b, 1
	ld c, 2*8
	ld hl, GBPalFire
	call SetGBCPalettes
	
	;Setting OBJ Palette 1 to Fire palette
	ld b, 1
	ld c, 3*8
	ld hl, GBPalSoil
	call SetGBCPalettes
	
	;Setting palette index of power sprites to 1
	ld a, %00000001
	ld hl, $C413
	ld [hl], a
	ld hl, $C417
	ld [hl], a
	ld hl, $C41B
	ld [hl], a
	ld hl, $C41F
	ld [hl], a
	;ld b, 0 			;will be setting bg palette
    ;ld c, 0*8			;palette no 0 (back)
	;ld hl, GBPalBg1
	;call SetGBCPalettes
	
	;ld b, 0 			;will be setting bg palette
    ;ld c, 1*8			;palette no 1 (back)
	;ld hl, GBPalFg1
	;call SetGBCPalettes
	
;****************************************************************************
;*This section can probably be skipped if moving to the next level/resetting
;*I'll keep it for now in case different tile data needs to eventually be
;*loaded from level to level or something. But should be optimized later.
	
	
	ld de, VRAM_TILES_BACKGROUND
	ld hl, game_tile_data
	ld bc, game_tile_data_end-game_tile_data
	call memcpy
	
	ld de, VRAM_TILES_SPRITE
	ld hl, sprite_tile_data
	ld bc, sprite_tile_data_end-sprite_tile_data
	call memcpy
	
	ld hl, $8FF0		;Make Filler tile dark
	ld a, $FF
	ld b, 16
	call memcpy_single
	
	;Fills D201 with FF so that we don't read last level's loose blocks
	ld hl, $D201
	ld [hl], $FF
	
;****************************************************************************
;*This below section is (I'm pretty sure) unnecessary if resetting a level

	ld a, [curr_level]
	rla

	ld b, 0
	ld c, a
	ld hl, level_map_addrs
	add hl, bc
	
	ld b, h
	ld c, l
	ld a, [bc]
	ld l, a
	ld [curr_level_addr_lo], a
	inc bc
	ld a, [bc]			;Get (level_map_addrs + (curr_level*2)) into hl.
	ld h, a				;addr is stored backwards, so load l first, then h
	ld [curr_level_addr_hi], a
;****************************************************************************

	ld de, $9C00
	ld bc, 10
	di
	call load_game_map
	call load_game_map_caps
	;call add_loose_partitions
	ld hl, $D100
	ld de, $9C00
	ld b, $5A			;90 vals in 16x16 map
	call load_game_palette_map
	call load_sprite
	call load_song
	ei
	ret
	
load_game_map:
	xor a
	ld [block_palette], a		;If we're dealing with bg blocks, use palette 0
	
	ld a, [hl]
	push af
	push de
		call get_block_0x_addr	;Finds the curr block's tile values
		ld a, [de]
		ld [tile_1], a
		inc de
		ld a, [de]
		ld [tile_2], a
		inc de
		ld a, [de]
		ld [tile_3], a
		inc de
		ld a, [de]
		ld [tile_4], a
	pop de
	pop af
	
	push hl
		ld hl, block_palette
		cp 6
		jr z, load_game_map_slime_2
		cp 7
		jr c, load_game_map_bg		; If we're dealing with wood blocks
		ld [hl], 1					; (vals 7 and onward), use palette 1
		cp $1A
		jr nz, load_game_map_loose
		ld [hl], STEEL_PAL
load_game_map_loose:
		cp $1B						
		call z, unpack_1B					;Special code for loose block
		cp $1C								;If ice wood block
		jr c, load_game_map_hot_steel
		cp $1F								;Basically we're seeing if block val is within
		jr nc, load_game_map_soil			;1C and 1D (our curr ice blocks)
		ld [hl], ICE_PAL
load_game_map_soil:
		jr nz, load_game_map_hot_steel
		ld [hl], SOIL_PAL
load_game_map_hot_steel:
		cp $20
		jr nz, load_game_map_slime
		ld [hl], HOT_PAL
load_game_map_slime:
		cp $21
		jr c, load_game_map_bg				;Checking if block val is within
		cp $24								;21 and 23
		jr nc, load_game_map_bg
load_game_map_slime_2:
		ld [hl], SLIME_PAL

load_game_map_bg:		;Skipped to if we are dealing with a bg block
	;The separation between this and unpack_block may be useful in the future
	;But for now, nothing is needed here--just continues to unpack_block
	
unpack_block:
	pop hl
	
	push hl
	push bc
		ld h, d					;Setting hl to vram address b/c we'll 
		ld l, e					;do 16 bit additions
		ld bc, 31				;Down a line in vram, 1 to the left
		
		ld a, [tile_1]
		call lcd_wait
		ldi [hl], a
		ld a, [tile_2]
		ld [hl], a
		add hl, bc
		ld a, [tile_3]
		call lcd_wait			;Having this 2nd one seems to make the load reliable enough
		ldi [hl], a
		ld a, [tile_4]
		ld [hl], a
		
		ld a, $D1
		ld h, a
		ld a, [palette_map_addr_lo]
		ld l, a
		inc a
		ld [palette_map_addr_lo], a
		ld a, [block_palette]
		;call lcd_wait
		ld [hl], a				;Fill out palette map as we read tilemap. Stored from $D100-$D15A
	pop bc
	pop hl	
	
	
load_game_map_2:
	inc de
	inc de
	inc hl
	dec bc
	ld a, b
	or c
	jp nz, load_game_map
	
	push hl
		ld b, 12
		ld c, 2				;So we know to if the second line is filled with ff
		ld h, d
		ld l, e
		
;Fill the top, right, and bottom boundaries with tile FF
load_game_map_filler:
		ld a, $FF
		call lcd_wait
		ldi [hl], a
		dec b
		jr nz, load_game_map_filler
		
		dec c
		jr z, load_game_map_filler_2
		push bc
			ld bc, 20
			add hl, bc
		pop bc
		
		ld b, 12
		jr load_game_map_filler
		
load_game_map_filler_2:
		ld d, h
		ld e, l
	pop hl
	
	ld bc, 10
	
	ld a, d
	cp $9E
	jp nz, load_game_map
	ld a, e
	cp $40
	jp nz, load_game_map
	
	ret

;Loose block
unpack_1B:
	;push hl
		;ld h, $D2
		;ld a, [curr_vram_ind]
		;ld l, a
unpack_1B_loop:
		;ld a, [curr_vram_section]
		;cp d
		;jr nc, unpack_1B_2
		;inc a
		;ld [curr_vram_section], a
		;ld [hl], $FE
		;inc l
		;jr unpack_1B_loop
		
unpack_1B_2:
		;ld [hl], e
		;inc l
		;ld [hl], $FF
		;ld a, l
		;ld [curr_vram_ind], a
	;pop hl
	ret	

load_game_map_caps:
	ld hl, $9BC0
	ld b, 64
	ld a, $FF
	
	call memcpy_single
	
	ld hl, $9E40
	ld b, 64
	call memcpy_single
	ret
	
load_game_palette_map::
	call VBK_On
	
	ld c, 10
load_game_palette_map_2:
	ldi a, [hl]
	push hl
	push bc
		ld h, d
		ld l, e
		ld bc, 31
		call lcd_wait
		ldi [hl], a
		ld [hl], a
		add hl, bc
		call lcd_wait
		ldi [hl], a
		ld [hl], a
	pop bc
	pop hl
	
	inc de
	inc de
	
	dec c
	jr nz, load_game_palette_map_3
	ld a, 44
	add e
	ld e,a
	ld a,0
	adc d
	ld d,a
	
	ld c, 10
		
load_game_palette_map_3:
	
	dec b
	jr nz, load_game_palette_map_2
	
	call VBK_Off
	ret
	
load_sprite:
	call lcd_wait	
	
	ld a, [curr_level]			;Using curr_level to get starting position.
	ld b, 0
	ld c, a
	ld hl, game_start_pos
	add hl, bc
	ld a, [hl]
	
	ld b, a
	and %00001111
	ld [player_y], a
	ld [new_player_y], a
	ld a, b
	swap a
	and %00001111
	ld [player_x], a
	ld [new_player_x], a
	
	rla
	rla
	rla
	rla
	add 8				;Tile X pos * 16 + 8 = Sprite X pos
	ld [spr_x], a
	
	ld a, [player_y]
	rla
	rla
	rla
	rla
	add 16
	ld [spr_y], a
	
load_sprite_2::
	ld hl, $C400		;Our DMA transfer will be reading from C400 for sprite data
	ldi [hl], a 		;a register holds spr_y value
	ld a, [spr_x]
	ldi [hl], a 		;X
	ld a, $00			;Tile Number
	ldi [hl], a			
	ld a,%00000000
	ldi [hl], a
	
	ld a, [spr_y] 
	ldi [hl], a 		;Y
	ld a, [spr_x]
	add 8
	ldi [hl], a 		;X
	ld a, $01			;Tile Number
	ldi [hl], a			
	ld a,%00000000
	ldi [hl], a
	
	ld a, [spr_y]
	add 8
	ldi [hl], a 		;Y
	ld a, [spr_x]
	ldi [hl], a 		;X
	ld a, $02			;Tile Number
	ldi [hl], a			
	ld a,%00000000
	ldi [hl], a

	ld a, [spr_y]
	add 8
	ldi [hl], a 		;Y
	ld a, [spr_x]
	add 8
	ldi [hl], a 		;X
	ld a, $03			;Tile Number
	ldi [hl], a			
	ld a,%00000000
	ldi [hl], a
	
	ret
	
	
	
;'a' comes with block value.
;Outputs with de holding addr to block_0x header from block_tiles.inc
get_block_0x_addr::
	or a				;Clear carry flag
	rla					;Multiply a by 2 (since each address at 'blocks' is 2 bytes)

	ld de, blocks
	add e			;Make sure this 100% can't go over FF
	ld e, a			;Now de is at blocks+uncovered_block
	jr nc, get_block_0x_addr_nc
	inc d
		
get_block_0x_addr_nc:	;No carry
	push hl
		ld h, d
		ld l, e
		ldi a, [hl]
		ld e, a		;e first because addr comes backwards (like 3514 when it should be 1435)
		ld a, [hl]
		ld d, a		;Now de is at block_0X tile info
	pop hl
	ret
	
	
;Sets up array of available powers in curr level at $C300
;a comes with level_powers val
power_setup:
	xor a 
	ld [power], a
	
	ld a, [curr_level]			;Using curr_level to get starting position.
	ld b, 0
	ld c, a
	ld hl, level_powers
	add hl, bc
	ld a, [hl]
	
	ld hl, power_index
	ld [hl], $FF
	or a
	ret z
	
	ld hl, $C300
	ld b, a
	ld a, 1
	
	bit 0, b
	jr z, power_setup_fire
	ldi [hl], a
power_setup_fire:
	inc a						;a is now 2 for fire
	bit 1, b
	jr z, power_setup_earth
	ldi [hl], a
power_setup_earth:
	inc a						;a is now 3 for earth
	bit 2, b
	jr z, power_setup_end
	ldi [hl], a
power_setup_end:
	ld a, l
	cp 2						;If l is only at C301, we only have 1 power and
								;so power_index won't need to be set
	ld [hl], $FF
	ld l, 0
	ld a, [hl]
	ld [power], a
	ret c						;Regarding the cp 2
	
	xor a
	ld [power_index], a
	ret
	

;Does NOT need to be called if resetting a level	
load_song:
	ld a, [curr_level]
	ld hl, level_songs+1
	
load_song_loop:
	ld b, [hl]
	cp b				;Check if curr lvl is below the next stretch of lvls
	jr c, load_song_2
	inc hl
	inc hl
	jr load_song_loop
	
load_song_2:
	dec hl				;Get song #
	ld b, [hl]
	ld a, [curr_song]
	cp b				;Check if curr_song is what should be playing
	ret z				;ret if so
	
	xor a
	ld [music_trigger], a	;Turn off music
	ld a, b
	ld [curr_song], a
	
	ld hl, song_banks
	call add_a_to_hl
	ld a, [hl]
	ld [curr_song_bank], a
	
	ld a, b					;This chunk uses curr_song val to get the song_descriptor addr
	add a					;Double since we're looking through 2-byte addrs
	ld hl, song_order		
	call add_a_to_hl
	ldi a, [hl]
	ld c, a
	ld a, [hl]
	ld b, a
	push bc
	pop hl
	
	ld a, [curr_song_bank]
	ld [rROMB0], a 
	call music_player_init
	ld a, 1
	ld [rROMB0], a
	
	ld b, 0
	ld c, 1
	call hUGE_mute_channel
	ld b, 1
	ld c, 1
	call hUGE_mute_channel
	ret
	