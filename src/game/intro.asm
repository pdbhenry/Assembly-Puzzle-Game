SECTION "intro_vars", WRAM0

curr_screen: DB			;Index of what map we're currently on
screen_change: DB		;If 0, no change; If 1, go to next map, if 2, go to prev map
talking: DW				;If set, it holds the addr of the dialogue to be written
jumping: DB				;Used in intro for the non-functional jump. Used in conjunction with falling (tell if rising/falling)

curr_word: DS 10
text_tick: DB
text_tile: DW			;Addr of curr tile where letter is to be placed
text_wait: DB
text_remain_space: DB	;Remaining space for characters in curr line, with max val of 18

curr_npc_data: DW

TEXT_DELAY EQU 3
TEXT_DROPOFF EQU 50 	;Frames until finished text leaves screen
TEXT_LINE_1 EQU $9841	;Tile addr of start of line 1 in text box
TEXT_LINE_2 EQU $9881
TEXT_CONT_TILE	EQU $98B2	;Tile addr of tile where continue symbol goes

TEXT_MAX_SPACE EQU 18
TEXT_CONT_CHAR EQU $C6
SPACE_CHAR	EQU $20


SECTION "intro", ROM0

include "include/gb/constants.inc"

update_intro::
	ld a, [fade_state]
	cp 1
	jp z, pre_input
	cp 2
	jr z, do_fade_out
	;cp 3					;Do fade in
	;jp z, prep_cutscene
	;cp 4
	
;Doing fade in
	;call wait_vblank
	ld hl, GBPalBg1
	ld bc, 0*8				;b = 0 -> BG Pal changes, c = 0 -> Palette index 0
	call fade_in
	ld hl, GBPalFg1
	ld bc, 1*8
	call fade_in
	ld hl, GBPalSoil
	ld bc, 2*8
	call fade_in
	ld hl, GBPalSteel
	ld bc, 3*8
	call fade_in
	ld hl, GBPalFrost
	ld bc, 4*8
	call fade_in
	ld hl, GBPalFire
	ld bc, 5*8
	call fade_in
	ld hl, GBPalSlime
	ld bc, 6*8
	call fade_in
	
	ld a, [fade_step]
	inc a
	ld [fade_step], a
	
	ld a, c
	cp 66
	ret nz		;jp nz, end_frame	;ret nz					;!!!
	call reset_fade_step
	ld a, 1
	ld [fade_state], a
	ret
	
do_fade_out:


pre_input:
	ld a, [text_wait]
	or a
	jp z, .talk_check
	
	dec a
	ld [text_wait], a
	jp .window_show			;Resume intro loop
	
.talk_check
	ld a, [talking]
	or a
	jp z, move_check
	
.window_show
	ld de, LCD_LINE_Y			;If talking, make sure we are rendering window at top of screen
	ld hl, LCD_CTRL
.window_loop:
	ld a, [de]
	or a
	jr nz, .window_loop
	
	set 5, [hl]
.window_loop_2:
	ld a, [de]
	cp 56						;Window height
	jr nz, .window_loop_2
	
	res 5, [hl]
	
	ld a, [talking]				;I find that doing talking_anim after ensuring that window is showing means
	or a						;no flickering of the window
	jp nz, talking_anim			

	
move_check:
	ld a, [moving]
	or a
	jr nz, move_anim
	
	ld a, [jumping]
	or a
	jp nz, jump_anim
	
get_input:
	call read_joypad
	ld a, [io_p15]
	bit BUTTON_A, a
	jp z, start_jump
	
get_dir_input:
	ld a, [io_p14]
	cp %11101111
	jp z, end_frame
	
	push af
		ld a, [player_x]
		ld [new_player_x], a
	pop af
	
	ld [moving], a
	ld b, a
	ld a, [new_player_x]
	
	bit BUTTON_LEFT, b
	jr nz, get_dir_input_2
	dec a
	ld b, 1						;This will be facing's value, which tells us its not right input
	jp check_collision
	
get_dir_input_2:
	bit BUTTON_RIGHT, b
	jp nz, finish_move
	inc a
	ld b, 0						;This will be facing's value, which tells us it is right input
	
check_collision:
	ld [new_player_x], a
	cp 10						;Check bounds
	ld a, b
	ld [facing], a	
	jp c, check_collision_2		;If in bounds, check for blocking tile. If not, screen change
	
	jr nz, screen_change_left
	ld a, 1 
	jr screen_change_end
screen_change_left:
	ld a, -1
screen_change_end:
	ld [screen_change], a
	jr begin_move
	
check_collision_2:
	call get_tile	
	call lcd_wait
	ld a, [hl]
	
	cp PASSABLE_TILE
	;jr c, begin_move 	
	jp nc, finish_move
	

begin_move:	
	ld a, [new_player_x]
	ld [player_x], a
		
	ld a, 16
	ld [moving_step], a
	
	ld hl, 0
	call change_sprite			;To update facing
	
	ld a, [moving]
	;jp end_frame



move_anim:				;Kinda lame we do this check each time...
	ld hl, spr_x
	bit 0, a
	jr z, move_anim_forward		;Right
	bit 1, a
	jr z, move_anim_backward	;Left
	
move_anim_forward:		;This is used to move either down or right depending on what's loaded in hl
	inc [hl]
	jr moving_step_check
	
move_anim_backward:		;Used to move up or left
	dec [hl]
	
moving_step_check:
	ld a, [moving_step]
	dec a
	ld [moving_step], a
	
	cp 8							;Halfway point of moving to a tile
	jp nz, moving_step_check_2		
	ld hl, $0100
	call change_sprite
	
;Changing curr_screen value
	ld a, [screen_change]
	or a
	jp z, end_frame
	ld b, a
	ld a, [curr_screen]
	add b
	ld [curr_screen], a
	add a							;Double, since we're trying to get addr out of list of 2-byte addrs
	ld b, a							;Storing doubled curr_screen value
	
;Updating our curr_npc_data addr
	ld hl, intro_map_npc_pos
	add l
	ld l, a
	ld a, 0
	adc h
	ldi a, [hl]
	ld [curr_npc_data+1], a
	ld a, [hl]
	ld [curr_npc_data], a
	
	ld a, b							;Restoring doubled curr_screen value
	
;Adding curr_screen index to intro_map_addrs to get addr of new map
	ld de, intro_map_addrs			
	add e
	ld e, a
	ld a, 0
	adc d
	ld d, a
	
;Moving sprite during screen change
	ld a, [screen_change]
	cp 1
	jr nz, loop_sprite_back
	
	xor a
	ld [player_x], a
	ld a, 0
	jr loop_sprite_end
loop_sprite_back:
	ld a, 9
	ld [player_x], a
	ld a, 160
loop_sprite_end:
	ld [spr_x], a
	xor a
	ld [screen_change], a
	
;Updating Sirloin sprite pos
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
	

;Loading new map
	xor a
	ld [palette_map_addr_lo], a		;Resetting palette map index
	
	ld a, [de]						;Setting hl to the map addr
	ld l, a
	inc de
	ld a, [de]
	ld h, a
	
	ld de, $9C00
	ld bc, 10
	di
	call load_intro_map
	ld hl, palette_map_addr_2
	ld de, $9C00
	ld b, $5A			;90 vals in 16x16 map
	call load_game_palette_map
	ei
	ret
	
moving_step_check_2:
	cp 0
	jp nz, end_frame
	
	ld hl, 0
	call change_sprite
	
	ld a, [player_x]
	ld b, a
	ld a, [facing]
	bit 0, a
	jr nz, moving_step_check_left
	inc b							;Just want to inc b once. Do it twice to negate the dec after.
	inc b
moving_step_check_left:
	dec b
	
	ld a, [curr_npc_data]
	ld h, a
	ld a, [curr_npc_data+1]
	ld l, a
	
.loop
	ldi a, [hl]						;With each loop, inc hl by 3 to get to the next item of npc data
	cp $FF
	jr z, moving_step_check_end
	swap a
	and %00001111
	cp b
	jr z, .end_loop
	inc hl
	inc hl
	jr .loop
	
.end_loop
	ldi a, [hl]
	ld [talking+1], a
	ld a, [hl]
	ld [talking], a	
	
	ld a, HIGH(TEXT_LINE_1)
	ld [text_tile], a
	ld a, LOW(TEXT_LINE_1)
	ld [text_tile+1], a
	
	call clear_text
	ld a, TEXT_MAX_SPACE
	ld [text_remain_space], a

moving_step_check_end:
	jp finish_move
	
	
	
	
	
talking_anim:
	ld a, [text_tick]
	dec a
	ld [text_tick], a
	jp nz, move_check
	
	ld a, TEXT_DELAY
	ld [text_tick], a
	
	ld a, [text_tile]
	ld d, a
	ld a, [text_tile+1]
	ld e, a
	
	ld a, [talking]			;Holds curr char of dialogue
	ld h, a
	ld a, [talking+1]
	ld l, a	
	
	ldi a, [hl]
	cp 255
	jr nz, talking_anim_2
	ld a, TEXT_DROPOFF
	ld [text_wait], a		;End of dialogue case
	xor a
	ld [talking], a
	jp move_check			;Resume intro loop
	
talking_anim_2:
	cp SPACE_CHAR
	jr nz, talking_anim_3
	
	ld c, a							;Store char value for later
	push hl	
		ld b, 1						;We can assume the char after a space is not a space
		inc hl						;and start with a word length of 1
talking_new_word_loop:
		ldi a, [hl]
		
		cp SPACE_CHAR
		jr z, talking_new_word
		cp 255
		jr z, talking_new_word
		
		inc b
		jr talking_new_word_loop
	
talking_new_word:
	pop hl
	
	ld a, [text_remain_space]

	inc b
	cp b
	jr c, talking_new_line
	
	ld a, c						;Get char value back
	jr talking_anim_3
	
talking_new_line:
	ld a, TEXT_MAX_SPACE
	ld [text_remain_space], a
	
	ld a, e
	cp $80
	jr c, .to_line_2
	;ld [text_wait], a
	;ld hl, TEXT_CONT_TILE
	;call lcd_wait
	;ld [hl], TEXT_CONT_CHAR
	call move_text_up
	
	
.to_line_2:
	ld a, HIGH(TEXT_LINE_2)
	ld [text_tile], a
	ld d, a
	ld a, LOW(TEXT_LINE_2)
	ld [text_tile+1], a
	ld e, a
	
	ldi a, [hl]

talking_anim_3:
	add 96
	call lcd_wait
	ld [de], a
	inc de
	
	ld a, [text_remain_space]
	dec a
	ld [text_remain_space], a
	
	ld a, h
	ld [talking], a
	ld a, l
	ld [talking+1], a
	
	ld a, d
	ld [text_tile], a
	ld a, e
	ld [text_tile+1], a
	
	jp move_check
	
	
start_jump:
	ld a, [io_p15_old]
	bit BUTTON_A, a
	jr z, end_frame
	
	ld a, 1
	ld [falling], a		;Indicate that we are rising
	
jump_anim:
	ld a, [falling]
	ld b, a
	ld a, [jumping]
	add b
	ld [jumping], a
	
	ld a, b					;*-1
	cpl
	inc a
	ld b, a
	
	ld a, [spr_y]
	add b
	ld [spr_y], a
	
	ld a, [jumping]
	cp 8
	jr nz, jump_anim_2
	ld hl, $0100
	call change_sprite
jump_anim_2:
	and $0F					;If first byte is 0, it's either 0 or 16
	jr nz, jump_anim_3
	ld hl, $0000
	call change_sprite
	ld a, [falling]
	dec a
	jr nz, jump_anim_3
	ld a, $FE				;Fall by 2 pixels at a time
	ld [falling], a
jump_anim_3:
	or a
	
	jp end_frame_jump
	
	
	
change_screen:
	
	
no_move:



	
;Currently flows into end_frame
finish_move:
	xor a
	ld [moving], a
	
end_frame:
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
	
end_frame_jump:
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

	ret
	
	
	
clear_text:
	ld hl, TEXT_LINE_1
	ld b, 18
	xor a
	
.top_loop:
	call lcd_wait
	ldi [hl], a
	dec b
	jr nz, .top_loop
	
	ld hl, TEXT_LINE_2
	ld b, 18
	
.bot_loop:
	call lcd_wait
	ldi [hl], a
	dec b
	jr nz, .bot_loop
	
	;ld hl, TEXT_CONT_TILE			;Clear cont symbol
	;ld [hl], a
	
	ret
	
	
move_text_up:
	push hl
		ld hl, TEXT_LINE_1
		ld de, TEXT_LINE_2
		ld b, TEXT_MAX_SPACE
	
.loop
		call lcd_wait
		ld a, [de]
		ldi [hl], a
		xor a
		ld [de], a
		inc de
		dec b
		jr nz, .loop
		
		;ld hl, TEXT_LINE_2
		;ld a, h
		;ld [text_tile], a
		;ld a, l
		;ld [text_tile+1], a
		
	pop hl
	ret
	
	
	
;----------------------------------------------------------------------------------




	
load_intro_data::
	xor a
	ld [curr_screen], a
	ld [screen_change], a 
	ld [talking], a
	ld [jumping], a
	ld [text_wait], a
	
	ld [fade_state], a	
	ld [moving], a
	ld [ori], a
	ld [palette_map_addr_lo], a
	
	ld a, TEXT_DELAY
	ld [text_tick], a
	ld a, TEXT_MAX_SPACE
	ld [text_remain_space], a
	
	ld hl, intro_map_npc_pos
	ldi a, [hl]
	ld [curr_npc_data+1], a
	ld a, [hl]
	ld [curr_npc_data], a
	
	ld b, $00
	call clear_sprite_map
	
	ld de, VRAM_TILES_BACKGROUND
	ld hl, game_tile_data
	ld bc, game_tile_data_end-game_tile_data
	call memcpy
	
	ld hl, VRAM_TILES_BACKGROUND_2				;For bitmap_loop, hl is destination and de is source
	ld de, BitmapFont
	ld bc, BitmapFontEnd-BitmapFont
	call bitmap_loop
	
	ld de, VRAM_TILES_BACKGROUND_2+(2*(BitmapFontEnd-BitmapFont))
	ld hl, menu_1_tile_data
	ld bc, menu_1_tile_data_end-menu_1_tile_data
	call memcpy
	
	ld de, VRAM_TILES_SPRITE
	ld hl, sprite_tile_data
	ld bc, sprite_tile_data_end-sprite_tile_data
	call memcpy
	
	ld hl, $8FF0		;Make Filler tile dark
	ld a, $FF
	ld b, 16
	call memcpy_single
	
	;ld hl, LCD_CTRL
	;set 5, [hl]
	ld a, 0
	ldh [WIN_Y],a		;WY - Window Y Position 0-143 (0=Top)
	ld a, 7
	ldh [WIN_X],a		;WX - Window X Position minus 7 (7=Left)
	
	ld hl, $9800		;Placing top/bottom/left/right borders
	ld b, 20
	ld c, 7
	ld d, $C0
	ld e, 1
	call draw_box
	
	ld hl, $9800
	ld b, 1
	ld c, 140
	call set_bg_pal
	
	ld hl, intro_0_mini_map_data
	ld de, $9C00
	ld bc, 10
	di
	call load_intro_map
	ld hl, palette_map_addr_2
	ld de, $9C00
	ld b, $5A			;90 vals in 16x16 map
	call load_game_palette_map
	call load_sprite
	ei
	ret
	
	
	
load_intro_map:
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
		jr c, load_intro_map_bg		; If we're dealing with wood blocks
		ld [hl], 1					; (vals 7 and onward), use palette 1
		cp $1A
		jr nz, load_game_map_loose
		ld [hl], STEEL_PAL
load_game_map_loose:
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
		jr c, load_intro_map_bg				;Checking if block val is within
		cp $24								;21 and 23
		jr nc, load_intro_map_bg
load_game_map_slime_2:
		ld [hl], SLIME_PAL

load_intro_map_bg:		;Skipped to if we are dealing with a bg block
	;The separation between this and unpack_block may be useful in the future
	;But for now, nothing is needed here--just continues to unpack_block
	
unpack_block:
	pop hl
	
	push hl
	push bc
		ld h, d					;Setting hl to vram address b/c we'll 
		ld l, e					;do 16 bit additions
		ld bc, 31				;Down a line in vram, 1 to the left
		
.wait:
		ldh a, [rSTAT]				;lcd_wait
		and STATF_BUSY
		jr nz, .wait
		
		ld a, [tile_1]
		ldi [hl], a
		ld a, [tile_2]
		ld [hl], a
		add hl, bc
		
.wait_2:
		ldh a, [rSTAT]				;lcd_wait
		and STATF_BUSY
		jr nz, .wait_2
		
		ld a, [tile_3]
		ldi [hl], a
		ld a, [tile_4]
		ld [hl], a
		
		ld hl, palette_map_addr_2
		ld a, [palette_map_addr_lo]
		push bc		
			ld b, 0
			ld c, a
			add hl, bc
		pop bc
		
		inc a
		ld [palette_map_addr_lo], a
		ld a, [block_palette]
		;call lcd_wait
		ld [hl], a				;Fill out palette map as we read tilemap. Stored from $D100-$D15A
	pop bc
	pop hl	
	
	
load_intro_map_2:
	inc de
	inc de
	inc hl
	dec bc
	ld a, b
	or c
	jp nz, load_intro_map
	
	ld bc, 44

	push hl
		ld h, d					;Setting hl to vram address b/c we'll 
		ld l, e					;do 16 bit additions
		add hl, bc
		ld d, h
		ld e, l
	pop hl
	
	ld a, d
	cp $9E
	jr nz, load_intro_map_3
	ld a, e
	cp $40
	ret z
	
load_intro_map_3:
	ld bc, 10
	jp load_intro_map


	
load_sprite:
	ld a, 7
	ld [player_y], a
	ld [new_player_y], a
	
	ld a, 2
	ld [player_x], a
	ld [new_player_x], a
	
	ld a, 40			;Tile X pos * 16 + 8 = Sprite X pos
	ld [spr_x], a
	
	ld a, 128
	ld [spr_y], a
	
	call load_sprite_2 	;From game.asm, inserts sprite data into the 4 sprites of Sirloin.
	
	ret
	