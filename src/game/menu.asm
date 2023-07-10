SECTION "menu_vars", WRAM0

curr_level:: DB
fade_step:: DB 

flicker_timer: DB
flicker_pal: DB
start_fade: DB
eye_timer: DB
menu_page: DB		;0 = Title Screen, 1 = Continue/New Game/Level Select/Options(?), 2 = Level Select
select_ind: DB		;What the player is selecting in a menu
select_up_down: DB		;If set, we went up
menu_switch: DB		;For miscellaneous use throughout the menu
choice_ind: DB		;Used when confirming if player wants to start new game

FLICKER_DELAY 	equ $32
EYE_DELAY 		equ $04
PAL_VISIBLE 	equ %11100100
PAL_INVISIBLE 	equ %11111111
MENU_SPRITE_X 	equ 72
MENU_SPRITE_Y 	equ 93
	
MENU_1_TEXT_START equ $9CC4
MENU_2_TEXT_START equ $9C41
MENU_2_MAP_START  equ $9CA5

MAX_SELECT 		equ 2

SECTION "menu", ROM0

;include "include/assets/game_map.inc"
;include "include/assets/menu_map.inc"
include "include/gb/constants.inc"
	
update_menu::
	ld a, [start_fade]
	cp 1
	jr nz, update_menu_4
	ld hl, GBPalMenu
	ld bc, 0
	call fade_out
	
	ld a, [menu_page]
	cp 2
	jr nz, update_menu_2
	ld hl, GBPalMenuLvl
	jr update_menu_3
update_menu_2:
	ld hl, GBPalFg1
update_menu_3:
	ld bc, 1*8
	call fade_out
	

	ld a, [fade_step]
	inc a
	ld [fade_step], a
	
	ld a, c
	cp 66
	ret nz
	
	ld bc, 2*8					;To ensure blocks are not white for a frame when
	call SetGBCPalettesBlack	;loading into a level. This way, the fade_in is
	ld bc, 3*8					;smooth
	call SetGBCPalettesBlack
	ld bc, 4*8
	call SetGBCPalettesBlack
	ld bc, 5*8
	call SetGBCPalettesBlack
	ld bc, 6*8
	call SetGBCPalettesBlack
	
	call reset_fade_step		;Not sure if something this simple needs to be a called function
	
	xor a
	ld [music_trigger], a		;Turn off menu music
	ld b, 0						;Mute any lasting notes from Channels 1&2
	ld c, 1
	call hUGE_mute_channel
	ld b, 1
	ld c, 1
	call hUGE_mute_channel
	
	ld a, [menu_page]
	or a
	jp z, start_intro
	jp start_game

update_menu_4:	
	ld a, [menu_page]
	cp 0
	jr z, menu_page_0
	cp 1
	jr z, menu_page_1
	cp 2
	jp z, menu_page_2
	cp 3
	jp z, menu_page_new_game
	ret
	
	
menu_page_0:
	call read_joypad
	ld a, [io_p15]
	bit BUTTON_A, a
	jp z, menu_check_save
	
	ld hl, flicker_timer
	dec [hl]
	jr nz, flicker_start_line
	ld [hl], FLICKER_DELAY 		;Reset flicker_timer once it reaches 0
	ld hl, flicker_pal
	ld a, [hl]
	cp PAL_VISIBLE
	jr z, flicker_not_vis
	
flicker_vis:
	ld a, PAL_VISIBLE
	ld [hl], a
	jr flicker_start_line

flicker_not_vis:
	ld a, PAL_INVISIBLE
	ld [hl], a

flicker_start_line:
	ldh a, [LCD_LINE_Y]
	cp 117
	jr nz, flicker_start_line
	ld a, [flicker_pal]
	ld d, a
	ld bc, 0 					;Setting BG palette and palette #0
	ld hl, GBPalMenu
	call SetGBCPalettesAdv
	
flicker_end_line:
	ldh a, [LCD_LINE_Y]
	cp 126
	jr nz, flicker_end_line
	ld d, %11100100
	ld bc, 0
	ld hl, GBPalMenu
	call SetGBCPalettesAdv

check_move_eyes:
	ld hl, eye_timer
	dec [hl]
	ret nz
	
	ld [hl], EYE_DELAY
	call random_number
	cp 8
	ret nc
	call move_eyes			;in player.asm currently
	ret
	
	
menu_check_save:
	call check_level_save
	ld [curr_level], a
	or a
	jp z, start_fade_anim
	
	ld hl, menu_select_sfx	;Play menu select sound
	call play_wave_seq_init
	jp to_page_1
	
menu_page_1:
	call read_joypad
	ld a, [io_p14]
	bit BUTTON_DOWN, a
	jr z, menu_page_1_down
	bit BUTTON_UP, a
	jr z, menu_page_1_up
	ld a, [io_p15]
	bit BUTTON_A, a
	ret nz
	ld a, [io_p15_old]
	bit BUTTON_A, a
	jr nz, menu_page_1_select
	ret
menu_page_1_down:
	ld a, [io_p14_old]
	bit BUTTON_DOWN, a
	ret z				;If press isn't new, don't register it.
	ld a, [select_ind]
	cp MAX_SELECT
	ret nc				;If select_ind is 2, it can't inc (currently 3 (0,1,2) options in menu_1)
	push af
		xor a
		ld [select_up_down], a	;0 means we went down
		
		ld hl, menu_move_sfx	;Play menu select sound
		call play_wave_seq_init
	pop af
	inc a
	ld [select_ind], a
	jr menu_page_1_update
menu_page_1_up:
	ld a, [io_p14_old]
	bit BUTTON_UP, a
	ret z				;If press isn't new, don't register it.
	
	ld a, [select_ind]
	or 0
	ret z				;If select_ind is 0, it can't go lower
	push af
		inc a
		ld [select_up_down], a	;!0 means we went up
		
		ld hl, menu_move_sfx	;Play menu select sound
		call play_wave_seq_init
	pop af
	dec a
	ld [select_ind], a
	
menu_page_1_update:
	ld hl, MENU_1_TEXT_START
	ld bc, $40
	jr z, menu_page_1_post_loop			;If zero flag is set after inc/dec of select_ind,
										;Then select_ind is 0 and MENU_1_TEXT_START is the right addr
	ld bc, $40
menu_page_1_update_loop:
	add hl, bc
	dec a
	jr nz, menu_page_1_update_loop
menu_page_1_post_loop:
	call lcd_wait
	ld [hl], $1E						;Tile number of > character
	ld bc, $40
	ld a, [select_up_down]
	or a
	jr nz, menu_page_1_clear_gt
	ld bc, -$40
menu_page_1_clear_gt:
	add hl,bc
	call lcd_wait
	ld [hl],00
	ret
	
menu_page_1_select:
	ld hl, menu_select_sfx	;Play menu select sound
	call play_wave_seq_init
	
	ld a, [select_ind]
	cp 0
	jp z, start_fade_anim
	cp 1
	jp z, to_new_game_confirm
	cp 2
	jp z, to_page_2
	ret

	
menu_page_2:
	call read_joypad
	ld a, [io_p14]
	bit BUTTON_LEFT, a
	jr z, menu_page_2_left
	bit BUTTON_RIGHT, a
	jr z, menu_page_2_right
	ld a, [io_p15]
	bit BUTTON_A, a
	jr z, menu_page_2_select
	bit BUTTON_B, a
	ret nz
	ld hl, menu_back_sfx	;Play menu select sound
	call play_wave_seq_init	
	jp to_page_1
	
menu_page_2_left:	
	ld a, [io_p14_old]
	bit BUTTON_LEFT, a
	ret z
	
	ld a, [select_ind]
	or a
	ret z
	
	dec a
	ld [select_ind], a
	call get_level
	
	ld hl, menu_move_sfx
	call play_wave_seq_init
	jr menu_page_2_display
	
menu_page_2_right:	
	ld a, [io_p14_old]
	bit BUTTON_RIGHT, a
	ret z
	
	call check_level_save
	ld b, a
	ld a, [select_ind]	
	cp b
	ret z
	inc a
	call get_level
	cp 255
	ret z
	
	ld a, [select_ind]
	inc a
	ld [select_ind], a
	
	ld hl, menu_move_sfx
	call play_wave_seq_init
	
menu_page_2_display:
	ld hl, MENU_2_TEXT_START
	call printString
	call render_map
	
	ret
	
menu_page_2_select:
	ld a, [io_p15_old]
	bit BUTTON_A, a
	ret z
	ld a, [select_ind]
	ld [curr_level], a
	ld hl, menu_select_sfx	;Play menu select sound
	call play_wave_seq_init
	jp start_fade_anim
	
	
menu_page_new_game:
	call read_joypad
	ld a, [io_p14]
	bit BUTTON_LEFT, a
	jr z, menu_page_new_game_left
	bit BUTTON_RIGHT, a
	jr z, menu_page_new_game_right
	ld a, [io_p15]
	bit BUTTON_A, a
	jr nz, menu_page_new_game_2
	ld a, [io_p15_old]
	bit BUTTON_A, a
	jr nz, menu_page_new_game_select
menu_page_new_game_2:
	bit BUTTON_B, a
	jr z, menu_page_new_game_b
	ret
	
menu_page_new_game_left:
	ld a, [io_p14_old]
	bit BUTTON_LEFT, a
	ret z
	ld a, [choice_ind]
	or a
	ret z
	dec a
	ld [choice_ind], a
	
	ld hl, menu_move_sfx	;Play menu move sound
	call play_wave_seq_init
	ld bc, $1E00
	jr update_new_game_text
	
menu_page_new_game_right:
	ld a, [io_p14_old]
	bit BUTTON_RIGHT, a
	ret z
	ld a, [choice_ind]
	or a
	ret nz
	inc a
	ld [choice_ind], a
	
	ld hl, menu_move_sfx	;Play menu move sound
	call play_wave_seq_init
	ld bc, $001E
	
update_new_game_text:
	ld a, b
	ld hl, $9864
	ld [hl], a
	ld a, c
	ld hl, $986A
	ld [hl], a
	ret
	
menu_page_new_game_b:
	ld a, 1
	ld [choice_ind], a
menu_page_new_game_select:
	ld  hl,$FF40		;LCDC - LCD Control (R/W)	DWwBbOoC  
	res 5,[hl]			;Turn off Window
	
	ld a, [choice_ind]
	or 0
	jr nz, menu_page_new_game_select_2
	
	ld [curr_level], a
	call update_level_save
	ld hl, menu_select_sfx	;Play menu select sound
	call play_wave_seq_init
	jr start_fade_anim
menu_page_new_game_select_2:
	ld [menu_page], a	;a = 1
	
	ld hl, menu_back_sfx	;Play menu back sound
	call play_wave_seq_init
	ret
	
	
to_page_1:
	ld a, 1
	ld [menu_page], a
	call load_menu_1_data
	ret

to_new_game_confirm:
	ld  hl,$FF40		;LCDC - LCD Control (R/W)	DWwBbOoC  
	set 5,[hl]			;Turn on Window
	
	ld a, 3
	ld [menu_page], a
	ld a, 1
	ld [choice_ind], a
	ret
	
to_page_2:
	ld a, 2
	ld [menu_page], a
	call load_menu_2_data
	ret
	
start_fade_anim:
	ld a, 1
	ld [start_fade], a
	call reset_fade_step	;Function found in misc.asm
	ret

;----------------------------------------------------------------------------
	
load_menu_data::
	;Set variables
	ld hl, flicker_timer
	ld [hl], FLICKER_DELAY
	ld hl, flicker_pal
	ld [hl], PAL_VISIBLE
	ld hl, eye_timer
	ld [hl], EYE_DELAY
	
	xor a
	ld [start_fade], a			;In case menu needs to be returned to, reset vars
	ld [fade_step], a
	ld [menu_page], a
	
	ld hl, LCD_CTRL
	res 4, [hl] 		;Set BG tile data area to 9800-9BFF
	set 3, [hl] 		;Set BG tile map area to 9C00-9FFF	
	
	ld b, 0 			;will be setting bg palette
    ld c, 0*8			;palette no 0 (back)
	ld hl, GBPalMenu
	call SetGBCPalettes
	
	;Load 800 bytes of tile data into $9000
	ld de, VRAM_TILES_BACKGROUND
	ld hl, menu_tile_data
	ld bc, menu_tile_data_end-menu_tile_data
	call memcpy
	
	ld de, VRAM_TILES_SPRITE
	ld hl, menu_sprite_tile_data
	ld bc, menu_sprite_tile_data_end-menu_sprite_tile_data
	call memcpy
	
	;Load remaining tile data into $8800
	;ld de, VRAM_TILES_MIX
	;ld hl, menu_tile_data + VRAM_MAP_BLOCK0_SIZE
	;ld bc, menu_tile_data_size - VRAM_MAP_BLOCK0_SIZE
	;call memcpy
	
	ld de, VRAM_MAP_BG
	ld hl, menu_map_data
	ld bc, 20
	call load_menu_map
	call load_menu_goblin
	
	ld hl, LCD_CTRL		;Make faster with ldh like in music_player
	set 1, [hl]			;Enabling sprites
	
	ld hl, rIE
	set 0, [hl]			;Enabling VBlank interrupt
	ei
	
	ret
	
load_menu_map:
	;call lcd_wait
	push hl
		push de
			call memcpy
		pop de
		ld h, d
		ld l, e
		ld bc, 32
		add hl, bc
		ld d, h
		ld e, l
	pop hl
	ld bc, 20
	add hl, bc
	
	ld a, d
	cp $9E
	jr nz, load_menu_map
	ld a, e
	cp $40
	jr nz, load_menu_map
	ret	
	
load_menu_goblin:
	ld b, 1
	ld c, 0
	ld hl, GBPalGoblin
	call SetGBCPalettes
	
	ld hl, $C400		;Our DMA transfer will be reading from C400 for sprite data
	ld b, 16			;Number of sprites
	ld c, 4				;width (in sprites)
	ld d, 0				;# to add to Y pos
	ld e, 0				;# to add to X pos
	
load_menu_goblin_loop:
	ld a, MENU_SPRITE_Y
	add d
	ldi [hl], a 		;a register holds spr_y value
	ld a, MENU_SPRITE_X
	add e
	ldi [hl], a 		;X
	ld a, 16			
	sub b				;Tile Number
	ldi [hl], a			
	ld a, %00000000
	ldi [hl], a
	
	dec b
	ld a, b
	cp 0
	ret z
	
	ld a, e
	add 8
	ld e, a
	
	dec c
	ld a, c
	cp 0
	jr nz, load_menu_goblin_loop
	ld c, 4
	ld a, d
	add 8
	ld d, a
	xor a
	ld e, a
	jr load_menu_goblin_loop
	
	
load_menu_1_data:	
	xor a
	ld [select_ind], a
		
	ld b, $00				;$00 is the black tile
	call clear_bg_map_and_pal
	call clear_sprite_map
	
	di								;I disable interrupts because hUGEDriver will interrupt loads to VRAM otherwise
	ld hl, VRAM_TILES_BACKGROUND	; start of patterns in video ram
	ld de, BitmapFont
	ld bc, BitmapFontEnd-BitmapFont
	
;Since each bit is loaded twice for the font, we couldn't use a simple memcpy
	call bitmap_loop
	
	ld hl, menu_1_tile_data
	ld de, $9400
	ld bc, menu_1_tile_data_end-menu_1_tile_data
	call memcpy
	ei
	
	;Load text
	ld de, continue_text
	ld hl, MENU_1_TEXT_START
	call printString
	
	ld de, new_game_text
	ld hl, MENU_1_TEXT_START+$40
	call printString
	
	ld de, level_select_text
	ld hl, MENU_1_TEXT_START+$40*2
	call printString
	
	;Load window
	ld a,104
	ldh [WIN_Y],a		;WY - Window Y Position 0-143 (0=Top)
	ld a, 7
	ldh [WIN_X],a		;WX - Window X Position minus 7 (7=Left)
	
	ld de, are_you_sure_text
	ld hl, $9823
	call printString
	
	ld de, yes_no_text
	ld hl, $9865
	call printString
	
	ld hl, $9800		;Placing top/bottom/left/right borders
	ld b, 20
	ld c, 5
	ld d, $40
	ld e, 0
	call draw_box
	
	ret
	
;----------------------------------------------------------------------------
	
;Menu 2 is the Level Select page
load_menu_2_data:
	;ld hl, menu_select_sfx	;Play menu select sound
	;call play_wave_seq_init
	
	di						;I disable interrupts because hUGEDriver will interrupt loads to VRAM otherwise
	ld b, $00				;$00 is the black tile
	call clear_bg_map
		
	
	ld b, 0 			;will be setting bg palette
    ld c, 1*8			;palette no 0 (back)
	ld hl, GBPalMenuLvl
	call SetGBCPalettes
	
	ld hl, menu_mini_lvl_tile_data
	ld de, $9400
	ld bc, menu_mini_lvl_tile_data_end-menu_mini_lvl_tile_data
	call memcpy
	
	ld hl, $9C84		;Placing border corners
	call lcd_wait
	ld [hl], $40
	ld hl, $9C8F
	ld [hl], $41
	ld hl, $9DC4
	call lcd_wait
	ld [hl], $42
	ld hl, $9DCF
	ld [hl], $43
	
	ld hl, $9C85		;Placing top/bottom/left/right borders
	ld a, $44
	ld b, 10
	call memcpy_single
	ld hl, $9DC5
	ld a, $45
	ld b, 10
	call memcpy_single
	ld hl, $9CA4
	ld c, $46			;with memcpy_single_var, c is the tile value
	ld b, 9				;and a is the val to inc hl by each step
	ld a, 32			
	call memcpy_single_var
	ld hl, $9CAF
	ld c, $47
	ld b, 9
	ld a, 32
	call memcpy_single_var
	
	xor a
	ld [select_ind], a
	ld [menu_switch], a
	
	call get_level
	ld hl, MENU_2_TEXT_START
	call printString
	call render_map
	
	ld a, 1
	ld [menu_switch], a
	
	ei
	ret
	
	
;Call with 'a' holding level #
;Returns with de holding addr of level name
;and bc holding addr of level addrs
get_level:
	ld hl, level_names
	ld bc, level_map_addrs
	or a
	rla
	
	;Update level name addr, but retain a
	push af
		add l
		ld l, a
	pop af
	
	;Update level map addr
	add c
	ld c, a
	ld a, [bc]
	push af
		inc bc
		ld a, [bc]
		ld b, a
	pop af
	ld c, a
	
	ldi a, [hl]
	ld e, a
	ld a, [hl]			;If this byte is 255, we've reached the end of names
	ld d, a
	ret


printString:
	ld a, [de]
	cp 255
	ret z ;end of program
	call printChar
	inc de
	inc hl
	jr printString
	
printChar:
	sub 32
	call lcd_wait
	ld [hl], a
	ret
	
	
;Assuming bc has level map addrs
render_map:
	ld de, MENU_2_MAP_START
	
	;Load hl with level map addrs
	push bc
	pop hl
	
	ld bc, 9
	push bc
menu_page_2_render_map_loop:
		ld bc, 10
		
		call render_map_memcpy
		
		ld a, 22
		add e
		ld e, a
		jr nc, menu_page_2_render_map_loop_nc
		inc d
menu_page_2_render_map_loop_nc:
	pop bc
	dec c
	push bc
		jr nz, menu_page_2_render_map_loop	
	pop bc
	ret
	

;hl is source, de is destination, bc is size
render_map_memcpy:
	ldi a, [hl]
	cp GOAL_16					;CHANGE TO BLOCK VALS, NOT TILE
	jr z, render_map_goal
	cp CW_TURNER_16
	jr z, render_map_cw_turner
	cp OPP_TURNER_16
	jr z, render_map_opp_turner
	cp CW_TURNER_BLOCK_16
	jr z, render_map_cw_turner_block
	cp OPP_TURNER_BLOCK_16
	jr z, render_map_opp_turner_block
	cp IMPASSABLE_BLOCK_16
	jr z, render_map_impassable
	cp LOOSE_BLOCK_16
	jr z, render_map_loose
	CP SOIL_BLOCK_16
	jr z, render_map_soil
	cp TRUE_PASSABLE_BLOCK
	jr nc, render_map_not_passable
	xor a				;Black BG tile
	jr render_map_update_tile
		
;WE COULD SIMPLIFY this. Just have the cp instrs and if nz, inc a and continue
render_map_goal:
	ld a, $49
	jr render_map_update_tile
render_map_cw_turner:
	ld a, $4A
	jr render_map_update_tile
render_map_opp_turner:
	ld a, $4B
	jr render_map_update_tile
render_map_cw_turner_block:
	ld a, $4C
	jr render_map_update_tile
render_map_opp_turner_block:
	ld a, $4D
	jr render_map_update_tile
render_map_impassable:
	ld a, $4E
	jr render_map_update_tile
render_map_loose:
	ld a, $4F
	jr render_map_update_tile
render_map_soil:
	ld a, $50
	jr render_map_update_tile
render_map_not_passable:
	ld a, $48
render_map_update_tile:
	push af
.loop:
		ldh a, [rSTAT]
		and STATF_BUSY
		jr nz, .loop
	pop af
	
	ld [de], a
	
	ld a, [menu_switch]
	or a
	jr nz, render_map_finish
	
	push bc
		ld bc,$FF4F	;VBK - CGB Mode Only - VRAM Bank
		inc a	;Turn on GBC extras, a=1
		ld [bc],a	
		call lcd_wait
		ld [de], a	;Palette 1
		
		xor a		;Turn off GBC extras
		ld [bc],a	
	pop bc
render_map_finish:
	inc de
	dec bc
	ld a, c
	or b
	jr nz, render_map_memcpy
	ret
			
;----------------------------------------------------------------------------

continue_text:
db ">CONTINUE", 255
new_game_text:
db " NEW GAME", 255
level_select_text:
db " LEVEL SELECT", 255
are_you_sure_text:
db "ARE YOU SURE?", 255
yes_no_text:
db "YES  >NO", 255