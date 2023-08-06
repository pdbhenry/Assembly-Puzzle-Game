SECTION "game_vars", WRAM0

fade_state:: DB
player_x:: DB
player_y:: DB
new_player_x:: DB
new_player_y:: DB
spr_x:: DB
spr_y:: DB
ori:: DB 					;0=down, 1=up, 2=right, 3=left
moving:: DB					;if set, 0=down, 1=up, 2=right, 3=left; informs where the player is going (input or falling)
moving_step:: DB
move_xy:: DW				;Byte 1 is spr_x change per frame and byte 2 is spr_y change
move_xy_old:: DW				;Holds original movement of input before altered by a ramp
move_diag_down:: DB			;Holds the value spr_x or spr_y would change by to move diagonally.
moved:: DB					;If set, we have moved this frame. Used to facilitate smooth movement each frame
dest_tile_addr:: DW 		;Holds the tile address that goblin is actively moving to
curr_level_addr_hi:: DB
curr_level_addr_lo:: DB
falling:: DB				;If not zero, goblin is falling. Used for animation and to increase speed of move_anim
facing:: DB					;Used for knowing where the character is facing
blocks_pushed:: DB			;Stores # of blocks being pushed with a jump_check
;blocks_pushed_2:: DB		;Stores the same val, but doesn't dec
tile_1:: DB					;4 tiles to make a 16x16 block. Used when pushing and loading the map
tile_2:: DB					;From 1-4, tiles in order of top-left, top-right, bottom-left, bottom-right
tile_3:: DB
tile_4:: DB
block_palette:: DB
palette_map_addr_lo:: DB	;High byte will be static $D1. Only lo changes
palette_map_addr_2:: DS 90
ori_changed:: DB 			;Set when orientation was changed last frame
push_tile:: DB				;Just stores tile we're looking at currently during a check_block_push loop
block_push_addr_hi:: DB		;For block push anim. Holds addr of tile that was last shifted.
block_push_addr_lo:: DB		;This addr is so the second half of the anim knows where we left off.
block_push_next_line:: DB	;Holds val that brings us to the next row/col of tiles (u/d/l/r)
block_push_adj_tile:: DB	;Holds val that brings us to the adjacent tile (we change 2 at a time)
							;Make a switch that tells if at least 1 loose block is in the map (Ehh not too necessary)
uncovered_block_hi:: DB		;Holds address of header in block_tiles.inc for 
uncovered_block_lo:: DB		;block being uncovered in a push
turning_corner:: DB			;If set, Gobbo is mid-turning corner anim. Anim needs to resolve
old_ori:: DB				;Store previous ori to aid with corner turn animation
power:: DB					;Curr power, dictates Gobbo's appearance & B-button functionality. 0=default, 1=fire, 2=ice
;ice_block_count:: DB		;# of ice blocks being pushed
;ice_block_hi:: DB			;Addr of first ice block in sequence to be pushed
;ice_block_lo:: DB
;ice_block_dest_hi:: DB		;Addr of where first ice block will end up after a slide
;ice_block_dest_lo:: DB
;ice_slide_count:: DB		;Tell if ice has slid a whole step or half
;ice_slide_delay:: DB		;We don't want ice blocks to move 1 tile per frame so this slows it.
;ice_block_next_line:: DB
;ice_block_adj_tile:: DB
;ice_block_slimed:: DB		;If non-zero, the last ice block is slimed

facing_temp:: DB			;Giving facing val to this temp before verifying if its correct
using_power:: DB			;If set, Gobbo's power anim is still going and another can't be started
push_ori:: DB				;Holds ori of Gobbo when he started a push. Used when ori can change b4 a slide ends
rocketing:: DB				;Tells us if Gobbo has been burned and is shootin upwards
rocket_push:: DB			;If non-zero, tells us we've pushed a block with rocketing
earth_making:: DB			;If non-zero, earth power is being used. 1=soil, 2=wood being made
earth_addr_hi:: DB			;Map addr of where earth block is being made
earth_addr_lo:: DB
earth_below_pal:: DB
earth_curr_pal:: DB
mid_air:: DB
slime_block_hi:: DB			;Holds map addr of slime block being pulled
slime_block_lo:: DB
uncovered_slime_hi:: DB		;Holds the block_tiles.inc header addr of 
uncovered_slime_lo:: DB		;the block being uncovered by slime pull
slimed_hi:: DB				;Holds the map address of where a block
slimed_lo:: DB				;getting slimed. Used after push finishes.
power_index:: DB			;Used to tell us index of level's power array we are at.
casting:: DB				;If a flame is present (non-zero val) then we can't move.
earth_casting:: DB
rampable_1:: DB
rampable_2:: DB
rampable_3:: DB
rampable_4:: DB
ramp_front:: DB
ramping:: DB				;If set, we are moving up a ramp. Informs what happens at end of move_anim

block_front:: DW			;16 bits to hold value that needs to be added to hl to get blocks in front and below hl's current position
block_below:: DW			;(front and below depending on Sirloin's ori and face). Can get behind/above from these vals
new_ori:: DB					;Holds val of possible ori change depending on where Sirloin is walking

SECTION "game_vars_2", WRAM0[$C200]
open_slide_spot:: DB		;Gives the low byte of C1XX addr of first 
							;available spot for ice slide vars to be kept
							;First set to C163
ongoing_slides:: DB			;Val of how many lines of ice are sliding simultaneously
slides_count:: DB			;# of lines of ice we have yet to process.
curr_slide_spot:: DB
ice_slide_space:: DB 		;Probably needs to be DS <big number>

ramping_blocks_ind:: DB		;Holds index for array below (array currently supports up to 5 blocks (9 bytes * 5))
push_origins_ind:: DB		;Ind for arr below. push_origins allows multiple pushes in one frame
total_segs_pushed:: DB		;Ind for arr below
curr_push:: DB				;Holds curr ind of push we're processing
turn_switch:: DB			;If nz, we have turned and are checking if a block is right after that turn.
action_dir_vert:: DB		;A ramp's two action dirs are the directions that, if pushed or approached in, can lead to ramping
action_dir_hor:: DB			;Ex: A BL ramp (|\) has the actions dirs of UP and RIGHT

ramping_blocks:: DS 55		;11, Holds data of blocks that will be shifted by ramps (orig pos, new pos, ramp pos, pal, tiles)
push_origins:: DS 20		;4, Holds the map addr of first block in a push. Also holds the next/adj tile vals.
blocks_pushed_arr:: DS 5	;1, Holds the # of blocks pushed for each push happening
uncovered_blocks:: DS 10	;2, Holds addrs of header in block_tiles.inc for blocks being uncovered in a push

block_push_addr:: DS 10		;2, For block push anim. Holds addr of tile that was last shifted.
next_line_arr:: DS 5
adj_tile_arr:: DS 5

SECTION "game_vars_3", WRAM0[$C300]
visited_ind:: DB
visited:: DS 30				;Can push a max of 15 blocks currently
		
		
SECTION "game", ROM0

include "include/gb/constants.inc"

update_game::
	ld a, [fade_state]
	cp 1
	jp z, pre_input
	cp 2
	jr z, do_fade_out

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
	ld [music_trigger], a	;Enable music if it isn't already	
	ld b, 0
	ld c, 0
	call hUGE_mute_channel
	ld b, 1
	ld c, 0
	call hUGE_mute_channel
	ret 	;jp end_frame		;ret					;!!!
do_fade_out:
	;call wait_vblank
	ld hl, GBPalBg1
	ld bc, 0*8
	call fade_out
	ld hl, GBPalFg1
	ld bc, 1*8
	call fade_out
	ld hl, GBPalSoil
	ld bc, 2*8
	call fade_out
	ld hl, GBPalSteel
	ld bc, 3*8
	call fade_out
	ld hl, GBPalFrost
	ld bc, 4*8
	call fade_out
	ld hl, GBPalFire
	ld bc, 5*8
	call fade_out
	ld hl, GBPalSlime
	ld bc, 6*8
	call fade_out
	
	ld a, [fade_step]
	inc a
	ld [fade_step], a
	
	ld a, c
	cp 66
	ret nz;	jp nz, end_frame	;ret nz					;!!!
	ld a, [curr_level]			;Need a to come with curr_level in next function
	call update_level_save		;**************BRING BACK LATER
	call reset_fade_step
	jp z, start_game

;Checking things that would override input if set
pre_input:
	ld a, [ongoing_slides]
	or a
	jr z, pre_input_2
	
	call ice_slide
	ld a, [ongoing_slides]		;If, after ice_slide, ongoing_slides is now 0,
	or a						;and player is mid-air, we can check if player 
	jr nz, pre_input_2			;should fall
	
	ld hl, ice_stop_sfx
	call play_noise
	
	ld a, [mid_air]
	or a
	jr z, pre_input_2
	xor a
	ld [mid_air], a
	call get_tile
	ld d, 0
	jp moving_check_below
	
pre_input_2:
	;ld a, [ice_block_count]
	;or a
	;call nz, ice_slide
	ld a, [mid_air]
	or a
	jp nz, end_frame
	
	ld a, [casting]
	or a
	jp nz, end_frame
	
	ld a, [moving]
	or a
	jp nz, move_anim
	
	ld a, [turning_corner]
	or a
	jr z, get_input
	dec a
	ld [turning_corner], a
	jp nz, end_frame
	ld hl, 0
	call change_sprite
	call get_tile
	jp moving_check_below_no_call
	

get_input:
	call read_joypad
	ld a, [io_p15]
	bit BUTTON_A, a
	jp z, jump_check
	bit BUTTON_B, a
	jp z, power_check
	bit BUTTON_SELECT, a
	jp z, power_change
	bit BUTTON_START, a
	jp z, reset_level
	
get_dir_input:
	ld a, [io_p14]
	cp %11101111
	jp z, end_frame;	ret z					;!!! If no directions pressed, loop
	
	push af
		ld a, [player_x]
		ld [new_player_x], a
		ld a, [player_y]
		ld [new_player_y], a
	pop af
	
	ld b, a
	ld [moving], a

	call ori_face_var_setup						;Refines moving/facing and sets vars based on ori and face	
	cp $66										;If a returns with val $66, input does not result in a move
	jp z, finish_move
	
check_collision:
	;ld [facing_temp], a			;Now that moving is settled, we can set facing to the non-diagonal input
	
	ld a, [new_player_x]		;Check bounds
	cp 10
	jp nc, change_ori
	ld a, [new_player_y]
	cp 9
	jp nc, change_ori
	
	call get_tile	
	call lcd_wait
	ld a, [hl]
	
	cp PASSABLE_TILE	
	jp c, check_collision_2
	ld b, a
	ld a, [rampable_1]
	cp b
	jp nz, change_ori
	
	ld b, a						;Save ramp's TL tile
	ld a, [facing_temp]
	ld [facing], a
	jp ascend_ramp_init
	
check_collision_2:
	ld a, [facing_temp]
	ld [facing], a
	

;check_ramp_below:
;	push hl
;		ld a, [block_below]
;		ld b, a
;		ld a, [block_below+1]
;		ld c, a
;		add hl, bc
;		call lcd_wait
;		ld a, [hl]
;		ld b, a
;		ld a, [rampable_2]
;		cp b
;		jr nz, pre_begin_move
;		
;		ld a, [block_front]
;		ld b, a
;		ld a, [block_front+1]
;		ld c, a
;		add hl, bc
;		call lcd_wait
;		ld a, [hl]
;		cp PASSABLE_TILE
;		jr nc, pre_begin_move
;	
;		call use_ramp				;Sets destination tile, player_x/y, spr_x/y, and stops moving
;		ld a, [move_xy]				;Whichever is set to 0 (x or y), needs to be set to move_diag_down
;		or a
;		ld a, [move_diag_down]
;		jr nz, .move_y
;		ld [move_xy], a
;.move_y:
;		ld [move_xy+1], a
	
pre_begin_move:
	;pop hl
	xor a							;If approaching begin_move from here, we certainly have not moved
	ld [moved], a					;in this frame

;Set up for when we know Sirloin will be traveling from one grid space to another.
begin_move:
	xor a
	ld [ori_changed], a		;Orientation is not being changed this frame, so unset
	
	ld a, h
	ld [dest_tile_addr], a
	ld a, l
	ld [dest_tile_addr+1], a
	
	ld a, [new_player_x]
	ld [player_x], a
	ld a, [new_player_y]
	ld [player_y], a
		
	ld a, 16
	ld [moving_step], a
	
	ld a, [falling]
	or a
	jp nz, begin_move_end
	ld a, [earth_making]
	or a
	jr z, begin_move_2
	ld h, $1C
	call change_pose
	ld hl, earth_sfx
	call play_wave_adv_init
	jr begin_move_end
begin_move_2:
	ld hl, 0
	call change_sprite
begin_move_end:
	ld a, [moved]			;Check if we've moved in this frame (ending a fall, see fall continues, and jump
	or a					;back to begin_move)
	jp nz, end_frame
	inc a
	ld [moved], a
	
move_anim:					;Kinda lame we do this check each time...
	ld a, [move_xy]
	ld b, a
	ld a, [spr_x]
	add b
	ld [spr_x], a
	
	ld a, [move_xy+1]
	ld b, a
	ld a, [spr_y]
	add b
	ld [spr_y], a
	
move_sprite:
	call update_sprite_pos			;Update Sirloin sprite positions based on spr_x, spr_y
	
moving_step_check:
	ld a, [moving_step]
	ld b, a
	dec b
	ld a, [falling]					;If falling, moving_step decreases twice as fast
	or a
	jr z, moving_step_check_2
	dec b
moving_step_check_2:
	ld a, [rocketing]				;;If rocketing, moving_step decreases 4x as fast
	or a
	jr z, moving_step_check_3
	dec b
	dec b
	dec b
moving_step_check_3:
	ld a, b
	ld [moving_step], a
	cp 8							;Halfway point of moving to a tile
	jp nz, moving_step_check_4		
	ld hl, $0100
	call change_sprite
	ld a, [total_segs_pushed]		;Using total_segs_pushed as a way to tell
	or a							;if we're in the middle of pushing
	jp z, ret_ramp					;		end_frame		;ret z							;!!!
;------------------FINISHING BLOCK PUSH ANIM---------------------------------
	
finish_block_push:
	xor a
	ld [curr_push], a
	
finish_block_push_loop:
	ld b, a
	ld a, [total_segs_pushed]
	cp b
	jp z, ret_ramp					;moving_step_push_end
	
	ld a, [curr_push]
	sla a							;curr_push x 2, block_push_addr stores 2-byte addrs at a time
	ld de, block_push_addr
	add e
	ld e, a
	ld a, [de]
	ld h, a
	inc e
	ld a, [de]
	ld l, a
	
	ld a, [curr_push]
	ld de, next_line_arr
	add e
	ld e, a
	ld a, [de]
	ld b, 0
	cp $E0							;In case b is a negative number
	jr c, moving_step_push
	ld b, $FF
moving_step_push:
	ld c, a
	add hl, bc						;Move hl forward one tile
	ld b, a
	
	ld a, [curr_push]
	ld de, adj_tile_arr
	add e
	ld e, a
	ld a, [de]
	ld c, a
	
;	ld a, [blocks_pushed_2]
	ld a, [curr_push]
	ld de, blocks_pushed_arr
	add e
	ld e, a
	ld a, [de]
;	cp 2							;If blocks_pushed_2 is <default val of 2, 
;	jp c, ret_ramping_blocks;		moving_step_push_end	;we're just pushing ice and this anim is not needed
	ld [blocks_pushed], a
	
	push hl
		ld a, [curr_push]
		sla a						;curr_push * 2, uncovered_blocks has 2-byte units
		ld hl, uncovered_blocks
		add l
		ld l, a
		ldi a, [hl]
		ld d, a
		ld e, [hl]
	pop hl
	
	;Start tile_1/tile_2 off as top-left/top-right tiles
	ld a, [de]
	ld [tile_1], a
	inc de
	ld a, [de]
	ld [tile_2], a
	
	xor a							;!!! A potential fix to storing pal from
	ld [block_palette], a			;!!! first 8 pixel push
	
	ld a, b
	;push bc							;Have to retrieve next_line val again for dir check
	;	ld a, [curr_push]
	;	ld bc, next_line_arr
	;	add c
	;	ld c, a 
	;	ld a, [bc]
	;pop bc	
	cp 32							;If push is down
	jr nz, moving_step_push_left

	inc de
	ld a, [de]						;Bottom-left
	ld [tile_1], a					;Set both tiles to bg tile 2
	inc de
	ld a, [de]						;Bottom-right
	ld [tile_2], a
	jr moving_step_push_block
moving_step_push_left:
	cp $FF							;If push is left (-1)
	jr nz, moving_step_push_right

	inc de
	ld a, [de]						;Bottom-left
	ld [tile_2], a					;Set tile_2 to bg tile 2
	jr moving_step_push_block
moving_step_push_right:				;***Currently no different from left
	cp 1							;If push is right
	jr nz, moving_step_push_block		;Else, it's up and we have those vals loaded
	
	ld a, [de]						;Top-right
	ld [tile_1], a
	inc de
	inc de
	ld a, [de]						;Bottom-right
	ld [tile_2], a					;Set tile_2 to bg tile 2
	
moving_step_push_block:
	call block_push_loop
	ld a, [curr_push]
	inc a
	ld [curr_push], a
	jp finish_block_push_loop
	
ret_ramp:
	call ret_ramping_blocks

moving_step_push_end:	
	xor a							;Resetting total_segs_pushed here since we use it to  
	ld [total_segs_pushed], a		;check if we're currently pushing anything
	ld [ramping_blocks_ind], a
    ld [turn_switch], a
    ld [push_origins_ind], a
	ld [visited_ind], a
	
	ld b, 0				;To signal we are mid push for get_uncovered_tiles
	call moving_step_stick
	jp z, moving_step_push_end_4
	ld c, b							;We are isolating next_line to move slime
	ld b, 0							;block a half step
	ld a, c
	cp $E0
	jr c, moving_step_push_end_3
	ld b, $FF
moving_step_push_end_3:
	add hl, bc
	ld a, h
	ld [slime_block_hi], a
	ld a, l
	ld [slime_block_lo], a
moving_step_push_end_4:
	jp end_frame

	
moving_step_check_4:
	cp 6							;If 3/4th the way through a jump
	jr nz, moving_step_check_5
	ld a, [earth_making]			;and using earth power
	or a
	jp z, end_frame
	cp 3							;If higher than 2, 7th bit is set and we end
	jp nc, end_frame
	set 7, a						;To retain the earth_making val but show that 
	ld [earth_making], a			;we've created an anim already, we set 7th bit
	call create_earth_anim			
	jp end_frame
	
	
;---------------------------END OF MOVE---------------------------------
moving_step_check_5:
	or a							;!!!CP 0, END OF MOVE
	jp nz, end_frame				;ret nz							;!!!
	
	ld hl, 0
	call change_sprite
	
	ld b, 1							;To signal we are full push for get_uncovered_tiles
	call moving_step_stick
	xor a
	ld [slime_block_hi], a			;Slime stick is complete
	
	ld a, [slimed_hi]				;Checking if we have pushed a block 
	or a							;into a slime bg space
	jr z, moving_step_check_6
	ld h, a
	ld a, [slimed_lo]
	ld l, a
	ld a, SLIME_PAL
	call set_block_pal
	xor a
	ld [slimed_hi], a				;Reset slimed_hi
	;ld [slimed_lo], a
	
moving_step_check_6:
	ld a, [ramping]
	or a
	ld b, a
	jr z, moving_step_check_7
	call get_front
	call lcd_wait
	ld a, [hl]
	cp b
	jp z, ascend_ramp
	
	xor a							;If ramp leads to no block in front, move ahead past ramp without ascending
	ld [ramping], a
	ld a, [move_xy_old]
	ld [move_xy], a
	ld b, a
	ld a, [new_player_x]
	add b
	ld [new_player_x], a

	ld a, [move_xy_old+1]
	ld [move_xy+1], a
	ld b, a
	ld a, [new_player_y]
	add b
	ld [new_player_y], a
	jp begin_move
	
moving_step_check_7:
	ld a, [dest_tile_addr]
	ld h, a
	ld a, [dest_tile_addr+1]
	ld l, a
	
	call lcd_wait
	ld a, [hl]
	cp FINISH_TILE
	jr nz, moving_check_cw_turn
	ld a, 2
	ld [fade_state], a
	ld hl, curr_level
	inc [hl]						;Move to next level
	jp finish_move
	
moving_check_cw_turn:
	cp CW_TURN_TILE
	jr z, moving_do_cw_turn
	cp CW_TURN_SOLID_TILE
	jr nz, moving_check_opp_turn
moving_do_cw_turn:
	ld a, [ori]			;Nasty nasty way of getting 0 (down) to 3 (left),
	cpl					;1 (up) to 2 (right), 2 (right) to 0 (down),
	and %00000011
	cp 2				;and 3 (left) to 1 (up). Dw, it works.
	jr nc, moving_do_cw_turn_3
	bit 0, a
	jr z, moving_do_cw_turn_2
	dec a
	jr moving_do_cw_turn_3
moving_do_cw_turn_2:
	inc a
moving_do_cw_turn_3:
	ld [ori], a
	jr moving_finish_turn
	
moving_check_opp_turn:
	cp OPP_TURN_TILE
	jr z, moving_do_opp_turn
	cp OPP_TURN_SOLID_TILE
	jr nz, pre_moving_check_below
moving_do_opp_turn:
	ld a, [ori]			;Complementing the first bit
	ld b, a				;This turns 0 (down) to 1 (up),
	res 0, b			;1 to 0, 2 (right) to 3 (left),
	cpl					;and 3 to 2
	and %00000001
	add b
	ld [ori], a
	
moving_finish_turn:
	push hl
		ld hl, 0
		call change_sprite
	pop hl
	
pre_moving_check_below:
	ld a, [rocketing]
	or a
	jp nz, rocketing_check		;Skip check below if rocketing up
	
moving_check_below_no_call:		;Tells us moving_check_below is not being called
	ld d, 0						;Otherwise, it stops after finding below block
;Checks tile below (according to orientation) and sees if Goblin should fall
moving_check_below:
	ld b, 0
	ld a, [ori]
	cp 0
	jr nz, moving_check_below_up

	ld c, 64
	add hl, bc
	
	;ld a, [loose_below_switch]
	;add d						
	ld a, d 					;If d is non-zero, we're just calling 
	or a						;moving_check_below to get block below Gobbo
	ret nz
	
	ld a, RAMP_BL_TILE			;Storing what ramp tiles Sirloin could ramp down
	ld [rampable_1], a			;at this orientation
	ld a, RAMP_BR_TILE			;rampable_1 should be the one that would cause a positive change in non-gravity axis
	ld [rampable_2], a			;Ex: Gravity axis when ori is down is y. We are moving down in y pos, but up in non-gravity axis, x, with BL ramp
	ld a, 2						;Value added to ramp tile address to get the tile "in front" of it
	ld [ramp_front], a			;to check if a ramping would be blocked like [_\[_] !!!Check if it can be replaced by block_front
								;If rampable_2 is the ramp present, ramp_front is subbed instead of added
	
	ld a, [new_player_y]
	inc a
	ld [new_player_y], a
	ld a, %11100111						;Replicates 'down' input. We change 'moving' 
	ld b, %11101011						;Store 'up' input in case Gobbo is burned and shoots up
	ld [moving], a						;because sprite movement is based on this val. 
	xor a
	ld [move_xy], a
	ld a, 2
	ld [move_xy+1], a
	
	jp moving_check_below_2				;We want to fall down rather than left/right (our last input)
moving_check_below_up:
	cp 1
	jr nz, moving_check_below_right

	ld bc, -64
	add hl, bc
	
	;ld a, [loose_below_switch]
	;add d
	ld a, d
	or a
	ret nz
	
	ld a, RAMP_TL_TILE			;See comments in moving_check_below for same vars
	ld [rampable_1], a			
	ld a, RAMP_TR_TILE			
	ld [rampable_2], a			
	ld a, 2						
	ld [ramp_front], a
	
	ld a, [new_player_y]
	dec a
	ld [new_player_y], a
	ld a, %11101011
	ld b, %11100111
	ld [moving], a
	xor a
	ld [move_xy], a
	ld a, -2
	ld [move_xy+1], a
	
	jr moving_check_below_2
moving_check_below_right:
	cp 2
	jr nz, moving_check_below_left
	ld c, 2
	add hl, bc
	
	;ld a, [loose_below_switch]
	;add d
	ld a, d
	or a
	ret nz
	
	ld a, RAMP_TR_TILE			;See comments in moving_check_below for same vars
	ld [rampable_1], a			
	ld a, RAMP_BR_TILE			
	ld [rampable_2], a			
	ld a, $40						
	ld [ramp_front], a
	
	ld a, [new_player_x]
	inc a
	ld [new_player_x], a
	ld a, %11101110
	ld b, %11101101
	ld [moving], a
	ld a, 2
	ld [move_xy], a
	xor a
	ld [move_xy+1], a
	
	jr moving_check_below_2
moving_check_below_left:
	;ld a, [player_x]
	;cp 0
	;jp z, finish_move
	dec hl
	dec hl
	
	;ld a, [loose_below_switch]
	;add d
	ld a, d
	or a
	ret nz
	
	ld a, RAMP_TL_TILE			;See comments in moving_check_below for same vars
	ld [rampable_1], a			
	ld a, RAMP_BL_TILE			
	ld [rampable_2], a			
	ld a, 64						
	ld [ramp_front], a
	
	ld a, [new_player_x]
	dec a
	ld [new_player_x], a
	ld a, %11101101
	ld b, %11101110
	ld [moving], a
	ld a, -2
	ld [move_xy], a
	xor a
	ld [move_xy+1], a
	
moving_check_below_2:
	call lcd_wait			;We are reading from vram map
	ld a, [hl]
	cp IMPASSABLE_TILE
	jr nz, moving_check_below_3
	call get_tile_pal
	cp HOT_PAL
	jp nz, reset_move
	ld a, b
	ld [moving], a
	ld [rocketing], a		;Give rocketing a non-zero val
	xor a
	ld [falling], a
	call reset_player_pos
	call get_tile
	;call moving_check_above
	jp rocketing_check
	
	jp begin_move
	
moving_check_below_3:
	cp PASSABLE_TILE
	jp nc, moving_check_below_ramp
	ld a, 1
	ld [falling], a
	jp begin_move

;If a ramp is under us, we may slide down it	
moving_check_below_ramp:
	ld d, a
	ld a, [rampable_1]
	cp d
	jr nz, moving_check_below_ramp_2
	ld a, [ramp_front]
	add l
	ld l, a
	jr nc, .nc
	inc h
.nc:
	ld a, 2							;Value we are changing in non-gravity axis.
	jr moving_check_below_ramp_3
	
moving_check_below_ramp_2:
	ld a, [rampable_2]
	cp d
	jp nz, reset_move				;In this case, we are dealing with a non-passable, non-ramp tile and should stop moving
	ld a, [ramp_front]
	ld d, a
	ld a, l
	sub d
	ld l, a
	jr nc, .nc_2
	dec h
.nc_2:
	ld a, -2
	
moving_check_below_ramp_3:
	ld b, a
	
	call lcd_wait
	ld a, [hl]
	cp PASSABLE_TILE
	jp nc, reset_move				;If non-passable block is in front of ramp, don't ramp down
	
	ld a, [move_xy]					;Find which move is 0 (x or y)
	or a
	ld a, b
	jr nz, .move_y
	ld [move_xy], a
	jr moving_check_below_ramp_4

.move_y:
	ld [move_xy+1], a
	
moving_check_below_ramp_4:
	ld a, 1
	ld [falling], a					;CONTINUE HERE, SLIDING DOWN RAMP ANIM!!!!
	call use_ramp
	jp begin_move
	
;!!!!!!!!Falling on ramp code here
	

rocketing_check:
	;call get_above
	ld d, 1						;So moving_check_above knows to return
	call moving_check_above
	call lcd_wait
	ld a, [hl]
	cp PASSABLE_TILE
	jp c, rocketing_check_end
	cp SOIL_TILE
	jr nz, rocketing_check_2
	call destroy_soil
	jp begin_move
	
rocketing_check_2:
	ld a, [rocket_push]
	or a
	jr nz, rocketing_check_3
	inc a
	ld [rocket_push], a
	jp moving_check_above_3
	
rocketing_check_3:
	call invert_ori
	xor a
	ld [rocket_push], a
	ld a, [ongoing_slides]
	ld [mid_air], a				;
	jp reset_move
	
rocketing_check_end:
	jp begin_move
	
	
;Called by moving_step_check at cp 8 and cp 0
moving_step_stick:
	ld a, [uncovered_slime_hi]
	ld d, a
	ld a, [uncovered_slime_lo]
	ld e, a
	ld a, [ori]
	add a
	add b									;b is 1 if mid push, 0 if full push
	call get_uncovered_tiles_call
	
	ld a, [slime_block_hi]
	or a
	ret z;jp z, end_frame					;ret			;???!!! 
	ld h, a
	ld a, [slime_block_lo]
	ld l, a
	ld a, [block_push_next_line]
	ld b, a
	ld a, [block_push_adj_tile]
	ld c, a
	
	;xor a
	;ld [tile_1], a
	;ld [tile_2], a
	ld a, 3
	ld [blocks_pushed], a
	xor a
	ld [block_palette], a
	push hl
		call block_push_loop
	pop hl
	inc a									;Get rid of zero flag
	ret

	
;Resetting ice_block_count of current open slide spot
;Now resets earth_making as well
reset_ice_and_move:
	;pop hl			;hl was pushed in moving_check_above_4
	ld h, $C1
	ld a, [open_slide_spot]
	ld l, a
	xor a
	ld [hl], a		;leads into reset_move
	ld [earth_making], a
	
	ld a, [rocket_push]
	or a
	jr nz, rocketing_check_3

reset_move:
	xor a
	ld [total_segs_pushed], a
	ld [ramping_blocks_ind], a
	call reset_player_pos
	jp finish_move
	
reset_player_pos:
	ld a, [player_x]
	ld [new_player_x], a
	ld a, [player_y]
	ld [new_player_y], a
	ret						

	
;If d is non-zero, this function is being called for rocketing purposes
moving_check_above:
	ld b, 0
	ld a, [ori]
	cp 0
	jr nz, moving_check_above_up		;Sort by orientation
	
	ld a, [player_y]
	add d								;If d is non-zero, we'll offset a's val and not 	
	or a								;worry about finishing move
	jp z, finish_move					;Check if on edge of bounds
	
	ld bc, -64
	add hl, bc
	ld a, [new_player_y]
	dec a
	ld [new_player_y], a				;Set jump position
	ld a, %11101011						;Replicates 'up' input. We change 'moving' 
	ld [moving], a						;because sprite movement is based on this val. 
	xor a
	ld [move_xy], a
	ld a, -1
	ld [move_xy+1], a
	ld a, RAMP_TL_TILE
	ld [rampable_3], a					;rampable_3 holds tile val for ramp that, if jumped into,
	ld a, RAMP_TR_TILE					;push Sirloin in a positive direction.
	ld [rampable_4], a					;rampable_4 is for ramp that pushes in the negative direction
	ld a, $2						
	ld [ramp_front], a
	
	jp moving_check_above_2				;We want to jump up rather than left/right (our last input)
	
moving_check_above_up:
	cp 1
	jr nz, moving_check_above_right
	
	ld a, [player_y]
	add d
	cp 8
	jp z, finish_move
	
	ld c, 64
	add hl, bc
	ld a, [new_player_y]
	inc a
	ld [new_player_y], a
	ld a, %11100111
	ld [moving], a
	xor a
	ld [move_xy], a
	ld a, 1
	ld [move_xy+1], a
	
	ld a, RAMP_BL_TILE
	ld [rampable_3], a					
	ld a, RAMP_BR_TILE					
	ld [rampable_4], a
	ld a, $2						
	ld [ramp_front], a
	
	jr moving_check_above_2
moving_check_above_right:
	cp 2
	jr nz, moving_check_above_left
	
	ld a, [player_x]
	add d
	or a
	jp z, finish_move
	
	ld bc, -2
	add hl, bc
	
	ld a, [new_player_x]
	dec a
	ld [new_player_x], a
	ld a, %11101101
	ld [moving], a
	
	ld a, -1
	ld [move_xy], a
	xor a
	ld [move_xy+1], a
	
	ld a, RAMP_TL_TILE
	ld [rampable_3], a					
	ld a, RAMP_BL_TILE					
	ld [rampable_4], a
	ld a, $40						
	ld [ramp_front], a
	
	jr moving_check_above_2
moving_check_above_left:
	ld a, [player_x]
	add d
	cp 9
	jp z, finish_move
	
	ld c, 2
	add hl, bc
	
	ld a, [new_player_x]
	inc a
	ld [new_player_x], a
	ld a, %11101110
	ld [moving], a
	
	ld a, 1
	ld [move_xy], a
	xor a
	ld [move_xy+1], a
	
	ld a, RAMP_TR_TILE
	ld [rampable_3], a					
	ld a, RAMP_BR_TILE					
	ld [rampable_4], a
	ld a, $40						
	ld [ramp_front], a

moving_check_above_2:
	ld a, d				;If d is non-zero, ret
	or a
	ret nz
	
	call lcd_wait
	ld a, [hl]
	cp PASSABLE_TILE
	jp nc, moving_check_above_3
	
	ld a, [slime_block_hi]					;If we aren't pushing, we still need
	or a									;to find next_line and adj_tile
	call nz, get_sticky_dir					;values if slime stick is happening
	jp begin_move
	
moving_check_above_3:						;For rocketing to jump to
	push af
		call get_tile_pal
		cp ICE_PAL
		jr nz, moving_check_above_3_no_ice	;Only if block isn't ice do we need 
	pop af									;to consider impassible blocks
	
	ld d, $C2
	ld a, [open_slide_spot]
	ld e, a
	
	ld a, 1
	ld [de], a					;ice_block_count
	inc e						;ice_block_hi
	ld a, h						;If this is the first in a sequence of ice blocks,	
	ld [de], a					;save its addr
	inc e						;ice_block_lo
	ld a, l
	ld [de], a
	jr moving_check_above_ramp
	
moving_check_above_3_no_ice:
	pop af
	cp IMPASSABLE_TILE
	jp z, reset_move
	cp SOIL_TILE
	jr nz, moving_check_above_ramp
	
	push bc							;Preserve bc vals from moving_check_above
		call destroy_soil			;for get_sticky_dir
	pop bc
	ld a, [slime_block_hi]			;If destroying soil and pulling slime
	or a							;block, we still need to get next_line/
	call nz, get_sticky_dir
	jp begin_move
	

moving_check_above_ramp:
	cp RAMP_TILE_MIN					;Making sure curr block is a ramp. If not, skip past this.
	jp c, moving_check_above_4
	cp RAMP_TILE_MAX
	jp nc, moving_check_above_4
	
	ld d, a
	ld a, [rampable_3]
	cp d
	jr nz, .ramp_neg
	
.ramp_neg:
	ld a, [rampable_4]
	cp d
	jr nz, .not_rampable
	
.not_rampable:
	ld a, d
	call get_action_dirs
	ld a, [de]
	ld [action_dir_vert], a
	inc e
	ld a, [de]
	ld [action_dir_hor], a	
	
moving_check_above_4:
	call add_push_origins
	
	ld a, 2								;Default is 2
	ld [blocks_pushed], a
	ld a, h
	ld [dest_tile_addr], a
	ld a, l
	ld [dest_tile_addr+1], a
	
check_block_push:						;CHECKING BOUNDS
	add hl, bc
	
check_block_push_no_add:	
	ld a, h
	cp $9c								;If h is below 9c, blocks are pushed 
	jp c, reset_ice_and_move			;against bounds. Not valid push
	cp $9e
	jr nz, check_block_push_2
	ld a, l
	cp $13
	jp nc, reset_ice_and_move			;If hl is 9e13 or above, not valid push
		
check_block_push_2:						;CHECKING KINDS OF BLOCKS BEING PUSHED
	call lcd_wait
	ld a, [hl]
	ld [push_tile], a
	cp $FF
	jp z, reset_ice_and_move			;If [hl] is ff, we're out of horizontal bounds
	cp TRUE_PASSABLE_TILE
	jp c, start_block_push				;If we've pushed >=1 blocks and now find a bg tile, start push
	
	call add_visited					;Check if this curr block is newly visited. If not, cancel push
	cp $ff
	jp z, reset_ice_and_move
	

	
.check_ramp:
	ld a, [rampable_3]
	ld d, a
	ld a, [rampable_4]
	ld e, a
	
	ld a, [action_dir_vert]
	or a
	jp nz, .ramp_to_block
	
	ld a, [push_tile]
	
	cp RAMP_TILE_MIN					;Making sure curr block is a ramp. If not, skip past this.
	jp c, .check_post_turn
	cp RAMP_TILE_MAX
	jp nc, .check_post_turn
	
	call turn_dir						;Given what we're ramping off of, changes vals of rampable_3/4 and loads
	cp $FF								;de with the new direction after turn. If we're not ramping off of this curr
	jp z, .check_post_turn				;ramp, we are pushing it--we get its action dirs and skip .store_turning_block
	
.store_turning_block:
	ld a, [turn_switch]
	or a
	jr z, .first_turn
	ld a, [ramping_blocks_ind]
	sub 11
	ld [ramping_blocks_ind], a
	
.first_turn:
	push bc								;Storing position of ramp for when we animate it
		ld bc, ramping_blocks
		ld a, [ramping_blocks_ind]
		add c
		ld c, a
		ld a, h
		ld [bc], a
		inc c
		ld a, l
		ld [bc], a
	pop bc
	
	ld a, [turn_switch]
	or a
	jr z, .store_orig_block
	ld bc, ramping_blocks
	ld a, [ramping_blocks_ind]
	add 4
	add c
	ld c, a
	jr .skip_orig_block
	
.store_orig_block:
	push hl
		ld a, b
		cpl
		ld b, a
		ld a, c
		cpl
		inc a
		ld c, a
		add hl, bc				;Get position of block being pushed into the ramp

		ld bc, ramping_blocks
		ld a, [ramping_blocks_ind]
		add 2
		add c
		ld c, a
		
		ld a, h					;Store that position
		ld [bc], a
		inc c
		ld a, l
		ld [bc], a
		inc c
	pop hl
	
.skip_orig_block:
	add hl, de					;Get position that block will end up in after ramp
		
	ld a, h					;Store that position in both the block's data and
	ld [bc], a				;as the starting point of the next push
	inc c
	ld a, l
	ld [bc], a
	
	;!!! Maybe we can just store all non-linear push data in one array (what block is being erased, what direction of push)
	ld a, [ramping_blocks_ind]	;We've only filled 4 bytes, but keep space of 9 for later (pal, tiles)
	add 11
	ld [ramping_blocks_ind], a
	
	ld a, [turn_switch]			;If we're ramping into another ramp, there's, certainly no new seg to add.
	or a
	jr nz, .no_push
	
	ld a, [total_segs_pushed]	;Storing and resetting (?) blocks_pushed.
	ld bc, blocks_pushed_arr
	add c
	ld c, a
	ld a, [blocks_pushed]
	sub 2
	jr z, .no_push
	
	inc a						;If there is an actual segment to push, increment these vars
	ld [bc], a	
	ld a, [total_segs_pushed]
	inc a
	ld [total_segs_pushed], a
	
	ld a, [push_origins_ind]
	add 4
	ld [push_origins_ind], a
	
.no_push:						;In the case that the only block we are pushing is ramping, 
	xor a						;there's no call to block_push_loop needed
	ld [blocks_pushed], a
	
	ld a, 1
	ld [turn_switch], a
	
	ld b, d
	ld c, e
	
	jp check_block_push_no_add
	
;In the case that, instead of a block being pushed into a ramp, a ramp is pushed into a block
.ramp_to_block:
	call turn_dir_2
	cp $FF
	jp nz, check_block_push_no_add
	jp start_block_push
	
.check_post_turn:
	ld a, [turn_switch]					;If we have turned and there's a block after the turn, that means
	or a								;we have another segment to push.
	jr z, .check_ice
	
	xor a
	ld [turn_switch], a
	
	call add_push_origins
	
;.action_hor:
	;ld a, [action_dir_hor]
	;cp $FE
	;call z, turn_dir_neg
	
.check_ice:
	ld a, [open_slide_spot]
	ld d, $C2
	ld e, a								;curr ice_block_count
	
	;push af
	call get_tile_pal
	cp ICE_PAL
	jr nz, .no_ice					;Only if block isn't ice do we need 
	;pop af							;to consider impassible blocks
	
	ld a, [de]
	or a
	jr nz, .ice
	inc e						;ice_block_hi
	ld a, h						;If this is the first in a sequence of ice blocks	
	ld [de], a					;save its addr
	inc e						;ice_block_lo
	ld a, l
	ld [de], a
	dec e
	dec e						;ice_block_count
	xor a
.ice:
	inc a
	ld [de], a					;ice_block_count
	jr check_block_push_3

.no_ice:
	;pop af
	ld a, [push_tile]
	cp IMPASSABLE_TILE
	jp z, reset_ice_and_move
	
	xor a
	ld [de], a				;If we are pushing a non-ice block, res ice count

check_block_push_3:
	ld a, [blocks_pushed]
	or a					;If a is 0, we start blocks_pushed back at 2 rather than incrementing by 1
	jr nz, .skip_inc
	inc a
.skip_inc:
	inc a
	ld [blocks_pushed], a
	
	jp check_block_push

start_block_push:
	ld a, [ramping_blocks_ind]
	or a
	jr z, .cont
	;inc a
	;ld [total_segs_pushed], a
	;jr .cont

.ramps_present:
	call rem_ramping_blocks
.cont:
	ld a, [blocks_pushed]		;If we ended our push with blocks_pushed >= 2, we need to inc total_segs_pushed
	cp 2
	jr nc, .cont_2
	
	ld a, [total_segs_pushed]	;Storing and resetting (?) blocks_pushed.
	or a
	jp z, end_blocks_push
	jr start_block_push_2
	
.cont_2:
	ld a, [total_segs_pushed]
	inc a
	ld [total_segs_pushed], a
	
	dec a
	ld bc, blocks_pushed_arr
	add c
	ld c, a
	ld a, [blocks_pushed]
	ld [bc], a
	
start_block_push_2:
	call get_tile_pal
	cp SLIME_PAL					;If bg space we're pushing to is slimed, 
	jr nz, start_block_push_3		;we need to slime the block
	ld a, h
	ld [slimed_hi], a
	ld a, l
	ld [slimed_lo], a
	;xor a
	;ld [de], a						;ice_block_count
	
start_block_push_3:
	xor a
	ld [curr_push], a
	
start_block_push_loop:
	ld b, a
	ld a, [total_segs_pushed]
	cp b
	jp z, end_blocks_push			;Later (when we're looping over pushes)
	
	ld a, [curr_push]				;Get curr_push * 6 since push_origins has units of 6 bytes
	sla a
	sla a
	
	ld bc, push_origins
	add c
	ld c, a
	
	ld a, [bc]
	ld h, a
	inc c
	ld a, [bc]
	ld l, a
	inc c
	
	;Set 'a' to original map's block value
	call get_mini_map_val
	call filter_uncovered
	;Now 'a' holds uncovered block value
	
	call get_block_0x_addr	;Outputs with de holding block_0x header addr
		
	push hl				;Preserve the map addr of the block we are jumping to
		ld a, [curr_push]
		sla a
		ld hl, uncovered_blocks
		add l
		ld l, a
		ld a, d
		ldi [hl], a
		ld a, e
		ld [hl], a
	pop hl
		

	;Load tile_1/tile_2 with top-left/top-right by default
	ld a, [de]
	ld [tile_1], a
	inc de
	ld a, [de]
	ld [tile_2], a

	inc c
	ld a, [bc]
	;ld a, c
	cp 2
	jr nz, start_block_push_left
	ld b, 1				;b holds val added to hl to move to next row/col of tiles
	ld c, 32			;c holds val added to hl to move to adjacent tile
	
	inc de
	ld a, [de]
	ld [tile_2], a
	jr start_block_push_end
start_block_push_left:
	cp -2
	jr nz, start_block_push_down
	inc hl				;hl points to top-left tile of block. inc changes to top-right
	ld b, -1
	ld c, 32

	ld a, [de]			;Top-right
	ld [tile_1], a
	inc de
	inc de				;Bottom-right
	ld a, [de]
	ld [tile_2], a
	jr start_block_push_end
start_block_push_down:
	cp 64
	jr nz, start_block_push_up
	ld b, 32
	ld c, 1
	jr start_block_push_end
start_block_push_up:
	ld bc, 32
	add hl, bc
	ld b, -32
	ld c, 1
	ld a, 2
	inc de
	ld a, [de]
	ld [tile_1], a
	inc de
	ld a, [de]
	ld [tile_2], a

start_block_push_end:
	ld de, block_push_addr
	ld a, [curr_push]
	sla a
	add e
	ld e, a
	ld a, h
	ld [de], a
	inc e
	ld a, l
	ld [de], a
	
	ld de, next_line_arr
	ld a, [curr_push]
	add e
	ld e, a
	ld a, b
	ld [de], a
	
	ld de, adj_tile_arr
	ld a, [curr_push]
	add e
	ld e, a
	ld a, c
	ld [de], a
	
	;ld a, h
	;ld [block_push_addr_hi], a		;Storing these for when we have to finish
	;ld a, l							;the anim (move the last 8 pixels u/d/l/r)
	;ld [block_push_addr_lo], a		
	;ld a, b
	;ld [block_push_next_line], a
	;ld a, c
	;ld [block_push_adj_tile], a
	
	xor a
	ld [block_palette], a			;Bg palette is 0
	
	ld de, blocks_pushed_arr
	ld a, [curr_push]
	add e
	ld e, a
	ld a, [de]
	add a							;Double blocks pushed (to separate into tiles)
	dec a
	ld [blocks_pushed], a
	ld [de], a
	
	;ld d, $C2						;!!!!!! ICE stuff we will bring back later
	;ld a, [open_slide_spot]
	;ld e, a							;ice_block_count
;	push bc
;	push af
		ld d, $C2
		ld a, [open_slide_spot]
		ld e, a							;ice_block_count
		
;		ld a, [de]
;		add a
		;dec a						;????
		;ld d, a
;		ld b, a
;	pop af
		;sub d
;		sub b
;		ld [blocks_pushed_2], a
;	pop bc
	
	push hl
		ld hl, block_push_sfx
		ld a, [de]
		or a
		call nz, ice_slide_setup
		call play_noise
	pop hl
	call block_push_loop
	
	ld a, [curr_push]
	inc a
	ld [curr_push], a
	jp start_block_push_loop	

end_blocks_push:
	ld a, [dest_tile_addr]
	ld h, a
	ld a, [dest_tile_addr+1]
	ld l, a
	
	jp begin_move
	
	
;A called function
;Setup: hl comes with address of first tile in the push. From there, we use adj and next line vals to work
;up to other tiles. b is loaded with next line val, c with adj tile val. tile_1 and tile_2 come loaded with 
;proper bg tiles considering the direction of the push. block_palette comes loaded with bg pal
block_push_loop:
		call lcd_wait
		ld e, [hl]
		ld a, [tile_1]
		ld [hl], a
		ld a, e
		ld [tile_1], a
		
		call VBK_On
		call lcd_wait
		ld a, [block_palette]
		ld [hl], a
		call VBK_Off
		
		push hl
		push bc
			ld b, 0
			add hl, bc					;Get to adjacent tile
		pop bc
			call lcd_wait
			ld e, [hl]
			ld a, [tile_2]
			ld [hl], a
			ld a, e
			ld [tile_2], a
			
			call VBK_On
			call lcd_wait
			ld e, [hl]
			ld a, [block_palette]
			ld [hl], a
			ld a, e
			ld [block_palette], a
			call VBK_Off
		pop hl
		
		push bc
			ld a, b
			ld c, a
			ld b, 0
			cp $E0						;Checking if b is -2 or -32. 
			jr c, block_push_loop_3		;If so, make b filled with 1s here.
			ld b, $FF
block_push_loop_3:
			add hl, bc				;Move to next row/col of tiles for next loop
		pop bc
		
		ld a, [blocks_pushed]
		dec a
		ld [blocks_pushed], a
		jr nz, block_push_loop
		
		ret
		
		
		
ice_slide:
	ld a, [ongoing_slides]
	;or a
	;ret z
	ld [slides_count], a
	ld hl, ICE_MEM_START
	
ice_slide_loop:
	ld a, l
	ld [curr_slide_spot], a
	
	ld a, [hl]
	or a
	jp z, ice_slide_next
	
	;ld a, l
	;ld [curr_slide_spot], a
	
	ld a, 6
	add l
	ld l, a						;ice_slide_delay
	
	ld a, [rocket_push]
	or a
	jr nz, ice_slide_loop_2
	
	ld a, [hl]
	inc a
	and %11111011				;Every 4 frames, ice block moves
	jr ice_slide_loop_3
	
ice_slide_loop_2:
	ld a, [hl]
	inc a						;If rocket_pushed
	and %11111101				;Every 2 frames, ice block moves
	
ice_slide_loop_3:
	ld [hl], a
	jp nz, ice_slide_check
	
	call get_ice_block_addr
	
	ld b, d
	inc e						;ice_block_dest_hi
	ld c, e
	
	ld a, [bc]
	inc c						;ice_block_dest_lo
	ld d, a
	ld a, [bc]
	ld e, a						;de now holds ice slide destination addr
	
	inc c						;ice_slide_count
	ld a, [bc]
	bit 0, a
	jr nz, ice_slide_2			;If we've slid half a block, no need to check if we're at dest
;Cp curr og block pos to dest
	ld a, h
	cp d
	jr nz, ice_slide_2
	ld a, l
	cp e
	jr nz, ice_slide_2
	
	ld a, [curr_slide_spot]
	ld c, a
	
	ld a, c
	add 9						;ice_block_slimed
	ld c, a						
	ld a, [bc]
	or a
	jr z, ice_slide_end_slide
	
	ld a, c
	sub 9						;ice_block_count
	ld c, a	
	ld a, [bc]
	;inc a						;Get orig ice_block count by subtracting 1 
	sra a						;and halving
	dec a
	jr z, ice_slide_set_slime	;If a is 0, we are only pushing 1 block and don't
								;need to move up any # of blocks
	push af
		ld a, c
		add 7
		;sub 2					;ice_block_next_line
		ld c, a
		ld a, [bc]
		add a
		ld b, 0
		cp $C0
		jr c, ice_slide_slime_setup
		ld b, $FF
ice_slide_slime_setup:
		ld c, a					;bc now has ice_block_next_line
		ld h, d
		ld l, e					;Give ice_block_dest to hl so we can add
	pop af

;Loop used to find the last ice block being pushed and then slime it
ice_slide_slime_loop:
	add hl, bc
	dec a
	jr nz, ice_slide_slime_loop
	
ice_slide_set_slime:
	ld a, SLIME_PAL
	res 0, l
	res 5, l					;Doing this to get top-left tile
	call set_block_pal
	
ice_slide_end_slide:
	ld b, $C2
	ld a, [curr_slide_spot]
	ld c, a
	xor a
	ld [bc], a					;ice_block_count
	
	ld a, [ongoing_slides]
	dec a
	ld [ongoing_slides], a
	
	ld a, [open_slide_spot]
	cp c
	jr c, ice_slide_check
	ld a, c
	ld [open_slide_spot], a
	jr ice_slide_check
	
ice_slide_2:
	call get_uncovered_tiles

	ld d, $C2
	ld a, [curr_slide_spot]
	add 7
	ld e, a					;ice_block_next_line
	ld a, [de]
	ld b, a
	inc e					;ice_block_adj_tile
	ld a, [de]
	ld c, a				
	;ld a, [block_push_next_line]
	;ld b, a
	;ld a, [block_push_adj_tile]
	;ld c, a
	
	;ld d, $C2
	ld a, [curr_slide_spot]
	ld e, a
	ld a, [de]					;ice_block_count
	ld [blocks_pushed], a	
	
	xor a
	ld [block_palette], a
	
	push de
		call block_push_loop
	pop de
	
	ld a, 5
	add e
	ld e, a						;ice_slide_count
	ld a, [de]					
	inc a
	ld [de], a				
		
	inc e
	inc e						;ice_block_next_line
	
	ld a, [de]
	;ld a, [block_push_next_line]
	ld b, 0
	ld c, a
	cp $E0
	jr c, ice_slide_3
	ld b, $FF
	
ice_slide_3:
	call get_ice_block_addr
	add hl, bc
	call set_ice_block_addr
	
ice_slide_check:
	ld a, [slides_count]
	dec a
	ld [slides_count], a
	ret z

ice_slide_next:
	ld bc, ICE_SLIDE_VARS
	ld a, [curr_slide_spot]		;Get hl back to first addr of curr slide spot
	ld h, $C2
	ld l, a
	add hl, bc
	jp ice_slide_loop
	
	
;----------------------------------------------------------------------------
	
;Destroys hl
ice_slide_setup:
	push bc
		
		ld a, [open_slide_spot]
		ld [curr_slide_spot], a
		
		call get_ice_block_addr	;Loads hl with curr ice_block_hi/lo
		ld a, [block_push_next_line]
		;add a					;Double next line val to make it a full block jump
		ld b, 0
		ld c, a
		cp $C0
		jr c, ice_slide_setup_2
		ld b, $FF
ice_slide_setup_2:
		add hl, bc
		
		push bc
			ld a, [ori]
			ld [push_ori], a	;Save og ori when push happened			;!!!
			cp 0
			jr nz, ice_slide_setup_2_right
			ld bc, 32
			add hl, bc					;Get bottom-left tile when pushing from below
ice_slide_setup_2_right:
			cp 2
			jr nz, ice_slide_setup_3
			inc hl						;Get top-right tile when pushing from right
		
ice_slide_setup_3:		
		pop bc
		call set_ice_block_addr			;Put og ice block "up" one bc of its initial push
		dec e
		dec e							;ice_block_count
		
		add hl, bc						;Since we're in a half-step, change to full-step to better calc
		ld a, c							;the end point
		add a
		ld c, a					;Double "next line" value to jump whole blocks.
		ld a, [de]
		
		dec a
		jr z, ice_blocks_bypassed
bypass_ice_blocks:				;Gets our addr past the pushed ice blocks
		add hl, bc
		dec a
		jr nz, bypass_ice_blocks
		
ice_blocks_bypassed:
		ld a, e
		add 9					;ice_block_slimed
		ld e, a
		xor a
		ld [de], a				;Set ice_block_slimed to 0 by default
		
slide_find_wall:				;Loops until a wall is found to stop sliding ice
		call lcd_wait
		ld a, [hl]
		cp TRUE_PASSABLE_TILE
		jr nc, slide_get_dest
		call get_tile_pal
		cp SLIME_PAL
		jr z, slide_add_slime
		add hl, bc
		jr slide_find_wall
		
slide_add_slime:
		ld a, 1
		ld [de], a				;ice_block_slimed
		add hl, bc				;Move one block further to simulate hitting a wall

slide_get_dest:					;Pos of wall is used to find final pos of og ice block
		ld a, b
		cpl
		ld b, a					;Negate b
		ld a, c
		cpl
		inc a					;Two's compliment
		ld c, a
		
		ld a, e
		sub 9
		ld e, a					;ice_block_count
		ld a, [de]				
slide_get_dest_loop:
		add hl, bc
		dec a
		jr nz, slide_get_dest_loop
		
		inc e
		inc e
		inc e					;ice_block_dest_hi
		
		ld a, h
		ld [de], a
		inc e					;ice_block_dest_lo
		ld a, l
		ld [de], a
	
		ld a, [open_slide_spot]
		ld e, a						;ice_block_count
		
		ld a, [de]
		add a
		inc a
		ld [de], a		;Double ice block count for blocks_pushed var
		;xor a
		;ld [block_push_addr_hi], a
		ld a, 5
		add e
		ld e, a						;ice_slide_count
		
		ld a, 1
		ld [de], a		;Tells us if we've slid halfway or not
		dec a
		inc e			;ice_slide_delay
		ld [de], a
		ld a, [block_push_next_line]			;Save the direction the ice is sliding
		inc e			;ice_block_next_line
		ld [de], a
		ld a, [block_push_adj_tile]
		inc e			;ice_block_adj_tile
		ld [de], a
		
update_slide_stack:
		ld h, $C2
		ld a, [open_slide_spot]
		ld l, a
		ld bc, ICE_SLIDE_VARS
	
update_slide_stack_loop:
		add hl, bc
		ld a, [hl]
		or a
		jr nz, update_slide_stack_loop
		ld a, l
		ld [open_slide_spot], a
		ld a, [ongoing_slides]
		inc a
		ld [ongoing_slides], a
	
	pop bc	
	
	ld hl, ice_slide_sfx
	ret
	
	
;Loads ice_block_hi/lo into hl
;changes de
get_ice_block_addr:
	ld d, $C2
	ld a, [curr_slide_spot]
	ld e, a
	inc e
	
	ld a, [de]				;ice_block_hi
	inc e
	ld h, a
	ld a, [de]				;ice_block_lo
	ld l, a
	ret
	
;Loads hl into ice_block_hi/lo
;changes de
set_ice_block_addr:
	ld d, $C2
	ld a, [curr_slide_spot]
	ld e, a
	inc e
	
	ld a, h
	ld [de], a				;ice_block_hi
	inc e
	ld a, l
	ld [de], a				;ice_block_lo
	ret
	

destroy_soil:
	push hl	
		call get_spr_pos
		call create_soil_anim
		
		ld hl, soil_sfx
		call play_noise
	pop hl
	
	call get_tile_pal		;If soil being broken is slimed, it will leave
	cp SLIME_PAL			;behind a slime splatter
	jr nz, destroy_soil_2
	ld a, SLIME_BLOCK
	jr destroy_soil_3
	
destroy_soil_2:
	call get_mini_map_val	;Otherwise, find what the bg tile was originally
	call filter_uncovered

destroy_soil_3:
	call get_block_0x_addr
	call replace_block
	
	call lcd_wait			;If we created a slime bg, set pal accordingly
	ld a, [hl]
	cp SLIME_TILE
	ret nz
	ld a, SLIME_PAL
	call set_block_pal
	
	ret
	
;de has header of new block
;hl has addr of top-left tile of block to be changed
replace_block::			;This is basically already in game_load.asm...
	push hl
		ld bc, 31
		ld a, [de]
		call lcd_wait
		ldi [hl], a
		inc de
		ld a, [de]
		ld [hl], a
		add hl, bc
		inc de
		ld a, [de]
		call lcd_wait
		ldi [hl], a
		inc de
		ld a, [de]
		ld [hl], a
	pop hl
	
	push hl
		call VBK_On
		xor a
		call lcd_wait
		ldi [hl], a
		ld [hl], a
		add hl, bc
		call lcd_wait
		ldi [hl], a
		ld [hl], a
		call VBK_Off
	pop hl
	ret
	
jump_check:
	ld a, [io_p15_old]
	bit BUTTON_A, a
	jp z, end_frame			;ret z					;!!!
jump_check_2:				;Jumped to if we don't need to press A to jump (earth power)	
	call get_tile	
	;call lcd_wait
	;ld a, [hl]
	push hl
		ld a, [earth_addr_hi]
		or a
		jr nz, jump_check_3		;If using earth power, we don't care if slime block is below
		ld d, 1
		call moving_check_below
		call get_tile_pal
		cp SLIME_PAL
		jr nz, jump_check_3
		
		call lcd_wait
		ld a, [hl]
		cp IMPASSABLE_TILE		;If slimed block is impassable, we can't jump
		jr nz, jump_check_slime
	pop hl
	jp end_frame
	
jump_check_slime:
		ld a, h
		ld [slime_block_hi], a

		ld a, [ori]
		cp 0				;If down, push start point needs to be bottom-left tile
		jr nz, jump_check_slime_right
		ld a, l
		add 32
		ld l, a
		jr jump_check_slime_end
jump_check_slime_right:
		cp 2				;If right, push start point needs to be top-right tile
		jr nz, jump_check_slime_end
		ld a, l
		inc a
		ld l, a
jump_check_slime_end:
		ld a, l
		ld [slime_block_lo], a
		
		call get_mini_map_val
		call filter_uncovered
		;cp TRUE_PASSABLE_BLOCK
		;jr c, jump_check_slime_uncover
		;xor a				;If uncovered block was originally impassable, default it to blank bg block
;jump_check_slime_uncover:
		;Now 'a' holds uncovered block value
		call get_block_0x_addr	;Outputs with de holding block_0x header addr
		
		ld a, d
		ld [uncovered_slime_hi], a
		ld a, e
		ld [uncovered_slime_lo], a
		
jump_check_3:
	pop hl
	
	ld d, 0
	jp moving_check_above
	
	
power_change:
	ld a, [io_p15_old]
	bit BUTTON_SELECT, a
	jp z, end_frame
	
	ld a, [power_index]
	cp $FF
	jp z, end_frame
	inc a
	ld h, $C3
	ld l, a
	ld a, [hl]
	cp $FF					;If we're at end of power arr, we need to loop back
	jr nz, power_change_2
	ld l, 0
	ld a, [hl]
power_change_2:
	ld [power], a
	ld a, l
	ld [power_index], a
	call change_goblin_pal
	jp end_frame
	
power_check:
	ld a, [io_p15_old]
	bit BUTTON_B, a
	jp z, end_frame			;ret z					;!!!
	ld a, [using_power]		;If a power anim is still going on, don't allow
	or a					;another to be made
	jp nz, end_frame
	
	ld a, [power]
	or a
	jp z, end_frame			;ret z					;!!!
	ld [using_power], a		;Setting using_power since power is non-zero
	cp 1
	jp z, start_frost
	cp 2
	jp z, start_fire
	jp start_earth
	jp end_frame			;ret
	

start_frost:
	call get_front
	push hl
	pop de					;Load hl into de
	call create_frost_anim
	jp end_frame			;ret					!!!

start_fire:
	call get_front
	push hl
	pop de					;Load hl into de
	call create_flame_anim
	ld hl, fire_sfx			;Play fire sound effect
	call play_noise
	jp end_frame
	
start_earth:
	call get_tile
	push hl
		ld d, 1					;So moving_check_below returns after finding below block
		call moving_check_below
		call get_tile_pal
		ld [earth_below_pal], a
		
		call lcd_wait
		ld a, [hl]
		cp SOIL_TILE
		jr z, start_earth_end
		cp WOOD_TILE
		jr nz, start_earth_2
		ld a, 1
		jr start_earth_3
start_earth_2:
		cp IMPASSABLE_TILE
		jr nz, start_earth_end
		ld a, 2	
start_earth_3:
		ld [earth_making], a
	pop hl
	
	ld a, h
	ld [earth_addr_hi], a		;Save below block spr pos for anim
	ld a, l
	ld [earth_addr_lo], a
	
	;ld a, 1
	;ld [earth_casting], a
	
	jp jump_check_2

start_earth_end:
	pop hl
	xor a
	ld [using_power], a
	jp end_frame
	

finish_move:
	xor a
	ld [moving], a
	ld [falling], a
	ld [rocketing], a
	ld [earth_making], a
	ld [earth_addr_hi], a
	ld [using_power], a
	ld [slime_block_hi], a
	jp end_frame		;ret				;!!!
	
	
change_ori:
	ld a, [ori_changed]
	or a					;Check if ori_changed is 0
	jr z, change_ori_2
	ld a, [io_p14_old]
	ld b, a
	ld a, [io_p14]
	cp b					
	jr z, change_ori_3
change_ori_2:
	ld a, [ori]
	ld b, a					;Store old orientation
	ld a, [new_ori]			
	ld [ori], a		;Change to new orientation
	
	inc a
	ld [ori_changed], a 	;Just set ori_changed with some non-zero val to set.
	
	call wall_transfer
	;call loose_trigger
change_ori_3:
	jp reset_move

	
invert_ori:
	ld a, [ori]
	cpl							;Flipping bit 0 of ori
	and %00000001				;This makes down->up, right->left, and vice-versa
	ld b, a
	ld a, [ori]
	and %11111110
	or b
	ld [ori], a
	
	ld hl, 0						;So sprite isn't mid-walk or a corner one
	call change_sprite
	ret
	
;hl comes with tile address of where ramp will bring you
;May be pointless if always leading to begin_move
use_ramp:
	;ld a, h
	;ld [dest_tile_addr_hi], a
	;ld a, l
	;ld [dest_tile_addr_lo], a

	call get_spr_pos
	call get_player_pos
	ld a, d
	ld [player_x], a
	ld [new_player_x], a
	ld a, e
	ld [player_y], a
	ld [new_player_y], a
	
	;ld a, 1
	;ld [ramping], a
	
	ret
	

;Jumped to for the first ramp in a potential series of ramps
ascend_ramp_init:
	ld a, b
	ld [ramping], a			;a holds rampable_1
	
	ld a, [move_xy]				;Save originaly movement from input for end of ramp
	ld [move_xy_old], a
	ld a, [move_xy+1]
	ld [move_xy_old+1], a
	
	ld a, [ori]
	bit 0, a					;Check if ori is down/right (bit 0 not set) or up/left (bit 0 is set)
	ld a, 1						;If up/left ori, we will be ascending by adding to x or y pos (ascend
	jr nz, .ascend_2			;on ceiling means adding to y, ascending on left wall means adding to x)
	
	ld a, -1
.ascend_2:
	ld b, a
	ld a, [move_xy]				;Check which move is 0 (x or y)
	or a
	ld a, b
	jr nz, .move_y
	
	ld [move_xy], a
	jr ascend_ramp
	
.move_y:
	ld [move_xy+1], a

ascend_ramp:
	ld a, [block_below]		;Compliment block_below so we get # to add to get block above
	cpl
	ld b, a
	ld a, [block_below+1]
	cpl
	inc a
	ld c, a
	add hl, bc				;We now have block directly above ramp (use later for possible pushes)
	
	;ld d, h					;Store block directly above ramp in case we turn
	;ld e, l
	
	
	
.end:	
	call use_ramp
	jp begin_move
	
	
;Returns with hl holding the map address of block in front of Gobbo
;!!!Maybe optimize with block_front?
get_front:
	push bc
		ld a, [player_x]
		ld b, a
		ld a, [player_y]
		ld c, a
		
		ld a, [facing]
		bit 3, a
		jr nz, get_front_up
		inc c			;Down one y pos
	get_front_up:
		bit 2, a
		jr nz, get_front_left
		dec c			;Up one y pos
	get_front_left:
		bit 1, a
		jr nz, get_front_right
		dec b			;Left one x pos
	get_front_right:
		bit 0, a
		jr nz, get_front_2
		inc b			;right one x pos
	get_front_2:
		ld a, b
		ld [new_player_x], a
		ld a, c
		ld [new_player_y], a
		call get_tile
		call reset_player_pos
	pop bc
	ret

;Converts new_player x/y pos into a map address
;returns with map address stored in hl]
;Destroys a
get_tile::
	push bc
		ld a, [new_player_y]	;To get tile that the player is trying to move to,
		ld b, a					;hl = game_map_data + (new_player_x*2) + (new_player_y*40)
		ld c, 64
		push de					;Preserve the potential orientation value
			call multiply		;Changes hl to b*c
		pop de
		ld b, 0
		ld a, [new_player_x]
		ld c, a
		rl c
		add hl, bc
		ld b, h
		ld c, l
		ld a, $9C
		ld h, a
		ld a, $00
		ld l, a
		
		add hl, bc		
	pop bc
	ret

;Given a map addr, we get the corresponding orig val in the 
;level's mini map (from game_map.inc)	
;Given that hl holds map address
;Val is returned in 'a'
get_mini_map_val:
	push hl
	push bc
	push de
		res 0, l		;We want top-left tile which will never be odd
		res 5, l		;And be divisible by 64 (not have an additional 32)
		
		ld a, h
		sub $9C
		ld h, a			;Subtracting 9C00
		
		ld d, 0
		ld bc, -64
get_mini_map_val_loop:
		ld a, h
		cp 0
		jr nz, get_mini_map_val_loop_2
		ld a, l
		cp 64			;If hl is <64, exit loop
		jr c, get_mini_map_val_2
get_mini_map_val_loop_2:
		add hl,bc		;Subtract hl by 64
		ld a, d
		add 10
		ld d, a			;Increase d by 10 (down a Y position)
		jr get_mini_map_val_loop
		
			
get_mini_map_val_2:
		cp a			;Clear carry flag
		rr l			;Divide l by 2
		
		ld a, d
		add l
		ld l, a
		
		ld a, [curr_level_addr_hi]
		ld b, a
		ld a, [curr_level_addr_lo]
		ld c, a
		add hl, bc
		ld a, [hl]
	
	pop de
	pop bc		
	pop hl	
	ret

;Given a map addr in hl, de is returned with the corresponding spr pos (top-left of the block)	
get_spr_pos::
	push hl
	push bc
		res 0, l		;We want top-left tile which will never be odd
		res 5, l		;And be divisible by 64 (not have an additional 32)
		
		ld a, h
		sub $9C
		ld h, a			;Subtracting 9C00
		
		ld de, $0810	;Top-leftmost pos is $08,$10 (x,y)
		ld bc, -64
get_spr_pos_loop:
		ld a, h
		cp 0
		jr nz, get_spr_pos_loop_2
		ld a, l
		cp 64			;If hl is <64, exit loop
		jr c, get_spr_pos_2
get_spr_pos_loop_2:
		add hl, bc		;Subtract hl by 64
		ld a, e
		add $10
		ld e, a			;Increase d by 10 (down a Y position)
		jr get_spr_pos_loop
	
get_spr_pos_2:
		sla l
		sla l
		sla l			;Shift l left to multiply by 8
		ld a, d
		add l			
		ld d, a
	pop bc
	pop hl
	ret


;Given sprite pos in de, return player pos in de (d holds player_x, e holds player_y)
get_player_pos:
	ld a, d
	sub 8
	rra 
	rra
	rra
	rra
	and %00011111
	ld d, a
	
	ld a, e
	sub 16
	rra
	rra
	rra
	rra
	and %00011111
	ld e, a
	
	ret
	
	
	
;Given hl as map address, we load tile_1/tile_2 with the proper uncovered blocks
get_uncovered_tiles:
	push hl
		call get_mini_map_val
		cp PASSABLE_BLOCK
		jr c, get_uncovered_tiles_2
		xor a				;If uncovered block was originally impassable, default it to blank bg block
			
get_uncovered_tiles_2:
		call get_block_0x_addr
		
		ld a, [push_ori]		;2x ori and add 1 if in mid-push. That way 0=push up, full
		add a					;1=push up, mid, 2=push down, full, etc.
		ld b, a
		
		ld h, $C2
		ld a, [curr_slide_spot]
		add 5
		ld l, a					;ice_slide_count
		ld a, [hl]
		and %00000001
		add b
	pop hl
	
get_uncovered_tiles_call:		;A callable spot if the right regs are loaded properly
		cp 0
		jr z, get_uncovered_tiles_bottom
		cp 1
		jr z, get_uncovered_tiles_top
		cp 2
		jr z, get_uncovered_tiles_top
		cp 3
		jr z, get_uncovered_tiles_bottom
		cp 4
		jr z, get_uncovered_tiles_right
		cp 5
		jr z, get_uncovered_tiles_left
		cp 6
		jr z, get_uncovered_tiles_left
		cp 7
		jr z, get_uncovered_tiles_right

;down up right left
;For 1st push up or 2nd push down
get_uncovered_tiles_bottom:
		inc de
		inc de
		call lcd_wait
		ld a, [de]
		ld [tile_1], a
		inc de
		ld a, [de]
		ld [tile_2], a
		jp get_uncovered_tiles_end
;For 2nd push up or 1st push down
get_uncovered_tiles_top:
		call lcd_wait
		ld a, [de] 
		ld [tile_1], a
		inc de
		ld a, [de]
		ld [tile_2], a
		jp get_uncovered_tiles_end
;For 1st push left or 2nd push right
get_uncovered_tiles_right:
		inc de
		call lcd_wait
		ld a, [de]
		ld [tile_1], a
		inc de
		inc de
		ld a, [de]
		ld [tile_2], a
		jp get_uncovered_tiles_end
;For 1st push right or 2nd push left
get_uncovered_tiles_left:
		call lcd_wait
		ld a, [de]
		ld [tile_1], a
		inc de
		inc de
		ld a, [de]
		ld [tile_2], a
get_uncovered_tiles_end:
	ret

;Just an isolated form of start_block_push.
;Given b and c vals from moving_check_above, we
;fill in block_push_next_line and block_push_adj_tile
get_sticky_dir:
	ld a, c
	cp 2
	jr nz, get_sticky_dir_left
	ld b, 1
	ld c, 32
get_sticky_dir_left:
	cp -2
	jr nz, get_sticky_dir_down
	ld b, -1
	ld c, 32
get_sticky_dir_down:
	cp 64
	jr nz, get_sticky_dir_up
	ld b, 32
	ld c, 1
get_sticky_dir_up:
	cp -64
	jr nz, get_sticky_dir_end
	ld b, -32
	ld c, 1
get_sticky_dir_end:
	ld a, b
	ld [block_push_next_line], a
	ld a, c
	ld [block_push_adj_tile], a
	ret
	
;If uncovered block was originally bg slime or a solid block,
;default it to blank bg
filter_uncovered:
	cp TRUE_PASSABLE_BLOCK
	jr nc, filter_uncovered_2
	cp SLIME_BLOCK
	jr nz, filter_uncovered_3
filter_uncovered_2:
	xor a				;If uncovered block was originally impassable, default it to blank bg block
filter_uncovered_3:
	ret
	
	
reset_level:
	ld a, 2
	ld [fade_state], a
	jp finish_move

	
;This is where all game.asm loops end. This is to keep a consistent pause
;between frames.
end_frame:
	call run_anims
	;halt
	;call PauseBegin
	ret	







