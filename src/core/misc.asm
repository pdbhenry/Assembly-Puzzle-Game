SECTION "misc", ROM0

include "include/gb/constants.inc"

;fade_step equ $D004
seed equ $D005
seed_2 equ $D006
seed_3 equ $D007

FADE_DELAY equ $04


;----------------------------------------------------------------------------

;Returns with hl = b * c
;Put the smaller value into b!
multiply::
	ld hl, 0
	ld a, b
	or a
	ret z
	ld d, 0
	ld e, c
multiply_loop:
	add hl, de
	dec a
	ret z
	jr multiply_loop
	
	
;----------------------------------------------------------------------------

reset_fade_step::
	xor a
	ld [fade_step], a		
	ret

;hl needs to come loaded with desired palette address to fade on
fade_out::
	;call wait_vblank
	
    ld a, [fade_step]
    cp FADE_DELAY * 1  			;FADE_DELAY EQU $30
    jr z, fade_lgray
    cp FADE_DELAY * 2
    jr z, fade_dgray
    cp FADE_DELAY * 3
    jr z, fade_black
    cp FADE_DELAY * 4
    ret nz
	ld c, 66					;Order 66 tells us that fade is complete
	;call reset_fade_step
	ret

fade_out_2:
    ;jr inc_fade_step			inc_fade_step has been cut out
	ret
	
;hl needs to come loaded with desired palette address to fade on
fade_in::
	;call wait_vblank			;Don't remember why this is needed
	
	ld a, [fade_step]
	
	cp FADE_DELAY * 0
	jr z, fade_black
	cp FADE_DELAY * 1  			;FADE_DELAY EQU $30
    jr z, fade_dgray
    cp FADE_DELAY * 2
    jr z, fade_lgray
    cp FADE_DELAY * 3
    jr z, fade_normal
    cp FADE_DELAY * 4
	ret nz
	ld c, 66
	;call reset_fade_step
	ret
	
fade_normal:
	ld d, %11100100
	call SetGBCPalettesAdv
	ret
fade_lgray:						;Saying the color that is normally white
	ld d, %11111001				;(or the brightest) is now light gray
	call SetGBCPalettesAdv
	ret
fade_dgray:
	ld d, %11111110
	call SetGBCPalettesAdv
	ret
fade_black:
	ld d, %11111111
	call SetGBCPalettesAdv
	ret

;----------------------------------------------------------------------------

;For time in between animations
;PauseBegin::							;Specifically called when only loose blocks are moving
	;call wait_vblank
;	ld bc, $0800
;PauseLoop::
;	dec bc
;	ld a, c
;	or b
;	jr nz, PauseLoop	
;	ret
	
;----------------------------------------------------------------------------

;***Got this online, haven't checked how it works
random_number::
        ld hl,seed
        ldi a,[hl]
        sra a
        sra a
        sra a
        xor [hl]
        inc hl
        rra
        rl [hl]
        dec hl
        rl [hl]
        dec hl
        rl [hl]
        ldh a,[DIVIDER]          ; get divider register to increase
randomness:
        add     [hl]
        ret
		
;----------------------------------------------------------------------------

;hl loaded with top-left placement
;b loaded with x-dimension
;c loaded with y-dimension
;d loaded with top-left tile.
;If e is set, we have 4 unique sides rather than two (where top/bottom and left/right are same)
;Tiles should be loaded in order tl, tr, bl, br, t, (b,) l (, r)
draw_box::
	dec b
	dec b
	dec c
	dec c		;Sub 2 to get dimensions without corners

	ld a, d
	call lcd_wait
	ldi [hl], a	;Top-left border	

	push bc
		add 4	;To get top border tile
		call memcpy_single
	pop bc

	sub 3
	call lcd_wait
	ldi [hl], a	;Top-right border

	ld a, 30
	sub b			;Move down one line and left by [x-dimension (b+2)]
	call add_a_to_hl
	push hl
	push bc
		ld a, d
		add 5
		ld b, c		;Size (y-dimension) goes in b
		ld c, a		;Tile value goes in c
		ld a, 32	;Inc value goes in a
		call memcpy_single_var
	pop bc
	pop hl
	
	ld a,b
	inc a
	call add_a_to_hl
	
	push bc
		ld a, d
		add 5
		ld b, c
		ld c, a
		ld a, 32
		call memcpy_single_var
	pop bc
	
	ld a, 31
	sub b
	call add_a_to_hl
	ld a, d
	add 2
	call lcd_wait
	ldi [hl], a
	push bc
		add 2	;To get bot border tile
		call memcpy_single
	pop bc
	
	dec a
	call lcd_wait
	ld [hl], a
	ret 
	
	
;----------------------------------------------------------------------------	


add_a_to_hl::
	add l
	ld l, a
	jr nc, .notcarry
	inc h
.notcarry:
	ret

add_a_to_de::
	add e
	ld e, a
	jr nc, .notcarry
	inc d
.notcarry:
	ret
	
	
;----------------------------------------------------------------------------	
	
	
;Used to load the 1bpp font
bitmap_loop::
	call lcd_wait
	ld a, [de]
	ldi [hl], a
	ldi [hl], a
	inc de
	dec bc
	ld a, b
	or c
	jr nz, bitmap_loop
	ret