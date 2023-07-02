SECTION "lcd", ROM0

include "include/gb/constants.inc"

lcd_wait::
	push af
		;di
lcd_wait_again:
		ldh a,[rSTAT]  		;STAT - LCD Status (R/W)
		and %00000010		;MM=video mode (0/1 =Vram available)  		
		jr nz, lcd_wait_again 
		;ei
	pop af	
	ret

;----------------------------------------------------------------------------
	
lcd_off::
	ld hl, LCD_CTRL
	res 7, [hl]
	ret
	
;----------------------------------------------------------------------------

lcd_on::
	ld hl, LCD_CTRL
	set 7, [hl]
	ret
	
;----------------------------------------------------------------------------

;You might wanna di before calling. Otherwise, we can still return halfway 
;through a vblank and not at the start
wait_vblank::				;First loop waits for us to *NOT* be in VBlank
    ldh a, [LCD_LINE_Y]
    cp 144
    jp nc, wait_vblank
wait_vblank_2:				;So that second loop exits when we're at the very
	ldh a, [LCD_LINE_Y]		;beginning of VBlank
    cp 144
	jp c, wait_vblank_2
	ret
	
;----------------------------------------------------------------------------

;Clears the map, not tile data

;b needs to be loaded with 0
clear_bg_map::
    ld hl, $9C00

clear_bg_map_loop:
	ldh a, [rSTAT]
	and STATF_BUSY
	jr nz, clear_bg_map_loop
	
    ld [hl], b
	inc hl					;Can't load b into [hl] if we do ldi--command not allowed
    ld a, h
    cp $9E		;cp $9F		;For our purposes, we don't need to clear the entire tilemap, just the visible screen.
    jr nz, clear_bg_map_loop
    ld a, l
    cp $34		;cp $FF
    jr nz, clear_bg_map_loop
    ret

;b needs to be loaded with 0
clear_bg_map_and_pal::
	ld hl, $9C00
	ld de,$FF4F	;VBK - CGB Mode Only - VRAM Bank
	
clear_bg_map_and_pal_loop:
	call lcd_wait
    ld [hl], b
	
	ld a,1		;Turn on GBC extras
	call lcd_wait
	ld [de],a	
	
	ld [hl],0	;Palette 0
	
	xor a		;Turn off GBC extras
	ld [de],a			
	
	inc hl					;Can't load b into [hl] if we do ldi--command not allowed
    ld a, h
    cp $9F
    jr nz, clear_bg_map_and_pal_loop
    ld a, l
    cp $FF
    jr nz, clear_bg_map_and_pal_loop
    ret
	
	
;----------------------------------------------------------------------------
	
;Given a single pal value, sets a range of the bg to have that pal. Skips tiles outside
;main screen
;hl comes with bg addr
;b comes with pal value
;c comes with range of 
set_bg_pal::
	ld de,$FF4F	;VBK - CGB Mode Only - VRAM Bank
	ld a,1		;Turn on GBC extras
	ld [de],a	
set_bg_pal_loop:	
	call lcd_wait
	ld [hl],b	;Palette 0
	inc hl
	
	dec c
	jr z, set_bg_pal_end
	
	ld a, l					;If l's first hex digit is 4 and second hex digit is odd (14, 34, 54, etc.),
	and %00011111			;we're outside the screen. That's when we add 12 to hl so it wraps back and down
	cp %00010100
	jr nz, set_bg_pal_loop
	push de
		ld de, 12
		add hl, de
	pop de
	jr set_bg_pal_loop
	
set_bg_pal_end:
	xor a		;Turn off GBC extras
	ld [de],a		
	
	

	
;----------------------------------------------------------------------------
	
clear_sprite_map::
	ld hl, $C400
	
clear_sprite_map_loop:
    ld [hl], b
	inc hl
    ld a, l
    cp $A0
    jr nz, clear_sprite_map_loop
    ret