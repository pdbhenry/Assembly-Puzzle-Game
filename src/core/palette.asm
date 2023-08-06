SECTION "palette", ROM0

include "include/gb/constants.inc"

;hl should come loaded with the palette data address
;If b = 0, bg palette changes. if b != 0, object palette changes
;c comes loaded with palette index
SetGBCPalettes::
	ld a, 5
SetGBCPalettesLoop:
	dec a
	ret z
	
	push af
		ldi a,[hl]  	;GGGRRRRR
		ld e,a
		ldi a,[hl]  	;xBBBBBGG
		ld d,a
		;inc a 			;cp 255
		;ret z
		push hl
			;call lcd_wait ;Wait for VDP Sync
			ld a, b
			or 0
			jp nz, ObjPaletteAddr
			ld hl, BCPS
			jp SetGBCPalettesLoop_2
ObjPaletteAddr:
			ld hl, OCPS ;If b is not zero, set hl to Object Color Palette Specification address. FF6B is Object Color Palette Data
SetGBCPalettesLoop_2:		
			call lcd_wait
			ld [hl],c	;FF68 - BCPS/BGPI - CGB Mode Only - Background Palette Index
			inc hl			
			ld [hl],e	;FF69 - BCPD/BGPD - CGB Mode Only - Background Palette Data
			dec hl		
			inc	c		;Increase palette address
			call lcd_wait
			ld [hl],c	;FF68 - BCPS/BGPI - CGB Mode Only - Background Palette Index
			inc hl		
			ld [hl],d	;FF69 - BCPD/BGPD - CGB Mode Only - Background Palette Data
			inc c		;Increase palette address
		pop hl
	pop af
	jr SetGBCPalettesLoop
	
;----------------------------------------------------------------------------
;----------------------------------------------------------------------------
;----------------------------------------------------------------------------

;Because changing palette data at $FF47 (BG Palette Data) doesn't seem to work on GBC, 
;I made this advanced SetGBCPalettes function that performs that function.
;Used in PuzzleGame for menu flicker and fade.

;hl should come loaded with the palette data address
;If b = 0, bg palette changes. if b != 0, object palette changes
;c comes loaded with palette index
;d should come loaded with palette intensities (Ex: %11100100)
SetGBCPalettesAdv::
	;ld a, 5 		
	ld e, 5				;Will loop 4 times
	
SetGBCPalettesAdvLoop:
	dec e
	ret z
	
	xor a
	rr d
	rra
	rr d
	rra
	swap a
	rra 			;Right rotate until we have the 2 bits of d multiplied by 2	
	
SetGBCPalettesAdvLoop_2:
	push hl
	push de
		ld de, 0
		ld e, a
		add hl, de
		
		ldi a,[hl]  	;GGGRRRRR
		ld e,a
		ldi a,[hl]  	;xBBBBBGG
		ld d,a

		push hl
			;call lcd_wait ;Wait for VDP Sync
			ld a, b
			or 0
			jp nz, ObjPaletteAddrAdv
			ld hl, BCPS					;Background Color Palette Specification
			jp SetGBCPalettesAdvLoop_3
ObjPaletteAddrAdv:
			ld hl,OCPS  ;If b is not zero, set hl to Object Color Palette Specification address. FF6B is Object Color Palette Data
SetGBCPalettesAdvLoop_3:		
			call lcd_wait
			ld [hl],c	;FF68 - BCPS/BGPI - CGB Mode Only - Background Palette Index
			inc hl			
			ld [hl],e	;FF69 - BCPD/BGPD - CGB Mode Only - Background Palette Data
			dec hl		
			inc	c		;Increase palette address
			call lcd_wait
			ld [hl],c	;FF68 - BCPS/BGPI - CGB Mode Only - Background Palette Index
			inc hl		
			ld [hl],d	;FF69 - BCPD/BGPD - CGB Mode Only - Background Palette Data
			inc c		;Increase palette address
		pop hl
	pop de
	pop hl
	jr SetGBCPalettesAdvLoop

	
;b comes as 0 for bg pal or 1 for obj pal
;c comes with pal index
SetGBCPalettesBlack::
	ld de, 5			;d is 0 for black pal, e is 5 to loop 4 times
	
	ld a, b
	or 0
	jp nz, ObjPaletteAddrBlack
	ld hl, BCPS					;Background Color Palette Specification
	jp SetGBCPalettesBlackLoop
ObjPaletteAddrBlack:
	ld hl,OCPS  ;If b is not zero, set hl to Object Color Palette Specification address. FF6B is Object Color Palette Data
	
SetGBCPalettesBlackLoop:
	dec e
	ret z
		
	call lcd_wait
	ld [hl],c	;FF68 - BCPS/BGPI - CGB Mode Only - Background Palette Index
	inc hl			
	ld [hl],d	;FF69 - BCPD/BGPD - CGB Mode Only - Background Palette Data
	dec hl		
	inc	c		;Increase palette address
	call lcd_wait
	ld [hl],c	;FF68 - BCPS/BGPI - CGB Mode Only - Background Palette Index
	inc hl		
	ld [hl],d	;FF69 - BCPD/BGPD - CGB Mode Only - Background Palette Data
	inc c		;Increase palette address
	dec hl		;Setting hl back to its OG addr (BCPS or OCPS) for next loop
	jr SetGBCPalettesBlackLoop
		
		
;de holds addr of palette to be changed
;c holds index*2 of highlight we're using (*2 because each highlight is 2 bytes)
SetHighlight::
	ld hl, Highlights
	ld b, 0
	add hl, bc
	ld bc, 2
	call memcpy
	ret
	
	
		
VBK_On::
	push bc
		ld bc, VBANK	;VBK - CGB Mode Only - VRAM Bank
		ld a,1			;Turn on GBC extras
		ld [bc],a	
	pop bc
	ret
	
VBK_Off::
	push bc
		ld bc, VBANK	;VBK - CGB Mode Only - VRAM Bank
		xor a			;Turn off GBC extras
		ld [bc],a	
	pop bc
	ret
	
	
;hl comes with map addr of tile
get_tile_pal::	
	call VBK_On
	call lcd_wait
	ld a, [hl]
	push af
		call VBK_Off
	pop af
	
	ret
	
	
	
;UNFINISHED!
;Given wram_level address in hl, convert to address for wram_pals
level_to_pal:
	push bc
	push de
	push hl
		;ld bc, wram_level
		ld a, h
		sub b
		ld h, a
		ld a, l
		sub c
		ld l, a
		
		ld d, 0
		ld bc, -44
level_to_pal_2:
		ld a, h
		or a
		jr nz, level_to_pal_3
		ld a, l
		cp 44
		jr c, level_to_pal_4
level_to_pal_3:
		add hl, bc
		inc d
		jr level_to_pal_2
level_to_pal_4:
		ld e, l
		;ld hl, wram_pals
level_to_pal_5:
				
		
		
	pop hl
	pop de
	pop bc
	
	ret
	
;hl comes with map addr of top-left tile
;a comes with pal index val
set_block_pal::	
	push hl
		push af
			call VBK_On
		pop af
		call lcd_wait
		ldi [hl], a
		ld [hl], a
		ld b, a
		ld a, 31
		call add_a_to_hl
		ld a, b
		call lcd_wait
		ldi [hl], a
		ld [hl], a
		call VBK_Off
	pop hl
	ret

; White, light gray, dark gray, Black
;		 			xBBBBBGGGGGRRRRR
GBPalDefault::	
dw %0111111111111111	;col 0
dw %0101001010010100	;col 1
dw %0010100101001010	;col 2
dw %0000000000000000	;col 3
;				dw %1111111111111111	;End of list
				
GBPalGoblin::
dw %0111111111111111	;col 0
dw %0010101111011111	;col 1
dw %0011001000000000	;col 2
dw %0010000010000001	;col 3
;				dw %1111111111111111	;End of list
				
GBPalGoblinFire::
dw %0111111111111111	;col 0
dw %0010101111011111	;col 1
dw $A893				;col 2
dw %0010000010000001	;col 3
;				dw %1111111111111111	;End of list

GBPalGoblinIce::
dw %0111111111111111	;col 0
dw %0010101111011111	;col 1
dw $4183				;col 2
dw %0010000010000001	;col 3
;				dw %1111111111111111	;End of list
				
GBPalGoblinEarth::
dw %0111111111111111	;col 0
dw %0010101111011111	;col 1
dw $9A31				;col 2
dw %0010000010000001	;col 3
;					dw %1111111111111111	;End of list
					
GBPalFrost::
dw %0111111111111111	;col 0
dw $EAEE				;col 1
dw $4183				;col 2
dw $B4C0				;col 3
;				dw %1111111111111111	;End of list
				
GBPalFire::
dw %0111111111111111	;col 0
dw $2BDF				;col 1, Light Yellow
dw $0498				;col 2, Light Red
;dw $8EBC				;col 2, Orange
dw $0014				;col 3, Red
;				dw %1111111111111111	;End of list
				
GBPalSlime::
dw $0B97
dw $1E29
dw $0E00
dw $8124
;				dw $FFFF
				
GBPalMenu::
dw %0011111000011101	;col 0
dw %0010000100110011	;col 1
dw %0000010001001010	;col 2
dw %0000000000000000	;col 3
;				dw %1111111111111111	;End of list	
				
GBPalMenuLvl::
dw $8052
dw $96DE
dw $402C
dw $1861
;				dw $FFFF
				
;Blue/Teal background
GBPalBg1::
dw %0111111010101000
dw %0110000101100000
dw %0010010011100010
dw $0820
;				dw %1111111111111111
;Yellow/Orange foreground	
GBPalFg1::
dw $1B7F
dw $25D9
dw $1411
dw $1460
;				dw %1111111111111111
;Brown/Red soil
GBPalSoil::
dw $1E1F
dw $1C9B
dw $1408
dw $1E09
;				dw $FFFF
;Gray Steel
GBPalSteel::
dw $77FF
;dw $7AF5		;Light blue gray
dw $6718		;Shark gray
dw $4A2D
dw $24E5
;				dw $FFFF
				
Highlights::
dw $005D		;Bright red

;Blue menu variation
;				dw %0100111010011101	;col 0
;				dw %0100110100101000	;col 1
;				dw %0010100001000001	;col 2
;				dw %0000000000000000	;col 3
;				dw %1111111111111111	;End of list