SECTION "sounds", ROMX

;LEN, ENV, POLY, GO
block_push_sfx::
	DB $01,$A2,$66,$80
fire_sfx::
	DB $1A,$F5,$72,$80
soil_sfx::
	DB $1E,$F5,$7C,$80
ice_slide_sfx::
	DB $1E,$A7,$11,$80
ice_stop_sfx::
	DB $3F,$F1,$89,$80
	
;1 byte for sound length. 1 for pitch (only first 3 bits).
menu_select_sfx::
	DB %11110000, %11000011, %11101111, %11000110, $FF
	
menu_back_sfx::
	DB %11110000, %11000110, %11101111, %11000011, $FF
	
menu_move_sfx::
	DB %11110000, %11000011, $FF
	
frost_ding_sfx::
	DB %11011000, %11000111, $FF
	
;First byte is delay (number of ticks before next note)
;Line of bytes: Freq it starts at (hi, lo), value notes change by, sign of change (0=pos, 1=neg), goal freq (hi, lo)
potion_sfx::
	DB 	1
	DB 	%01000011, %00000000, 64, 1, %01000000, %00000000
	DB  %01000000, %00000000, 8, 0, %01000000, %01000000
	DB	%01000000, %01000000, 96, 0, %01000111, %11000000, $FF
	
earth_sfx::
	DB 	1
	DB 	%01000011, %00000000, 128, 1, %01000000, %00000000
	DB	%01000000, %01000000, 112, 0, %01000011, %01010000, $FF
	
frost_sfx::
	DB 2
	DB %01000101, %00000000, 255, 1, %01000010, %00000011
	DB %01000101, %00000000, 255, 1, %01000010, %00000011, $FF
	
;ld hl,&FF30		;Soft triangle
;ld a, &12
;ldi (hl),a	
;ld a, &46
;ldi (hl),a	
;ld a, &8A
;ldi (hl),a
;ld a, &CD	
;ldi (hl),a
;ld a, &DC	
;ldi (hl),a
;ld a, &A8	
;ldi (hl),a
;ld a, &64	
;ldi (hl),a
;ld a, &21	
;ldi (hl),a	
;ld a, &12
;ldi (hl),a	
;ld a, &46
;ldi (hl),a	
;ld a, &8A
;ldi (hl),a
;ld a, &CD	
;ldi (hl),a
;ld a, &DC	
;ldi (hl),a
;ld a, &A8	
;ldi (hl),a
;ld a, &64	
;ldi (hl),a
;ld a, &21	
;ld (hl),a	

;ld hl,&FF30		;Buzzy, Justice sound
;ld a, &48
;ldi (hl),a	
;ld a, &48
;ldi (hl),a	
;ld a, &B5
;ldi (hl),a
;ld a, &98	
;ldi (hl),a
;ld a, &BB	
;ldi (hl),a
;ld a, &95	
;ldi (hl),a
;ld a, &B8	
;ldi (hl),a
;ld a, &48	
;ldi (hl),a	
;ld a, &48
;ldi (hl),a	
;ld a, &49
;ldi (hl),a	
;ld a, &4A
;ldi (hl),a
;ld a, &75	
;ldi (hl),a
;ld a, &45	
;ldi (hl),a
;ld a, &7A	
;ldi (hl),a
;ld a, &49	
;ldi (hl),a
;ld a, &48	
;ld (hl),a	

;ld hl,&FF30		;Default waveform
;ld a, &00
;ld b, &FF
;ldi (hl),a	
;ldi (hl),b	
;ldi (hl),a
;ldi (hl),b
;ldi (hl),a
;ldi (hl),b
;ldi (hl),a
;ldi (hl),b	
;ldi (hl),a	
;ldi (hl),b	
;ldi (hl),a
;ldi (hl),b
;ldi (hl),a
;ldi (hl),b
;ldi (hl),a
;ld (hl), b	