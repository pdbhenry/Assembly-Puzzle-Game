SECTION "save", ROM0

include "include/gb/constants.inc"

check_level_save::
	ld a, $0A
	ld [$0000], a			;Enabling SRAM
	
	ld hl, SRAM_START		;A000
	ldi a, [hl]	 			;Checking the 3-byte signature
	cp 48					;Signature is 48 45 89 (GU IL TY) :)
	jr nz, init_sram
	ldi a, [hl]
	cp 45
	jr nz, init_sram
	ldi a, [hl]
	cp 89
	jr nz, init_sram
	ld a, [hl]				;If signature is correct, return with 'a' 
							;holding latest level
close_sram:
	push af
		ld a, 0
		ld [$0000], a
	pop af
	
	ret						
	
init_sram:
	ld hl, SRAM_START+3			;Don't need to init signature addrs (SRAM - SRAM+2)
	ld bc, $10				;How much of SRAM is to be cleared. I don't
							;think I'll need much save data though		
init_sram_loop:
	xor a
	ldi [hl], a
	dec bc
	ld a, b
	or c
	jr nz, init_sram_loop

init_signature:
	ld a, 48
	ld hl, SRAM_START
	ldi [hl], a
	ld a, 45
	ldi [hl], a
	ld a, 89
	ldi [hl], a
	xor a
	ld [hl], a
	jr close_sram
	
;a comes loaded with 1+level that was just beat.
update_level_save::
	ld b, a
	ld a, $0A
	ld [$0000], a
	ld a, [SRAM_START+3]		;Latest level save data
	cp b
	jp nc, close_sram
	ld a, b
	ld [SRAM_START+3], a		;Update latest level
	jp close_sram