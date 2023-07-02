SECTION "memory", ROM0

include "include/gb/constants.inc"

clear_oam::
	xor a
	ld b, 160
	ld hl, OAMRAM
clear_oam_loop:
	ldi [hl], a
	dec b
	jr nz, clear_oam_loop

;----------------------------------------------------------------------------

initdma::                 ; Transfer DMA routine to RAM
	ld     de,$FF80		;HRAM that holds DMA code
	ld     hl,dmacode
	ld     bc,dmaend-dmacode
	call   memcpy
	ret

dmacode:                 ; Transfer sprite data from reg A pos. using DMA
	push af
		ld     a,$C4           ; <<< Set DMA from address to $c400
		ldh    [$FF46],a         ; Start DMA (ff46 is dma transfer address)
		ld     a,$28           ; Wait for 160ns
dma_wait:
		dec    a
		jr     nz,dma_wait
	pop af
	reti
dmaend:

;----------------------------------------------------------------------------

;DE is destination, HL is source, BC is size	
memcpy::
    dec bc
    inc b
    inc c
.copy
    ldh a, [rSTAT]
    and STATF_BUSY
    jr nz, .copy
    ldi a, [hl]
    ld [de], a
    inc de
    dec c
    jr nz, .copy
    dec b
    jr nz, .copy
    ret


memcpy_nonvram::
	ldi a, [hl]
	ld [de], a
	inc de
	dec bc
	ld a, c
	or b
	jr nz, memcpy_nonvram
	ret
	
;hl comes with addr of map
memcpy_screen::
	ld de, $9C00
	ld b, 20
	ld c, 18
.loop:
	ldh a, [rSTAT]				;lcd_wait
    and STATF_BUSY
    jr nz, .loop
	
	ldi a, [hl]
	ld [de], a
	inc de
	dec b
	jr nz, .loop
	
	dec c
	ret z
	
	ld a, c
	ld c, 12
	add hl, bc
	ld b, 20
	ld c, a
	jr .loop
	
	
;----------------------------------------------------------------------------

;HL is destination, A is value, B is size
memcpy_single::
	push af
.copy:
		ldh a, [rSTAT]
		and STATF_BUSY
		jr nz, .copy
	pop af
	ldi [hl], a				;A holds val so we can use ldi
	dec b
	jr nz, memcpy_single
	ret
	
	
;Add done here so we don't exit memcpy_single_var with hl a whole
;addition ahead of where it really ended
memcpy_single_var_add:	
	push af
		call add_a_to_hl
	pop af
;Call the func below, not the 'add' above
;HL is destination, C is value, B is size, A is val to inc hl each step
memcpy_single_var::
	call lcd_wait
	ld [hl], c
	dec b
	jr nz, memcpy_single_var_add
	ret