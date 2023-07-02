include "include/gb/constants.inc"

; Constants
STACK_SIZE EQU $7A
;; Stack starts at $FFFE

; $0000 - $003F: RST handlers.

SECTION "restarts", ROM0[$0000]
ret
REPT 7
    nop
ENDR
; $0008
ret
REPT 7
    nop
ENDR
; $0010
ret
REPT 7
    nop
ENDR
; $0018
ret
REPT 7
    nop
ENDR
; $0020
ret
REPT 7
    nop
ENDR
; $0028
ret
REPT 7
    nop
ENDR
; $0030
ret
REPT 7
    nop
ENDR
; $0038
ret
REPT 7
    nop
ENDR


; Reserved stack space
;SECTION "Stack", HRAM[$FFFE - STACK_SIZE]
;    ds STACK_SIZE

SECTION "music_player_vars", WRAM0
wave_delay 	equ 3
wave_tick:: db
mid_wave::	db
curr_wave_sfx:: dw

wave_adv_delay: DB
wave_adv_tick: DB
wave_change:: DB
curr_wave_hi: DB
curr_wave_lo:DB
change_sign: DB
hi_change: DB			;Tells us if the high 3 bits have changed. If not, we skip to adding to low 8 bits.
						;Reset to FF (non-zero) at setup of sound because otherwise it skips first note
set_reset: DB
curr_wave_adv_sfx:: dw

; Initialization
SECTION "music_player", ROM0


isr_wrapper::
	push af
	ld a, [music_trigger]
	or a
	jr nz, isr_wrapper_2
	pop af
	reti
isr_wrapper_2:
	ld a, [curr_song_bank]
	ld [rROMB0], a 
    push hl
    push bc
    push de
    call hUGE_dosound
	ld a, 1
	ld [rROMB0], a
	
	jr play_wave_seq		;For now, it's in ROM1
isr_wrapper_3:
	jp play_wave_adv
isr_wrapper_4:
    pop de
    pop bc
    pop hl
	
    pop af
	
    reti


;Input: hl with song descriptor
music_player_init::
    xor a
    ldh [rIF], a
    inc a
    ldh [rIE], a
    halt
    nop

    ; Enable sound globally
    ld a, $80
    ldh [rAUDENA], a
    ; Enable all channels in stereo
    ld a, $FF
    ldh [rAUDTERM], a
    ; Set volume
    ld a, $77
    ldh [rAUDVOL], a
	
	;Initialize wave channel
	ld a, %10000000
	ldh [rAUD3ENA], a
	ld a, %00100000
	ld [rAUD3LEVEL], a
	xor a
	ld [rAUD3LOW], a
	
    call hUGE_init

    ;; Enable the HBlank interrupt on scanline 0
    ldh a, [rSTAT]
    or a, STATF_LYC
    ldh [rSTAT], a
    xor a ; ld a, 0
    ldh [rLYC], a
	
	;For play_wave_seq
	ld [wave_tick], a			;From music_player
	ld a, $FF
	ld [mid_wave], a
	
	ld [curr_wave_adv_sfx], a
    ;ld a, IEF_LCDC		;%00000010
    ;ldh [rIE], a		
    ;ei
	;reti
	ret
	
;Input: hl with noise address
play_noise::
    ldi a, [hl]
    ldh [rAUD4LEN], a
    ldi a, [hl]
    ldh [rAUD4ENV], a
    ldi a, [hl]
    ldh [rAUD4POLY], a
    ld a, [hl]
    ldh [rAUD4GO], a
	ret
	

;hl comes with address of wave sound effect
play_wave_seq_init::
	xor a
	ld [mid_wave], a
	ld a, wave_delay-1
	ld [wave_tick], a
	
	ld a, h
	ld [curr_wave_sfx], a
	ld a, l
	ld [curr_wave_sfx+1], a
	;jr play_wave_seq_2
	ret

play_wave_seq::
	ld a, [mid_wave]
	cp $FF
	jr z, isr_wrapper_3

	ld a, [curr_wave_sfx]
	ld h, a
	ld a, [curr_wave_sfx+1]
	ld l, a
	
play_wave_seq_2:
	ld a, [wave_tick]
	inc a
	ld [wave_tick], a
	cp wave_delay
	jr nz, isr_wrapper_3
	
	xor a
	ld [wave_tick], a
	
	ld a, [mid_wave]
	add 2
	ld [mid_wave], a
	sub 2
	add l
	ld l, a
	ld a, 0
	adc a, h
	ld h, a
	ldi a, [hl]
	cp $FF
	jr nz, play_wave_seq_3
	ld a, $FF
	ld [mid_wave], a
	jp isr_wrapper_3
	
play_wave_seq_3:
	ldh [rAUD3LEN], a
	ld a, [hl]
	ldh [rAUD3HIGH], a
	
	jp isr_wrapper_3
	


	

;Wave Advanced
;Comes with hl holding addr of sound effect
play_wave_adv_init::
	xor a
	ld [wave_adv_tick], a
	;ld [mid_wave_change], a
	ldi a, [hl]
	ld [wave_adv_delay], a
	
	ld a, h
	ld [curr_wave_adv_sfx], a
	ld a, l
	ld [curr_wave_adv_sfx+1], a
	
	ld a, 1
	ld [set_reset], a
	ld a, 0
	call play_wave_adv_setup
	ret
	
	
play_wave_adv:
	ld a, [curr_wave_adv_sfx]
	cp $FF
	jp z, isr_wrapper_4
	
	ld a, [wave_adv_tick]
	inc a
	ld [wave_adv_tick], a
	ld b, a
	ld a, [wave_adv_delay]
	cp b							;Is tick at delay val yet? If so, we can play next note
	jp nz, isr_wrapper_4
	
	xor a
	ld [wave_adv_tick], a
	
	ld a, [curr_wave_adv_sfx]		;Get to correct addr of potion_sfx
	ld h, a
	ld a, [curr_wave_adv_sfx+1]
	ld l, a
	
	;ld a, [mid_wave]				;which is addr of goal pitch
	;add l
	;ld l, a
	;ld a, 0
	;adc 0
	;add h
	;ld h, a
	
	;ld a, [hi_change]
	;or a
	;jr z, PlayWaveChange_No_Hi
	ld a, [set_reset]
	or a
	jr z, play_wave_adv_no_reset
	ld a, [curr_wave_hi]
	set 7, a
	ld [rAUD3HIGH], a
	xor a
	ld [set_reset], a
	jr play_wave_adv_post_reset
play_wave_adv_no_reset:
	ld a, [curr_wave_hi]
	ld [rAUD3HIGH], a
play_wave_adv_post_reset:
	xor a
	ld [hi_change], a
	
play_wave_adv_no_hi:
	ld a, [curr_wave_lo]
	ld [rAUD3LOW], a
	
	ld a, %11110000
	ld [rAUD3LEN], a
	
	
	ld a, [change_sign]
	or a
	ld a, [wave_change]
	ld b, a
	ld a, [curr_wave_lo]
	jr z, play_wave_adv_pos

	sub b
	ld [curr_wave_lo], a
	jr nc, play_wave_adv_2
	ld a, [curr_wave_hi]
	dec a
	ld [curr_wave_hi], a
	jr play_wave_adv_hi_change
play_wave_adv_pos:
	add b
	ld [curr_wave_lo], a
	jr nc, play_wave_adv_2
	ld a, [curr_wave_hi]
	inc a
	ld [curr_wave_hi], a
play_wave_adv_hi_change:
	ld a, 1
	ld [hi_change], a
	
play_wave_adv_2:
	ld a, [curr_wave_hi]
	ld b, a
	ldi a, [hl]
	cp b
	jp nz, isr_wrapper_4
	ld a, [curr_wave_lo]
	ld b, a
	ldi a, [hl]
	cp b
	jp nz, isr_wrapper_4		;Check if we're at goal pitch	
	
	call play_wave_adv_setup
	jp isr_wrapper_4



play_wave_adv_setup:
	ldi a, [hl]
	cp $FF
	jr nz, play_wave_adv_setup_2
	;xor a
	;ld [mid_wave], a
	ld a, $FF
	ld [curr_wave_adv_sfx], a
	ret
	
play_wave_adv_setup_2:
	ld [curr_wave_hi], a
	ldi a, [hl]
	ld [curr_wave_lo], a
	ldi a, [hl]
	ld [wave_change], a
	ldi a, [hl]
	ld [change_sign], a
	
	ld a, h
	ld [curr_wave_adv_sfx], a
	ld a, l
	ld [curr_wave_adv_sfx+1], a
	
	ld a, $FF
	ld [hi_change], a
	ret