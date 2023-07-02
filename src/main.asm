include "include/gb/constants.inc"
include "include/gb/macros.inc"
include "include/gb/hUGE.inc"

SECTION "rst", ROM0[$00]				;RST 0-7
SECTION "vblank_interrupt", ROM0[$40]	;Interrupt: Vblank
	
	call isr_wrapper
    jp $ff80                			;DMA code found in memory.asm
SECTION "stat_interrupt", ROM0[$48]		;Interrupt: LCD-Stat
	reti
    ;nop
    ;jp isr_wrapper						;calls hUGE_dosound
SECTION "timer_interrupt", ROM0[$50]	;Interrupt: Timer
    reti
	;nop
    ;jp isr_wrapper
SECTION "serial_interrupt", ROM0[$58]	;Interrupt: Serial
    reti
SECTION "joypad_interrupt", ROM0[$60]	;Interrupt:Joypad
    reti

SECTION "header", ROM0[$100]
nop         ;0100   0103    Entry point (start of program)
jp  begin
;0104-0133  Nintendo logo (must match rom logo) 
db $CE,$ED,$66,$66,$CC,$0D,$00,$0B,$03,$73,$00,$83,$00,$0C,$00,$0D
db $00,$08,$11,$1F,$88,$89,$00,$0E,$DC,$CC,$6E,$E6,$DD,$DD,$D9,$99
db $BB,$BB,$67,$63,$6E,$0E,$EC,$CC,$DD,$DC,$99,$9F,$BB,$B9,$33,$3E
db "IN A WIZ'S BOX " ;0134-0142 Game Name (UCASE)
db $80      ;0143       Color gameboy flag ($80 = GB+CGB,$C0 = CGB only)
db 0,0          ;0144-0145  Game Manufacturer code
db 0            ;0146       Super GameBoy flag ($00=normal, $03=SGB)
db $1A          ;0147       Cartridge type (special upgrade hardware) ($1A is MBC5+RAM)
db 2            ;0148       Rom size (0=32k, 1=64k,2=128k etc)
db 3            ;0149       Cart Ram size (0=none,1=2k 2=8k, 3=32k)
db 1            ;014A       Destination Code (0=JPN 1=EU/US)
db $33          ;014B       Old Licensee code (must be $33 for SGB)
db 0            ;014C       Rom Version Number (usually 0)
db 0            ;014D       Header Checksum - ‘ones complement' checksum of bytes 0134-014C… (not needed for emulators)
dw 0            ;014E-014F  Global Checksum – 16 bit sum of all rom bytes (except 014E-014F)… unused by gameboy



;include "include/songs/eery.asm"
;include "include/songs/strap_in_and_suit_up.asm"
	
begin:	
	call initdma
    
	call check_level_save	;To init sram
	ld a, 18
	call update_level_save
	;call check_level_save
			;will have check_level_save in final product to set a
	;ld [curr_level], a		;check_level_save returns with 'a' holding curr_level
	;jp start_menu
	
start_menu:
	ld b, 0				;For clear_bg_map in _RESET_
	_RESET_
	;call wait_vblank
	
	ld a, 2
	ld [curr_song_bank], a
	ld [rROMB0], a
	ld hl, suit_up
	call music_player_init
	ld a, 1
	ld [rROMB0], a

	ld [music_trigger], a	;Enable music
	ld [curr_song], a		;We're starting with suit_up
	
	call lcd_off
	call load_menu_data
	call lcd_on
	
main_loop_menu:
	halt
	;call wait_vblank
	call update_menu
	;call play_wave_seq
	jr main_loop_menu
	
start_intro::
	call load_intro_data
	nop
	
intro_loop:
	halt
	call update_intro
	jr intro_loop
	
start_game::
	ld b, 0
	_RESET_
	call clear_joypad
	;call lcd_off 			;Turning screen off means we get a frame of white.
	call load_game_data		;There must be a way to load new tile data while keeping the screen black
	;call lcd_on

main_loop_game:
	halt
	;ld a, [vblank_trigger]
	;or a					;Can be passed only after V_Blank interrupt
	;jr z, main_loop_game
	;xor a
	;ld [vblank_trigger], a	
	;call wait_vblank
	call update_game
	jr main_loop_game



SECTION "main_vars", WRAM0
music_trigger:: DB
curr_song:: DB
curr_song_bank:: DB