SECTION "joypad_vars", WRAM0

io_p14:: DB
io_p15:: DB
io_p14_old:: DB
io_p15_old:: DB

SECTION "joypad", ROM0

include "include/gb/constants.inc"

read_joypad::
    ; Read P14      [directions]
    ld hl, USER_IO
    ld a, $20       ;0010 0000 [selecting directions [because 4th bit is 0]]
    ld [hl], a
    ld a, [hl]
    ld hl, io_p14
    ld b, [hl]
    ld [hl], a
    ld hl, io_p14_old
    ld [hl], b

    ; Read P15      [buttons]
    ld hl, USER_IO
    ld a, $10       ;0001 0000 [selecting buttons [because 5th bit is 0]]
    ld [hl], a
    ld a, [hl]
    ld hl, io_p15
    ld b, [hl]
    ld [hl], a
    ld hl, io_p15_old
    ld [hl], b      ;Put what was previously in the io_p15 var into io_p15_old
    
    ; Reset
    ld hl, USER_IO
    ld a, $FF
    ld [hl], a      ;load %1111 1111 into USER_IO to close reading of input
    ret

clear_joypad::
    ld hl, io_p14
    ld [hl], NO_INPUT_P14
    ld hl, io_p15
    ld [hl], NO_INPUT_P15
    ld hl, io_p14_old
    ld [hl], NO_INPUT_P14
    ld hl, io_p15_old
    ld [hl], NO_INPUT_P15
    ret


