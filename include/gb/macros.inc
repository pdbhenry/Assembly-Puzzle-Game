MACRO _RESET_
    ;call wait_vblank
    ;call lcd_off
    call clear_bg_map
    ;call lcd_wait
    call clear_sprite_map
    ;call lcd_on
    ld sp, $ffff
    xor a
    ldh [LCD_SCROLL_Y], a
    ldh [LCD_SCROLL_X], a
ENDM
