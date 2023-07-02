; ///////////////////////
; //                   //
; //  File Attributes  //
; //                   //
; ///////////////////////

; Filename: IAWBMenu.png
; Pixel Width: 160px
; Pixel Height: 144px

; /////////////////
; //             //
; //  Constants  //
; //             //
; /////////////////

menu_tile_map_size EQU $0168
menu_tile_map_width EQU $14
menu_tile_map_height EQU $12

menu_tile_data_size EQU $06B0
menu_tile_count EQU $6B

; ////////////////
; //            //
; //  Map Data  //
; //            //
; ////////////////

SECTION "menu map", ROMX

menu_map_data::
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$01,$02,$03,$04,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$05,$06,$07,$08
DB $00,$00,$00,$00,$09,$0A,$0B,$00,$00,$00,$00,$00,$00,$00,$0C,$0D
DB $0E,$0F,$10,$11,$00,$00,$00,$00,$00,$12,$13,$14,$00,$00,$00,$00
DB $00,$00,$00,$15,$16,$17,$18,$19,$00,$00,$00,$00,$1A,$1B,$1C,$19
DB $00,$00,$00,$00,$00,$00,$00,$1D,$1E,$1F,$20,$00,$21,$00,$00,$22
DB $23,$24,$25,$26,$00,$00,$00,$00,$00,$00,$00,$27,$28,$29,$2A,$2B
DB $2C,$2D,$2E,$2F,$30,$31,$32,$33,$34,$35,$00,$00,$00,$00,$00,$36
DB $37,$38,$39,$00,$00,$00,$00,$3A,$3B,$3C,$3D,$3E,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3F,$40,$41,$42,$43,$44
DB $45,$46,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$47,$48
DB $49,$4A,$4B,$4C,$4D,$4E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$4F,$50,$00,$00,$00,$00,$51,$52,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$53,$54,$55,$56,$57,$58,$59,$5A,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$5B,$5C,$5D,$5E,$5F,$60
DB $61,$62,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$63,$64,$65,$66,$67,$68,$69,$6A,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $00,$00,$00,$00,$00,$00,$00,$00

; //////////////////////////////
; //                          //
; //  Title Screen Tile Data  //
; //                          //
; //////////////////////////////

menu_tile_data::
DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
DB $8E,$8E,$F1,$71,$FB,$FB,$FB,$FB,$FB,$FB,$FB,$F3,$F7,$F7,$F7,$F7
DB $FF,$7F,$FF,$FF,$FF,$FF,$FF,$FF,$CB,$C9,$E5,$A5,$ED,$ED,$ED,$ED
DB $FF,$FF,$FF,$FF,$FF,$FF,$FE,$FE,$FF,$FD,$FF,$FF,$FC,$FC,$FB,$FB
DB $FF,$FF,$FF,$FF,$FF,$FF,$3F,$3F,$DF,$DF,$FF,$DF,$3F,$3F,$BF,$BF
DB $96,$86,$F9,$71,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
DB $ED,$6D,$FE,$DC,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
DB $FB,$FB,$FE,$FC,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FE,$F0
DB $FF,$BF,$DF,$5F,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$4F,$07
DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FC,$FF,$FF
DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$12,$00,$9F,$1E
DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$7F,$3F,$0F,$07
DB $FF,$FF,$FF,$FF,$FF,$FF,$F8,$E0,$F8,$E0,$FC,$F8,$FF,$FC,$FF,$FC
DB $FF,$FF,$FF,$FF,$FF,$FF,$07,$07,$07,$07,$0F,$0F,$0F,$0F,$0F,$0F
DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$E3,$E0,$E0,$E0,$E0
DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$37,$0F,$07,$0F,$07
DB $FE,$FC,$FE,$FC,$FE,$FE,$FE,$FE,$FE,$FE,$FE,$FC,$FC,$FC,$FC,$FC
DB $0F,$0F,$0F,$0F,$7F,$3F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F
DB $9F,$1F,$9F,$1F,$9F,$1F,$9F,$1F,$9F,$1F,$9F,$1F,$9F,$1F,$9F,$1F
DB $C7,$83,$E3,$C1,$E1,$E0,$F1,$E0,$F1,$F0,$F0,$F0,$F0,$F0,$F0,$F0
DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$7F,$FF,$7F,$FF,$7F
DB $0F,$0F,$0F,$07,$87,$07,$87,$07,$87,$87,$87,$87,$87,$87,$E7,$81
DB $FC,$FC,$FF,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$F8,$FC,$F8,$F8,$F8
DB $3F,$3F,$3F,$3F,$3F,$0F,$3F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F
DB $FD,$FC,$FD,$FC,$FD,$F1,$FD,$F1,$F1,$F1,$F3,$F1,$F3,$F1,$F3,$E3
DB $FF,$7F,$FF,$7F,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FE,$FF,$FE,$FF,$FE
DB $9F,$1F,$9F,$1F,$9F,$1F,$9F,$1F,$1F,$1F,$1F,$0F,$1F,$0F,$0E,$00
DB $F0,$F0,$F0,$F0,$F1,$F0,$F1,$E0,$E1,$E1,$E3,$C1,$8F,$83,$0F,$0F
DB $E1,$E1,$E1,$E1,$E1,$E1,$F1,$E1,$F1,$E1,$F1,$E1,$F1,$F0,$F8,$F0
DB $F9,$F8,$FB,$F8,$FB,$E3,$FB,$E3,$FB,$E3,$E7,$E3,$E7,$E7,$E7,$C7
DB $8F,$07,$8F,$07,$8F,$07,$87,$87,$87,$87,$87,$87,$C7,$81,$C7,$C1
DB $F3,$E3,$F3,$E3,$EF,$E3,$EF,$E3,$EF,$E3,$EF,$CF,$EF,$CF,$DF,$CF
DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FB
DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$DA,$DA
DB $FF,$FE,$FF,$FE,$FF,$FE,$FF,$FE,$FF,$FE,$FF,$FE,$FF,$FE,$7F,$7E
DB $00,$00,$1F,$0F,$1F,$0F,$1F,$0F,$1F,$0F,$1F,$0F,$1F,$0F,$1F,$0F
DB $0F,$0F,$CF,$83,$E3,$E1,$F1,$F0,$F8,$F0,$F8,$F8,$F8,$F8,$FC,$FC
DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$7F,$FF,$3F,$7F,$3F,$3F,$3F
DB $F8,$F0,$F8,$F8,$F8,$F8,$F8,$F8,$FE,$F8,$FE,$FE,$FE,$FE,$FE,$FE
DB $E7,$C7,$DF,$C7,$DF,$C7,$DF,$C7,$1F,$1F,$3F,$1F,$3F,$1F,$3F,$1F
DB $C7,$C1,$C1,$C1,$F1,$C1,$F1,$C1,$F1,$F1,$F1,$F0,$F1,$F0,$F1,$F0
DB $DF,$CE,$DF,$DF,$DF,$1F,$DF,$1F,$3F,$1F,$3F,$1F,$3F,$1F,$3F,$3F
DB $70,$10,$3E,$26,$BE,$3E,$BD,$3C,$BD,$3D,$3B,$3B,$3B,$33,$F0,$F0
DB $79,$79,$7D,$79,$FD,$FD,$F5,$F4,$D0,$D0,$EE,$EE,$AE,$AE,$7F,$44
DB $E0,$C0,$F6,$E2,$F7,$F6,$F6,$F6,$F1,$F1,$F5,$F5,$76,$62,$EB,$03
DB $EF,$E1,$72,$72,$73,$72,$73,$73,$B3,$33,$73,$52,$F2,$32,$E5,$61
DB $F3,$D2,$D2,$52,$73,$73,$71,$71,$78,$78,$7C,$7C,$7E,$7E,$FE,$FE
DB $FF,$FE,$FF,$FE,$FF,$FE,$FF,$FE,$FF,$FE,$FF,$FE,$7F,$7E,$7E,$7E
DB $1F,$0F,$1F,$0F,$1F,$0F,$1F,$0F,$1F,$0F,$1F,$0F,$1F,$0F,$1F,$0F
DB $FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$F8,$F8,$F8,$F8
DB $3E,$3C,$3D,$39,$33,$33,$33,$33,$33,$33,$73,$33,$7B,$3B,$FE,$3C
DB $FF,$E1,$37,$33,$BA,$B2,$99,$99,$99,$99,$B6,$96,$BE,$BE,$ED,$6C
DB $FF,$7F,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$7F,$7F
DB $FE,$FE,$FF,$FE,$FF,$FE,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
DB $3F,$3F,$3F,$3F,$3F,$3F,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
DB $F0,$F0,$F8,$F8,$F8,$F8,$F8,$F8,$FE,$F8,$FE,$F8,$FE,$F8,$FE,$FE
DB $3F,$3F,$FF,$3F,$FF,$3F,$FF,$3F,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
DB $BE,$BE,$BE,$BE,$BE,$BC,$E3,$E3,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
DB $7E,$7E,$FE,$7E,$FE,$FE,$FE,$FE,$FE,$F8,$FF,$FF,$FF,$FF,$FF,$FF
DB $1F,$0F,$1F,$0F,$1F,$0F,$0F,$0F,$02,$00,$FF,$FF,$FF,$FF,$FF,$FF
DB $F9,$F0,$F3,$F0,$E3,$E1,$C7,$83,$1F,$0F,$FF,$FF,$FF,$FF,$FF,$FF
DB $FF,$7F,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
DB $80,$00,$40,$00,$20,$00,$B0,$00,$2B,$00,$23,$04,$2D,$02,$26,$09
DB $04,$00,$03,$00,$01,$00,$00,$00,$FD,$02,$BF,$40,$E0,$1F,$1F,$E0
DB $00,$00,$FF,$00,$00,$00,$90,$00,$BF,$40,$1F,$E0,$EF,$10,$77,$88
DB $01,$00,$FF,$00,$01,$00,$08,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00
DB $00,$00,$00,$00,$00,$00,$90,$00,$7F,$80,$03,$FC,$7C,$83,$5F,$A0
DB $01,$00,$01,$00,$02,$00,$04,$00,$D7,$28,$F7,$08,$0F,$F0,$DF,$20
DB $00,$00,$E0,$00,$1F,$00,$80,$00,$FF,$00,$FE,$01,$FF,$00,$FF,$00
DB $01,$00,$12,$00,$84,$00,$78,$00,$D0,$00,$C0,$20,$B0,$40,$70,$80
DB $2F,$00,$4F,$00,$4F,$00,$5F,$00,$CF,$00,$73,$0C,$0C,$03,$0F,$00
DB $50,$BF,$C0,$7F,$20,$FF,$10,$FF,$09,$FF,$07,$FF,$E3,$FF,$9F,$FF
DB $04,$FF,$02,$FF,$01,$FF,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
DB $00,$FF,$00,$FF,$00,$FF,$80,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
DB $81,$FF,$80,$FF,$80,$FF,$81,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
DB $40,$FF,$40,$FF,$80,$FF,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
DB $02,$FD,$03,$FE,$04,$FF,$0A,$FF,$90,$FF,$E0,$FF,$C2,$FF,$E1,$FF
DB $50,$A0,$70,$80,$70,$80,$70,$80,$B0,$40,$A0,$50,$BF,$40,$04,$F0
DB $0D,$02,$0F,$00,$0F,$00,$0E,$01,$0E,$01,$0E,$01,$0E,$01,$0A,$05
DB $8F,$FF,$AF,$FF,$8F,$FF,$0F,$FF,$0F,$FF,$0F,$FF,$4F,$FF,$0F,$FF
DB $FE,$FF,$F0,$FF,$F0,$FF,$F0,$FF,$F0,$FF,$F0,$FF,$F0,$FF,$F0,$FF
DB $F4,$00,$E5,$10,$F4,$00,$F4,$00,$F4,$00,$F8,$00,$F8,$00,$F8,$00
DB $0E,$01,$08,$07,$F7,$08,$1B,$04,$1F,$00,$9F,$00,$1F,$00,$05,$0A
DB $7F,$FF,$83,$FF,$07,$FF,$09,$FF,$10,$FF,$24,$FF,$C0,$7F,$7E,$BF
DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$FF,$00,$FF,$08,$FF,$01,$FF
DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$20,$FF,$40,$FF,$80,$FF,$00,$FF
DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$40,$FF,$40,$FF,$40,$FF,$48,$FF
DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$80,$FF,$80,$FF,$40,$FF,$A0,$FF
DB $F8,$FF,$C7,$FF,$E0,$FF,$91,$FF,$08,$FF,$05,$FF,$03,$FE,$02,$FD
DB $D8,$20,$7A,$80,$88,$70,$BE,$40,$A1,$50,$B0,$40,$70,$80,$70,$80
DB $06,$09,$05,$0A,$0B,$04,$4B,$00,$10,$00,$20,$00,$40,$00,$80,$00
DB $FE,$01,$DF,$20,$FF,$00,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $01,$FE,$FD,$02,$BA,$45,$F6,$09,$1F,$01,$20,$00,$44,$00,$80,$00
DB $FF,$00,$FF,$00,$FF,$00,$FD,$02,$C0,$00,$3F,$00,$00,$00,$04,$00
DB $BF,$40,$80,$7F,$BF,$40,$B7,$48,$40,$00,$C0,$00,$40,$00,$40,$00
DB $EF,$10,$07,$F8,$DB,$24,$CD,$32,$02,$00,$01,$00,$00,$00,$00,$00
DB $FF,$00,$FF,$00,$FE,$01,$BF,$40,$03,$00,$FC,$00,$80,$00,$48,$00
DB $50,$A0,$B0,$40,$C0,$20,$D0,$00,$F8,$00,$04,$00,$0A,$00,$01,$00
DB $FF,$F0,$FF,$F3,$FF,$F3,$FF,$F3,$FF,$F0,$FF,$F3,$FF,$F3,$FF,$F3
DB $FF,$3F,$FF,$9F,$FF,$9F,$FF,$92,$FF,$30,$FF,$F1,$FF,$F3,$FF,$F3
DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$38,$FF,$93,$FF,$90,$FF,$F3,$FF,$F8
DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$38,$FF,$D3,$FF,$10,$FF,$FF,$FF,$10
DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$18,$FF,$F3,$FF,$10,$FF,$9F,$FF,$30
DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$1F,$FF,$FF,$FF,$1F,$FF,$9F,$FF,$3F
DB $FF,$FC,$FF,$FB,$FF,$F6,$FF,$F5,$FF,$F4,$FF,$F5,$FF,$FB,$FF,$FC
DB $FF,$3F,$FF,$DF,$FF,$6F,$FF,$AF,$FF,$2F,$FF,$AF,$FF,$DF,$FF,$3F
menu_tile_data_end::

	
menu_1_tile_data::
DB $FF,$FF,$E7,$FF,$C3,$FF,$98,$E7,$98,$E7,$C3,$FF,$E7,$FF,$E7,$FF
DB $FF,$FF,$E7,$FF,$C3,$FF,$19,$E7,$19,$E7,$C3,$FF,$E7,$FF,$E7,$FF
DB $E7,$FF,$E7,$FF,$C3,$FF,$98,$E7,$98,$E7,$C3,$FF,$E7,$FF,$FF,$FF
DB $E7,$FF,$E7,$FF,$C3,$FF,$19,$E7,$19,$E7,$C3,$FF,$E7,$FF,$FF,$FF
DB $FF,$FF,$FF,$FF,$FF,$FF,$00,$FF,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF
DB $E7,$FF,$E7,$FF,$E7,$FF,$E7,$FF,$E7,$FF,$E7,$FF,$E7,$FF,$E7,$FF
menu_1_tile_data_end::


menu_mini_lvl_tile_data::
;Border. I know I could reduce to a quarter of size by hor/vert flips but
;That uses CGB mode. Maybe I'll try to make the game playable on OG GB.
DB $FF,$FF,$FF,$FF,$C7,$FF,$C4,$FF,$C0,$FF,$F0,$FF,$E0,$FF,$E0,$FF
DB $FF,$FF,$FF,$FF,$E3,$FF,$23,$FF,$03,$FF,$0F,$FF,$07,$FF,$07,$FF
DB $E0,$FF,$E0,$FF,$F0,$FF,$C0,$FF,$C4,$FF,$C7,$FF,$FF,$FF,$FF,$FF
DB $07,$FF,$07,$FF,$0F,$FF,$03,$FF,$23,$FF,$E3,$FF,$FF,$FF,$FF,$FF
DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$3C,$FF,$00,$FF,$00,$FF
DB $00,$FF,$00,$FF,$3C,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
DB $F8,$FF,$F8,$FF,$FC,$FF,$FC,$FF,$FC,$FF,$FC,$FF,$F8,$FF,$F8,$FF
DB $1F,$FF,$1F,$FF,$3F,$FF,$3F,$FF,$3F,$FF,$3F,$FF,$1F,$FF,$1F,$FF
menu_mini_lvl_tile_data_border_end::

DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
DB $FF,$FF,$FF,$E7,$FF,$C3,$FF,$81,$FF,$81,$FF,$C3,$FF,$E7,$FF,$FF
DB $FF,$FF,$FF,$FF,$FF,$FF,$BB,$FF,$99,$FF,$C0,$FF,$E1,$FF,$FB,$FF
DB $FF,$FF,$C3,$FF,$81,$FF,$99,$FF,$9D,$FF,$05,$FF,$8F,$FF,$DF,$FF
DB $3C,$3C,$7E,$7E,$FF,$FF,$BB,$FF,$99,$FF,$C0,$FF,$60,$7E,$38,$3C
DB $3C,$3C,$42,$7E,$81,$FF,$99,$FF,$9D,$FF,$05,$FF,$0E,$7E,$1C,$3C
DB $00,$00,$00,$7E,$00,$5A,$00,$7E,$00,$7E,$00,$5A,$00,$7E,$00,$00
DB $81,$81,$00,$7E,$00,$7E,$00,$66,$00,$66,$00,$7E,$00,$7E,$81,$81
DB $C0,$F4,$10,$FE,$10,$DE,$20,$2E,$40,$EE,$8C,$EE,$10,$FD,$00,$3C
menu_mini_lvl_tile_data_end::
	

menu_sprite_tile_data::
	DB $00,$0F,$0F,$30,$07,$78,$00,$FF,$00,$1F,$04,$0F,$03,$0F,$03,$0F
	DB $00,$00,$00,$C0,$00,$E0,$00,$F0,$00,$F8,$00,$FE,$8E,$FF,$D0,$FF
	DB $00,$00,$00,$03,$00,$07,$00,$0F,$00,$1F,$00,$3F,$1C,$FF,$02,$FF
	DB $00,$F8,$F0,$0C,$E0,$1E,$00,$FF,$00,$F8,$00,$F0,$20,$F0,$60,$F0
	DB $02,$03,$00,$03,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	DB $CE,$F1,$5F,$E0,$37,$CC,$3F,$CC,$3E,$41,$1C,$63,$01,$7F,$07,$3F
	DB $1C,$E3,$3E,$C1,$37,$CC,$7F,$EC,$0F,$F0,$C6,$39,$E4,$1F,$64,$9E
	DB $C0,$E0,$80,$C0,$00,$80,$00,$80,$00,$80,$00,$00,$00,$00,$00,$00
	DB $00,$00,$00,$00,$00,$00,$01,$00,$02,$01,$00,$03,$00,$03,$00,$03
	DB $12,$1D,$3C,$3F,$0F,$FF,$0F,$FF,$1F,$FD,$3F,$FF,$7F,$FF,$7F,$FF
	DB $80,$FC,$42,$FE,$C2,$FF,$22,$FF,$F2,$FF,$FB,$FF,$FF,$FF,$FF,$FF
	DB $00,$00,$00,$00,$00,$80,$00,$C0,$80,$E0,$80,$F0,$00,$F0,$00,$F0
	DB $01,$03,$00,$01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	DB $3F,$FF,$3F,$FF,$27,$3F,$03,$3F,$70,$8F,$41,$FF,$C2,$FE,$7C,$7C
	DB $FF,$FF,$FF,$FF,$F9,$FF,$F0,$FF,$01,$3E,$20,$3F,$10,$1F,$0F,$0F
	DB $80,$F0,$00,$E0,$00,$00,$00,$00,$C0,$00,$40,$C0,$40,$C0,$80,$80
	DB $CE,$F1,$5F,$E0,$3F,$C0,$37,$CC,$3E,$4D,$1C,$63,$01,$7F,$07,$3F
	DB $1C,$E3,$3E,$C1,$3F,$C0,$77,$EC,$0F,$FC,$C6,$39,$E4,$1F,$64,$9E
	DB $CE,$F1,$57,$EC,$3F,$CC,$3F,$C0,$3E,$41,$1C,$63,$01,$7F,$07,$3F
	DB $1C,$E3,$36,$CD,$3F,$CC,$7F,$E0,$0F,$F0,$C6,$39,$E4,$1F,$64,$9E
	DB $CE,$F1,$5F,$E0,$3B,$C6,$3F,$C6,$3E,$41,$1C,$63,$01,$7F,$07,$3F
	DB $1C,$E3,$3E,$C1,$3B,$C6,$7F,$E6,$0F,$F0,$C6,$39,$E4,$1F,$64,$9E
	DB $CE,$F1,$5F,$E0,$2F,$D8,$3F,$D8,$3E,$41,$1C,$63,$01,$7F,$07,$3F
	DB $1C,$E3,$3E,$C1,$2F,$D8,$7F,$F8,$0F,$F0,$C6,$39,$E4,$1F,$64,$9E
menu_sprite_tile_data_end::

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;					Font (1bpp / Black & White (Inverted))										
BitmapFont::
DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
DB $EF,$E7,$E7,$E7,$E7,$FF,$E7,$FF
DB $D7,$93,$D7,$FF,$FF,$FF,$FF,$FF
DB $FF,$D7,$83,$D7,$83,$D7,$FF,$FF
DB $E7,$C1,$B7,$C3,$ED,$83,$E7,$FF
DB $FD,$3B,$37,$EF,$DF,$B9,$79,$FF
DB $EF,$D7,$D7,$8D,$6B,$73,$8D,$FF
DB $F3,$E3,$CF,$FF,$FF,$FF,$FF,$FF
DB $E7,$E7,$CF,$CF,$CF,$E7,$E7,$FF
DB $E7,$E7,$F3,$F3,$F3,$E7,$E7,$FF
DB $F7,$B6,$D5,$E3,$EB,$DD,$BE,$FF
DB $FF,$E7,$E7,$81,$E7,$E7,$FF,$FF
DB $FF,$FF,$FF,$FF,$FF,$E7,$E7,$CF
DB $FF,$FF,$FF,$81,$81,$FF,$FF,$FF
DB $FF,$FF,$FF,$FF,$FF,$E7,$E7,$FF
DB $FD,$FB,$F7,$EF,$DF,$BF,$7F,$FF
DB $83,$39,$29,$29,$29,$39,$83,$FF
DB $EF,$E7,$E7,$E7,$E7,$E7,$F7,$FF
DB $C3,$81,$F9,$C3,$9F,$81,$C3,$FF
DB $C3,$81,$F9,$E3,$F9,$81,$C3,$FF
DB $E7,$C3,$9B,$33,$83,$F3,$F7,$FF
DB $C3,$81,$9F,$83,$F9,$81,$C1,$FF
DB $C3,$81,$9F,$83,$99,$99,$C3,$FF
DB $C3,$81,$F9,$F3,$E7,$E7,$EF,$FF
DB $C3,$99,$99,$C3,$99,$99,$C3,$FF
DB $C3,$99,$99,$C1,$F9,$81,$C3,$FF
DB $FF,$FF,$E7,$E7,$FF,$E7,$E7,$FF
DB $FF,$FF,$E7,$E7,$FF,$E7,$E7,$CF
DB $F3,$E3,$C7,$9F,$C7,$E3,$F3,$FF
DB $FF,$FF,$81,$FF,$FF,$81,$FF,$FF
DB $9F,$8F,$C7,$F3,$C7,$8F,$9F,$FF
DB $C3,$89,$F9,$E3,$FF,$E7,$E7,$FF
DB $83,$31,$59,$49,$39,$0F,$83,$FF
DB $E7,$C3,$99,$99,$81,$99,$DB,$FF
DB $C3,$99,$99,$83,$99,$99,$C3,$FF
DB $C7,$83,$3F,$3F,$3F,$83,$C7,$FF
DB $C3,$9B,$99,$99,$99,$9B,$C7,$FF
DB $C3,$81,$9F,$87,$9F,$81,$C3,$FF
DB $C7,$83,$9F,$87,$9F,$9F,$DF,$FF
DB $C3,$99,$3F,$3F,$33,$99,$C3,$FF
DB $DB,$99,$99,$81,$99,$99,$DB,$FF
DB $C3,$E7,$E7,$E7,$E7,$E7,$C3,$FF
DB $F7,$F3,$F3,$F3,$B3,$03,$87,$FF
DB $DB,$99,$93,$87,$93,$99,$DB,$FF
DB $DF,$9F,$9F,$9F,$9F,$81,$C1,$FF
DB $BB,$11,$01,$29,$29,$29,$BB,$FF
DB $BB,$19,$09,$21,$31,$39,$BB,$FF
DB $C7,$93,$39,$39,$39,$93,$C7,$FF
DB $C7,$93,$9B,$83,$9F,$9F,$DF,$FF
DB $C7,$93,$39,$39,$35,$8B,$C5,$FF
DB $C3,$99,$99,$83,$93,$99,$D9,$FF
DB $C3,$81,$9F,$C3,$F9,$81,$C3,$FF
DB $C3,$81,$E7,$E7,$E7,$E7,$F7,$FF
DB $DB,$99,$99,$99,$99,$99,$C3,$FF
DB $DB,$99,$99,$99,$99,$C3,$E7,$FF
DB $BB,$39,$29,$29,$01,$11,$BB,$FF
DB $39,$93,$C7,$C7,$93,$39,$BB,$FF
DB $DB,$99,$99,$C3,$E7,$E7,$F7,$FF
DB $83,$03,$F3,$E7,$CF,$81,$83,$FF
DB $E3,$CF,$CF,$CF,$CF,$CF,$E3,$FF
DB $7F,$BF,$DF,$EF,$F7,$FB,$FD,$FF
DB $C7,$F3,$F3,$F3,$F3,$F3,$C7,$FF
DB $E7,$E7,$E7,$E7,$81,$81,$E7,$E7
DB $E7,$E7,$E7,$E7,$C3,$C3,$E7,$E7
BitmapFontEnd::
