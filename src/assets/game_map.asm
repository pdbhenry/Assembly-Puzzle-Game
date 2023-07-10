;0 - Blank bg						1 - Bg right end-board
;2 - Bg two boards meet in middle 	3 - Goal
;4 - CW Turner						5 - Opposite Turner
;6 - Slime bg						7 - Bg_Placeholder_2
;8 - Bg_Placeholder_3				9 - Bg_Placeholder_4
;A - Bg_Placeholder_5				B - Bg_Placeholder_6
;C - Bg_Placeholder_7				D - Bg_Placeholder_8
;E - CW Turner Block				F - Opposite Turner Block	
;10 - Block default					11 - Block top cover
;12 - Block bottom cover			13 - Block left cover
;14 - Block top/bottom cover		15 - Block top/left cover
;16 - Block bottom/left cover		17 - Block all sides cover
;18 - Block notch					19 - Block notch + bottom cover
;1A - Impassable Block				1B - Loose Block
;1C - Ice Wood Block				1D - Ice "Impassable" Block
;1E - Ice Soil Block				1F - Soil Block
;20 - Red Hot Impassable Block		21 - Slime Wood
;22 - Slime Steel Block				23 - Slime Soil
;24 - Ramp BR						25 - Ramp BL
;26 - Ramp TR						27 - Ramp TL

SECTION "game map", ROMX

level_0_mini_map_data:
	DB $10,$10,$10,$10,$10,$10,$10,$10,$10,$10
	DB $10,$10,$10,$10,$10,$10,$10,$10,$10,$10
	DB $10,$10,$10,$10,$10,$10,$10,$10,$10,$10
	DB $10,$10,$10,$0, $0, $0, $3, $10,$10,$10
	DB $10,$10,$10,$0, $0, $0, $0, $10,$10,$10
	DB $10,$10,$10,$0, $0, $0, $0, $10,$10,$10
	DB $10,$10,$10,$1F,$1F,$1F,$1F,$10,$10,$10
	DB $10,$10,$10,$10,$10,$10,$10,$10,$10,$10
	DB $10,$10,$10,$10,$10,$10,$10,$10,$10,$10

level_1_mini_map_data:
	DB $10,$10,$10,$10,$10,$10,$10,$10,$10,$10
	DB $0, $0, $0, $0, $0, $0, $0, $10,$10,$10
	DB $0, $1, $0, $0, $0, $0, $0, $10,$10,$10
	DB $0, $0, $0, $0, $2, $0, $10,$10,$10,$10
	DB $0, $0, $0, $0, $0, $0, $3, $13,$10,$10
	DB $0, $0, $10,$0, $0, $0, $10,$10,$10,$10
	DB $10,$10,$10,$0, $0, $2, $0, $10,$10,$10
	DB $0, $0, $1, $0, $0, $0, $0, $10,$10,$10
	DB $0, $0, $0, $0, $0, $0, $0, $10,$10,$10
	
level_2_mini_map_data:
	DB $0, $0, $1, $0, $0, $0, $0, $0, $0, $0
	DB $0, $0, $0, $0, $0, $0, $0, $2, $0, $0
	DB $0, $0, $10,$0, $10,$10,$0, $0, $0, $0
	DB $10,$10,$10,$0, $10,$0, $0, $10,$0, $0
	DB $0, $0, $0, $0, $0, $0, $10,$10,$0, $0
	DB $0, $0, $0, $15,$0, $0, $0, $0, $0, $0
	DB $10,$10,$10,$10,$0, $0, $2, $0, $0, $0
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0
	DB $0, $1, $0, $0, $10,$3, $10,$10,$0, $0 
	
level_3_mini_map_data:
	DB $10,$10,$0, $13,$10,$10,$10,$10,$10,$10
	DB $10,$10,$11,$18,$10,$10,$10,$10,$10,$10
	DB $10,$10,$10,$10,$10,$10,$10,$10,$10,$10
	DB $10,$10,$10,$10,$10,$10,$10,$10,$10,$10
	DB $10,$10,$1F,$10,$10,$10,$10,$10,$10,$10
	DB $10,$10,$10,$10,$10,$10,$10,$10,$10,$10
	DB $10,$10,$10,$3, $10,$10,$10,$10,$10,$10
	DB $12,$12,$12,$12,$12,$12,$0, $0, $13,$10
	DB $0, $0, $0, $0, $1, $0, $0, $0, $13,$10
	
level_4_mini_map_data:
	DB $10,$10,$10,$10,$10,$10,$10,$10,$10,$10
	DB $10,$10,$10,$10,$10,$10,$10,$10,$10,$10
	DB $10,$10,$10,$10,$10,$0, $0, $0, $10,$10
	DB $0, $0, $0, $0, $0, $0, $0, $0, $10,$10
	DB $0, $0, $0, $0, $0, $3, $0, $0, $10,$10
	DB $0, $0, $0, $0, $0, $0, $0, $10,$10,$10
	DB $0, $0, $0, $10,$0, $0, $0, $10,$10,$10
	DB $10,$10,$0, $10,$0, $0, $0, $10,$10,$10
	DB $0, $0, $0, $0, $0, $0, $0, $10,$10,$10
	
level_5_mini_map_data:
	DB $10,$10,$10,$10,$10,$10,$10,$10,$10,$10
	DB $10,$10,$10,$10,$10,$10,$10,$10,$10,$10
	DB $12,$12,$12,$12,$12,$12,$12,$12,$12,$12
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $10
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $10
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $10
	DB $0, $10,$10,$10,$10,$10,$3, $0, $0, $10 
	DB $1A,$10,$10,$10,$10,$10,$1A,$0, $0, $10
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $10
	
level_6_mini_map_data:
	DB $10,$10,$10,$10,$10,$10,$10,$10,$10,$10
	DB $0, $0, $0, $0, $0, $10,$10,$10,$10,$10
	DB $0, $10,$10,$10,$0, $10,$10,$10,$10,$10
	DB $0, $10,$0, $10,$0, $10,$0, $0, $0, $0
	DB $0, $10,$0, $10,$0, $0, $0, $0, $0, $0
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0
	DB $10,$10,$10,$10,$0, $0, $0, $3, $0, $0
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0
	DB $10,$10,$0, $0, $0, $0, $0, $0, $0, $0
 
;With Ice power
level_7_mini_map_data:
	DB $0, $0, $0, $0, $0, $0, $0, $10,$0, $0
	DB $10,$10,$0, $0, $10,$0, $0, $10,$0, $0
	DB $3, $0, $0, $0, $10,$0, $10,$10,$0, $0
	DB $10,$10,$0, $0, $10,$0, $10,$0, $0, $0
	DB $0, $0, $0, $0, $10,$0, $10,$0, $0, $0
	DB $0, $0, $0, $0, $10,$0, $10,$0, $0, $0
	DB $10,$10,$10,$10,$10,$0,$10,$10, $10,$10
	DB $0, $0, $10,$10,$10,$10,$10,$10,$10,$0
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0
	
level_8_mini_map_data:
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0
	DB $1A,$1A,$1A,$1A,$1A,$1A,$0, $0, $0, $0
	DB $0, $0, $0, $0, $0, $1A,$0, $0, $0, $0
	DB $0, $10,$10,$10,$0, $1A,$0, $0, $0, $0
	DB $0, $10,$3, $10,$0, $1A,$0, $0, $0, $0
	DB $0, $10,$10,$10,$0, $1A,$0, $0, $0, $0
	DB $0, $0, $0, $0, $0, $1A,$0, $0, $0, $0
	
level_9_mini_map_data:
	DB $10,$10,$0, $10,$10,$1F,$0, $0, $1F,$1F
	DB $10,$10,$0, $10,$1F,$0, $0, $0, $1F,$1F
	DB $10,$1F,$0, $0, $10,$0, $0, $0, $0, $1F
	DB $1F,$1F,$0, $10,$10,$0, $0, $0, $0, $1F
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0
	DB $10,$1F,$0, $10,$10,$0, $0, $0, $0, $0
	DB $10,$10,$0, $0, $10,$0, $0, $3, $0, $0
	DB $10,$10,$0, $0, $10,$0, $0, $0, $0, $0
	DB $10,$10,$0 ,$0, $10,$1F,$0, $0, $0, $1F
	
;With Ice power
level_10_mini_map_data:
	DB $10,$10,$0, $0, $0, $0, $10,$3, $10,$0
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0
	DB $0, $0, $0, $0, $0, $0, $10,$10,$10,$0
	DB $10,$0, $10,$10,$0, $10,$10,$10,$10,$10
	DB $10,$0, $10,$0, $0, $10,$0, $10,$10,$10
	DB $10,$0, $10,$0, $0, $10,$0, $10,$10,$10
	DB $0, $0, $0, $0, $0, $10,$10,$10,$10,$10
	DB $10,$0, $0, $10,$10,$10,$10,$10,$10,$10
	DB $10,$10,$10,$10,$10,$10,$10,$10,$10,$10
	
level_11_mini_map_data:
	DB $21,$22,$0, $0, $0, $0, $0, $0, $0, $10
	DB $21,$0, $6, $0, $1, $0, $0, $0, $0, $10
	DB $0, $1, $0, $0, $0, $1, $0, $1F,$2, $21
	DB $0, $0, $2, $0, $10,$0, $0, $1A,$0, $1A
	DB $0, $0, $6, $0, $0, $1C,$1F,$0, $0, $20
	DB $0, $0, $0, $0, $0, $0, $0, $6, $2, $1A
	DB $10,$0, $0, $0, $10,$1C,$0, $1F,$1E,$1F
	DB $0, $1A,$1C,$1C,$1C,$1C,$0, $10,$0, $10
	DB $2, $2, $2 ,$2, $2, $2, $1C,$0, $0 ,$21
	
level_12_mini_map_data:
	DB $0, $10,$0, $0, $0, $0, $0, $0, $0, $0
	DB $0, $10,$1C,$0, $0, $0, $0, $0, $0, $0
	DB $0, $10,$1C,$0, $0, $0, $0, $0, $0, $0
	DB $0, $10,$1C,$0, $0, $0, $0, $0, $0, $0
	DB $0, $10,$1C,$0, $0, $0, $3, $0, $0, $0
	DB $0, $10,$1C,$0, $0, $0, $0, $0, $0, $0
	DB $0, $10,$1C,$0, $0, $0, $0, $0, $0, $0
	DB $0, $10,$10,$0, $0, $0, $0, $0, $0, $0
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0
	
level_13_mini_map_data:
	DB $0, $10,$1C,$0, $0, $10,$3, $10,$0, $0
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0
	DB $0, $10,$10,$0, $0, $0, $0, $0, $0, $0
	DB $0, $0, $0, $0, $1A,$0, $0, $0, $0, $0
	DB $1A,$1A,$1A,$1A,$1A,$0, $0, $0, $0, $0
	DB $0, $0, $0, $0, $1A,$0, $0, $0, $0, $0
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0
	
level_14_mini_map_data:
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0
	DB $0, $0, $0, $0, $10,$10,$0, $0, $0, $0
	DB $0, $0, $10,$10,$10,$10,$10,$0, $0, $0
	DB $0, $0, $10,$0, $0, $0, $0, $10,$10,$0
	DB $0, $0, $10,$0, $1A,$0, $1C,$0, $0, $0
	DB $0, $0, $10,$0, $10,$0, $0, $0, $0, $0
	DB $0, $0, $0, $0, $0, $10,$10,$10,$0, $0
	DB $0, $0, $0, $0, $0, $0, $10,$10,$0, $0
	DB $0, $0, $0, $0, $0, $0, $10,$0, $0, $0
	
level_15_mini_map_data:
	DB $0, $10,$0, $0, $0, $0, $0, $0, $0, $0
	DB $0, $10,$1C,$0, $0, $0, $0, $0, $0, $0
	DB $0, $10,$1C,$0, $0, $0, $0, $0, $0, $0
	DB $0, $10,$1C,$0, $0, $0, $0, $0, $0, $0
	DB $0, $10,$1C,$0, $0, $0, $3, $0, $0, $0
	DB $0, $10,$1C,$0, $0, $0, $0, $0, $0, $0
	DB $0, $1C,$1C,$0, $0, $0, $0, $0, $0, $0
	DB $0, $10,$10,$0, $0, $0, $0, $0, $0, $0
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0
	
level_16_mini_map_data:
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0
	DB $0, $10,$10,$10,$10,$10,$10,$10,$0, $0
	DB $0, $10,$27,$0, $0, $0, $26,$10,$0, $0
	DB $0, $10,$0, $1A,$10,$1A,$0, $10,$0, $0
	DB $0, $10,$0, $10,$3, $10,$0, $10,$0, $0
	DB $0, $10,$0, $1A,$10,$1A,$0, $10,$0, $0
	DB $0, $10,$25,$0, $0, $0, $24,$10,$0, $0
	
level_17_mini_map_data:
	DB $0, $0, $0, $0, $0, $0, $0, $0, $26,$10
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0
	DB $10,$10,$25,$0, $0, $0, $0, $0, $0, $0
	DB $10,$10,$0, $25,$0, $0, $0, $0, $0, $0
	DB $10,$10,$0, $24,$0, $0, $0, $0, $0, $0
	DB $10,$10,$10,$10,$10,$10,$25,$0, $0, $0
	DB $10,$10,$10,$10,$10,$10,$10,$0, $0, $0
	
level_18_mini_map_data:
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0
	DB $0, $0, $0, $10,$0, $0, $0, $0, $0, $0
	DB $0, $0, $10,$0, $26,$10,$0, $0, $0, $0
	DB $0, $0, $27,$0, $10,$0, $26,$10,$0, $0
	DB $10,$27,$0, $0, $0, $0, $0, $26,$10,$10
	DB $10,$0, $0, $1C,$0, $0, $0, $0, $0, $10
	DB $27,$0, $0, $1C,$0, $24,$25,$0, $25,$0
	DB $0, $0, $0, $0, $24,$27,$10,$1C,$0, $0
	DB $0, $0, $24,$10,$0, $0, $0, $0, $0, $0
	
level_19_mini_map_data:
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0
	DB $0, $0, $0, $10,$0, $0, $0, $0, $0, $0
	DB $0, $0, $0, $10,$0, $0, $0, $0, $0, $0
	DB $27,$10,$0, $24,$0, $0, $0, $0, $0, $0
	DB $10,$0, $0, $0, $0, $0, $0, $0, $0, $0
	DB $10,$0, $27,$10,$0, $27,$0, $10,$0, $0
	DB $10,$0, $10,$0, $0, $10,$0 ,$10,$0, $0
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0
	
game_tile_data::
	DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
	DB $FE,$FF,$FE,$FF,$FE,$FF,$F6,$FF,$FE,$FF,$FE,$FF,$FE,$FF,$FE,$FF
	DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$FF
	DB $FE,$FF,$FE,$FF,$FE,$FF,$F6,$FF,$FE,$FF,$FE,$FF,$FE,$FF,$00,$FF
	DB $FF,$FF,$FF,$FF,$FF,$FF,$EF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
	DB $FF,$FF,$FF,$FF,$FF,$FF,$EF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$FF
	DB $FF,$FF,$EF,$FF,$FC,$EF,$FB,$F4,$E7,$F8,$EE,$F0,$D4,$E0,$D8,$E0
	DB $FF,$FF,$FF,$FF,$3F,$FF,$CB,$3F,$EF,$17,$57,$0F,$3B,$07,$1B,$07
	DB $D8,$E0,$DC,$E0,$EE,$F0,$EF,$F0,$F3,$EC,$FC,$FF,$FF,$FF,$00,$FF
	DB $1B,$07,$3B,$07,$77,$0F,$A7,$1F,$DF,$2F,$3F,$F7,$F7,$FF,$00,$FF
	DB $FF,$FF,$FF,$FF,$FE,$FE,$FC,$FC,$FE,$F6,$FF,$EF,$EF,$EF,$83,$83
	DB $FF,$FF,$7F,$7F,$7F,$7F,$3F,$1F,$7F,$6F,$7F,$7F,$F7,$F7,$E3,$E3
	DB $C7,$C7,$EF,$EF,$FE,$FE,$FE,$F6,$FC,$F8,$FE,$FE,$FE,$FE,$00,$FF
	DB $C1,$C1,$F7,$F7,$FF,$F7,$7F,$6F,$3F,$3F,$7F,$7F,$FF,$FF,$00,$FF
	DB $FF,$FF,$FE,$FE,$FC,$FC,$F8,$F8,$FE,$FE,$EF,$EE,$CF,$CE,$87,$80
	DB $FF,$FF,$7F,$7F,$3F,$3F,$1F,$1F,$7F,$7F,$F7,$77,$F3,$73,$E1,$01
	DB $87,$80,$CF,$CE,$EF,$EE,$FE,$FE,$F8,$F8,$FC,$FC,$FE,$FE,$00,$FF
	DB $E1,$01,$F3,$73,$F7,$77,$7F,$7F,$1F,$1F,$3F,$3F,$7F,$7F,$00,$FF
	DB $FF,$FF,$FF,$C7,$E7,$87,$E7,$83,$FF,$C2,$FF,$E0,$FF,$E0,$EF,$F0
	DB $FF,$FF,$F9,$F9,$FD,$31,$FF,$01,$8B,$07,$CF,$07,$EF,$07,$EF,$03
	DB $EF,$F0,$FF,$F0,$F7,$E0,$FB,$C1,$DB,$E3,$CF,$F7,$FF,$FF,$00,$FF
	DB $FF,$03,$FF,$03,$FF,$0F,$D3,$23,$F3,$C3,$DF,$E3,$FF,$FF,$00,$FF
	DB $17,$17,$5F,$1F,$3E,$3E,$FC,$FC,$7E,$76,$FF,$EF,$EF,$EF,$83,$83
	DB $E8,$E8,$7A,$78,$7C,$7C,$3F,$1F,$7E,$6E,$7F,$7F,$F7,$F7,$E3,$E3
	DB $C7,$C7,$EF,$EF,$FE,$FE,$7E,$76,$FC,$F8,$3E,$3E,$5E,$1E,$17,$17
	DB $C1,$C1,$F7,$F7,$FF,$F7,$7E,$6E,$3F,$3F,$7C,$7C,$FA,$F8,$E8,$E8
	DB $17,$17,$5E,$1E,$3C,$3C,$F8,$F8,$7E,$7E,$EF,$EE,$CF,$CE,$87,$80
	DB $E8,$E8,$7A,$78,$3C,$3C,$1F,$1F,$7E,$7E,$F7,$77,$F3,$73,$E1,$01
	DB $87,$80,$CF,$CE,$EF,$EE,$7E,$7E,$F8,$F8,$3C,$3C,$5E,$1E,$17,$17
	DB $E1,$01,$F3,$73,$F7,$77,$7E,$7E,$1F,$1F,$3C,$3C,$7A,$78,$E8,$E8
	DB $FF,$01,$BB,$01,$FF,$01,$FF,$01,$FF,$01,$FF,$01,$FF,$01,$FF,$01
	DB $FF,$01,$FD,$03,$FD,$03,$F9,$07,$F1,$0F,$A1,$1B,$81,$7F,$FF,$FF
	DB $FF,$01,$FD,$03,$FD,$03,$F9,$07,$F1,$0F,$A1,$1B,$81,$7F,$FF,$FF
	DB $FF,$01,$B9,$03,$FD,$03,$F9,$07,$F1,$0F,$E1,$1F,$81,$7F,$FF,$FF
	DB $FF,$00,$21,$00,$21,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00
	DB $FF,$00,$01,$00,$01,$00,$01,$00,$09,$00,$09,$00,$09,$00,$01,$00
	DB $01,$00,$FF,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$FF,$00
	DB $A1,$00,$A1,$00,$A1,$00,$81,$00,$81,$00,$81,$00,$81,$00,$81,$00
	DB $81,$00,$89,$00,$89,$00,$89,$00,$89,$00,$81,$00,$81,$00,$FF,$00
	DB $FF,$00,$A1,$00,$A1,$00,$81,$00,$81,$00,$81,$00,$81,$00,$81,$00
	DB $A1,$00,$21,$00,$21,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00
	DB $FF,$FF,$87,$F8,$83,$FC,$99,$FE,$D0,$B7,$E0,$9F,$F0,$8F,$F8,$87
	DB $FF,$FF,$21,$DF,$91,$6F,$D9,$3F,$F5,$13,$73,$8D,$39,$C7,$1D,$E3
	DB $FC,$83,$BE,$C1,$9F,$E0,$9F,$F8,$97,$F0,$83,$FC,$81,$FE,$FF,$FF
	DB $0F,$F1,$07,$F9,$03,$FD,$99,$7F,$D1,$37,$E1,$1F,$F1,$0F,$FF,$FF
	DB $C0,$FF,$80,$FF,$1F,$E0,$3F,$C0,$3F,$C0,$3F,$C0,$3C,$C3,$3D,$C2
	DB $03,$FF,$01,$FF,$F8,$07,$FC,$03,$FC,$03,$FC,$03,$3C,$C3,$BC,$43
	DB $3D,$C2,$3C,$C3,$3F,$C0,$3F,$C0,$3F,$C0,$1F,$E0,$80,$FF,$C0,$FF
	DB $BC,$43,$3C,$C3,$FC,$03,$FC,$03,$FC,$03,$F8,$07,$01,$FF,$03,$FF
	DB $83,$88,$03,$04,$3C,$03,$7E,$00,$98,$44,$B8,$46,$12,$E9,$0E,$F9
	DB $03,$FF,$0D,$C3,$5F,$C1,$7E,$01,$6D,$00,$FD,$02,$F5,$0A,$FE,$01
	DB $5C,$FB,$4A,$87,$06,$81,$7E,$01,$FE,$01,$7D,$82,$B9,$C6,$C6,$F9
	DB $DC,$23,$79,$8C,$07,$F8,$02,$8D,$30,$0F,$F8,$07,$F9,$07,$31,$CF
	DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
	DB $FF,$FF,$FE,$FE,$FC,$FC,$F8,$F8,$F0,$F0,$E0,$E0,$C0,$C0,$80,$80
	DB $FF,$FF,$FE,$FE,$FC,$FC,$F8,$F8,$F0,$F0,$E0,$E0,$C0,$C0,$80,$80
	DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	DB $FF,$FF,$7F,$7F,$3F,$3F,$1F,$1F,$0F,$0F,$07,$07,$03,$03,$01,$01
	DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
	DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	DB $FF,$FF,$7F,$7F,$3F,$3F,$1F,$1F,$0F,$0F,$07,$07,$03,$03,$01,$01
	DB $80,$80,$C0,$C0,$E0,$E0,$F0,$F0,$F8,$F8,$FC,$FC,$FE,$FE,$FF,$FF
	DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
	DB $80,$80,$C0,$C0,$E0,$E0,$F0,$F0,$F8,$F8,$FC,$FC,$FE,$FE,$FF,$FF
	DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	DB $01,$01,$03,$03,$07,$07,$0F,$0F,$1F,$1F,$3F,$3F,$7F,$7F,$FF,$FF
	DB $01,$01,$03,$03,$07,$07,$0F,$0F,$1F,$1F,$3F,$3F,$7F,$7F,$FF,$FF
	DB $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
	DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
	DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00	;Just white, not used as of now

game_tile_data_end::

;Left-most nibble is x position, right-most is y position. Each new byte is for the next level
;x,y pos is based on a 9x8 screen of 16x16 tiles
game_start_pos::
	DB $35,$48,$08,$08,$06,$38,$17,$28,$08,$04,$65,$88,$28,$03,$55,$28,$28,$03,$08,$08

level_powers::
	DB $00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$01,$07,$00,$00,$00,$00,$00,$00,$00,$00

;Details the songs that accompany stretches of levels. From 0-2, its eerie (Song 0). From 3 onward, it's suit up (Song 1)
;Song #, Level #, Song #, Level #, Song #,..., $FF
level_songs::
	DB 0, 3, 1, $FF
	
level_map_addrs::
	DW level_0_mini_map_data, level_1_mini_map_data, level_2_mini_map_data
	DW level_3_mini_map_data, level_4_mini_map_data, level_5_mini_map_data
	DW level_6_mini_map_data, level_7_mini_map_data, level_8_mini_map_data
	DW level_9_mini_map_data, level_10_mini_map_data, level_11_mini_map_data
	DW level_12_mini_map_data, level_13_mini_map_data, level_14_mini_map_data
	DW level_15_mini_map_data, level_16_mini_map_data, level_17_mini_map_data
	DW level_18_mini_map_data, level_19_mini_map_data
	
level_names::
	DW level_0_name, level_1_name, level_2_name, level_3_name
	DW level_4_name, level_5_name, level_6_name, level_7_name
	DW level_8_name, level_9_name, level_10_name, level_11_name
	DW level_12_name, level_13_name, level_14_name, level_15_name
	DW level_16_name, level_17_name, level_18_name, level_19_name, 255, 255
	
level_0_name:
	DB "  A STRANGE BOX   ",255
level_1_name:
	DB " YOUR FIRST FALL  ",255
level_2_name:
	DB "  TURNED AROUND   ",255
level_3_name:
	DB "  USE YOUR HEAD   ",255
level_4_name:
	DB "   FALL > JUMP    ",255
level_5_name:
	DB "TAUNTING PLATFORM ", 255
level_6_name:  
	DB "       FEED       ",255
level_7_name:
	DB "     WITTLING     ",255
level_8_name:
	DB "     HATCHING     ",255
level_9_name:
	DB "      STEPS       ",255
level_10_name:
	DB "      OOOOO       ",255
level_11_name:
	DB "    LOOSE TEST    ",255
level_12_name:
	DB "PRINT             ",255
level_13_name:
	DB "SEQUENCE          ",255
level_14_name:
	DB "FORBIDDEN         ",255
level_15_name:
	DB "PUNCH OUT         ",255
level_16_name:
	DB "MECHANISM         ",255
level_17_name:
	DB "RAMP              ",255
level_18_name:
	DB "RAMP UP           ",255
level_19_name:
	DB "UPRIGHT           ",255