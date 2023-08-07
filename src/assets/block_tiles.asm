;Tells us the tile numbers of each 16x16 block in the order of
;Top-left, top-right, bottom-left, bottom-right
SECTION "block tiles", ROMX

block_00::
DB $0,$0,$2,$2
block_01:
DB $0,$1,$2,$3
block_02:
DB $1,$4,$3,$5	
block_03:
DB $6,$7,$8,$9


block_04:
DB $A,$B,$C,$D	
block_05:
DB $E,$F,$10,$11
block_06:
DB $12,$13,$14,$15	
;Placeholder BG Blocks
block_07:
DB $E,$F,$10,$11
block_08:
DB $A,$B,$C,$D	
block_09:
DB $E,$F,$10,$11
block_0A:
DB $A,$B,$C,$D	
block_0B:
DB $E,$F,$10,$11
block_0C:
DB $A,$B,$C,$D	
block_0D:
DB $E,$F,$10,$11
;

block_0E:
DB $16,$17,$18,$19
block_0F:
DB $1A,$1B,$1C,$1D
block_10::
DB $1E,$1F,$20,$21
block_11:
DB $22,$23,$20,$21
block_12:
DB $1E,$1F,$20,$24
block_13:
DB $25,$1F,$26,$21
block_14:
DB $22,$23,$20,$24
block_15:
DB $27,$23,$26,$21
block_16:
DB $25,$1F,$26,$24
block_17:
DB $27,$23,$26,$24
block_18:
DB $28,$1F,$20,$21
block_19:
DB $28,$1F,$20,$24
block_1A:
DB $29,$2A,$2B,$2C
block_1B:
DB $2D,$2E,$2F,$30
block_1C:
DB $1E,$1F,$20,$21
block_1D:
DB $29,$2A,$2B,$2C
block_1E:
DB $31,$32,$33,$34
block_1F:
DB $31,$32,$33,$34
block_20:
DB $29,$2A,$2B,$2C
block_21:
DB $1E,$1F,$20,$21
block_22:
DB $29,$2A,$2B,$2C
block_23:
DB $31,$32,$33,$34
block_24:
DB $35,$36,$37,$38
block_25:
DB $39,$3A,$3B,$3C
block_26:
DB $3D,$3E,$3F,$40
block_27:
DB $41,$42,$43,$44
block_28:
DB $55,$56,$57,$58
	
blocks::
	DW block_00, block_01, block_02, block_03, block_04, block_05, block_06
	DW block_07, block_08, block_09, block_0A, block_0B, block_0C, block_0D
	DW block_0E, block_0F, block_10, block_11, block_12, block_13, block_14
	DW block_15, block_16, block_17, block_18, block_19, block_1A, block_1B
	DW block_1C, block_1D, block_1E, block_1F, block_20, block_21, block_22
	DW block_23, block_24, block_25, block_26, block_27, block_28