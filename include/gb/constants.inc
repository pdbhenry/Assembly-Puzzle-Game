;GB Constants
;----------------------------------------------------------------------------

OAMRAM      			EQU    	$FE00 	; $FE00->$FE9F
rIF 					EQU    	$FF0F	; Interrupt Flag
LCD_CTRL                EQU    	$FF40
rSTAT	                EQU    	$FF41
LCD_SCROLL_Y            EQU    	$FF42
LCD_SCROLL_X            EQU    	$FF43
LCD_LINE_Y              EQU    	$FF44
rLYC		            EQU    	$FF45	; LY Compare (R/W)
OAM_DMA_TRANS           EQU    	$FF46
LCD_BG_PAL              EQU    	$FF47
OBJ0_PAL                EQU    	$FF48
OBJ1_PAL                EQU    	$FF49
WIN_Y                   EQU    	$FF4A
WIN_X                   EQU    	$FF4B
VBANK					EQU	   	$FF4F
BCPS					EQU    	$FF68
OCPS					EQU	   	$FF6A
rIE						EQU	   	$FFFF	; Interrupt Enable
LCD_WIDTH               EQU    	160
LCD_HEIGHT              EQU    	144
rROMB0 					EQU 	$2000
VRAM_TILES_SPRITE       EQU    	$8000
VRAM_TILES_BACKGROUND_2 EQU		$8800	; The next block of tiles. Tile #s start at $80
VRAM_TILES_BACKGROUND   EQU    	$9000
VRAM_MAP_CHR            EQU    	$9800
VRAM_MAP_BG             EQU    	$9C00
VRAM_MAP_BLOCK0_SIZE    EQU    	$0800
SRAM_START				EQU	   	$A000 
USER_IO                 EQU    	$FF00
DIVIDER					EQU	   	$FF04
NO_INPUT_P14            EQU    	$EF      ; LEFT, RIGHT, UP, DOWN
NO_INPUT_P15            EQU    	$DF      ; A, B, SELECT, START
BUTTON_RIGHT            EQU    	$0
BUTTON_LEFT             EQU    	$1
BUTTON_UP               EQU    	$2
BUTTON_DOWN             EQU    	$3
BUTTON_A                EQU    	$0
BUTTON_B                EQU    	$1
BUTTON_SELECT           EQU    	$2
BUTTON_START            EQU	   	$3


 
rAUDVOL					EQU    	$FF24
rAUDTERM				EQU    	$FF25
rAUDENA					EQU    	$FF26	


rAUD1SWEEP 				EQU 	$FF10
rAUD1LEN			 	EQU 	$FF11
rAUD1ENV 				EQU	   	$FF12
rAUD1LOW 				EQU 	$FF13
rAUD1HIGH 				EQU 	$FF14

rAUD2LEN 				EQU 	$FF16
rAUD2ENV 				EQU 	$FF17
rAUD2LOW 				EQU 	$FF18
rAUD2HIGH 				EQU 	$FF19

rAUD3ENA				EQU    	$FF1A
rAUD3LEN				EQU    	$FF1B
rAUD3LEVEL				EQU    	$FF1C
rAUD3LOW				EQU    	$FF1D
rAUD3HIGH				EQU    	$FF1E

rAUD4LEN				EQU    	$FF20
rAUD4ENV				EQU    	$FF21
rAUD4POLY				EQU    	$FF22
rAUD4GO					EQU    	$FF23

AUD3WAVERAM 			EQU 	$FF30

STATF_LYC     			EQU		%01000000 ; LYC=LY Coincidence (Selectable)
STATF_BUSY			    EQU  	%00000010 ; When set, VRAM access is unsafe
IEF_TIMER  				EQU 	%00000100 ; Timer Overflow
IEF_LCDC   				EQU 	%00000010 ; LCDC (see STAT)
IEF_VBLANK 				EQU 	%00000001 ; V-Blank



;GAME CONSTANTS
PASSABLE_TILE 				equ $1E			;Tiles 0-1D are passable.
TRUE_PASSABLE_TILE			equ $16			;Tiles 0-15 are true-passable (can't be pushed)
WOOD_TILE					equ $1E			;The default wood's top-left tile
IMPASSABLE_TILE				equ $29			
FINISH_TILE 				equ 6
BLANK_BG_TILE 				equ 2			;Tile number of default bg wood tile
LOOSE_TILE					equ $2D
CW_TURN_TILE				equ $0A
OPP_TURN_TILE				equ $0E
SLIME_TILE					equ $12
CW_TURN_SOLID_TILE			equ $16
OPP_TURN_SOLID_TILE			equ $1A
SOIL_TILE					equ $31
RAMP_TILE_MIN				equ $35			;The val of the first top-left ramp tile number
RAMP_TILE_MAX				equ $42			;One above the last top-left ramp tile number
RAMP_BR_TILE				equ $35
RAMP_BL_TILE				equ $39
RAMP_TR_TILE				equ $3D
RAMP_TL_TILE				equ $41
FRAME_TILE					equ $55

TURNING_DELAY				equ 6			;Turning should last 6 calls to PauseBegin
ICE_SLIDE_VARS				equ 10			;# of vars needed for each line of ice sliding
ICE_MEM_START				equ $C204
ICE_MEM_START_LO			equ $04
SLIME_BLOCK					equ $6
PASSABLE_BLOCK 				equ $10			;Blocks 0-F are passable.
SOIL_BLOCK					equ $1E
WOOD_BLOCK					equ $10
TRUE_PASSABLE_BLOCK			equ $0E			;Blocks 0-D are true-passable (can't be pushed)

DEFAULT_PAL					equ 0
WOOD_PAL					equ 1
SOIL_PAL					equ 2
STEEL_PAL					equ 3
ICE_PAL						equ 4
HOT_PAL						equ 5
SLIME_PAL					equ 6

BLANK_BG_16 				equ 0
GOAL_16						equ 3
CW_TURNER_16 				equ 4
OPP_TURNER_16 				equ 5
CW_TURNER_BLOCK_16 			equ $E
OPP_TURNER_BLOCK_16 		equ $F
IMPASSABLE_BLOCK_16 		equ $1A
LOOSE_BLOCK_16 				equ $1B
SOIL_BLOCK_16 				equ $1F

UP_PUSH						equ $C0
DOWN_PUSH					equ $40
RIGHT_PUSH					equ $02
LEFT_PUSH					equ $FE
menu_tile_data_size EQU $06B0

SIR_TL						equ $08
SIR_TR						equ $09
SIR_BL						equ $0A
SIR_BR						equ $0B
SIR_WALK_BL					equ $0C
SIR_WALK_BR					equ $0D
SIR_SIDE_TL					equ $0E
SIR_SIDE_TR					equ $0F
SIR_SIDE_BL					equ $10
SIR_SIDE_BR					equ $11
SIR_SIDE_WALK_TR			equ $12
SIR_SIDE_WALK_BR			equ $13
SIR_TURN_TL					equ $14
SIR_TURN_TR					equ $15
SIR_TURN_BL					equ $16
SIR_TURN_BR					equ $17
SIR_SIDE_TURN_TL			equ $18
SIR_SIDE_TURN_TR			equ $19
SIR_SIDE_TURN_BL			equ $1A
SIR_SIDE_TURN_BR			equ $1B
SIR_CAST_TL					equ $1C
SIR_CAST_TR					equ $1D
SIR_CAST_BL					equ $1E
SIR_CAST_BR					equ $1F
SIR_SIDE_CAST_TL			equ $20
SIR_SIDE_CAST_TR			equ $21
SIR_SIDE_CAST_BL			equ $22
SIR_SIDE_CAST_BR			equ $23
SIR_EARTH_TL				equ $24
SIR_EARTH_TR				equ $25
SIR_EARTH_BL				equ $26
SIR_EARTH_BR				equ $27
SIR_SIDE_EARTH_TL			equ $28
SIR_SIDE_EARTH_TR			equ $29
SIR_SIDE_EARTH_BL			equ $2A
SIR_SIDE_EARTH_BR			equ $2B

SOIL_ANIM_TILE				equ $00		;First tile val of the four soil anim sprites
SOIL_SPR_PAL				equ $03

;FRAME_TILE					equ $04

FROST_ANIM_TILE				equ $2C
FROST_SPR_PAL				equ $01

FLAME_ANIM_TILE				equ $30
FLAME_SPR_PAL				equ $02

EARTH_ANIM_TILE				equ $34
EARTH_SPR_PAL				equ $02
