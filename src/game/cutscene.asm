SECTION "cutscene_vars", WRAM0

cutscene_id: DB
cutscene_palettes:: DW

SECTION "cutscene", ROM0

update_cutscene:







fade_in:
	ld a, [cutscene_palettes]
	ld d, a
	ld a, [cutscene_palettes+1]
	ld e, a
	
.loop								;call fade_in on all palettes listed under cutscene_palettes
	ld a, [de]
	cp 255
	jr z, .loop_end
	ld l, a
	
	;inc de							;Uncomment later
	;ldi a, [de]
	ld h, a
	call fade_in	
	jr .loop
	
.loop_end
	ld a, [fade_step]
	inc a
	ld [fade_step], a
	
	
	
end_frame:



load_screen:
	;ld 
	
	
	