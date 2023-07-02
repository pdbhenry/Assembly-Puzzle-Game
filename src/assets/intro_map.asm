SECTION "intro map", ROMX

intro_0_mini_map_data::
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0 
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0 
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0 
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0 
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0 
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0 
	DB $10,$0, $0, $0, $0, $0, $0, $0, $0, $0 
	DB $10,$0, $0, $0, $0, $0, $0, $0, $0, $0 
	DB $1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F
	
intro_1_mini_map_data::
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0 
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0 
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0 
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0 
	DB $1F,$1F,$1F,$0, $0, $0, $0, $0, $0, $0 
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0 
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0 
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0 
	DB $1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F

intro_2_mini_map_data::
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0 
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0 
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0 
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0 
	DB $0, $0, $0, $1F,$1F,$1F,$0, $0, $0, $0 
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0 
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0 
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0 
	DB $1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F

intro_3_mini_map_data::
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0 
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0 
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0 
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0 
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $0 
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $10
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $10
	DB $0, $0, $0, $0, $0, $0, $0, $0, $0, $10 
	DB $1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F

intro_map_addrs::
	DW intro_0_mini_map_data, intro_1_mini_map_data, intro_2_mini_map_data, intro_3_mini_map_data
	
	
intro_dialogue_0::
	DB "THIS IS A WALL. REAL BIG ONE TOO! WHAT ELSE IS THERE TO SAY? I CAN GO ON AND ON.",255
	
intro_dialogue_3::
	DB "WHY HELLO, YOUNG SIRLOIN!",255
	
intro_map_npc_pos::
	DW intro_map_npc_0, intro_map_npc_1, intro_map_npc_2, intro_map_npc_3
	
;1st byte tells us x and y pos of npc (high and low nibbles). 
;2nd and 3rd bytes tell us screen x and y pos of prompt.
intro_map_npc_0:
	DB $07
	DW intro_dialogue_0
	DB $FF
intro_map_npc_1:
	DB $FF
intro_map_npc_2:
	DB $FF
intro_map_npc_3:
	DB $87
	DW intro_dialogue_3
	DB $FF