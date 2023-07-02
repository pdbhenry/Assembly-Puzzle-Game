;SONG		#	BANK
;Eerie 		0 	2
;Suit Up 	1	2	

SECTION "music_data", ROM0

;Numbers our songs. Eerie is Song #0, Suit Up is #1
song_order::
	DW eerie_song, suit_up
	
;Tells us what bank we'll find the song data in. Eerie and Suit Up are in Bank #2
song_banks::
	DB 2, 2