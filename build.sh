rgbasm -olcd.o src/core/lcd.asm

rgbasm -omemory.o src/core/memory.asm

rgbasm -omisc.o src/core/misc.asm

rgbasm -opalette.o src/core/palette.asm

rgbasm -ojoypad.o src/core/joypad.asm

rgbasm -omenu.o src/game/menu.asm

rgbasm -osave.o src/game/save.asm 

rgbasm -oplayer.o src/game/player.asm 

rgbasm -osprite_anims.o src/game/sprite_anims.asm

rgbasm -ogame.o src/game/game.asm

# Assemble the main file into an object
rgbasm -omain.o src/main.asm

# Link the objects together and run rgbfix
rgblink -oiawb.gb -noutput.sym main.o game.o sprite_anims.o player.o save.o menu.o joypad.o palette.o misc.o memory.o lcd.o
rgbfix -v -p0 -c iawb.gb
