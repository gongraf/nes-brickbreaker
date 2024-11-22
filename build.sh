#!/bin/bash
ca65 brkout.asm -o brkout.o --debug-info
ld65 brkout.o -o brkout.nes -t nes --dbgfile brkout.dbg

rm  *.o
