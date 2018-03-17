#!/usr/bin/env sh

# Create the bootstrap code
rm -f shrink.dat
vasmm68k_mot -Fbin shrink.s -o shrink.dat

# Run the munger
python py/shrink.py FINAL_50.PRG 44_50HZ.TOS
python py/shrink.py FINAL_60.PRG 44_60HZ.TOS



