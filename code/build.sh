#!/usr/bin/env sh
build_vasm()
{
	OPTS=$2
	
	# Preprocess code to a temp file
	m4 -P $1.s > $1.tmp
	# Build from temp file
	vasmm68k_mot $OPTS -spaces -Felf $1.tmp -o obj/$1.o
}

build_all()
{
	OPTS=$1
	build_vasm main "$OPTS"
}

# Clean all temp files
sh ./clean.sh

# Now stop on error!
set -e

# asm

OBJS="obj/main.o"
# link

# debugging build
build_all "-DSMALL_MUSIC=1 -DSNDH_MUSIC=0 -DDEBUG=1 -DKEYBOARD=1 -DSET_50HZ=1"
vlink $OBJS -b ataritos -o DEBUG.PRG

# final build but with symbols
build_all "-DSMALL_MUSIC=1 -DSNDH_MUSIC=0 -DDEBUG=0 -DKEYBOARD=1 -DSET_50HZ=1"
vlink $OBJS -b ataritos -o PROFILE.PRG

# Final build
build_all "-DSMALL_MUSIC=1 -DSNDH_MUSIC=0 -DDEBUG=0 -DKEYBOARD=0 -DSET_50HZ=1 -opt-allbra"
vlink $OBJS -b ataritos -s -o FINAL_50.PRG

build_all "-DSMALL_MUSIC=1 -DSNDH_MUSIC=0 -DDEBUG=0 -DKEYBOARD=0 -DSET_50HZ=0 -opt-allbra"
vlink $OBJS -b ataritos -s -o FINAL_60.PRG
