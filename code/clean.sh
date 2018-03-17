#!/usr/bin/env sh
# Clean all temp files
rm *.prg
rm *.PRG
rm *.o
rm *.lst
rm generated/*
rm obj/*
rm scripts/*.bin
mkdir generated
mkdir obj
set -e
