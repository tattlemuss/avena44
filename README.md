This is a dump of the code to build "44" by Avena, a 4k intro for the Atari ST.

Under a Unix-like shell (I use Windows Git's bash shell), use ./build.sh to build the code.

Build requirements:

- [vasm](http://sun.hasenbraten.de/vasm/) as the assembler, specifically a build of "vasmm68k_mot"
- [vlink](http://sun.hasenbraten.de/vlink/) as the linker
- [m4](https://www.gnu.org/software/m4/m4.html) as a macro processor

Variants of these are available on Windows.

DEBUG.PRG and PROFILE.PRG are builds with debug information and debug keypresses to change
speed, palette, effects etc. FINAL_*.PRG are the release builds.

The script ./shrink.sh is used to pack the results, then calls py/shrink.py to wrap the output into
a TOS executable.

For more information about the code, see the notes on http://www.clarets.org/steve where there should be a
blogpost at some point. This is a raw dump of the final code, so there are many pointless vestiges of
failed paths and inefficiences left in the files. Commenting is sporadic and often misleading.

All errors are my own.

Steve
March 2018
