import struct
import os

HEADER_FORMAT = '>HIIIIIIH'
SHRINKLER_CODE = 'shrink.dat'

def read_struct(fh, fmt):
    # Read a binary structure from the file of the given format
    size = struct.calcsize(fmt)
    data = fh.read(size)
    return struct.unpack(fmt, data)

def write_struct(fh, fmt, data):
    # Read a binary structure from the file of the given format
    size = struct.calcsize(fmt)
    packed = struct.pack(fmt, *data)
    fh.write(packed)

def read_data(filename):
    fh = open(filename, 'rb')
    return bytearray(fh.read())

def pack(fh_in, filename_out):
    # Load a TOS-format executable file and create the disassembly context
    # See http://toshyp.atari.org/en/005005.html for TOS header details
    # and https://github.com/libretro/hatari/blob/master/tools/debugger/gst2ascii.c for an example of reading DRI
    (header, textlen, datalen, bsslen, symbollen, reserved1, flags, relocinfo) = read_struct(fh_in, HEADER_FORMAT)
    if header != 0x601a:
        raise Exception('Incorrect PRG file header')
    
    print "Text length %d" % textlen
    print "Data length %d" % datalen
    print " BSS length %d" % bsslen

    # Read the text
    filename_tmp = "FINAL.TMP"
    filename_tmp_packed = "FINAL.TMP" + ".PAK"

    textsec = fh_in.read(textlen)
    fh_out = open(filename_tmp, "wb")
    fh_out.write(textsec)
    fh_out.close()

    # Pack it
    import subprocess
    subprocess.call(['../../Shrinkler/build/native/Shrinkler', '-d', filename_tmp, filename_tmp_packed])

    # New layout

    # HEADER        | PRG header
    # TEXT          | copy of shrink.dat (patched for data out address)
    # TEXT          | packed data 
    # 
    # BSS           | 3K stack             
    # BSS           | unpacked code
    # BSS           | unpacked BSS

    def align(size):
        return (size + 1) & 0xfffffffe
    unpacked_code_size = os.path.getsize(filename_tmp)
    packed_data_size = align(os.path.getsize(filename_tmp_packed))
    shrinkler_header_size = os.path.getsize(SHRINKLER_CODE)

    # Shrinkler needs 3K, plus extra for subroutine stack
    stack_size = 1536 * 2 + 256

    print "shrinkler_header_size %d" % shrinkler_header_size
    print "packed_data_size %d" % packed_data_size
    print "unpacked_code_size %d" % unpacked_code_size

    # Now add the header, code
    header_size = struct.calcsize(HEADER_FORMAT)
    new_text_size = shrinkler_header_size + packed_data_size
    new_bss_size = stack_size + unpacked_code_size + bsslen

    new_exe_size = header_size + new_text_size
    print "header_size %d" % header_size
    print "new_text_size %d" % new_text_size
    print "new_bss_size %d" % new_bss_size
    print "FINAL EXE SIZE %d" % new_exe_size

    new_header = (
        header, new_text_size, 0, new_bss_size, 0, reserved1, flags, relocinfo
    )
    fh_out = open(filename_out, 'wb')
    write_struct(fh_out, HEADER_FORMAT, new_header)

    shrinkler_code = read_data(SHRINKLER_CODE)

    # Work out where the unpacked stuff goes
    offset_to_unpacked = shrinkler_header_size + packed_data_size + stack_size - 2

    assert(shrinkler_code[6] == 0x12)
    assert(shrinkler_code[7] == 0x34)
    shrinkler_code[6] = (offset_to_unpacked) / 256
    shrinkler_code[7] = (offset_to_unpacked) & 255

    fh_out.write(shrinkler_code)
    fh_out.write(read_data(filename_tmp_packed))
    fh_out.write(bytearray(4))
    fh_out.close()

if __name__ == '__main__':
    import sys
    fh_in = open(sys.argv[1], 'rb')
    pack(fh_in, sys.argv[2])
    fh_in.close()