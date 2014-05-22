Instructions for Setting up FLEX9

XSA-3S1000 + XST3.0 + IDE harddrive

1. Build the bitstream by going to the rtl/System09_Xess_-3S1000 and typing
'make bit' in a cygwin shell.

2. Connect a serial cable from the XST3.0 board to the serial port on the PC.

3. Connect an IDE hard disk to the IDE connector on the XST3 board. It should be
at least 128 MB for 4 logical Flex disk drives.

4. Connect a PS/2 keyboard and VGA monitor to the XSA-3S1000 board.

5. Open Hyperterm and configure the port for 57600 8-N-1.

6. Download the bitstream to the FPGA using 'make xsload'. If you have the USB 
download cable, you can build and download the bitstream by typing 'make usbxsload'. 

7. Use the 'UF' command at the S-BUG prompt to format an empty drive. 

8. Use the 'UX' command at the S-BUG prompt to download the FLEX9 disk image
using the XModem protocol in Hyperterm. The disk image file is FLEXESS.dsk.
Downloading takes about 11 minutes at 57.6k baud.

9. Flex9 is embedded in the bitstream to load in a block RAM on the FPGA. Once
the disk image is downloaded, start FLEX by setting the program counter to $CD00
(using ^P CD00 at the S-BUG prompt), then type 'G' at the S-BUG prompt. If things 
are working correctly Flex should request the date then give the "+++" prompt. 

10. Test the disk drive by typing 'DIR,0' at the flex prompt (no single quotes).
It should list the files on drive 0.

11. The UX download program writes a bootstrap loader on the disk after the disk 
image is downloaded. The Flex image must be linked to to the boot strap using 
the "LINK 0.FLEX9IDE.SYS" command at the FLEX prompt. Then, next time System09
is powered on, simply issue the 'D' command at the S-BUG prompt to boot FLEX.
