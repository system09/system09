#===================================================================
# File:        Makefile
# Author:      David Burnette 
# Created:     July 5, 2007
# 
# Description: 
#  Makefile to build the System09 by John Kent
# 
#  This makefile will build John Kent's entire System09 project 
#  (RTL synthesis and monitor ROMs) and even download the final 
#  bitstream to the prototype board.
# 
#  You can use Xilinx ISE interactively to add new RTL source files
#  to this project.
#
# Usage:
#  Use 'make help' to get a list of options.
#
# Dependencies:
#  Depends on makefile fragments in the 'MKFRAGS' directory.
#
# Revision History:
#   dgb  2007-07-05  Original version
#
#   dgb  2008-04-07  Split out files into fragments. Modified
#                    ROM source generation to be per src directory.
#
#===================================================================

MKFRAGS := ../../mkfiles
export MKFRAGS

#===================================================================
# User-modifiable variables
#
# This name must match the name of the design in Xilinx ISE (case
# sensitive). 
DESIGN_NAME := my_system09
#
# Constraint file (unfortunately it cannot be extracted from ISE)
UCF_FILE    := my_system09.ucf
#
# Technology family (unfortunately it cannot be extracted from ISE)
FAMILY      := spartan3

# List of ROM VHDL files
.PHONY: roms
roms:
	@$(MAKE) -C ../../src/sys09bug sys09xes.vhd
	@$(MAKE) -C ../../src/Flex9 flex9ide.vhd

#===================================================================
# You should not need to edit anything below this line

# XESS Tools
XSLOAD     := C:/Progra~1/XSTOOLs/xsload.exe

include ../../mkfiles/xilinx_rules.mk

#===================================================================
# TARGETS

.PHONY: all
all: bit 

.PHONY: bit
bit: roms $(DESIGN_NAME).bit

.PHONY: impact
impact: roms bit do_impact

prom: roms $(DESIGN_NAME).mcs

.PHONY: xsload
xsload: roms $(DESIGN_NAME).bit
	@$(ECHO)
	@$(ECHO) "======= Downloading bitstream to XSA-3S1000 using XSLOAD (parallel) ="
	$(XSLOAD) -p 0 -b xsa-3s1000 -fpga $<

usbxsload.bit: roms $(DESIGN_NAME).bit
	@$(ECHO)
	@$(ECHO) "======= Generating special bitstream with StartUpClk=JtagClk ========"
	$(GREP) -v StartUpClk $(BITGEN_OPTIONS_FILE) >tmp.ut
	$(ECHO) "-g StartUpClk:JtagClk" >>tmp.ut
	$(BITGEN) $(BITGEN_FLAGS) -f tmp.ut $(DESIGN_NAME).ncd usbxsload.bit

.PHONY: usbxsload
usbxsload: roms usbxsload.bit
	@$(ECHO)
	@$(ECHO) "======= Downloading bitstream to XSA-3S1000 using XSLOAD (USB) ======"
	$(XSLOAD) -usb 0 -b xsa-3s1000 -fpga usbxsload.bit

.PHONY: usbflash0
usbflash0: roms prom $(DESIGN_NAME).bit
	$(XSLOAD) -usb 0 -b xsa-3s1000 -flash $(DESIGN_NAME).mcs
	
.PHONY: help
help:
	@$(ECHO) "Use this Makefile to regenerate the entire System09 bitstream"
	@$(ECHO) "after modifying any of the source RTL or 6809 assembler code."
	@$(ECHO) ""
	@$(ECHO) "This makefile uses the following project files from the Xilinx ISE"
	@$(ECHO) "   $(XST_FILE)"
	@$(ECHO) ""
	@$(ECHO) "You use Xilinx ISE interactively to add new RTL source files."
	@$(ECHO) ""
	@$(ECHO) "            Availiable targets"
	@$(ECHO) 
	@$(ECHO) "  For building all or part of the system:"
	@$(ECHO) "    roms      - Run asm09 and then generate the VHDL RTL rom files"
	@$(ECHO) "    bit       - Rebuild the entire system and generate the bitstream file"
	@$(ECHO) "    all       - Rebuild everything"
	@$(ECHO) "    prom      - Rebuild the entire system and generate an MCS prom file"
	@$(ECHO) "    exo       - Rebuild the entire system and generate an EXO prom file"
	@$(ECHO) 
	@$(ECHO) "  For downloading the bitstream to the board:"
	@$(ECHO) "    xsload    - Download the bitstream to the FPGA via XSLOAD"
	@$(ECHO) "    usbxsload - Download the bitstream to the FPGA via usbXSLOAD"
	@$(ECHO) "    usbflash0 - Download the bitstream Flash slot 0 via usbXSLOAD"
	@$(ECHO) "    impact    - Download the bitstream to the FPGA via iMPACT"
	@$(ECHO) 
	@$(ECHO) "  For project maintenance:"
	@$(ECHO) "    help      - Print this help text"
	@$(ECHO) "    clean     - Clean up the ISE files"
	@$(ECHO) ""

.PHONY: clean
clean:
	-$(MAKE) -C ../../src/sys09bug clean
	-$(MAKE) -C ../../src/Flex9 clean
	-$(RM) *.ncd *.ngc *.ngd *.twr *.bit *.mcs *.stx *.ucf.untf *.mrp 
	-$(RM) *.ncl *.ngm *.prm *_pad.txt *.twx *.log *.syr *.par *.exo *.xpi
	-$(RM) *.cmd_log *.ngr *.bld *_summary.html *.nc1 *.pcf *.bgn
	-$(RM) *.pad *.placed_ncd_tracker *.routed_ncd_tracker *_pad.csv *.drc
	-$(RM) *.pad_txt $(DESIGN_NAME)_impact.cmd *.unroutes
	-$(RMDIR) _ngo _xmsgs

