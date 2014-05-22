System09 XuLA port 
==================

This is a port of the System09 project to the XESS 200Kgate Spartan3A XuLA board.
It uses 1MB of the SDRAM. Only half the data bus is used.
System09 XuLA port was implemented with Xilinx ISE Design Suite 12.4

Check XuLA.ucf for the pin assignment.
It uses a MAX3232 for the RS232 interface, PS/2 keyboard interface and 3 bit RGB VGA video output.
Video format is 80 characters x 25 character with 8 horizontal pixels per character
The design doubles the 12MHz crystal on the XuLA board for the pixel clock

Care must be taken with the Spartan3A inputs when connecting to 5V outputs
as the inputs are normally not diode clamped. 
The UCF file specified PCI inputs on the input pin which should be diode clamped 
but only after the Spartan3A FPGA on the XuLA board is configured.
Series current limiting resistors should be used when connecting a 5V logic output to an FPGA input.
Check the UCF file to determine which inputs are diode clamped.
Series limitted 5V logic outputs should not be connected to Spartan3A inputs until it is configured
as a PCI33 input.

John Kent
2011-10-09

The System09 XuLA port is licenced under GNU GPL version 3.


--  Copyright (C) 2011 John Kent
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
