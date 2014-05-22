--=============================================================================--
--                                                                             --
--  System09 - Synthesizable System On a Chip - VHDL FPGA core top level file. --
--                                                                             --
--=============================================================================--
--
-- File name      : System09_BurchED_B3.vhd
--
-- Purpose        : This is the top level file for a 6809 instruction compatible system on a chip 
--                  It has been designed for the BurchED B3 Spartan2+ FPGA board
--                  using the Xilinx XC2S200 Spartan 2 FPGA and Xilinx ISE 7.1 software.
--                  It has been implemented with the BurchED B3 FPGA board, 
--                  modified B3-SRAM module and B3-FPGA-CPU-IO module. 
--                  It also supports an IDE CF card interface using a CF to IDE interface adapter.
--                  It uses a monochrome version of the VDU due to limitted Block RAM of XC2S200
--
-- Dependencies   : ieee.Std_Logic_1164
--                  ieee.std_logic_unsigned
--                  ieee.std_logic_arith
--                  ieee.numeric_std
--
-- Uses           : 
--                  clk_div        (..\Spartan2\clk_div.vhd)       System Clock Divider
--                  cpu09          (..\VHDL\cpu09.vhd)             CPU core
--                  B3_SRAM        (..\VHDL\B3_SRAM.vhd)           BurchED B3 SRAM module interface
--                  acia6850       (..\VHDL\acia6850.vhd)          RS232 Serial Interface
--                  ACIA_Clock     (..\VHDL\ACIA_Clock.vhd)        ACIA Baud Rate Clock Divider
--                  keyboard       (..\VHDL\keyboard.vhd)          PS/2 Keyboard register interface
--                  ps2_keyboard   (..\VHDL\ps2_keyboard.vhd)      PS/2 Keyboard interface logic
--                  keymap_rom     (..\Spartan2\keymap_rom_b4.vhd) PS/2 Keyboard key code look up table
--                  vdu8_mono      (..\VHDL\vdu8_mono.vhd)         80 x 25 Monochrome Visual Display Unit.
--                  char_rom       (..\Spartan2\char_rom2k_b4.vhd) Character Generator ROM
--                  ram_2k         (..\Spartan2\ram2k_b4.vhd)      Text buffer RAM
--                  timer          (..\VHDL\timer.vhd)             Timer module
--                  trap           (..\VHDL\trap.vhd)              Bus Trap interrupt
--                  spp            (..\VHDL\spp.vhd)               Simple Parallel Port
--                  peripheral_bus (..\VHDL\peripheral_bus.vhd)    16 bit IDE Peripheral Bus interface
--                  sys09bug_F800  (..\Spartan2\sys09b3s_b4.vhd)   Sysbug09 Monitor ROM
--                  dat_ram        (..\VHDL\datram.vhd)            Dynamic Address Translation (DAT)
-- 
-- Author         : John E. Kent      
--                  dilbert57@opencores.org
--
-- Memory Map     :
--
-- $0000 - $DFFF System RAM (256K Mapped via DAT)
-- $E000 - ACIA (SWTPc)
-- $E010 - Reserved for SWTPc FD-01 FD1771 FDC
-- $E020 - Keyboard
-- $E030 - VDU
-- $E040 - Reserved for SWTPc MP-T (was Compact Flash)
-- $E050 - Timer
-- $E060 - Bus Trap (Hardware Breakpoint Interrupt Logic)
-- $E070 - Reserved for Trace Buffer
-- $E080 - Reserved for SWTPc MP-ID 6821 PIA (?)
-- $E090 - Reserved for SWTPc MP-ID 6840 PTM (?)
-- $E0A0 - SPP Printer Port
-- $E0B0 - Reserved
-- $E0C0 - Reserved
-- $E100 - $E13F IDE / Compact Flash Card
-- $E140 - $E17F Reserved for Ethernet MAC (XESS)
-- $E180 - $E1BF Reserved for Expansion Slot 0 (XESS)
-- $E1C0 - $E1FF Reserved for Expansion Slot 1 (XESS)
-- $E200 - $EFFF Dual Port RAM interface
-- $F000 - $F7FF Reserved SWTPc DMAF-2
-- $F800 - $FFFF Sys09bug ROM (Read only)
-- $FFF0 - $FFFF DAT - Dynamic Address Translation (Write Only)
--
--
--  Copyright (C) 2003 - 2010 John Kent
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
--===========================================================================--
--
--                         Revision History:
--
--===========================================================================--
-- Version 0.1 - 20 March 2003
-- Version 0.2 - 30 March 2003
-- Version 0.3 - 29 April 2003
-- Version 0.4 - 29 June 2003
--
-- Version 0.5 - 19 July 2003
-- prints out "Hello World"
--
-- Version 0.6 - 5 September 2003
-- Runs SBUG
--
-- Version 1.0- 6 Sep 2003 - John Kent
-- Inverted SysClk
-- Initial release to Open Cores
--
-- Version 1.1 - 17 Jan 2004 - John Kent
-- Updated miniUart.
--
-- Version 1.2 - 25 Jan 2004 - John Kent
-- removed signals "test_alu" and "test_cc" 
-- Trap hardware re-instated.
--
-- Version 1.3 - 11 Feb 2004 - John Kent
-- Designed forked off to produce System09_VDU
-- Added VDU component
--	VDU runs at 25MHz and divides the clock by 2 for the CPU
-- UART Runs at 57.6 Kbps
--
-- Version 1.4 - 21 Nov 2004 - John Kent
-- Changes to make compatible with Spartan3 starter kit version
-- Designed to run with a 50MHz clock input.
-- the VDU divides 50 MHz to generate a 
-- 25 MHz VDU Pixel Clock and a 12.5 MHz CPU clock
-- Changed Monitor ROM signals to make it look like
-- a standard 2K memory block
-- Re-assigned I/O port assignments so it is possible to run KBUG9
--
-- Version 1.5 - 3rd February 2007 - John Kent
-- Changed VDU8 to use external clock divider
-- renamed miniUART to ACIA_6850
-- Memory decoding of ROM & IO now uses DAT
--
-- Version 1.6 - 7th Februaury 2007 - John Kent
-- Made ACIA Clock generator an external component
-- Added Generics to VDU and Keyboard
-- Changed decoding
--
-- Version 1.7 - 20th May 2007 - John Kent
-- Added 4 wait states to CF access
-- Removed DAT memory map control of ROM & IO
-- to allow for full use of RAM as a RAM disk.
-- Mapped in all 16 bits of the CF data bus.
--
-- Version 1.8 - 1st July 2007 - John Kent
-- Copied B5-X300 top level to B3 version.
--
-- Version 2.0 - 6th September 2008 - John Kent
-- added IDE interface for a CF card.
-- Used separate Clock DLL for generating clocks
--
-- Version 2.1 - 23rd Februaury 2008 - John Kent
-- Renamed Monitor ROM
--
-- Version 2.2 - 28th August 2010 - John Kent
-- Renamed ACIA_6850 to acia6850
-- Updated CPU & VDU component signal names & generics
-- Made peripheral bus interface a separate component
-- Made BED_SRAM a separate component
-- Made the LED flasher a separate component
-- Updated Header
--
--===========================================================================
--
library ieee;
   use ieee.std_logic_1164.all;
   use IEEE.STD_LOGIC_ARITH.ALL;
   use IEEE.STD_LOGIC_UNSIGNED.ALL;
   use ieee.numeric_std.all;
library unisim;
   use unisim.vcomponents.all;

entity System09 is
  port(
    clk_in      : in  Std_Logic;  -- System Clock input
    rst_n       : in  Std_logic;  -- Master Reset input (active low)
    LED         : out Std_logic;  -- Diagnostic LED Flasher

    -- B3-SRAM Memory Interface signals

    ram_csn     : out Std_Logic;
    ram_wrln    : out Std_Logic;
    ram_wrun    : out Std_Logic;
    ram_addr    : out Std_Logic_Vector(16 downto 0);
    ram_data    : inout Std_Logic_Vector(15 downto 0);

    -- End of B3-SRAM Memory Interface signals

    -- B3-FPGA-CPU-IO Module signals

    -- Asychronous Communications Interface Adapater signals (RS232 Serial Port) ($E00X)
    acia_rxd    : in  Std_Logic;
    acia_txd    : out Std_Logic;
    acia_rts_n  : out Std_Logic;
    acia_cts_n  : in  Std_Logic;

    -- PS/2 Keyboard Interface ($E02X)
    kb_clock    : inout Std_logic;
    kb_data     : inout Std_Logic;

	-- PS/2 Mouse interface
--	 mouse_clock : in  Std_Logic;
--	 mouse_data  : in  Std_Logic;

    -- Visual Display Unit output signals ($E03X)
    vga_vsync   : out Std_Logic;
    vga_hsync   : out Std_Logic;
    vga_blue    : out std_logic_vector(1 downto 0);
    vga_green   : out std_logic_vector(1 downto 0);
    vga_red     : out std_logic_vector(1 downto 0);

    -- Buzzer
--	   buzzer      : out std_logic;

    -- End of B3-FPGA-CPU-IO Module signals

    -- Parallel Printer Port    ($E0AX)
    pp_data      : out std_logic_vector(7 downto 0);
    pp_stat      : in  std_logic_vector(7 downto 3);
    pp_ctrl      : out std_logic_vector(3 downto 0);

    -- Peripheral Bus ($E100 - $E1FF) 
    pb_iord_n    : out std_logic;
    pb_iowr_n    : out std_logic;
    pb_addr      : out std_logic_vector(2 downto 0);
    pb_data      : inout std_logic_vector(15 downto 0);

    -- IDE Compact Flash ($E100 - $E13F)
    ide_rst_n    : out std_logic; -- ide pin 1
    ide_cs0_n    : out std_logic; -- ide pin 37
    ide_cs1_n    : out std_logic; -- ide pin 38
    ide_dmarq    : in  std_logic; -- ide pin 21
    ide_dmack_n  : out std_logic; -- ide pin 29
    ide_iordy    : in  std_logic; -- ide pin 27
    ide_con_csel : out std_logic; -- ide pin 28
    ide_intrq    : in  std_logic; -- ide pin 31
    ide_iocs16_n : in  std_logic; -- ide pin 32
    ide_pdiag_n  : in  std_logic; -- ide pin 34
    ide_dasp_n   : out std_logic; -- ide pin 39

    -- Dual port RAM interface bus
    bus_clk      : in  std_logic;
    bus_cs_n     : in  std_logic;
    bus_rw       : in  std_logic;
    bus_addr     : in  std_logic_vector(12 downto 0);
    bus_data_in  : in  std_logic_vector(7 downto 0);
    bus_data_out : out std_logic_vector(7 downto 0)
 );
end System09;

-------------------------------------------------------------------------------
-- Architecture for System09
-------------------------------------------------------------------------------
architecture rtl of System09 is
  -----------------------------------------------------------------------------
  -- constants
  -----------------------------------------------------------------------------
  constant SYS_CLK_FREQ  : integer := 50000000;  -- FPGA System Clock
  constant VGA_CLK_FREQ  : integer := 25000000;  -- VGA Pixel Clock
  constant CPU_CLK_FREQ  : integer := 12500000;  -- CPU Clock
  constant BAUD_RATE     : integer := 57600;	  -- Baud Rate
  constant ACIA_CLK_FREQ : integer := BAUD_RATE * 16;

  -----------------------------------------------------------------------------
  -- Signals
  -----------------------------------------------------------------------------

  -- System Clock
  signal sys_clk       : std_logic;

  -- CPU Interface signals
  signal cpu_rst       : Std_Logic;
  signal cpu_clk       : Std_Logic;
  signal cpu_rw        : std_logic;
  signal cpu_vma       : std_logic;
  signal cpu_halt      : std_logic;
  signal cpu_hold      : std_logic;
  signal cpu_firq      : std_logic;
  signal cpu_irq       : std_logic;
  signal cpu_nmi       : std_logic;
  signal cpu_addr      : std_logic_vector(15 downto 0);
  signal cpu_data_in   : std_logic_vector(7 downto 0);
  signal cpu_data_out  : std_logic_vector(7 downto 0);

  -- B3 Static RAM            ($0000 - $DFFF)
  signal ram_cs        : std_logic; -- memory chip select
  signal ram_data_out  : std_logic_vector(7 downto 0);

  -- ACIA Console serial port ($E000 - $E00F)
  signal acia_data_out : Std_Logic_Vector(7 downto 0);  
  signal acia_cs       : Std_Logic;
  signal acia_irq      : Std_Logic;
  signal acia_clk      : Std_Logic;

  -- PS/2 Keyboard interface  ($E02X)
  signal kbd_data_out  : std_logic_vector(7 downto 0);
  signal kbd_cs        : std_logic;
  signal kbd_irq       : std_logic;

  -- Visual Display Unit      ($E03X)
  signal vga_clk       : std_logic;
  signal vdu_cs        : std_logic;
  signal vdu_data_out  : std_logic_vector(7 downto 0);
  signal vga_red_o     : std_logic;
  signal vga_green_o   : std_logic;
  signal vga_blue_o    : std_logic;

  -- Timer                    ($E05X)
  signal timer_data_out : std_logic_vector(7 downto 0);
  signal timer_cs      : std_logic;
  signal timer_irq     : std_logic;

  -- Bus trap                 ($E06X)
  signal trap_cs       : std_logic;
  signal trap_data_out : std_logic_vector(7 downto 0);
  signal trap_irq      : std_logic;

  -- Trace                    ($E07X)
--  signal trace_cs       : std_logic;
--  signal trace_data_out : std_logic_vector(7 downto 0);
--  signal trace_irq      : std_logic;

  -- Simple Parallel I/O port ($E0AX)
  signal spp_data_out  : std_logic_vector(7 downto 0);
  signal spp_cs        : std_logic;

  -- Peripheral Bus           ($E1XX)
  signal pb_cs         : std_logic;
  signal pb_data_out   : std_logic_vector(7 downto 0);
  signal pb_hold       : std_logic;

  -- Peripheral Bus Chip Selects ($E1XX)
  signal ide_cs        : std_logic;	 -- IDE CF interface   ($E100 - $E13F)
  signal ether_cs      : std_logic;  -- Ethernet interface ($E140 - $E17F)	
  signal slot1_cs      : std_logic;	 -- Expansion slot1    ($E180 - $E1BF)
  signal slot2_cs      : std_logic;	 -- Expansion slot 2   ($E1C0 - $E1FF)

  -- Dual Port RAM for Bus Interfacing	($E200 - $E7FF)
  signal dpr_data_out  : std_logic_vector(7 downto 0);
  signal dpr_cs        : std_logic;
  signal dpr_wr        : std_logic;

  -- External Bus Interface
  signal bus_iclk     : std_logic;
  signal bus_gclk     : std_logic;
  signal bus_cs       : std_logic;
  signal bus_wr       : std_logic;

  -- Monitor ROM ($F800 - $FFFF)
  signal rom_data_out  : Std_Logic_Vector(7 downto 0);
  signal rom_cs        : std_logic;

  -- Dynamic Address Translation	($FFF0 - $FFFF)
  signal dat_cs       : std_logic;
  signal dat_addr     : std_logic_vector(7 downto 0);

-----------------------------------------------------------------
--
--                     Clock generator
--
-----------------------------------------------------------------

component clock_div
  port(
    clk_in      : in  std_Logic;  -- System Clock input
	 sys_clk     : out std_logic;  -- System Clock Out    (1/1)
	 vga_clk     : out std_logic;  -- VGA Pixel Clock Out (1/2)
    cpu_clk     : out std_logic   -- CPU Clock Out       (1/4)
  );
end component;

-----------------------------------------------------------------
--
--                      LED Flasher
--
-----------------------------------------------------------------

component flasher
  port (
    clk      : in  std_logic;           -- Clock input
    rst      : in  std_logic;           -- Reset input (active high)
    LED      : out Std_Logic            -- LED output        
  );
end component;


-----------------------------------------------------------------
--
--                  6809 Compatible CPU core
--
-----------------------------------------------------------------

component cpu09
  port (    
    clk       : in  std_logic;
    rst       : in  std_logic;
    rw        : out std_logic;
    vma       : out std_logic;
    addr      : out std_logic_vector(15 downto 0);
    data_in   : in  std_logic_vector(7 downto 0);
    data_out  : out std_logic_vector(7 downto 0);
    halt      : in  std_logic;
    hold      : in  std_logic;
    irq       : in  std_logic;
    nmi       : in  std_logic;
    firq      : in  std_logic
  );
end component;

-----------------------------------------------------------------
--
-- Dynamic Address Translation Registers ($FFF0 - $FFFF)
--
-----------------------------------------------------------------
component dat_ram
  port (
    clk      : in  std_logic;
    rst      : in  std_logic;
    cs       : in  std_logic;
    rw       : in  std_logic;
    addr_lo  : in  std_logic_vector(3 downto 0);
    addr_hi  : in  std_logic_vector(3 downto 0);
    data_in  : in  std_logic_vector(7 downto 0);
    data_out : out std_logic_vector(7 downto 0)
  );
end component;

-------------------------------------------------
--
-- Sys09Bug Block RAM Monitor ROM ($F800-$FFFF)
--
-------------------------------------------------
component sys09bug_F800
  port (
     clk      : in  std_logic;
     rst      : in  std_logic;
     cs       : in  std_logic;
     addr     : in  std_logic_vector (10 downto 0);
     rw       : in  std_logic;
     data_in  : in  std_logic_vector (7 downto 0);
     data_out : out std_logic_vector (7 downto 0)
  );
end component;

-----------------------------------------------------------------
--
-- 6850 ACIA RS232 Interface  ($E000 - $E00F)
--
-----------------------------------------------------------------

component acia6850
  port (
     clk      : in  Std_Logic;  -- System Clock
     rst      : in  Std_Logic;  -- Reset input (active high)
     cs       : in  Std_Logic;  -- miniUART Chip Select
     rw       : in  Std_Logic;  -- Read / Not Write
     irq      : out Std_Logic;  -- Interrupt
     addr     : in  Std_Logic;  -- Register Select
     data_in  : in  Std_Logic_Vector(7 downto 0); -- Data Bus In 
     data_out : out Std_Logic_Vector(7 downto 0); -- Data Bus Out
     RxC      : in  Std_Logic;  -- Receive Baud Clock
     TxC      : in  Std_Logic;  -- Transmit Baud Clock
     RxD      : in  Std_Logic;  -- Receive Data
     TxD      : out Std_Logic;  -- Transmit Data
     DCD_n    : in  Std_Logic;  -- Data Carrier Detect
     CTS_n    : in  Std_Logic;  -- Clear To Send
     RTS_n    : out Std_Logic );  -- Request To send
end component;

-----------------------------------------------------------------
--
-- ACIA Clock divider
--
-----------------------------------------------------------------

component ACIA_Clock
  generic (
     SYS_CLK_FREQ  : integer := SYS_CLK_FREQ;
     ACIA_CLK_FREQ : integer := ACIA_CLK_FREQ
  );   
  port (
     clk      : in  Std_Logic;  -- System Clock Input
     acia_clk : out Std_logic   -- ACIA Clock output
  );
end component;

----------------------------------------
--
-- PS/2 Keyboard ($E020 - $E02F)
--
----------------------------------------

component keyboard
  generic(
     KBD_CLK_FREQ : integer := CPU_CLK_FREQ
  );
  port (
     clk         : in    std_logic;
     rst         : in    std_logic;
     cs          : in    std_logic;
     rw          : in    std_logic;
     addr        : in    std_logic;
     data_in     : in    std_logic_vector(7 downto 0);
     data_out    : out   std_logic_vector(7 downto 0);
     irq         : out   std_logic;
     kbd_clk     : inout std_logic;
     kbd_data    : inout std_logic
  );
end component;

----------------------------------------
--
-- Video Display Unit. ($E030 - $E03F)
--
----------------------------------------
component vdu8_mono
  generic(
    VGA_CLK_FREQ           : integer := VGA_CLK_FREQ; -- HZ
	 VGA_HOR_CHARS          : integer := 80; -- CHARACTERS 25.6us
	 VGA_HOR_CHAR_PIXELS    : integer := 8;  -- PIXELS 0.32us
	 VGA_HOR_FRONT_PORCH    : integer := 16; -- PIXELS 0.64us
	 VGA_HOR_SYNC           : integer := 96; -- PIXELS 3.84us
	 VGA_HOR_BACK_PORCH     : integer := 48; -- PIXELS 1.92us
	 VGA_VER_CHARS          : integer := 25; -- CHARACTERS 12.8ms
	 VGA_VER_CHAR_LINES     : integer := 16; -- LINES 0.512ms
	 VGA_VER_FRONT_PORCH    : integer := 10; -- LINES 0.320ms
	 VGA_VER_SYNC           : integer := 2;  -- LINES 0.064ms
	 VGA_VER_BACK_PORCH     : integer := 34  -- LINES 1.088ms
  );
  port(
    -- control register interface
    vdu_clk      : in  std_logic;	 -- CPU Clock - 12.5MHz
    vdu_rst      : in  std_logic;
    vdu_cs       : in  std_logic;
    vdu_addr     : in  std_logic_vector(2 downto 0);
    vdu_rw       : in  std_logic;
    vdu_data_in  : in  std_logic_vector(7 downto 0);
    vdu_data_out : out std_logic_vector(7 downto 0);

    -- vga port connections
    vga_clk      : in  std_logic;	-- VGA Pixel Clock - 25 MHz
    vga_red_o    : out std_logic;
    vga_green_o  : out std_logic;
    vga_blue_o   : out std_logic;
    vga_hsync_o  : out std_logic;
    vga_vsync_o  : out std_logic
  );
end component;

----------------------------------------
--
-- Timer module  ($E050 - $E05F)
--
----------------------------------------

component timer
  port (
    clk       : in std_logic;
    rst       : in std_logic;
    cs        : in std_logic;
    addr      : in std_logic;
    rw        : in std_logic;
    data_in   : in std_logic_vector(7 downto 0);
    data_out  : out std_logic_vector(7 downto 0);
    irq       : out std_logic
  );
end component;

------------------------------------------------------------
--
-- Bus Trap / Hardware Breakpoint ($E060 - $E06F)
--
------------------------------------------------------------

component trap
  port (	
    clk        : in  std_logic;
    rst        : in  std_logic;
    cs         : in  std_logic;
    rw         : in  std_logic;
    vma        : in  std_logic;
    addr       : in  std_logic_vector(15 downto 0);
    data_in    : in  std_logic_vector(7 downto 0);
    data_out   : out std_logic_vector(7 downto 0);
    irq        : out std_logic
  );
end component;

------------------------------------------------------------
--
-- Bus Trace logic	($E070 - $E07F)
--
------------------------------------------------------------
--component trace is
--  port (	
--    clk           : in  std_logic;
--    rst           : in  std_logic;
--    rs            : in  std_logic;	  -- register select
--    bs            : in  std_logic;	  -- bank select
--    rw            : in  std_logic;
--    vma           : in  std_logic;
--    addr          : in  std_logic_vector(15 downto 0);
--    data_in       : in  std_logic_vector(7 downto 0);
--    reg_data_out  : out std_logic_vector(7 downto 0);
--    buff_data_out : out std_logic_vector(7 downto 0);
--    cpu_data_in   : in  std_logic_vector(7 downto 0);
--    irq           : out std_logic
--  );
--end component;

----------------------------------------
--
-- Simple Parallel Port	($E0A0 - $E0AF)
--
----------------------------------------
component spp
  port (	
    clk       : in  std_logic;
    rst       : in  std_logic;
    cs        : in  std_logic;
    addr      : in  std_logic_vector(2 downto 0);
    rw        : in  std_logic;
    data_in   : in  std_logic_vector(7 downto 0);
    data_out  : out std_logic_vector(7 downto 0);
    irq       : out std_logic;
    hold      : out std_logic;
    spp_data  : out std_logic_vector(7 downto 0);
    spp_stat  : in  std_logic_vector(7 downto 3);
    spp_ctrl  : out std_logic_vector(3 downto 0)
   );
end component;

------------------------------------------------------------
--
-- Peripheral Bus interface (IDE CF) ($E100 - $E1FF)
--
------------------------------------------------------------

component peripheral_bus is
  port (
    --
    -- CPU Interface signals
    --
    clk      : in  std_logic;                     -- System Clock
    rst      : in  std_logic;                     -- Reset input (active high)
    cs       : in  std_logic;                     -- Peripheral Bus Chip Select
    addr     : in  std_logic_vector(7 downto 0);  -- Register Select
    rw       : in  std_logic;                     -- Read / Not Write
    data_in  : in  std_logic_vector(7 downto 0);  -- Data Bus In 
    data_out : out std_logic_vector(7 downto 0);  -- Data Bus Out
    hold     : out std_logic;                     -- Hold bus cycle output
    --
    -- Peripheral Bus Interface Signals
    -- IO + ($00 - $FF) 
    -- (for compatibility with XSA-3S1000 / XST 3.0)
    --
    pb_rd_n  : out   std_logic; -- ide pin 25
    pb_wr_n  : out   std_logic; -- ide pin 23
    pb_addr  : out   std_logic_vector( 4 downto 0);
    pb_data  : inout std_logic_vector(15 downto 0);

    -- Peripheral chip selects on Peripheral Bus 
    ide_cs   : out  std_logic;  -- IDE / CF interface ($00 - $3F)
    eth_cs   : out  std_logic;  -- Ethernet interface ($40 - $7F)
    sl1_cs   : out  std_logic;  -- Expansion slot 1   ($80 - $BF)
    sl2_cs   : out  std_logic   -- Expansion slot 2   ($C0 - $FF)
    );
end component;  

------------------------------------------------------------
--
-- External Bus interface Dual port RAM ($E200 - $EFFF)
--
------------------------------------------------------------

component RAMB4_S8_S8
port (
  RSTA:  IN  std_logic;
  CLKA:  IN  std_logic;
  ENA:   IN  std_logic;
  WEA:   IN  std_logic;
  ADDRA: IN  std_logic_vector(8 downto 0);
  DIA:   IN  std_logic_vector(7 downto 0);
  DOA:   OUT std_logic_vector(7 downto 0);
  RSTB:  IN  std_logic;
  CLKB:  IN  std_logic;
  ENB:   IN  std_logic;
  WEB:   IN  std_logic;
  ADDRB: IN  std_logic_vector(8 downto 0);
  DIB:   IN  std_logic_vector(7 downto 0);
  DOB:   OUT std_logic_vector(7 downto 0)
  );
end component;

component IBUF
port (
  I : IN  std_logic;
  O : OUT std_logic
  );
end component;

component BUFG
port (
  I : IN  std_logic;
  O : OUT std_logic
  );
end component;

------------------------------------------------------------
--
--           BED SRAM interface ($0000 - $DFFF)
--
------------------------------------------------------------
component BED_SRAM
  port (
    --
    -- CPU Interface signals
    --
    clk       : in  std_logic;                     -- System Clock (twice the CPU clock)
    rst       : in  std_logic;                     -- Reset input (active high)
    cs        : in  std_logic;                     -- RAM Chip Select
    addr      : in  std_logic_vector(17 downto 0); -- RAM address bus
    rw        : in  std_logic;                     -- Read / Not Write
    data_in   : in  std_logic_vector(7 downto 0);  -- Data Bus In 
    data_out  : out std_logic_vector(7 downto 0);  -- Data Bus Out
    --
    -- B3_SRAM Interface Signals
    --
    ram_csn   : out   Std_Logic;
    ram_wrln  : out   Std_Logic;
    ram_wrun  : out   Std_Logic;
    ram_addr  : out   Std_Logic_Vector(16 downto 0);
    ram_data  : inout Std_Logic_Vector(15 downto 0)

    );
end component;

begin
-----------------------------------------------------------------------------
-- Instantiation of internal components
-----------------------------------------------------------------------------
----------------------------------------
--
-- Clock generator
--
----------------------------------------
my_clock_div: clock_div port map (
    clk_in   => clk_in,   -- Clock input
    sys_clk  => sys_clk,  -- System Clock Out    (1/1)
    vga_clk  => vga_clk,  -- VGA Pixel Clock Out (1/2)
    cpu_clk  => cpu_clk   -- CPU Clock Out       (1/4)
  );

-----------------------------------------
--
-- LED Flasher
--
-----------------------------------------

my_LED_flasher : flasher port map (
    clk      => cpu_clk,
    rst      => cpu_rst,
    LED      => LED        
  );

----------------------------------------
--
-- CPU09 CPU Core
--
----------------------------------------
my_cpu : cpu09  port map (    
    clk       => cpu_clk,
    rst       => cpu_rst,
    rw        => cpu_rw,
    vma       => cpu_vma,
    addr      => cpu_addr(15 downto 0),
    data_in   => cpu_data_in,
    data_out  => cpu_data_out,
    halt      => cpu_halt,
    hold      => cpu_hold,
    irq       => cpu_irq,
    nmi       => cpu_nmi,
    firq      => cpu_firq
  );

----------------------------------------
--
-- Dynamic Address Translation ($FFF0-$FFFF)
--
----------------------------------------
my_dat : dat_ram port map (
    clk        => cpu_clk,
    rst        => cpu_rst,
    cs         => dat_cs,
    rw         => cpu_rw,
    addr_hi    => cpu_addr(15 downto 12),
    addr_lo    => cpu_addr(3 downto 0),
    data_in    => cpu_data_out,
    data_out   => dat_addr(7 downto 0)
  );

----------------------------------------
--
-- SYS09BUG Monitor ROM ($F800-$FFFF)
--
----------------------------------------
my_rom : SYS09BUG_F800 port map (
       clk      => cpu_clk,
       rst      => cpu_rst,
       cs       => rom_cs,
       rw       => '1',
       addr     => cpu_addr(10 downto 0),
       data_in  => cpu_data_out,
       data_out => rom_data_out
    );

----------------------------------------
--
-- ACIA RS232 Serial interface ($E000-$E00F)
--
----------------------------------------
my_ACIA  : acia6850 port map (
    --
    -- CPU Interface
    --
    clk	     => cpu_clk,
    rst       => cpu_rst,
    cs        => acia_cs,
    rw        => cpu_rw,
    irq       => acia_irq,
    addr      => cpu_addr(0),
    data_in   => cpu_data_out,
    data_out  => acia_data_out,
    --
    -- RS232 Interface
    --
    RxC       => acia_clk,
    TxC       => acia_clk,
    RxD       => acia_rxd,
    TxD       => acia_txd,
    DCD_n     => '0',
    CTS_n     => acia_cts_n,
    RTS_n     => acia_rts_n
  );

----------------------------------------
--
-- ACIA Baud Clock
--
----------------------------------------
my_ACIA_Clock : ACIA_Clock
  generic map(
    SYS_CLK_FREQ  => SYS_CLK_FREQ,
    ACIA_CLK_FREQ => ACIA_CLK_FREQ
  ) 
  port map(
    clk        => sys_clk,
    acia_clk   => acia_clk
  ); 

----------------------------------------
--
-- PS/2 Keyboard Interface ($E020-$E02F)
--
----------------------------------------
my_keyboard : keyboard
  generic map (
    KBD_CLK_FREQ => CPU_CLK_FREQ
  ) 
  port map(
    clk          => cpu_clk,
    rst          => cpu_rst,
    cs           => kbd_cs,
    rw           => cpu_rw,
    addr         => cpu_addr(0),
    data_in      => cpu_data_out(7 downto 0),
    data_out     => kbd_data_out(7 downto 0),
    irq          => kbd_irq,
    kbd_clk      => kb_clock,
    kbd_data     => kb_data
  );

------------------------------------------------
--
-- Video Display Unit instantiation ($E030-$E03F)
--
-------------------------------------------------
my_vdu : vdu8_mono 
  generic map(
    VGA_CLK_FREQ           => VGA_CLK_FREQ, -- 25MHZ
    VGA_HOR_CHARS          => 80, -- CHARACTERS 25.6us
    VGA_HOR_CHAR_PIXELS    => 8,  -- PIXELS 0.32us
    VGA_HOR_FRONT_PORCH    => 16, -- PIXELS 0.64us
    VGA_HOR_SYNC           => 96, -- PIXELS 3.84us
    VGA_HOR_BACK_PORCH     => 48, -- PIXELS 1.92us
    VGA_VER_CHARS          => 25, -- CHARACTERS 12.8ms
    VGA_VER_CHAR_LINES     => 16, -- LINES 0.512ms
    VGA_VER_FRONT_PORCH    => 10, -- LINES 0.320ms
    VGA_VER_SYNC           => 2,  -- LINES 0.064ms
    VGA_VER_BACK_PORCH     => 34  -- LINES 1.088ms
  )
  port map(

    -- CPU Control Registers interface
    vdu_clk       => cpu_clk,					 -- 12.5 MHz System Clock in
    vdu_rst       => cpu_rst,
    vdu_cs        => vdu_cs,
    vdu_rw        => cpu_rw,
    vdu_addr      => cpu_addr(2 downto 0),
    vdu_data_in   => cpu_data_out,
    vdu_data_out  => vdu_data_out,

    -- vga port connections
    vga_clk       => vga_clk,					 -- 25 MHz VDU pixel clock
    vga_red_o     => vga_red_o,
    vga_green_o   => vga_green_o,
    vga_blue_o    => vga_blue_o,
    vga_hsync_o   => vga_hsync,
    vga_vsync_o   => vga_vsync
  );

----------------------------------------
--
-- Timer Module
--
----------------------------------------
my_timer  : timer port map (
    clk       => cpu_clk,
    rst       => cpu_rst,
    cs        => timer_cs,
    rw        => cpu_rw,
    addr      => cpu_addr(0),
    data_in   => cpu_data_out,
    data_out  => timer_data_out,
    irq       => timer_irq
    );

----------------------------------------
--
-- Bus Trap Interrupt logic
--
----------------------------------------
my_trap : trap port map (	
    clk        => cpu_clk,
    rst        => cpu_rst,
    cs         => trap_cs,
    rw         => cpu_rw,
    vma        => cpu_vma,
    addr       => cpu_addr,
    data_in    => cpu_data_out,
    data_out   => trap_data_out,
    irq        => trap_irq
    );

----------------------------------------
--
-- Bus Trace logic
--
----------------------------------------
--my_trace : trace port map (	
--    clk           => sys_clk,
--    rst           => cpu_rst,
--    rs            => trace_cs,
--    bs            => bank_cs,
--    rw            => cpu_rw,
--    vma           => cpu_vma,
--    addr          => cpu_addr,
--    data_in       => cpu_data_out,
--    reg_data_out  => trace_data_out,
--    buff_data_out => bank_data_out,
--    cpu_data_in   => cpu_data_in,
--    irq           => trace_irq
--    );

----------------------------------------
--
-- Simple Parallel Port
--
----------------------------------------
my_spp  : spp port map (
    clk       => cpu_clk,
    rst       => cpu_rst,
    cs        => spp_cs,
    rw        => cpu_rw,
    addr      => cpu_addr(2 downto 0),
    data_in   => cpu_data_out,
    data_out  => spp_data_out,
    spp_data  => pp_data,
    spp_stat  => pp_stat,
    spp_ctrl  => pp_ctrl,
    hold      => open,
    irq       => open
  );

------------------------------------------------
--
-- 16 bit Peripheral Bus interface ($E100-$E1FF)
--
------------------------------------------------
my_pb : peripheral_bus port map (
    --
    -- CPU Interface signals
    --
    clk       => cpu_clk,
    rst       => cpu_rst,
    cs        => pb_cs,
    addr      => cpu_addr(7 downto 0),
    rw        => cpu_rw,
    data_in   => cpu_data_out, 
    data_out  => pb_data_out,
    hold      => pb_hold,
    --
    -- Peripheral Bus Interface Signals
    -- IO + ($00 - $FF) 
    --
    pb_rd_n   => pb_iord_n,
    pb_wr_n   => pb_iowr_n,
    pb_addr(2 downto 0) => pb_addr,
    pb_addr(4 downto 3) => open,
    pb_data   => pb_data,

    -- Peripheral chip selects on Peripheral Bus 
    ide_cs    => ide_cs,
    eth_cs    => ether_cs,
    sl1_cs    => slot1_cs,
    sl2_cs    => slot2_cs
    );

------------------------------------------------------
--
-- External Bus interface Dual port RAM ($E200 - $EFFF)
--
-------------------------------------------------------
my_dpr : RAMB4_S8_S8 port map (
  RSTA  => cpu_rst, 
  CLKA  => cpu_clk, 
  ENA   => dpr_cs, 
  WEA   => dpr_wr, 
  ADDRA => cpu_addr(8 downto 0), 
  DIA   => cpu_data_out, 
  DOA   => dpr_data_out,
  RSTB  => cpu_rst, 
  CLKB  => bus_gclk,
  ENB   => bus_cs,
  WEB   => bus_wr, 
  ADDRB => bus_addr(8 downto 0),
  DIB   => bus_data_in, 
  DOB   => bus_data_out 
  );

my_dpr_ibuf : IBUF port map (
  I     => bus_clk,
  O     => bus_iclk
  );

my_dpr_bufg : BUFG port map (
  I     => bus_iclk,
  O     => bus_gclk
  );

-----------------------------------------------
--
-- BED SRAM interface (256KBytes) ($0000-$DFFF)
--
-----------------------------------------------
my_bed_sram : BED_SRAM port map (
    --
    -- CPU Interface signals
    --
    clk       => vga_clk,                        -- VGA Clock (twice the CPU clock)
    rst       => cpu_rst,                        -- Reset input (active high)
    cs        => ram_cs,                         -- RAM Chip Select
    addr(17 downto 12) => dat_addr( 5 downto 0), -- High RAM address goes to the DAT
    addr(11 downto  0) => cpu_addr(11 downto 0), -- Low RAM address goes to the CPU
    rw        => cpu_rw,                         -- Read / Not Write
    data_in   => cpu_data_out,                   -- Data Bus In 
    data_out  => ram_data_out,                   -- Data Bus Out
    --
    -- B3_SRAM Interface Signals
    --
    ram_csn   => ram_csn,
    ram_wrln  => ram_wrln,
    ram_wrun  => ram_wrun,
    ram_addr  => ram_addr,
    ram_data  => ram_data
    );
  	 
----------------------------------------------------------------------
--
-- Process to decode memory map
--
----------------------------------------------------------------------

my_decoder: process( cpu_addr, cpu_rw, cpu_vma,
                     dat_addr,
                     rom_data_out,
                     acia_data_out,
                     kbd_data_out,
                     vdu_data_out,
                     timer_data_out,
                     trap_data_out,
                     spp_data_out,
                     dpr_data_out,
                     pb_data_out,
                     ram_data_out )
begin
   cpu_data_in <= (others=>'0');
   dat_cs      <= '0';
   rom_cs      <= '0';
   acia_cs     <= '0';
   kbd_cs      <= '0';
   vdu_cs      <= '0';
   timer_cs    <= '0';
   trap_cs     <= '0';
   spp_cs      <= '0';
   dpr_cs      <= '0';
   pb_cs       <= '0';
   ram_cs      <= '0';

   if cpu_addr( 15 downto 8 ) = "11111111" then
      --
      -- Dynamic Address Translation $FFF0 - $FFFF
      --
      cpu_data_in <= rom_data_out;
      dat_cs      <= cpu_vma;              -- write DAT
      rom_cs      <= cpu_vma;              -- read  ROM

   elsif (dat_addr(3 downto 0) = "1111") and (cpu_addr(11) = '1') then -- $XF800 - $XFFFF
      --
      -- Sys09Bug Monitor ROM $F000 - $FFFF
      --
      cpu_data_in <= rom_data_out;
      rom_cs      <= cpu_vma;

   elsif dat_addr(3 downto 0) = "1110" then -- $XE000 - $XEFFF
      --
      -- IO Devices $E000 - $E7FF
      --
      case cpu_addr(11 downto 8) is

      --
      -- SWTPC peripherals from $E000 to $E0FF
      --
      when "0000" =>
         case cpu_addr(7 downto 4) is
         --
         -- ACIA RS232 Console Port $E000 - $E00F
         --
         when "0000" => -- $E000
            cpu_data_in <= acia_data_out;
            acia_cs     <= cpu_vma;

         --
         -- Reserved
         -- Floppy Disk Controller port $E010 - $E01F
         --
         when "0001" => -- $E010
            null;

         --
         -- Keyboard port $E020 - $E02F
         --
         when "0010" => -- $E020
            cpu_data_in <= kbd_data_out;
            kbd_cs <= cpu_vma;

         --
         -- VDU port $E030 - $E03F
         --
         when "0011" => -- $E030
            cpu_data_in <= vdu_data_out;
            vdu_cs      <= cpu_vma;

         --
         -- Reserved SWTPc MP-T Timer $E040 - $E04F
         --
         when "0100" => -- $E040
            cpu_data_in <= (others=> '0');

         --
         -- Timer $E050 - $E05F
         --
         when "0101" => -- $E050
            cpu_data_in <= timer_data_out;
            timer_cs    <= cpu_vma;

         --
         -- Bus Trap Logic $E060 - $E06F
         --
         when "0110" => -- $E060
            cpu_data_in <= trap_data_out;
            trap_cs     <= cpu_vma;
 
         --
         -- Bus Trace Logic $E070 - $E07F
         --
--         when "0111" => -- $E070
--            cpu_data_in <= trace_data_out;
--            trace_cs    <= cpu_vma;

         --
         -- Reserved SWTPc MP-ID PIA Timer/Printer Port $E080 - $E08F
         --
         when "1000" => -- $E080
            null;

         --
         -- Reserved SWTPc MP-ID PTM 6840 Timer Port $E090 - $E09F
         --

         --
         -- Simple Parallel Port $E0A0 - $E0AF
         --
         when "1010" => -- $E0A0
            cpu_data_in <= spp_data_out;
            spp_cs      <= cpu_vma;

         --
         -- Remaining 5 slots reserved for non SWTPc Peripherals
         --
         when others => -- $E0B0 to $E0FF
            cpu_data_in <= (others=> '0');

         end case;
      --
      -- XST-3.0 Peripheral Bus goes here
      -- $E100 to $E1FF
      -- Four devices
      -- IDE, Ethernet, Slot1, Slot2
      --
      when "0001" =>
         cpu_data_in <= pb_data_out;
         pb_cs       <= cpu_vma;

      --
      -- $E200 to $EFFF reserved for future use
      --
      when others =>
         cpu_data_in <= dpr_data_out;
         dpr_cs      <= cpu_vma;

      end case;

   --
   -- Flex RAM $0C000 - $0DFFF
   --
-- elsif dat_addr(7 downto 1) = "0000110" then -- $0C000 - $0DFFF
--    cpu_data_in <= flex_data_out;
--    flex_cs     <= cpu_vma;

   --
   -- Everything else is RAM
   --
   else
      cpu_data_in <= ram_data_out;
      ram_cs      <= cpu_vma;
   end if;

end process;

--
-- IDE drive / CF card signals ($E100 - $E13F)
-- Located on peripheral bus
--
ide_bus: process( cpu_rst, cpu_addr, ide_cs )
begin
  ide_cs0_n    <= not( ide_cs ) or cpu_addr(4);
  ide_cs1_n    <= not( ide_cs and cpu_addr(4));
  ide_dmack_n  <= '1';
  ide_rst_n    <= not cpu_rst;
  ide_con_csel <= '0';
  ide_dasp_n   <= not ide_cs;

end process;

--
-- Assign CPU interface signals
--
cpu_controls : process( rst_n, pb_hold,
                        acia_irq, kbd_irq, trap_irq, timer_irq )
begin
  cpu_rst  <= not rst_n; -- CPU reset is active high
  cpu_irq  <= acia_irq or kbd_irq;
  cpu_nmi  <= trap_irq;
  cpu_firq <= timer_irq;
  cpu_halt <= '0';
  cpu_hold <= pb_hold;
end process;

--
-- Assign DPR bus interface signals
--
my_dpr_bus : process( bus_cs_n, bus_rw, cpu_rw )
begin
  bus_cs <= not bus_cs_n;
  bus_wr <= not bus_rw;
  dpr_wr <= not cpu_rw;
--  trace_data_out <= (others=>'0');
end process;

--
-- Assign VDU VGA output signals
-- only 8 colours are handled.
--
my_vga_out: process( vga_red_o, vga_green_o, vga_blue_o )
begin
  vga_red(0)   <= vga_red_o;
  vga_red(1)   <= vga_red_o;
  vga_green(0) <= vga_green_o;
  vga_green(1) <= vga_green_o;
  vga_blue(0)  <= vga_blue_o;
  vga_blue(1)  <= vga_blue_o;
end process;

end rtl; --===================== End of architecture =======================--
