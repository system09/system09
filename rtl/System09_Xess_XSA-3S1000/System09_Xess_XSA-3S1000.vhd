--=============================================================================--
--                                                                             --
--  System09 - Synthesizable System On a Chip - VHDL FPGA core top level file. --
--                                                                             --
--=============================================================================--
--
--
-- File name      : System09_Xess_XSA-3S1000.vhd
--
-- Entity name    : System09
--
-- Purpose        : Top level file for 6809 compatible system on a chip
--                  Designed with Xilinx XC3S1000 Spartan 3 FPGA.
--                  Implemented With XESS XSA-3S1000 FPGA board.
--
-- Dependencies   : ieee.Std_Logic_1164
--                  ieee.std_logic_unsigned
--                  ieee.std_logic_arith
--                  ieee.numeric_std
--                  work.common.all;
--                  WORK.xsasdram.all;
--                  unisim.vcomponents
--
-- Uses           : cpu09         (..\VHDL\cpu09.vhd)               CPU core
--                  dat_ram       (..\VHDL\datram.vhd)              Dynamic Address Translation
--                  monrom        (..\Spartan3\sys09xes_b16.vhd)    Monitor ROM
--                  XSASDRAMCntl  (.\XSASDRAMCntl.vhd)              SDRAM Controller
--                  flex_ram      (..\Spartan3\flex9_ram8k_b16.vhd) Flex operating system
--                  acia6850      (..\VHDL\acia6850.vhd)            ACIA (UART)
--                  ACIA_Clock    (..\VHDL\ACIA_Clock.vhd)          ACIA Baud Rate Clock Divider
--                  keyboard      (..\VHDL\keyboard.vhd)            PS/2 Keyboard register interface
--                  ps2_keyboard  (..\VHDL\ps2_keyboard.vhd)        PS/2 Keyboard interface logic
--                  keymap_rom    (..\Spartan2\keymap_rom.vhd)      PS/2 Keyboard key code look up table
--                  vdu8          (..\VHDL\vdu8.vhd)		        Video Display Unit
--                                (..\Spartan3\char_rom2K_b16.vhd)  Character Generator ROM (B16_RAM)
--                                (..\Spartan3\ram2k_b16.vhd)       Text & Attribute RAM Buffer
--                  timer         (..\VHDL\timer.vhd)               Interrupt timer
--                  trap          (..\VHDL\trap.vhd)                Hardware breakpoint
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
-- $E060 - Reserved for Bus Trap (Hardware Breakpoint Logic)
-- $E070 - Reserved for Trace Buffer
-- $E080 - Reserved for SWTPc MP-ID 6821 PIA (?)
-- $E090 - Reserved for SWTPc MP-ID 6840 PTM (?)
-- $E0A0 - Switches in / LEDS out
-- $E0B0 - 7 Segment display
-- $E0C0 - Reserved
-- $E0D0 - Reserved
-- $E0E0 - Reserved
-- $E0F0 - Reserved
-- $E100 - $E13F Reserved IDE / Compact Flash Card
-- $E140 - $E17F Reserved for Ethernet MAC (XESS)
-- $E180 - $E1BF Reserved for Expansion Slot 0 (XESS)
-- $E1C0 - $E1FF Reserved for Expansion Slot 1 (XESS)
-- $E200 - $EFFF Reserved for Future I/O
-- $F000 - $F7FF RAM for Sys09bug monitor extensions
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
--
--===========================================================================----
--
-- Revision History:
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
-- Version 2.0 - 2 September 2004 - John Kent
-- ported to Digilent Xilinx Spartan3 starter board
--	removed Compact Flash and Trap Logic.
-- Replaced SBUG with KBug9s
--
-- Version 3.0 - 29th August 2006 - John Kent
-- Adapted to XSA-3S1000 board.
-- Removed DAT and miniUART.
-- Used 32KBytes of Block RAM.
--
-- Version 3.1 - 15th January 2007 - John Kent
-- Modified vdu8 interface
-- Added a clock divider
--
-- Version 3.2 - 25th February 2007 - John Kent
-- reinstated ACIA_6850 and ACIA_Clock
-- Updated VDU8 & Keyboard with generic parameters
-- Defined Constants for clock speed calculations
--
-- Version 3.3 - 1st July 2007 - John Kent
-- Made VDU mono to save on one RAMB16
-- Used distributed memory for Key Map ROM to save one RAMB16
-- Added Flex RAM at $C000 to $DFFF using 4 spare RAMB16s
-- Added timer and trap logic
-- Added IDE Interface for Compact Flash
-- Replaced KBug9s and stack with Sys09Bug.
--
-- Version 4.0 - 1st February 2008 - John kent
-- Replaced Block RAM with SDRAM Interface
-- Modified Hold timing for SDRAM
-- Added CF and Ethernet interface 
-- via the 16 bit peripheral bus at $E100
--
-- Version 4.1 - 2011-08-28 - John Kent
-- Updated header
-- Note that some of the revisions may not have been documented
--
--===========================================================================--
library ieee;
   use ieee.std_logic_1164.all;
   use IEEE.STD_LOGIC_ARITH.ALL;
   use IEEE.STD_LOGIC_UNSIGNED.ALL;
   use ieee.numeric_std.all;
library work;
	use work.common.all;
	use WORK.xsasdram.all;
library unisim;
   use unisim.vcomponents.all;

entity system09 is
  port(
    CLKA         : in  Std_Logic;  -- 100MHz Clock input
--    CLKB         : in  Std_Logic;  -- 50MHz Clock input
	 SW2_N        : in  Std_logic;  -- Master Reset input (active low)
	 SW3_N        : in  Std_logic;  -- Non Maskable Interrupt input (active low)

 	 -- PS/2 Keyboard
	 ps2_clk      : inout Std_logic;
	 ps2_dat      : inout Std_Logic;

	 -- CRTC output signals
	 vga_vsync_n  : out Std_Logic;
    vga_hsync_n  : out Std_Logic;
    vga_blue     : out std_logic_vector(2 downto 0);
    vga_green    : out std_logic_vector(2 downto 0);
    vga_red      : out std_logic_vector(2 downto 0);

    -- RS232 Port
	 RS232_RXD    : in  Std_Logic;
	 RS232_TXD    : out Std_Logic;
    RS232_CTS    : in  Std_Logic;
    RS232_RTS    : out Std_Logic;

	 -- Status 7 segment LED
	 S            : out std_logic_vector(7 downto 0);

    -- SDRAM side
    SDRAM_clkfb  : in  std_logic;            -- feedback SDRAM clock after PCB delays
    SDRAM_clkout : out std_logic;            -- clock to SDRAM
    SDRAM_CKE    : out std_logic;            -- clock-enable to SDRAM
    SDRAM_CS_N   : out std_logic;            -- chip-select to SDRAM
    SDRAM_RAS_N  : out std_logic;            -- SDRAM row address strobe
    SDRAM_CAS_N  : out std_logic;            -- SDRAM column address strobe
    SDRAM_WE_N   : out std_logic;            -- SDRAM write enable
    SDRAM_BA     : out std_logic_vector(1 downto 0);  -- SDRAM bank address
    SDRAM_A      : out std_logic_vector(12 downto 0);  -- SDRAM row/column address
    SDRAM_D      : inout  std_logic_vector(15 downto 0);  -- data from SDRAM
    SDRAM_DQMH   : out std_logic;            -- enable upper-byte of SDRAM databus if true
    SDRAM_DQML   : out std_logic;            -- enable lower-byte of SDRAM databus if true

	 -- Peripheral I/O bus $E100 - $E1FF
    PB_RD_N      : out std_logic;
    PB_WR_N      : out std_logic;
    PB_A         : out std_logic_vector(4 downto 0);
    PB_D         : inout std_logic_vector(15 downto 0);

    -- IDE Compact Flash $E100 - $E13F
    ide_dmack_n  : out std_logic;
	 ide_cs0_n    : out std_logic;
	 ide_cs1_n    : out std_logic;

    -- Ethernet $E140 - $E17F
    ether_cs_n   : out std_logic;
    ether_aen    : out std_logic; -- Ethernet address enable not 
    ether_bhe_n  : out std_logic; -- Ethernet bus high enable 
    ether_clk    : in  std_logic; -- Ethernet clock 
    ether_rdy    : in  std_logic; -- Ethernet ready
    ether_irq    : in  std_logic; -- Ethernet irq - Shared with BAR6

    -- Slot 1 $E180 - $E1BF
	 slot1_cs_n   : out std_logic;
--	 slot1_irq    : in  std_logic;

    -- Slot 2 $E1C0 - $E1FF
	 slot2_cs_n   : out std_logic;
--	 slot2_irq    : in  std_logic;

-- CPU Debug Interface signals
--    cpu_reset_o     : out Std_Logic;
--    cpu_clk_o       : out Std_Logic;
--    cpu_rw_o        : out std_logic;
--    cpu_vma_o       : out std_logic;
--    cpu_halt_o      : out std_logic;
--    cpu_hold_o      : out std_logic;
--    cpu_firq_o      : out std_logic;
--    cpu_irq_o       : out std_logic;
--    cpu_nmi_o       : out std_logic;
--    cpu_addr_o      : out std_logic_vector(15 downto 0);
--    cpu_data_in_o   : out std_logic_vector(7 downto 0);
--    cpu_data_out_o  : out std_logic_vector(7 downto 0);
    
	 -- Disable Flash
	 FLASH_CE_N   : out std_logic
	 );
end system09;

-------------------------------------------------------------------------------
-- Architecture for System09
-------------------------------------------------------------------------------
architecture rtl of system09 is

  -----------------------------------------------------------------------------
  -- constants
  -----------------------------------------------------------------------------

  -- SDRAM
  constant MEM_CLK_FREQ         : natural := 100_000; -- operating frequency of Memory in KHz
  constant SYS_CLK_DIV          : real    := 2.0;    -- divisor for FREQ (can only be 1.0, 1.5, 2.0, 2.5, 3.0, 4.0, 5.0, 8.0 or 16.0)
  constant PIPE_EN              : boolean := false;  -- if true, enable pipelined read operations
  constant MAX_NOP              : natural := 10000;  -- number of NOPs before entering self-refresh
  constant MULTIPLE_ACTIVE_ROWS : boolean := false;  -- if true, allow an active row in each bank
  constant DATA_WIDTH           : natural := 16;     -- host & SDRAM data width
  constant NROWS                : natural := 8192;   -- number of rows in SDRAM array
  constant NCOLS                : natural := 512;    -- number of columns in SDRAM array
  constant HADDR_WIDTH          : natural := 24;     -- host-side address width
  constant SADDR_WIDTH          : natural := 13;     -- SDRAM-side address width

  constant SYS_CLK_FREQ         : natural := ((MEM_CLK_FREQ*2)/integer(SYS_CLK_DIV*2.0))*1000;  -- FPGA System Clock
  constant CPU_CLK_FREQ         : natural := 25_000_000;  -- CPU Clock (Hz)
  constant CPU_CLK_DIV          : natural := (SYS_CLK_FREQ/CPU_CLK_FREQ);
  constant VGA_CLK_FREQ         : natural := 25_000_000;  -- VGA Pixel Clock
  constant VGA_CLK_DIV          : natural := ((MEM_CLK_FREQ*1000)/VGA_CLK_FREQ);
  constant BAUD_RATE            : integer := 57600;	  -- Baud Rate
  constant ACIA_CLK_FREQ        : integer := BAUD_RATE * 16;

  constant TRESET               : natural := 300;      -- min initialization interval (us)
  constant RST_CYCLES           : natural := 1+(TRESET*(MEM_CLK_FREQ/1_000));  -- SDRAM power-on initialization interval

  type hold_state_type is ( hold_release_state, hold_request_state );

  -----------------------------------------------------------------------------
  -- Signals
  -----------------------------------------------------------------------------
  -- BOOT ROM
  signal rom_cs         : Std_logic;
  signal rom_data_out   : Std_Logic_Vector(7 downto 0);

  -- Flex Memory & Monitor Stack
  signal flex_cs        : Std_logic;
  signal flex_data_out  : Std_Logic_Vector(7 downto 0);

  -- ACIA/UART Interface signals
  signal acia_data_out  : Std_Logic_Vector(7 downto 0);  
  signal acia_cs        : Std_Logic;
  signal acia_irq       : Std_Logic;
  signal acia_clk       : Std_Logic;
  signal rxd            : Std_Logic;
  signal txd            : Std_Logic;
  signal DCD_n          : Std_Logic;
  signal RTS_n          : Std_Logic;
  signal CTS_n          : Std_Logic;

  -- keyboard port
  signal keyboard_data_out : std_logic_vector(7 downto 0);
  signal keyboard_cs       : std_logic;
  signal keyboard_irq      : std_logic;

  -- RAM
  signal ram_cs         : std_logic; -- memory chip select
  signal ram_data_out   : std_logic_vector(7 downto 0);
  signal ram_rd_req     : std_logic; -- ram read request	(asynch set on ram read, cleared falling CPU clock edge)
  signal ram_wr_req     : std_logic; -- ram write request (set on rising CPU clock edge, asynch clear on acknowledge) 
  signal ram_hold       : std_logic; -- hold off slow accesses
  signal ram_release    : std_logic; -- Release ram hold

  -- CPU Interface signals
  signal cpu_reset      : Std_Logic;
  signal cpu_clk        : Std_Logic;
  signal cpu_rw         : std_logic;
  signal cpu_vma        : std_logic;
  signal cpu_halt       : std_logic;
  signal cpu_hold       : std_logic;
  signal cpu_firq       : std_logic;
  signal cpu_irq        : std_logic;
  signal cpu_nmi        : std_logic;
  signal cpu_addr       : std_logic_vector(15 downto 0);
  signal cpu_data_in    : std_logic_vector(7 downto 0);
  signal cpu_data_out   : std_logic_vector(7 downto 0);

  -- Dynamic Address Translation
  signal dat_cs       : std_logic;
  signal dat_addr     : std_logic_vector(7 downto 0);

  -- Video Display Unit
  signal vdu_cs         : std_logic;
  signal vdu_data_out   : std_logic_vector(7 downto 0);
  signal vga_red_o      : std_logic;
  signal vga_green_o    : std_logic;
  signal vga_blue_o     : std_logic;

  -- timer
  signal timer_data_out : std_logic_vector(7 downto 0);
  signal timer_cs       : std_logic;
  signal timer_irq      : std_logic;

  -- trap
  signal trap_cs        : std_logic;
  signal trap_data_out  : std_logic_vector(7 downto 0);
  signal trap_irq       : std_logic;

  -- Peripheral Bus port
  signal pb_data_out   : std_logic_vector(7 downto 0);
  signal pb_cs         : std_logic;	  -- peripheral bus chip select
  signal pb_wru        : std_logic;	  -- upper byte write strobe
  signal pb_wrl        : std_logic;	  -- lower byte write strobe
  signal pb_rdu        : std_logic;	  -- upper byte read strobe
  signal pb_rdl        : std_logic;	  -- lower byte read strobe
  signal pb_hold       : std_logic;	  -- hold peripheral bus access
  signal pb_release    : std_logic;	  -- release hold of peripheral bus
  signal pb_count      : std_logic_vector(3 downto 0); -- hold counter
  signal pb_hold_state : hold_state_type;
  signal pb_wreg       : std_logic_vector(7 downto 0); -- lower byte write register
  signal pb_rreg       : std_logic_vector(7 downto 0); -- lower byte read register

  -- Peripheral chip selects on Peripheral Bus
  signal ide_cs        : std_logic; -- IDE CF interface
  signal ether_cs      : std_logic;	-- Ethernet interface
  signal slot1_cs      : std_logic;	-- Expansion slot 1
  signal slot2_cs      : std_logic;	-- Expansion slot 2

  signal rst_i         : std_logic;     -- internal reset signal
  signal clk_i         : std_logic;     -- internal master clock signal
  signal lock          : std_logic;     -- SDRAM clock DLL lock indicator

  -- signals that go through the SDRAM host-side interface
  signal opBegun       : std_logic;        -- SDRAM operation started indicator
  signal earlyBegun    : std_logic;        -- SDRAM operation started indicator
  signal ramDone       : std_logic;        -- SDRAM operation complete indicator
  signal rdDone        : std_logic;        -- SDRAM read operation complete indicator
  signal wrDone        : std_logic;        -- SDRAM write operation complete indicator
  signal hAddr         : std_logic_vector(HADDR_WIDTH-1 downto 0);  -- host address bus
  signal hDIn          : std_logic_vector(DATA_WIDTH-1 downto 0);  -- host-side data to SDRAM
  signal hDOut         : std_logic_vector(DATA_WIDTH-1 downto 0);  -- host-side data from SDRAM
  signal hRd           : std_logic;        -- host-side read control signal
  signal hWr           : std_logic;        -- host-side write control signal
  signal hUds          : std_logic;        -- host-side upper data strobe
  signal hLds          : std_logic;        -- host-side lower data strobe
  signal rdPending     : std_logic;        -- read operation pending in SDRAM pipeline
  type ram_type is (ram_state_0, 
                    ram_state_rd1, ram_state_rd2,
                    ram_state_wr1,
						  ram_state_3 );
  signal ram_state     : ram_type;


--  signal BaudCount   : std_logic_vector(5 downto 0);
  signal CountL        : std_logic_vector(23 downto 0);
  signal clk_count     : natural range 0 to CPU_CLK_DIV;
  signal Clk25         : std_logic;
  signal vga_clk       : std_logic;

-----------------------------------------------------------------
--
-- CPU09 CPU core
--
-----------------------------------------------------------------

component cpu09
  port (    
	 clk:	     in	std_logic;
    rst:      in	std_logic;
    vma:	     out	std_logic;
    addr:     out	std_logic_vector(15 downto 0);
    rw:	     out	std_logic;		-- Asynchronous memory interface
	 data_out: out std_logic_vector(7 downto 0);
    data_in:  in	std_logic_vector(7 downto 0);
	 irq:      in  std_logic;
	 firq:     in  std_logic;
	 nmi:      in  std_logic;
	 halt:     in  std_logic;
	 hold:     in  std_logic
  );
end component;


----------------------------------------
--
-- 4K Block RAM Monitor ROM
--
----------------------------------------
component mon_rom
    Port (
       clk   : in  std_logic;
		 rst   : in  std_logic;
		 cs    : in  std_logic;
		 rw    : in  std_logic;
       addr  : in  std_logic_vector (11 downto 0);
       data_out : out std_logic_vector (7 downto 0);
       data_in : in  std_logic_vector (7 downto 0)
    );
end component;


----------------------------------------
--
-- 8KBytes Block RAM for FLEX9
-- $C000 - $DFFF
--
----------------------------------------
component flex_ram
  Port (
    clk      : in  std_logic;
    rst      : in  std_logic;
    cs       : in  std_logic;
    rw       : in  std_logic;
    addr     : in  std_logic_vector (12 downto 0);
    data_out    : out std_logic_vector (7 downto 0);
    data_in    : in  std_logic_vector (7 downto 0)
    );
end component;

-----------------------------------------------------------------
--
-- 6850 Compatible ACIA / UART
--
-----------------------------------------------------------------

component acia6850
  port (
     clk      : in  Std_Logic;  -- System Clock
     rst      : in  Std_Logic;  -- Reset input (active high)
     cs       : in  Std_Logic;  -- miniUART Chip Select
     rw       : in  Std_Logic;  -- Read / Not Write
     addr     : in  Std_Logic;  -- Register Select
     data_in  : in  Std_Logic_Vector(7 downto 0); -- Data Bus In 
     data_out : out Std_Logic_Vector(7 downto 0); -- Data Bus Out
     irq      : out Std_Logic;  -- Interrupt
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
     SYS_CLK_FREQ  : integer :=  SYS_CLK_FREQ;
	  ACIA_CLK_FREQ : integer := ACIA_CLK_FREQ
  );   
  port (
     clk      : in  Std_Logic;  -- System Clock Input
	  ACIA_clk : out Std_logic   -- ACIA Clock output
  );
end component;


----------------------------------------
--
-- PS/2 Keyboard
--
----------------------------------------

component keyboard
  generic(
  KBD_CLK_FREQ : integer := CPU_CLK_FREQ
  );
  port(
  clk             : in    std_logic;
  rst             : in    std_logic;
  cs              : in    std_logic;
  rw              : in    std_logic;
  addr            : in    std_logic;
  data_in         : in    std_logic_vector(7 downto 0);
  data_out        : out   std_logic_vector(7 downto 0);
  irq             : out   std_logic;
  kbd_clk         : inout std_logic;
  kbd_data        : inout std_logic
  );
end component;

----------------------------------------
--
-- Video Display Unit.
--
----------------------------------------
component vdu8
      generic(
        VDU_CLK_FREQ           : integer := CPU_CLK_FREQ; -- HZ
        VGA_CLK_FREQ           : integer := VGA_CLK_FREQ; -- HZ
	     VGA_HOR_CHARS          : integer := 80; -- CHARACTERS
	     VGA_VER_CHARS          : integer := 25; -- CHARACTERS
	     VGA_PIX_PER_CHAR       : integer := 8;  -- PIXELS
	     VGA_LIN_PER_CHAR       : integer := 16; -- LINES
	     VGA_HOR_BACK_PORCH     : integer := 40; -- PIXELS
	     VGA_HOR_SYNC           : integer := 96; -- PIXELS
	     VGA_HOR_FRONT_PORCH    : integer := 24; -- PIXELS
	     VGA_VER_BACK_PORCH     : integer := 13; -- LINES
	     VGA_VER_SYNC           : integer := 2;  -- LINES
	     VGA_VER_FRONT_PORCH    : integer := 35  -- LINES
      );
      port(
		-- control register interface
      vdu_clk      : in  std_logic;	 -- CPU Clock - 25MHz
      vdu_rst      : in  std_logic;
		vdu_cs       : in  std_logic;
		vdu_rw       : in  std_logic;
		vdu_addr     : in  std_logic_vector(2 downto 0);
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
-- Timer module
--
----------------------------------------

component timer
  port (
     clk       : in std_logic;
     rst       : in std_logic;
     cs        : in std_logic;
     rw        : in std_logic;
     addr      : in std_logic;
     data_in   : in std_logic_vector(7 downto 0);
	  data_out  : out std_logic_vector(7 downto 0);
	  irq       : out std_logic
	  );
end component;

------------------------------------------------------------
--
-- Bus Trap logic
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


----------------------------------------
--
-- Dynamic Address Translation Registers
--
----------------------------------------
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


component XSASDRAMCntl
  generic(
    FREQ                 :     natural := MEM_CLK_FREQ;-- operating frequency in KHz
    CLK_DIV              :     real    := SYS_CLK_DIV; -- divisor for FREQ (can only be 1.0, 1.5, 2.0, 2.5, 3.0, 4.0, 5.0, 8.0 or 16.0)
    PIPE_EN              :     boolean := PIPE_EN;     -- if true, enable pipelined read operations
    MAX_NOP              :     natural := MAX_NOP;     -- number of NOPs before entering self-refresh
    MULTIPLE_ACTIVE_ROWS :     boolean := MULTIPLE_ACTIVE_ROWS;  -- if true, allow an active row in each bank
    DATA_WIDTH           :     natural := DATA_WIDTH;  -- host & SDRAM data width
    NROWS                :     natural := NROWS;       -- number of rows in SDRAM array
    NCOLS                :     natural := NCOLS;       -- number of columns in SDRAM array
    HADDR_WIDTH          :     natural := HADDR_WIDTH; -- host-side address width
    SADDR_WIDTH          :     natural := SADDR_WIDTH  -- SDRAM-side address width
    );
  port(
    -- host side
    clk                  : in  std_logic;  -- master clock
    bufclk               : out std_logic;  -- buffered master clock
    clk1x                : out std_logic;  -- host clock sync'ed to master clock (and divided if CLK_DIV>1)
    clk2x                : out std_logic;  -- double-speed host clock
    lock                 : out std_logic;  -- true when host clock is locked to master clock
    rst                  : in  std_logic;  -- reset
    rd                   : in  std_logic;  -- initiate read operation
    wr                   : in  std_logic;  -- initiate write operation
    uds                  : in  std_logic;  -- upper data strobe
    lds                  : in  std_logic;  -- lower data strobe
    earlyOpBegun         : out std_logic;  -- read/write/self-refresh op begun     (async)
    opBegun              : out std_logic;  -- read/write/self-refresh op begun (clocked)
    rdPending            : out std_logic;  -- read operation(s) are still in the pipeline
    done                 : out std_logic;  -- read or write operation is done
    rdDone               : out std_logic;  -- read done and data is available
    hAddr                : in  std_logic_vector(HADDR_WIDTH-1 downto 0);  -- address from host
    hDIn                 : in  std_logic_vector(DATA_WIDTH-1 downto 0);  -- data from host
    hDOut                : out std_logic_vector(DATA_WIDTH-1 downto 0);  -- data to host
    status               : out std_logic_vector(3 downto 0);  -- diagnostic status of the FSM         

    -- SDRAM side
    sclkfb               : in    std_logic;           -- clock from SDRAM after PCB delays
    sclk                 : out   std_logic;           -- SDRAM clock sync'ed to master clock
    cke                  : out   std_logic;           -- clock-enable to SDRAM
    cs_n                 : out   std_logic;           -- chip-select to SDRAM
    ras_n                : out   std_logic;           -- SDRAM row address strobe
    cas_n                : out   std_logic;           -- SDRAM column address strobe
    we_n                 : out   std_logic;           -- SDRAM write enable
    ba                   : out   std_logic_vector(1 downto 0);  -- SDRAM bank address bits
    sAddr                : out   std_logic_vector(SADDR_WIDTH-1 downto 0);  -- SDRAM row/column address
    sData                : inout std_logic_vector(DATA_WIDTH-1 downto 0);  -- SDRAM in/out databus
    dqmh                 : out   std_logic;           -- high databits I/O mask
    dqml                 : out   std_logic            -- low databits I/O mask
    );
end component;

--
-- Clock buffer
--
component BUFG 
   Port (
     i: in std_logic;
	  o: out std_logic
  );
end component;

begin
  -----------------------------------------------------------------------------
  -- Instantiation of internal components
  -----------------------------------------------------------------------------

my_cpu : cpu09  port map (    
	 clk	     => cpu_clk,
    rst       => cpu_reset,
    vma       => cpu_vma,
    addr      => cpu_addr(15 downto 0),
    rw	     => cpu_rw,
	 data_out  => cpu_data_out,
    data_in   => cpu_data_in,
	 irq       => cpu_irq,
	 firq      => cpu_firq,
	 nmi       => cpu_nmi,
	 halt      => cpu_halt,
	 hold      => cpu_hold
  );

my_rom : mon_rom port map (
       clk   => cpu_clk,
		 rst   => cpu_reset,
		 cs    => rom_cs,
		 rw    => '1',
       addr  => cpu_addr(11 downto 0),
       data_in => cpu_data_out,
       data_out => rom_data_out
    );

my_flex : flex_ram port map (
    clk       => cpu_clk,
    rst       => cpu_reset,
	 cs        => flex_cs,
	 rw        => cpu_rw,
    addr      => cpu_addr(12 downto 0),
    data_out     => flex_data_out,
    data_in     => cpu_data_out
    );

my_acia  : acia6850 port map (
	 clk	     => cpu_clk,
	 rst       => cpu_reset,
    cs        => acia_cs,
	 rw        => cpu_rw,
    addr      => cpu_addr(0),
	 data_in   => cpu_data_out,
	 data_out  => acia_data_out,
    irq       => acia_irq,
	 RxC       => acia_clk,
	 TxC       => acia_clk,
	 RxD       => rxd,
	 TxD       => txd,
	 DCD_n     => dcd_n,
	 CTS_n     => cts_n,
	 RTS_n     => rts_n
	 );


my_ACIA_Clock : ACIA_Clock
  generic map(
    SYS_CLK_FREQ  =>  SYS_CLK_FREQ,
	 ACIA_CLK_FREQ => ACIA_CLK_FREQ
  ) 
  port map(
    clk        => Clk_i,
    acia_clk   => acia_clk
  ); 

----------------------------------------
--
-- PS/2 Keyboard Interface
--
----------------------------------------
my_keyboard : keyboard
   generic map (
	KBD_CLK_FREQ => CPU_CLK_FREQ
	) 
   port map(
	clk          => cpu_clk,
	rst          => cpu_reset,
	cs           => keyboard_cs,
	rw           => cpu_rw,
	addr         => cpu_addr(0),
	data_in      => cpu_data_out(7 downto 0),
	data_out     => keyboard_data_out(7 downto 0),
	irq          => keyboard_irq,
	kbd_clk      => ps2_clk,
	kbd_data     => ps2_dat
	);

----------------------------------------
--
-- Video Display Unit instantiation
--
----------------------------------------
my_vdu : vdu8 
  generic map(
      VDU_CLK_FREQ           => CPU_CLK_FREQ, -- HZ
      VGA_CLK_FREQ           => VGA_CLK_FREQ, -- HZ
	   VGA_HOR_CHARS          => 80, -- CHARACTERS
	   VGA_VER_CHARS          => 25, -- CHARACTERS
	   VGA_PIX_PER_CHAR       => 8,  -- PIXELS
	   VGA_LIN_PER_CHAR       => 16, -- LINES
	   VGA_HOR_BACK_PORCH     => 40, -- PIXELS
	   VGA_HOR_SYNC           => 96, -- PIXELS
	   VGA_HOR_FRONT_PORCH    => 24, -- PIXELS
	   VGA_VER_BACK_PORCH     => 13, -- LINES
	   VGA_VER_SYNC           => 2,  -- LINES
	   VGA_VER_FRONT_PORCH    => 35  -- LINES
  )
  port map(

		-- Control Registers
		vdu_clk       => cpu_clk,					 -- 12.5 MHz System Clock in
      vdu_rst       => cpu_reset,
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
      vga_hsync_o   => vga_hsync_n,
      vga_vsync_o   => vga_vsync_n
   );

----------------------------------------
--
-- Timer Module
--
----------------------------------------
my_timer  : timer port map (
    clk       => cpu_clk,
	 rst       => cpu_reset,
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
    rst        => cpu_reset,
    cs         => trap_cs,
    rw         => cpu_rw,
	 vma        => cpu_vma,
    addr       => cpu_addr,
    data_in    => cpu_data_out,
	 data_out   => trap_data_out,
	 irq        => trap_irq
    );


my_dat : dat_ram port map (
    clk       => cpu_clk,
	 rst       => cpu_reset,
	 cs        => dat_cs,
	 rw        => cpu_rw,
	 addr_hi   => cpu_addr(15 downto 12),
	 addr_lo   => cpu_addr(3 downto 0),
    data_in   => cpu_data_out,
	 data_out  => dat_addr(7 downto 0)
	 );

  ------------------------------------------------------------------------
  -- Instantiate the SDRAM controller that connects to the memory tester
  -- module and interfaces to the external SDRAM chip.
  ------------------------------------------------------------------------
  u1 : xsaSDRAMCntl
    generic map(
      FREQ                 => MEM_CLK_FREQ,
		CLK_DIV              => SYS_CLK_DIV,
      PIPE_EN              => PIPE_EN,
		MAX_NOP              => MAX_NOP,
      MULTIPLE_ACTIVE_ROWS => MULTIPLE_ACTIVE_ROWS,
      DATA_WIDTH           => DATA_WIDTH,
      NROWS                => NROWS,
      NCOLS                => NCOLS,
      HADDR_WIDTH          => HADDR_WIDTH,
      SADDR_WIDTH          => SADDR_WIDTH
      )
    port map(
	   -- Host Side
      clk                  => CLKA,     -- master clock from external clock source (unbuffered)
      bufclk               => open,     -- buffered master clock output
      clk1x                => clk_i,    -- synchronized master clock (accounts for delays to external SDRAM)
      clk2x                => open,     -- synchronized doubled master clock
      lock                 => lock,     -- DLL lock indicator
      rst                  => rst_i,    -- reset
      rd                   => hRd,      -- host-side SDRAM read control from memory tester
      wr                   => hWr,      -- host-side SDRAM write control from memory tester
      uds                  => hUds,     -- host-side SDRAM upper data strobe
      lds                  => hLds,     -- host-side SDRAM lower data strobe
      rdPending            => rdPending,-- read operation to SDRAM is in progress
      opBegun              => opBegun,  -- indicates memory read/write has begun
      earlyOpBegun         => earlyBegun,  -- early indicator that memory operation has begun
      rdDone               => rdDone,   -- indicates SDRAM memory read operation is done
      done                 => ramDone, -- indicates SDRAM memory read or write operation is done
      hAddr                => hAddr,    -- host-side address from memory tester to SDRAM
      hDIn                 => hDIn,     -- test data pattern from memory tester to SDRAM
      hDOut                => hDOut,    -- SDRAM data output to memory tester
      status               => open,     -- SDRAM controller state (for diagnostics)
		-- SDRAM Side
      sclkfb               => SDRAM_clkfb,    -- clock feedback with added external PCB delays
      sclk                 => SDRAM_clkout,   -- synchronized clock to external SDRAM
      cke                  => SDRAM_cke,      -- SDRAM clock enable
      cs_n                 => SDRAM_cs_n,     -- SDRAM chip-select
      ras_n                => SDRAM_ras_n,    -- SDRAM RAS
      cas_n                => SDRAM_cas_n,    -- SDRAM CAS
      we_n                 => SDRAM_we_n,     -- SDRAM write-enable
      ba                   => SDRAM_ba,       -- SDRAM bank address
      sAddr                => SDRAM_A,        -- SDRAM address
      sData                => SDRAM_D,        -- SDRAM databus
      dqmh                 => SDRAM_dqmh,     -- SDRAM DQMH
      dqml                 => SDRAM_dqml      -- SDRAM DQML
      );

cpu_clk_buffer : BUFG port map(
    i => Clk25,
	 o => cpu_clk
    );	 

vga_clk_buffer : BUFG port map(
    i => Clk25,
	 o => vga_clk
    );	 
  	 
----------------------------------------------------------------------
--
-- Process to decode memory map
--
----------------------------------------------------------------------

mem_decode: process( cpu_addr, cpu_rw, cpu_vma,
							dat_addr,
					      rom_data_out,
							flex_data_out,
						   acia_data_out,
							keyboard_data_out,
							vdu_data_out,
							pb_data_out,
							timer_data_out,
							trap_data_out,
							ram_data_out
							)
begin
      cpu_data_in <= (others=>'0');
      dat_cs      <= '0';
      rom_cs      <= '0';
      flex_cs     <= '0';
	   acia_cs     <= '0';
	   keyboard_cs <= '0';
	   vdu_cs      <= '0';
	   timer_cs    <= '0';
	   trap_cs     <= '0';
	   pb_cs       <= '0';
	   ide_cs      <= '0';
	   ether_cs    <= '0';
	   slot1_cs    <= '0';
	   slot2_cs    <= '0';
	   ram_cs      <= '0';
      if cpu_addr( 15 downto 8 ) = "11111111" then
	      cpu_data_in <= rom_data_out;
         dat_cs      <= cpu_vma;              -- write DAT
         rom_cs      <= cpu_vma;              -- read  ROM
	   --
		-- Sys09Bug Monitor ROM $F000 - $FFFF
		--
	   elsif dat_addr(3 downto 0) = "1111" then -- $XF000 - $XFFFF
		   cpu_data_in <= rom_data_out;
			rom_cs      <= cpu_vma;

      --
		-- IO Devices $E000 - $E7FF
		--
		elsif dat_addr(3 downto 0) = "1110" then -- $XE000 - $XEFFF
			case cpu_addr(11 downto 8) is
			--
			-- SWTPC peripherals from $E000 to $E0FF
			--
			when "0000" =>
		     case cpu_addr(7 downto 4) is
			  --
			  -- Console Port ACIA $E000 - $E00F
			  --
			  when "0000" => -- $E000
		       cpu_data_in <= acia_data_out;
			    acia_cs     <= cpu_vma;

           --
           -- Reserved
			  -- Floppy Disk Controller port $E010 - $E01F
			  --

           --
           -- Keyboard port $E020 - $E02F
			  --
			  when "0010" => -- $E020
             cpu_data_in <= keyboard_data_out;
			    keyboard_cs <= cpu_vma;

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
			  -- Reserved SWTPc MP-ID PIA Timer/Printer Port $E080 - $E08F
			  --

           --
			  -- Reserved SWTPc MP-ID PTM 6840 Timer Port $E090 - $E09F
			  --

			  --
			  -- Remaining 6 slots reserved for non SWTPc Peripherals
			  --
			  when others => -- $E0A0 to $E0FF
			    null;
		     end case;
			--
			-- XST-3.0 Peripheral Bus goes here
			--	$E100 to $E1FF
			--	Four devices
			-- IDE, Ethernet, Slot1, Slot2
			--
			when "0001" =>
			  cpu_data_in <= pb_data_out;
			  pb_cs       <= cpu_vma;
		     case cpu_addr(7 downto 6) is
			  --
			  -- IDE Interface $E100 to $E13F
			  --
			  when "00" =>
			    ide_cs   <= cpu_vma;
			  --
			  -- Ethernet Interface $E140 to $E17F
			  --
			  when "01" =>
			    ether_cs <= cpu_vma;
			  --
			  -- Slot 1 Interface $E180 to $E1BF
			  --
			  when "10" =>
			    slot1_cs <= cpu_vma;
			  --
			  -- Slot 2 Interface $E1C0 to $E1FF
			  --
			  when "11" =>
			    slot2_cs <= cpu_vma;
           --
			  -- Nothing else
			  --
           when others =>
			    null;
           end case;
         --
			--	$E200 to $EFFF reserved for future use
			--
        	when others =>
			  null;
         end case;
		--
		-- Flex RAM $0C000 - $0DFFF
		--
		elsif dat_addr(7 downto 1) = "0000110" then -- $0C000 - $0DFFF
		   cpu_data_in <= flex_data_out;
			flex_cs     <= cpu_vma;
		--
		-- Everything else is RAM
		--
		else
 		   cpu_data_in <= ram_data_out;
		   ram_cs      <= cpu_vma;
	  end if;
end process;


--
-- 16-bit Peripheral Bus
-- 6809 Big endian
-- ISA bus little endian
-- Not sure about IDE interface
--
peripheral_bus: process( clk_i, cpu_reset, cpu_rw, cpu_addr, cpu_data_out,
                         pb_cs, pb_wreg, pb_rreg )
begin
  pb_wru <= pb_cs and (not cpu_rw) and (not cpu_addr(0));
  pb_wrl <= pb_cs and (not cpu_rw) and      cpu_addr(0) ;
  pb_rdu <= pb_cs and      cpu_rw  and (not cpu_addr(0));
  pb_rdl <= pb_cs and      cpu_rw  and      cpu_addr(0) ;
  pb_a   <= cpu_addr(5 downto 1);

  --
  -- Register upper byte from CPU on first CPU write
  -- and lower byte from the peripheral bus on first CPU read
  --
  if cpu_reset = '1' then
    pb_wreg <= (others => '0');
    pb_rreg <= (others => '0');
  elsif clk_i'event and clk_i ='1' then
    if pb_wru = '1' then
	   pb_wreg <= cpu_data_out;
    end if;
    if pb_rdu = '1' then
	   pb_rreg <= pb_d(7 downto 0);
    end if;
   end if;
  --
  -- Peripheral bus read and write strobes are
  -- Syncronized with the 50 MHz clock
  -- and are asserted until the peripheral bus hold is released
  --
  if cpu_reset = '1' then
    pb_wr_n <= '1';
    pb_rd_n <= '1';
  elsif clk_i'event and clk_i ='1' then
	 if pb_hold = '1' then
	   pb_wr_n  <= not pb_wrl;
	   pb_rd_n  <= not pb_rdu;
    else
      pb_wr_n <= '1';
      pb_rd_n <= '1';
    end if;
  end if;
  --
  -- The peripheral bus will be an output 
  -- the registered even byte on data(15 downto 8)
  -- and the CPU odd bytes on data(7 downto 0)
  -- on odd byte writes
  --
  if pb_wrl = '1' then
    pb_d <= pb_wreg & cpu_data_out;
  else
    pb_d <= (others => 'Z');
  end if;

  --
  -- On even byte reads,
  -- the CPU reads the low (even) byte of the peripheral bus
  -- On odd byte reads,
  -- the CPU reads the registered (odd byte) input from the peripheral bus
  --
  if pb_rdu = '1' then
    pb_data_out <= pb_d(15 downto 8);
  elsif pb_rdl = '1' then
    pb_data_out <= pb_rreg;
  else
    pb_data_out <= (others => '0');
  end if;
  
end process;

--
-- Hold Peripheral bus accesses for a few cycles
--
peripheral_bus_hold: process( cpu_clk, cpu_reset, pb_rdu, pb_wrl ) --, ether_rdy )
begin
    if cpu_reset = '1' then
		 pb_release    <= '0';
		 pb_count      <= "0000";
	    pb_hold_state <= hold_release_state;
	 elsif rising_edge(cpu_clk) then
  --
  -- The perpheral bus hold signal should be generated on 
  -- 16 bit bus read which will be on even byte reads or 
  -- 16 bit bus write which will be on odd byte writes.
  -- 
	    case pb_hold_state is
		 when hold_release_state =>
          pb_release <= '0';
		    if (pb_rdu = '1') or (pb_wrl = '1') then
			    pb_count      <= "0100";
				 pb_hold_state <= hold_request_state;
          elsif (pb_rdl = '1') or (pb_wru = '1') then
             pb_release    <= '1';
				 pb_hold_state <= hold_release_state;
			 end if;

		 when hold_request_state =>
			 if pb_count = "0000" then
--            if ether_rdy = '1' then
              pb_release    <= '1';
				  pb_hold_state <= hold_release_state;
--            end if;
          else
		       pb_count <= pb_count - "0001";
			 end if;
       when others =>
		    null;
       end case;
	 end if;
end process;

--
-- Compact Flash Control
--
compact_flash: process( ide_cs, cpu_addr )
begin
	 ide_cs0_n  <= not( ide_cs ) or cpu_addr(4);
	 ide_cs1_n  <= not( ide_cs and cpu_addr(4));
	 ide_dmack_n  <= '1';
end process;

--
-- Interrupts and other bus control signals
--
interrupts : process( SW3_N,
							 pb_cs, pb_hold, pb_release, ram_hold,
--							 ether_irq, 
                      acia_irq, 
							 keyboard_irq, 
							 trap_irq, 
							 timer_irq
							 )
begin
    pb_hold    <= pb_cs and (not pb_release);
    cpu_irq    <= acia_irq or keyboard_irq;
	 cpu_nmi    <= trap_irq or not( SW3_N );
	 cpu_firq   <= timer_irq;
	 cpu_halt   <= '0';
	 cpu_hold   <= pb_hold or ram_hold;
    FLASH_CE_N <= '1';
end process;


--
-- Flash 7 segment LEDS
--
my_led_flasher: process( clk_i, rst_i, CountL )
begin
    if rst_i = '1' then
		   CountL <= "000000000000000000000000";
    elsif rising_edge(clk_i) then
		   CountL <= CountL + 1;
    end if;
--	 S(7 downto 0) <= CountL(23 downto 16);
end process;

--
-- Generate CPU & Pixel Clock from Memory Clock
--
my_prescaler : process( clk_i, clk_count )
begin
  if rising_edge( clk_i ) then

    if clk_count = 0 then
	   clk_count <= CPU_CLK_DIV-1;
	 else
      clk_count <= clk_count - 1;
	 end if;

    if clk_count = 0 then
	    clk25 <= '0';
    elsif clk_count = (CPU_CLK_DIV/2) then
	    clk25 <= '1';
    end if;

  end if;
end process;

--
-- Reset button and reset timer
--
my_switch_assignments : process( rst_i, SW2_N, lock )
begin
  rst_i <= not SW2_N;
  cpu_reset <= rst_i or (not lock);
end process;

--
-- RS232 signals:
--
my_acia_assignments : process( RS232_RXD, RS232_CTS, txd, rts_n )
begin
  rxd       <= RS232_RXD;
  cts_n     <= RS232_CTS;
  dcd_n     <= '0';
  RS232_TXD <= txd;
  RS232_RTS <= rts_n;
end process;

--
-- Pin assignments for ethernet controller
--
my_ethernet_assignments : process( clk_i, cpu_reset, ether_cs )
begin
    ether_cs_n  <= not ether_cs;
    ether_aen   <= not ether_cs; -- Ethernet address enable not 
    ether_bhe_n <= '1';          -- Ethernet bus high enable - 8 bit access only
end process;

--
-- I/O expansion slot assignments
--
my_slot_assignments : process( slot1_cs, slot2_cs)
begin
    slot1_cs_n <= not slot1_cs;
    slot2_cs_n <= not slot2_cs;
end process;

--
-- VGA ouputs
--
my_vga_assignments : process( vga_red_o, vga_green_o, vga_blue_o )
begin
  VGA_red(0)   <= vga_red_o;
  VGA_red(1)   <= vga_red_o;
  VGA_red(2)   <= vga_red_o;
  VGA_green(0) <= vga_green_o;
  VGA_green(1) <= vga_green_o;
  VGA_green(2) <= vga_green_o;
  VGA_blue(0)  <= vga_blue_o;
  VGA_blue(1)  <= vga_blue_o;
  VGA_blue(2)  <= vga_blue_o;
end process;

--
-- SDRAM read write control
--
my_sdram_rw : process( clk_i, cpu_reset, 
                       opBegun, ramDone,
							  ram_state,
                       ram_rd_req, ram_wr_req )
begin
  if( cpu_reset = '1' ) then
	 hRd        <= '0';
    hWr        <= '0';
	 ram_hold   <= '0';
	 ram_state  <= ram_state_0;

  elsif( falling_edge(clk_i) ) then
    --
	 -- ram state machine
	 --
    case ram_state is

    when ram_state_0 =>
		if ram_rd_req = '1' then 
        ram_hold   <= '1';
	     hRd        <= '1';
		  ram_state  <= ram_state_rd1;
      elsif ram_wr_req = '1' then
	     ram_hold   <= '1';
        hWr        <= '1';
	     ram_state  <= ram_state_wr1;
      end if;

    when ram_state_rd1 =>
	   if opBegun = '1' then
		  hRd        <= '0';
		  ram_state  <= ram_state_rd2;
      end if;

    when ram_state_rd2 =>
	   if ramDone = '1' then
		  ram_hold   <= '0';
		  ram_state  <= ram_state_3;
		end if;

    when ram_state_wr1 =>
	   if opBegun = '1' then
		  ram_hold   <= '0';
		  hWr        <= '0';
		  ram_state  <= ram_state_3;
      end if;

    when ram_state_3 =>
	   if ram_release = '1' then
		  ram_state  <= ram_state_0;
      end if;

	 when others =>
		hRd        <= '0';
		hWr        <= '0';
		ram_hold   <= '0';
		ram_state  <= ram_state_0;
	 end case;  	  

  end if;
end process;

--
-- SDRAM Address and data bus assignments
--
my_sdram_addr_data : process( cpu_addr, dat_addr,
                                cpu_data_out, hDout )
begin
  hAddr(23 downto 19)  <= "00000";
  hAddr(18 downto 11)  <= dat_addr;
  hAddr(10 downto 0)   <= cpu_addr(11 downto 1);
  hUds                 <= not cpu_addr(0);
  hLds                 <=     cpu_addr(0);
  if cpu_addr(0) = '0' then
     hDin( 7 downto 0) <= (others=>'0');
     hDin(15 downto 8) <= cpu_data_out;
     ram_data_out      <= hDout(15 downto 8);
  else
     hDin( 7 downto 0) <= cpu_data_out;
     hDin(15 downto 8) <= (others=>'0');
     ram_data_out      <= hDout( 7 downto 0);
  end if;
end process;

--
-- Hold RAM until falling CPU clock edge
--
ram_bus_hold: process( cpu_clk, cpu_reset, ram_hold )
begin
    if ram_hold = '1' then
		 ram_release   <= '0';
	 elsif falling_edge(cpu_clk) then
		 ram_release   <= '1';
	 end if;
end process;

--
-- CPU read data request on rising CPU clock edge
--
ram_read_request: process( hRd, cpu_clk, ram_cs, cpu_rw, ram_release )
begin
	 if hRd = '1' then
		ram_rd_req   <= '0';
	 elsif rising_edge(cpu_clk) then
	   if (ram_cs = '1') and (cpu_rw = '1') and (ram_release = '1') then
		  ram_rd_req   <= '1';
      end if;
 	 end if;
end process;

--
-- CPU write data to RAM valid on rising CPU clock edge
--
ram_write_request: process( hWr, cpu_clk, ram_cs, cpu_rw, ram_release )
begin
    if hWr = '1' then
		 ram_wr_req   <= '0';
	 elsif rising_edge(cpu_clk) then
	 	if (ram_cs = '1') and (cpu_rw = '0') and (ram_release = '1') then
		  ram_wr_req   <= '1';
      end if;
	 end if;
end process;



status_leds : process( rst_i, cpu_reset, lock )
begin
    S(0) <= rst_i;
	 S(1) <= cpu_reset;
	 S(2) <= lock;
	 S(3)	<= countL(23);
	 S(7 downto 4) <= "0000";
end process;

--debug_proc : process( cpu_reset, cpu_clk, cpu_rw, cpu_vma,
--                      cpu_halt, cpu_hold,
--                      cpu_firq, cpu_irq, cpu_nmi,
--                      cpu_addr, cpu_data_out, cpu_data_in )
--begin
--  cpu_reset_o    <= cpu_reset;
--  cpu_clk_o      <= cpu_clk;
--  cpu_rw_o       <= cpu_rw;
--  cpu_vma_o      <= cpu_vma;
--  cpu_halt_o     <= cpu_halt;
--  cpu_hold_o     <= cpu_hold;
--  cpu_firq_o     <= cpu_firq;
--  cpu_irq_o      <= cpu_irq;
--  cpu_nmi_o      <= cpu_nmi;
--  cpu_addr_o     <= cpu_addr;
--  cpu_data_out_o <= cpu_data_out;
--  cpu_data_in_o  <= cpu_data_in;
--end process;


end rtl; --===================== End of architecture =======================--

