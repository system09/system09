--=============================================================================--
--                                                                             --
--  System09 - Synthesizable System On a Chip - VHDL FPGA core top level file. --
--                                                                             --
--=============================================================================--
--
--
-- File name      : System09_Xess_XuLA.vhd
--
-- Entity name    : System09
--
-- Purpose        : Top level file for 6809 compatible system on a chip
--                  Designed with Xilinx XC3S200A Spartan 3A FPGA.
--                  Implemented with XESS XuLA board
--
-- Dependencies   : ieee.Std_Logic_1164
--                  ieee.std_logic_unsigned
--                  ieee.std_logic_arith
--                  ieee.numeric_std
--
-- Uses           : XuLA_clk      (..\VHDL\XuLA_clk.vhd)           DCM clock generator
--                  cpu09         (..\VHDL\cpu09.vhd)              CPU core
--                  mon_rom       (..\Spartan3\sys09bug_xes_rom4K_b16.vhd)   Monitor ROM
--                  acia6850      (..\VHDL\acia6850.vhd)           ACIA (UART)
--                  ACIA_Clock    (..\VHDL\ACIA_Clock.vhd)         ACIA Baud Rate Clock Divider
--                  keyboard      (..\VHDL\keyboard.vhd)           PS/2 Keyboard register interface
--                  ps2_keyboard  (..\VHDL\ps2_keyboard.vhd)       PS/2 Keyboard interface logic
--                  keymap_rom    (..\Spartan3\keymap_rom_b4.vhd)  PS/2 Keyboard key code look up table
--                  vdu8          (..\VHDL\vdu8.vhd)		          Video Display Unit
--                  vdu_char_rom  (..\Spartan3\char_rom2K_b16.vhd) Character Generator ROM (B16_RAM)
--                  char_buff_ram (..\Spartan3\ram2k_b16.vhd)      Text RAM Buffer
--                  attr_buff_ram (..\Spartan3\ram2k_b16.vhd)      Attribute RAM Buffer
--                  timer         (..\VHDL\timer.vhd)              Timer
--                  xula_ioport   (..\VHDL\xula_ioport.vhd)        Parallel I/O port for the XuLA board
--                  dat_ram       (..\VHDL\datram.vhd)             Dynamic Address Translation
--                  XSASDRAMCntl  (.\xsasdramcntl.vhd)             SDRAM Controller for the XuLA board
--                  sdramCntl     (.\sdramcntl.vhd)                SDRAM Controller for the XuLA board
--                  XSASDRAM      (.\xsasdramcntl.vhd)             SDRAM Controller for the XuLA board
--                  common        (.\common.vhd)
--                  sdram         (.\sdramcntl.vhd)
--                  XuLA          (.\XuLA.ucf)                     Pin assignments for the XuLA board
-- 
-- Author         : John E. Kent      
--                  dilbert57@opencores.org      
--
-- Memory Map     :
--
-- $0000 - $DFFF System RAM (1MB Mapped via DAT)
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
-- $E0A0 - Reserved for Switches in / LEDS out
-- $E0B0 - IO Port
-- $E0C0 - $E0FF Reserved for future I/O
-- $E100 - $EFFF Reserved for Future I/O
-- $F000 - $F7FF RAM for Sys09bug monitor extensions
-- $F800 - $FFFF Sys09bug ROM (Read only)
-- $FFF0 - $FFFF DAT - Dynamic Address Translation (Write Only)
--
--
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
--===========================================================================--
--
--                              Revision History:
--
--===========================================================================--
--
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
-- Version 5.0 - 30th April 2011
-- XESS XSA-3S1000 version Ported to the XESS XuLA board
-- Peripheral bus removed
-- UDS & LDS removed from SDRAM controller 
-- as the XuLA board does not support them
-- Updated some of the signal names 
-- as the XSA-3S1000 version appears to be out of date.
-- Added a DCM clock manager to generate 24MHz CPU & VGA clocks
-- and 48MHz system clock for the SDRAM from the 12MHz FPGA clock.
-- Removed Flex RAM at $C000-$DFFF although it could be reinstated
-- as there are 4 spare 2KByte block RAMs
--
--===========================================================================--
library ieee;
   use ieee.std_logic_1164.all;
   use IEEE.STD_LOGIC_ARITH.ALL;
   use IEEE.STD_LOGIC_UNSIGNED.ALL;
   use ieee.numeric_std.all;
--library work;
--	use work.common.all;
--	use WORK.xsasdram.all;
library unisim;
   use unisim.vcomponents.all;

entity My_System09 is
  port(
    FPGA_CLK     : in    std_Logic;  -- 12MHz Clock input
	 SW2_N        : in    std_logic;  -- Master Reset input (active low)
	 SW3_N        : in    std_logic;  -- Non Maskable Interrupt input (active low)

 	 -- PS/2 Keyboard
	 keyb_clk     : inout std_logic;
	 keyb_dat     : inout std_Logic;
	 
	 -- CRTC output signals
    vga_red      : out   std_logic;
    vga_green    : out   std_logic;
    vga_blue     : out   std_logic;
    vga_hsync_n  : out   std_Logic;
	 vga_vsync_n  : out   std_Logic;

    -- RS232 Port
	 RS232_RXD    : in    std_Logic;
	 RS232_TXD    : out   std_Logic;
    RS232_CTS    : in    std_Logic;
    RS232_RTS    : out   std_Logic;

    -- I/O Port
	 PA           : inout std_logic_vector(7 downto 0);
	 PB           : inout std_logic_vector(4 downto 0);
	 PC           : in    std_logic_vector(7 downto 5);
	 
    -- SDRAM side
    SDRAM_CLKFB  : in    std_logic;            -- feedback SDRAM clock after PCB delays
    SDRAM_CLK    : out   std_logic;            -- clock to SDRAM
    SDRAM_RAS_N  : out   std_logic;            -- SDRAM row address strobe
    SDRAM_CAS_N  : out   std_logic;            -- SDRAM column address strobe
    SDRAM_WE_N   : out   std_logic;            -- SDRAM write enable
    SDRAM_BS     : out   std_logic;            -- SDRAM bank address
    SDRAM_A      : out   std_logic_vector(11 downto 0);  -- SDRAM row/column address
    SDRAM_D      : inout std_logic_vector(15 downto 0)  -- data from SDRAM
	 );
end My_System09;

-------------------------------------------------------------------------------
-- Architecture for System09
-------------------------------------------------------------------------------
architecture rtl of My_System09 is

  -----------------------------------------------------------------------------
  -- constants
  -----------------------------------------------------------------------------
  constant SYS_CLK_FREQ  : integer :=  48_000_000;  -- FPGA System Clock
  constant VGA_CLK_FREQ  : integer :=  24_000_000;  -- VGA Pixel Clock
  constant CPU_CLK_FREQ  : integer :=  24_000_000;  -- CPU Clock
  constant BAUD_RATE     : integer :=      57_600;  -- Baud Rate
  constant ACIA_CLK_FREQ : integer := BAUD_RATE * 16;

  type hold_state_type is ( hold_release_state, hold_request_state );

  -----------------------------------------------------------------------------
  -- Signals
  -----------------------------------------------------------------------------
  signal sys_clk        : std_logic;  -- 48MHz system clock
  signal rst_n          : std_logic;  -- Master Reset input (active low)
  signal nmi_n          : std_logic;  -- Non Maskable Interrupt input (active low)

  -- BOOT ROM
  signal rom_cs         : Std_logic;
  signal rom_data_out   : Std_Logic_Vector(7 downto 0);

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

  -- Keyboard port
  signal key_data_out   : std_logic_vector(7 downto 0);
  signal key_cs         : std_logic;
  signal key_irq        : std_logic;

  -- IO port
  signal iop_data_out   : std_logic_vector(7 downto 0);
  signal iop_cs         : std_logic;
  signal iop_irq        : std_logic;

  -- RAM
  signal ram_cs         : std_logic; -- memory chip select
  signal ram_data_out   : std_logic_vector(7 downto 0);
  signal ram_hold       : std_logic; -- hold off slow accesses

  -- CPU Interface signals
  signal cpu_rst        : Std_Logic;
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
  signal vga_clk        : std_logic;

  -- timer
  signal timer_data_out : std_logic_vector(7 downto 0);
  signal timer_cs       : std_logic;
  signal timer_irq      : std_logic;

-- SDRAM

  constant  FREQ                 :     natural := (SYS_CLK_FREQ/1000); -- operating frequency in KHz
  constant  CLK_DIV              :     real    := 1.0;    -- divisor for FREQ (can only be 1.0, 1.5, 2.0, 2.5, 3.0, 4.0, 5.0, 8.0 or 16.0)
  constant  PIPE_EN              :     boolean := false;  -- if true, enable pipelined read operations
  constant  MAX_NOP              :     natural := 10000;  -- number of NOPs before entering self-refresh
  constant  MULTIPLE_ACTIVE_ROWS :     boolean := false;  -- if true, allow an active row in each bank
  constant  DATA_WIDTH           :     natural := 16;     -- host & SDRAM data width
  constant  NROWS                :     natural := 4096;   -- number of rows in SDRAM array
  constant  NCOLS                :     natural := 512;    -- number of columns in SDRAM array
  constant  HADDR_WIDTH          :     natural := 24;     -- host-side address width
  constant  SADDR_WIDTH          :     natural := 12;     -- SDRAM-side address width

  signal   rst_i        : std_logic;     -- internal reset signal
  signal   clk_i        : std_logic;     -- internal master clock signal
  signal   clk_b        : std_logic;     -- buffered master clock signal
  signal   lock         : std_logic;     -- SDRAM clock DLL lock indicator

  -- signals that go through the SDRAM host-side interface
  signal opBegun        : std_logic;        -- SDRAM operation started indicator
  signal earlyBegun     : std_logic;        -- SDRAM operation started indicator
  signal ramDone        : std_logic;        -- SDRAM operation complete indicator
  signal rdDone         : std_logic;        -- SDRAM read operation complete indicator
  signal wrDone         : std_logic;        -- SDRAM write operation complete indicator
  signal hAddr          : std_logic_vector(HADDR_WIDTH-1 downto 0);  -- host address bus
  signal hDIn           : std_logic_vector(DATA_WIDTH-1 downto 0);  -- host-side data to SDRAM
  signal hDOut          : std_logic_vector(DATA_WIDTH-1 downto 0);  -- host-side data from SDRAM
  signal hRd            : std_logic;        -- host-side read control signal
  signal hWr            : std_logic;        -- host-side write control signal
  signal rdPending      : std_logic;        -- read operation pending in SDRAM pipeline
  signal SDRAM_ba       : std_logic_vector(1 downto 0);
  
  type ram_rd_type is (rd_state0, rd_state1, rd_state2, rd_state3);
  type ram_wr_type is (wr_state0, wr_state1, wr_state2, wr_state3, wr_state4);
  signal ram_rd_state   : ram_rd_type;
  signal ram_wr_state   : ram_wr_type;

-----------------------------------------------------------------
--
-- XuLA System clock generator
--
-----------------------------------------------------------------

component XuLA_clk
  generic(
        FPGA_CLK_FREQ          : integer := 12000000;     -- 12MHZ
	     CPU_CLK_FREQ           : integer := CPU_CLK_FREQ; -- 24MHz
	     VDU_CLK_FREQ           : integer := VGA_CLK_FREQ; -- 24MHz
		  RAM_CLK_FREQ           : integer := SYS_CLK_FREQ  -- 48MHz
  );
  port(
    fpga_clk     : in  std_logic;      -- 12MHz FPGA Clock
    cpu_clk      : out std_logic;      -- 24MHz CPU clock
    vdu_clk      : out std_logic;      -- 24MHz VDU clock
	 ram_clk      : out std_logic       -- 48MHz RAM clock
    );
end component;

-----------------------------------------------------------------
--
-- CPU09 CPU core
--
-----------------------------------------------------------------

component cpu09
  port (    
	 clk:	     in	std_logic;
    rst:      in	std_logic;
    rw:	     out	std_logic;		-- Asynchronous memory interface
    vma:	     out	std_logic;
    addr:     out	std_logic_vector(15 downto 0);
    data_in:  in	std_logic_vector(7 downto 0);
	 data_out: out std_logic_vector(7 downto 0);
	 halt:     in  std_logic;
	 hold:     in  std_logic;
	 irq:      in  std_logic;
	 nmi:      in  std_logic;
	 firq:     in  std_logic
  );
end component;


----------------------------------------
--
-- 4K Block RAM Monitor ROM
--
----------------------------------------
component mon_rom
    Port (
       clk      : in  std_logic;
		 rst      : in  std_logic;
		 cs       : in  std_logic;
		 rw       : in  std_logic;
       addr     : in  std_logic_vector (11 downto 0);
       data_in  : in  std_logic_vector (7 downto 0);
       data_out : out std_logic_vector (7 downto 0)
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
     clk       : in    std_logic;
     rst       : in    std_logic;
     cs        : in    std_logic;
     rw        : in    std_logic;
     addr      : in    std_logic;
     data_in   : in    std_logic_vector(7 downto 0);
     data_out  : out   std_logic_vector(7 downto 0);
     irq       : out   std_logic;
     kbd_clk   : inout std_logic;
     kbd_data  : inout std_logic
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
	     VGA_VER_SYNC           : integer := 1;  -- LINES
	     VGA_VER_FRONT_PORCH    : integer := 36  -- LINES
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

----------------------------------------
--
-- SDRAM Interface
--
----------------------------------------

component XSASDRAMCntl
  generic(
    FREQ                 :     natural := FREQ;        -- operating frequency in KHz
    CLK_DIV              :     real    := CLK_DIV;     -- divisor for FREQ (can only be 1.0, 1.5, 2.0, 2.5, 3.0, 4.0, 5.0, 8.0 or 16.0)
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
    sData                : inout std_logic_vector(DATA_WIDTH-1 downto 0)  -- SDRAM in/out databus
    );
end component;

----------------------------------------
--
-- Parallel I/O port
--
----------------------------------------

component xula_ioport
  port (	
    clk       : in    std_logic;
    rst       : in    std_logic;
    cs        : in    std_logic;
    rw        : in    std_logic;
    addr      : in    std_logic_vector(1 downto 0);
    data_in   : in    std_logic_vector(7 downto 0);
    data_out  : out   std_logic_vector(7 downto 0);
    porta_io  : inout std_logic_vector(7 downto 0);
    portb_io  : inout std_logic_vector(4 downto 0);
    portc_in  : in    std_logic_vector(7 downto 5);
    irq       : out   std_logic
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

my_clk : XuLA_clk port map(
    fpga_clk     => fpga_clk,     -- 12MHz FPGA Clock
    cpu_clk      => cpu_clk,      -- 24MHz CPU clock
    vdu_clk      => vga_clk,      -- 24MHz VDU VGA clock
	 ram_clk      => sys_clk       -- 48MHz System / RAM clock
    );

my_cpu : cpu09  port map (    
	 clk	     => cpu_clk,
    rst       => cpu_rst,
    rw	     => cpu_rw,
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

my_rom : mon_rom port map (
    clk      => cpu_clk,
    rst      => cpu_rst,
    cs       => rom_cs,
    rw       => cpu_rw,
    addr     => cpu_addr(11 downto 0),
    data_in  => cpu_data_out,
    data_out => rom_data_out
    );

my_acia  : acia6850 port map (
	 clk	     => cpu_clk,
	 rst       => cpu_rst,
    cs        => acia_cs,
	 rw        => cpu_rw,
    irq       => acia_irq,
    addr      => cpu_addr(0),
	 data_in   => cpu_data_out,
	 data_out  => acia_data_out,
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
    SYS_CLK_FREQ  => SYS_CLK_FREQ,
	 ACIA_CLK_FREQ => ACIA_CLK_FREQ
  ) 
  port map(
    clk        => sys_clk,
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
      rst          => cpu_rst,
      cs           => key_cs,
      rw           => cpu_rw,
      addr         => cpu_addr(0),
      data_in      => cpu_data_out(7 downto 0),
      data_out     => key_data_out(7 downto 0),
      irq          => key_irq,
      kbd_clk      => keyb_clk,
      kbd_data     => keyb_dat
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
	   VGA_VER_SYNC           => 1,  -- LINES
	   VGA_VER_FRONT_PORCH    => 36  -- LINES
  )
  port map(

		-- Control Registers
		vdu_clk       => cpu_clk,
      vdu_rst       => cpu_rst,
		vdu_cs        => vdu_cs,
		vdu_rw        => cpu_rw,
		vdu_addr      => cpu_addr(2 downto 0),
		vdu_data_in   => cpu_data_out,
		vdu_data_out  => vdu_data_out,

      -- vga port connections
      vga_clk       => vga_clk,					 -- 25 MHz VDU pixel clock
      vga_red_o     => vga_red,
      vga_green_o   => vga_green,
      vga_blue_o    => vga_blue,
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
-- I/O Port
--
----------------------------------------

my_ioport : xula_ioport
  port map (	
    clk       => cpu_clk,
    rst       => cpu_rst,
    cs        => iop_cs,
    rw        => cpu_rw,
    addr      => cpu_addr(1 downto 0),
    data_in   => cpu_data_out,
    data_out  => iop_data_out,
    porta_io  => pa,
    portb_io  => pb(4 downto 0),
    portc_in  => pc(7 downto 5),
    irq       => iop_irq
  );

----------------------------------------
--
-- Dynamic Address Translation
--
----------------------------------------

my_dat : dat_ram port map (
    clk       => cpu_clk,
	 rst       => cpu_rst,
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
      FREQ                 => FREQ,
      CLK_DIV              => CLK_DIV,
      PIPE_EN              => PIPE_EN,
      MAX_NOP              => MAX_NOP,
      DATA_WIDTH           => DATA_WIDTH,
      MULTIPLE_ACTIVE_ROWS => MULTIPLE_ACTIVE_ROWS,
      NROWS                => NROWS,
      NCOLS                => NCOLS,
      HADDR_WIDTH          => HADDR_WIDTH,
      SADDR_WIDTH          => SADDR_WIDTH
      )
    port map(
	   -- Host Side
      clk                  => SYS_CLK,  -- master clock from external clock source (unbuffered)
      bufclk               => clk_b,    -- buffered master clock output
      clk1x                => clk_i,    -- synchronized master clock (accounts for delays to external SDRAM)
      clk2x                => open,     -- synchronized doubled master clock
      lock                 => lock,     -- DLL lock indicator
      rst                  => rst_i,    -- reset
      rd                   => hRd,      -- host-side SDRAM read control from memory tester
      wr                   => hWr,      -- host-side SDRAM write control from memory tester
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
      sclk                 => SDRAM_clk,      -- synchronized clock to external SDRAM
      cke                  => open,           -- SDRAM clock enable
      cs_n                 => open,           -- SDRAM chip-select
      ras_n                => SDRAM_ras_n,    -- SDRAM RAS
      cas_n                => SDRAM_cas_n,    -- SDRAM CAS
      we_n                 => SDRAM_we_n,     -- SDRAM write-enable
      ba                   => SDRAM_ba,       -- SDRAM bank address
      sAddr                => SDRAM_A,        -- SDRAM address
      sData                => SDRAM_D         -- SDRAM databus
      );
	 
----------------------------------------------------------------------
--
-- Process to decode memory map
--
----------------------------------------------------------------------

mem_decode: process( cpu_clk,
                     cpu_addr, cpu_rw, cpu_vma,
							dat_addr,
					      rom_data_out,
						   acia_data_out,
							key_data_out,
							vdu_data_out,
							timer_data_out,
							iop_data_out,
							ram_data_out
							)
begin
      cpu_data_in <= (others=>'0');
      dat_cs      <= '0';
      rom_cs      <= '0';
	   acia_cs     <= '0';
	   key_cs      <= '0';
	   vdu_cs      <= '0';
	   timer_cs    <= '0';
		iop_cs      <= '0';
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
             cpu_data_in <= key_data_out;
			    key_cs <= cpu_vma;

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
             cpu_data_in <= (others=> '0');

           --
			  -- Reserved SWTPc MP-ID PIA Timer/Printer Port $E080 - $E08F
			  --

           --
			  -- Reserved SWTPc MP-ID PTM 6840 Timer Port $E090 - $E09F
			  --

           --
           -- IO Port $E0B0 - $E0BF
			  --
			  when "1011" => -- $E0B0
             cpu_data_in <= iop_data_out;
             iop_cs    <= cpu_vma;

			  --
			  -- Remaining 6 slots reserved for non SWTPc Peripherals
			  --
			  when others => -- $E0A0 to $E0FF
             cpu_data_in <= (others=> '0');
		     end case;
         --
			--	$E100 to $EFFF reserved for future use
			--
        	when others =>
           cpu_data_in <= (others=> '0');
         end case;
		--
		-- Everything else is RAM
		--
		else
 		   cpu_data_in <= ram_data_out;
		   ram_cs      <= cpu_vma;
	  end if;
end process;


--
-- Interrupts and other bus control signals
--
interrupts : process( rst_n, rst_i, nmi_n,
							 ram_cs, ram_hold,
                      acia_irq, 
							 key_irq, 
							 timer_irq,
							 iop_irq
							 )
begin
 	 cpu_rst <= (not rst_n) or rst_i; -- CPU reset is active high
    cpu_irq   <= acia_irq or key_irq;
	 cpu_nmi   <= not( nmi_n );
	 cpu_firq  <= timer_irq or iop_irq;
	 cpu_halt  <= '0';
	 cpu_hold  <= ram_hold;
end process;

--
-- Push buttons
--
my_switch_assignments : process( SW2_N, SW3_N )
begin
--  rst_n    <= SW2_N;
--  nmi_n    <= SW3_N;
  rst_n <= '1';
  nmi_n <= '1';
end process;

  ------------------------------------------------------------------------
  -- internal reset flag is set active by config. bitstream
  -- and then gets reset after clocks start.
  ------------------------------------------------------------------------
  process(clk_b)
  begin
    if rising_edge(clk_b) then
      if lock = '0' then
        rst_i <= '1';                   -- keep in reset until DLLs start up and lock
      else
        rst_i <= '0';                    -- release reset once DLLs lock
      end if;
    end if;
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
-- SDRAM assignments
--
my_sdram_assignments : process( cpu_clk, clk_i, cpu_rst, 
                                opBegun, rdDone, wrDone,
										  ram_rd_state, ram_wr_state,
                                cpu_addr, dat_addr,
                                cpu_data_out, hDout,
										  ram_cs, cpu_rw, ram_hold, SDRAM_ba )
begin
  if( clk_i'event and clk_i='0' ) then
    if( cpu_rst = '1' ) then
      hWr    <= '0';
	   hRd    <= '0';
	   wrDone <= '0';
	   ram_wr_state <= wr_state0;
	   ram_rd_state <= rd_state0;
    else
      --
	   -- read state machine
      --
      case ram_rd_state is

      when rd_state0 =>
	     if (ram_hold = '1') and (cpu_rw = '1') then
		    hRd          <= '1';
		    ram_rd_state <= rd_state1;
        end if;

      when rd_state1 =>
	     if opBegun = '1' then
		    ram_rd_state <= rd_state2;
        end if;

      when rd_state2 =>
	     if rdDone = '1' then
		    hRd <= '0';
		    ram_rd_state <= rd_state3;
        end if;

      when rd_state3 =>
	     if rdDone = '0' then
		    ram_rd_state <= rd_state0;
        end if;

	   when others =>
		  hRd          <= '0';
		  ram_rd_state <= rd_state0;
	   end case;  	  

	   --
	   -- Write state machine
	   --
      case ram_wr_state is

      when wr_state0 =>
	     if (ram_hold = '1') and (cpu_rw = '0') then
		    hWr          <= '1';
          wrDone       <= '0';
		    ram_wr_state <= wr_state1;
        end if;

      when wr_state1 =>
	     if opBegun = '1' then
		    hWr          <= '0';
          wrDone       <= '0';
		    ram_wr_state <= wr_state2;
        end if;

      when wr_state2 =>
		  hWr          <= '0';
        wrDone       <= '0';
		  ram_wr_state <= wr_state3;

      when wr_state3 =>
		  hWr          <= '0';
        wrDone       <= '1';
		  ram_wr_state <= wr_state4;

      when wr_state4 =>
		  hWr          <= '0';
        wrDone       <= '0';
		  ram_wr_state <= wr_state0;

	   when others =>
		  hWr          <= '0';
        wrDone       <= '0';
		  ram_wr_state <= wr_state0;

      end case;  	  
    end if;
  end if;
  --
  -- Strobe host RD and WR signals high on RAM select
  -- Return low when cycle has started
  --
  if( cpu_clk'event and cpu_clk='1' ) then
    if( cpu_rst = '1' ) then
	   ram_hold     <= '0';
    else
      --
      -- Hold is intitiated when the RAM is selected
      -- and released when access cycle is complete
      -- 
	   if (ram_hold = '0') and (ram_cs = '1') then
		  ram_hold <= '1';
      elsif (ram_hold = '1') and ((rdDone = '1') or (wrDone = '1')) then
        ram_hold <= '0';
      end if;
    end if;
  end if;

  hAddr(23 downto 20) <= (others=>'0');
  hAddr(19 downto 12) <= dat_addr;
  hAddr(11 downto 0)  <= cpu_addr(11 downto 0);
  hDin( 7 downto 0)   <= cpu_data_out;
  hDin(15 downto 8)   <= (others=>'0');
  ram_data_out        <= hDout(7 downto 0);
  SDRAM_BS            <= SDRAM_ba(0);
end process;

end rtl; --===================== End of architecture =======================--

