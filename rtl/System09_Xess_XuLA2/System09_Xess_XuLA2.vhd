--=============================================================================--
--                                                                             --
--  System09 - Synthesizable System On a Chip - VHDL FPGA core top level file. --
--                                                                             --
--=============================================================================--
--
--
-- File name      : System09_Xess_XuLA2.vhd
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
-- Version 5.0 - 2013-03-18 - John Kent
-- adapted for XESS XuLA2
--===========================================================================--
library ieee;
   use ieee.std_logic_1164.all;
   use IEEE.STD_LOGIC_ARITH.ALL;
   use IEEE.STD_LOGIC_UNSIGNED.ALL;
   use ieee.numeric_std.all;
library work;
	use work.CommonPckg.all;
library unisim;
   use unisim.vcomponents.all;

entity system09 is
  port(
    fpgaclk_i     : in    std_Logic;  -- 12MHz Clock input
    chanclk_io    : inout std_logic;
    chan_io       : inout std_logic_vector(31 downto 0);

    usdflashCs_bo : out   std_logic; --  LOC = T8;  --- SD Card Flash chip select
	 flashCs_bo    : out   std_logic; --  LOC = T3;  -- spi Flash chip select
    sclk_o        : out   std_logic; --  LOC = R11; -- spi clock 
    mosi_o        : out   std_logic; --  LOC = T10; -- master out slave in
    miso_i        : in    std_logic; --  LOC = P10; -- master in slave out

    -- SDRAM side
    SDclkfb_i  : in  std_logic;            -- feedback SDRAM clock after PCB delays
    SDclk_o    : out std_logic;            -- clock to SDRAM
    SDCKE_o    : out std_logic;            -- clock-enable to SDRAM
    SDCe_bo    : out std_logic;            -- chip-select to SDRAM
    SDRAS_bo   : out std_logic;            -- SDRAM row address strobe
    SDCAS_bo   : out std_logic;            -- SDRAM column address strobe
    SDWE_bo    : out std_logic;            -- SDRAM write enable
    SDBS_o     : out std_logic_vector(1 downto 0);  -- SDRAM bank address
    SDAddr_o   : out std_logic_vector(12 downto 0);  -- SDRAM row/column address
    SDData_io  : inout  std_logic_vector(15 downto 0);  -- data from SDRAM
    SDDQMH_o   : out std_logic;            -- enable upper-byte of SDRAM databus if true
    SDDQML_o   : out std_logic             -- enable lower-byte of SDRAM databus if true
	 );
end system09;

-------------------------------------------------------------------------------
-- Architecture for System09
-------------------------------------------------------------------------------
architecture rtl of system09 is

  -----------------------------------------------------------------------------
  -- constants
  -----------------------------------------------------------------------------

  constant FPGA_CLK_FREQ : integer := 12_000_000; -- FPGA Input Clock Frequency
  constant CPU_CLK_FREQ  : integer := 24_000_000;  -- CPU Clock Frequency
  constant VGA_CLK_FREQ  : integer := 24_000_000;  -- VGA Pixel Clock Frequency
  constant SYS_CLK_FREQ  : integer := 48_000_000;  -- System Clock Frequency
  constant RAM_CLK_FREQ  : integer := 96_000_000;  -- SDRAM System Clock Frequency

  -- SDRAM
  constant MEM_CLK_FREQ         : real    := 96.0;   -- operating frequency of Memory in MHz
  constant SYS_CLK_DIV          : real    := 1.0;    -- divisor for FREQ (can only be 1.0, 1.5, 2.0, 2.5, 3.0, 4.0, 5.0, 8.0 or 16.0)
  constant CLK_DIV              : real    := 2.0;
  constant PIPE_EN              : boolean := false;  -- if true, enable pipelined read operations
  constant MAX_NOP              : natural := 10000;  -- number of NOPs before entering self-refresh
  constant MULTIPLE_ACTIVE_ROWS : boolean := false;  -- if true, allow an active row in each bank
  constant DATA_WIDTH           : natural := 16;     -- host & SDRAM data width
  constant NROWS                : natural := 4096;   -- number of rows in SDRAM array
  constant NCOLS                : natural := 512;    -- number of columns in SDRAM array
  constant HADDR_WIDTH          : natural := 23;     -- host-side address width
  constant SADDR_WIDTH          : natural := 13;     -- SDRAM-side address width
  
  constant CPU_CLK_DIV          : natural := (SYS_CLK_FREQ/CPU_CLK_FREQ);
  constant VGA_CLK_DIV          : natural := ((SYS_CLK_FREQ*1000)/VGA_CLK_FREQ);
  constant BAUD_RATE            : integer := 57600;	  -- Baud Rate
  constant ACIA_CLK_FREQ        : integer := BAUD_RATE * 16;

  -----------------------------------------------------------------------------
  -- Signals
  -----------------------------------------------------------------------------
  signal ram_clk        : std_logic;  -- 96MHz RAM clock
  signal ram_clk_n      : std_logic;  -- Inverted 96MHz RAM clock
  signal sys_clk        : std_logic;  -- 48MHz system clock
  signal vga_clk        : std_logic;
  signal rst_i          : std_logic;  -- internal reset signal
  signal lock_o         : std_logic;  -- SDRAM clock DLL lock indicator
  signal rst_n          : std_logic;  -- Master Reset input (active low)
  signal nmi_n          : std_logic;  -- Non Maskable Interrupt input (active low)
  signal count          : std_logic_vector(15 downto 0) := "1111111111111111"; -- power on reset delay

  -- CPU Interface signals
  signal cpu_rst      : Std_Logic;
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

  -- keyboard port
  signal keyboard_data_out : std_logic_vector(7 downto 0);
  signal keyboard_cs       : std_logic;
  signal keyboard_irq      : std_logic;

  -- Video Display Unit
  signal vdu_cs         : std_logic;
  signal vdu_data_out   : std_logic_vector(7 downto 0);
  signal vga_red_o      : std_logic;
  signal vga_green_o    : std_logic;
  signal vga_blue_o     : std_logic;
  signal vga_hsync_n    : std_logic;
  signal vga_vsync_n    : std_logic;

  -- timer
  signal timer_data_out : std_logic_vector(7 downto 0);
  signal timer_cs       : std_logic;
  signal timer_irq      : std_logic;

  -- trap
  signal trap_cs        : std_logic;
  signal trap_data_out  : std_logic_vector(7 downto 0);
  signal trap_irq       : std_logic;

  -- spi master  
  signal spi_data_out   : std_logic_vector(7 downto 0);
  signal spi_cs         : std_logic;
  signal spi_irq        : std_logic;
  signal spi_cs_n       :std_logic_vector(7 downto 0); -- This is the chip select output

  -- Flex Memory & Monitor Stack
  signal flex_cs        : Std_logic;
  signal flex_data_out  : Std_Logic_Vector(7 downto 0);

  -- RAM
  type ram_type is (ram_state_0, ram_state_rd1, ram_state_rd2, ram_state_wr1, ram_state_3 );
  signal ram_cs         : std_logic; -- memory chip select
  signal ram_data_out   : std_logic_vector(7 downto 0);
  signal ram_rd_req     : std_logic; -- ram read request	(asynch set on ram read, cleared falling CPU clock edge)
  signal ram_wr_req     : std_logic; -- ram write request (set on rising CPU clock edge, asynch clear on acknowledge) 
  signal ram_hold       : std_logic; -- hold off slow accesses
  signal ram_release    : std_logic; -- Release ram hold
  signal ram_state      : ram_type;

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

-----------------------------------------------------------------
--
-- Clock buffer
--
-----------------------------------------------------------------

component BUFG 
   Port (
     i: in std_logic;
	  o: out std_logic
  );
end component;

-----------------------------------------------------------------
--
-- XuLA System clock generator
--
-----------------------------------------------------------------

component XuLA_clk
  generic(
        FPGA_CLK_FREQ          : integer := FPGA_CLK_FREQ; -- 12MHZ
	     CPU_CLK_FREQ           : integer := CPU_CLK_FREQ;  -- 24MHz
	     VDU_CLK_FREQ           : integer := VGA_CLK_FREQ;  -- 24MHz
	     SYS_CLK_FREQ           : integer := SYS_CLK_FREQ;  -- 48MHz
		  RAM_CLK_FREQ           : integer := RAM_CLK_FREQ   -- 96MHz
  );
  port(
    fpga_clk     : in  std_logic;      -- 12MHz FPGA Clock
    cpu_clk      : out std_logic;      -- 24MHz CPU clock
    vdu_clk      : out std_logic;      -- 24MHz VDU clock
	 sys_clk      : out std_logic;      -- 48MHz SYS clock
	 ram_clk      : out std_logic;      -- 96MHz RAM clock
	 ram_clk_n    : out std_logic;      -- 96MHz RAM clock
	 lock         : out std_logic
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
--        VDU_CLK_FREQ           : integer := CPU_CLK_FREQ; -- HZ
        VGA_CLK_FREQ           : integer := VGA_CLK_FREQ; -- HZ
	     VGA_HOR_CHARS          : integer := 80; -- CHARACTERS
	     VGA_VER_CHARS          : integer := 25; -- CHARACTERS
	     VGA_HOR_CHAR_PIXELS    : integer := 8;  -- PIXELS
	     VGA_VER_CHAR_LINES       : integer := 16; -- LINES
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
-- SPI Master
--
----------------------------------------

component spi_master
  port (
    --
    -- CPU Interface Signals
    --
    clk                : in  std_logic;
    reset              : in  std_logic;
    cs                 : in  std_logic;
    rw                 : in  std_logic;
    addr               : in  std_logic_vector(1 downto 0);
    data_in            : in  std_logic_vector(7 downto 0);
    data_out           : out std_logic_vector(7 downto 0);
    irq                : out std_logic;
    --
    -- SPI Interface Signals
    --
    spi_miso           : in  std_logic;
    spi_mosi           : out std_logic;
    spi_clk            : out std_logic;
    spi_cs_n           : out std_logic_vector(7 downto 0)
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
    data_out : out std_logic_vector (7 downto 0);
    data_in  : in  std_logic_vector (7 downto 0)
    );
end component;

----------------------------------------
--
-- SDRAM
-- $0000 - $BFFF
--
----------------------------------------  component SdramCntl is

component SdramCntl
    generic(
      FREQ_G : real := 96.0; -- Operating frequency in MHz.
      IN_PHASE_G : boolean := true; -- SDRAM and controller work on same or opposite clock edge.
      PIPE_EN_G : boolean := false; -- If true, enable pipelined read operations.
      MAX_NOP_G : natural := 10000; -- Number of NOPs before entering self-refresh.
      ENABLE_REFRESH_G : boolean := true; -- If true, row refreshes are automatically inserted.
      MULTIPLE_ACTIVE_ROWS_G : boolean := false; -- If true, allow an active row in each bank.
      DATA_WIDTH_G : natural := 16; -- Host & SDRAM data width.
      -- Parameters for Winbond W9812G6JH-75 (all times are in nanoseconds).
      NROWS_G : natural := 4096; -- Number of rows in SDRAM array.
      NCOLS_G : natural := 512; -- Number of columns in SDRAM array.
      HADDR_WIDTH_G : natural := 23; -- Host-side address width.
      SADDR_WIDTH_G : natural := 13; -- SDRAM-side address width.
      T_INIT_G : real := 200_000.0; -- min initialization interval (ns).
      T_RAS_G : real := 45.0; -- min interval between active to precharge commands (ns).
      T_RCD_G : real := 20.0; -- min interval between active and R/W commands (ns).
      T_REF_G : real := 64_000_000.0; -- maximum refresh interval (ns).
      T_RFC_G : real := 65.0; -- duration of refresh operation (ns).
      T_RP_G : real := 20.0; -- min precharge command duration (ns).
      T_XSR_G : real := 75.0 -- exit self-refresh time (ns).
      );
    port(
      -- Host side.
      clk_i          : in  std_logic; -- Master clock.
      lock_i         : in  std_logic := YES; -- True if clock is stable.
      rst_i          : in  std_logic := NO; -- Reset.
      rd_i           : in  std_logic := NO; -- Initiate read operation.
      wr_i           : in  std_logic := NO; -- Initiate write operation.
      uds_i          : in  std_logic; -- host-side SDRAM upper data strobe
      lds_i          : in  std_logic; -- host-side SDRAM lower data strobe
      earlyOpBegun_o : out std_logic; -- Read/write/self-refresh op has begun (async).
      opBegun_o      : out std_logic; -- Read/write/self-refresh op has begun (clocked).
      rdPending_o    : out std_logic; -- True if read operation(s) are still in the pipeline.
      done_o         : out std_logic; -- Read or write operation is done_o.
      rdDone_o       : out std_logic; -- Read operation is done_o and data is available.
      addr_i         : in  std_logic_vector(HADDR_WIDTH_G-1 downto 0) := (others => ZERO); -- Address from host to SDRAM.
      data_i         : in  std_logic_vector(DATA_WIDTH_G-1 downto 0) := (others => ZERO); -- Data from host to SDRAM.
      data_o         : out std_logic_vector(DATA_WIDTH_G-1 downto 0); -- Data from SDRAM to host.
      status_o       : out std_logic_vector(3 downto 0); -- Diagnostic status of the FSM .

      -- SDRAM side.
      sdCke_o        : out std_logic; -- Clock-enable to SDRAM.
      sdCe_bo        : out std_logic; -- Chip-select to SDRAM.
      sdRas_bo       : out std_logic; -- SDRAM row address strobe.
      sdCas_bo       : out std_logic; -- SDRAM column address strobe.
      sdWe_bo        : out std_logic; -- SDRAM write enable.
      sdBs_o         : out std_logic_vector(1 downto 0); -- SDRAM bank address.
      sdAddr_o       : out std_logic_vector(SADDR_WIDTH_G-1 downto 0); -- SDRAM row/column address.
      sdData_io      : inout std_logic_vector(DATA_WIDTH_G-1 downto 0); -- Data to/from SDRAM.
      sdDqmh_o       : out std_logic; -- Enable upper-byte of SDRAM databus if true.
      sdDqml_o       : out std_logic -- Enable lower-byte of SDRAM databus if true.
      );
end component;

--component XSASDRAMCntl
--  generic(
--    FREQ                 :     natural := MEM_CLK_FREQ; -- operating frequency in KHz
--    CLK_DIV              :     real    := CLK_DIV;     -- divisor for FREQ (can only be 1.0, 1.5, 2.0, 2.5, 3.0, 4.0, 5.0, 8.0 or 16.0)
--    PIPE_EN              :     boolean := PIPE_EN;     -- if true, enable pipelined read operations
--    MAX_NOP              :     natural := MAX_NOP;     -- number of NOPs before entering self-refresh
--    MULTIPLE_ACTIVE_ROWS :     boolean := MULTIPLE_ACTIVE_ROWS;  -- if true, allow an active row in each bank
--    DATA_WIDTH           :     natural := DATA_WIDTH;  -- host & SDRAM data width
--    NROWS                :     natural := NROWS;       -- number of rows in SDRAM array
--    NCOLS                :     natural := NCOLS;       -- number of columns in SDRAM array
--    HADDR_WIDTH          :     natural := HADDR_WIDTH; -- host-side address width
--    SADDR_WIDTH          :     natural := SADDR_WIDTH  -- SDRAM-side address width
--    );
--  port(
--    -- host side
--    clk_i                 : in  std_logic;  -- master clock
--    bufclk_o              : out std_logic;  -- buffered master clock
--    clk1x_o              : out std_logic;  -- host clock sync'ed to master clock (and divided if CLK_DIV>1)
--    clk2x_o              : out std_logic;  -- double-speed host clock
--    lock_o               : out std_logic;  -- true when host clock is locked to master clock
--    rst_i                : in  std_logic;  -- reset
--    rd_i                 : in  std_logic;  -- initiate read operation
--    wr_i                 : in  std_logic;  -- initiate write operation
--	 lds_i                : in  std_logic;  -- lower data strobe
--    uds_i                : in  std_logic;  -- upper data strobe
--    earlyOpBegun_o        : out std_logic;  -- read/write/self-refresh op begun     (async)
--    opBegun_o            : out std_logic;  -- read/write/self-refresh op begun (clocked)
--    rdPending_o          : out std_logic;  -- read operation(s) are still in the pipeline
--    done_o               : out std_logic;  -- read or write operation is done
--    rdDone_o             : out std_logic;  -- read done and data is available
--    Addr_i               : in  std_logic_vector(HADDR_WIDTH-1 downto 0);  -- address from host
--    Data_i               : in  std_logic_vector(DATA_WIDTH-1 downto 0);  -- data from host
--    Data_o               : out std_logic_vector(DATA_WIDTH-1 downto 0);  -- data to host
--    status_o              : out std_logic_vector(3 downto 0);  -- diagnostic status of the FSM         

    -- SDRAM side
--    sdClkfb_i            : in    std_logic;           -- clock from SDRAM after PCB delays
--    sdClk_o              : out   std_logic;           -- SDRAM clock sync'ed to master clock
--    sdCke_o              : out   std_logic;      -- Clock-enable to SDRAM.
--    sdCe_bo              : out   std_logic;      -- Chip-select to SDRAM.
--    sdRas_bo             : out   std_logic;      -- SDRAM row address strobe.
--    sdCas_bo             : out   std_logic;      -- SDRAM column address strobe.
--    sdWe_bo              : out   std_logic;      -- SDRAM write enable.
--    sdBs_o               : out   std_logic_vector(1 downto 0);  -- SDRAM bank address.
--    sdAddr_o             : out   std_logic_vector(SADDR_WIDTH-1 downto 0);  -- SDRAM row/column address.
--    sdData_io            : inout std_logic_vector(DATA_WIDTH-1 downto 0);  -- Data to/from SDRAM.
--    sdDqmh_o             : out   std_logic;  -- Enable upper-byte of SDRAM databus if true.
--    sdDqml_o             : out   std_logic  -- Enable lower-byte of SDRAM databus if true.
--    );
--end component;


begin
  -----------------------------------------------------------------------------
  -- Instantiation of internal components
  -----------------------------------------------------------------------------
--
-- Clock generator
--
my_clk : XuLA_clk port map(
    fpga_clk     => fpgaclk_i,     -- 12MHz FPGA Clock
    cpu_clk      => cpu_clk,      -- 24MHz CPU clock
    vdu_clk      => vga_clk,      -- 24MHz VDU VGA clock
	 sys_clk      => sys_clk,      -- 48MHz System / RAM clock
	 ram_clk      => ram_clk,      -- 96MHz System / RAM clock
	 ram_clk_n    => ram_clk_n,    -- 96MHz System / RAM clock
	 lock         => lock_o
    );

ODDR2_inst : ODDR2
   generic map(
      DDR_ALIGNMENT => "NONE",
      INIT => '0',
      SRTYPE => "SYNC")
   port map (
      Q =>  SDclk_o,   -- 1-bit output data
      C0 => ram_clk,   -- 1-bit clock input
      C1 => ram_clk_n, -- 1-bit clock input
      CE => '1',       -- 1-bit clock enable input
      D0 => '1',
      D1 => '0',
      R  => '0',       -- 1-bit reset input
      S  => '0'        -- 1-bit set input
   );


--
-- CPU
--
my_cpu : cpu09  port map (    
	 clk	     => cpu_clk,
    rst       => cpu_rst,
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

--
-- Dynamic address translation
--
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

--
-- Monitor ROM
--
my_rom : mon_rom port map (
       clk   => cpu_clk,
		 rst   => cpu_rst,
		 cs    => rom_cs,
		 rw    => '1',
       addr  => cpu_addr(11 downto 0),
       data_in => cpu_data_out,
       data_out => rom_data_out
    );

my_flex : flex_ram port map (
    clk       => cpu_clk,
    rst       => cpu_rst,
	 cs        => flex_cs,
	 rw        => cpu_rw,
    addr      => cpu_addr(12 downto 0),
    data_out     => flex_data_out,
    data_in     => cpu_data_out
    );

my_acia  : acia6850 port map (
	 clk	     => cpu_clk,
	 rst       => cpu_rst,
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
	cs           => keyboard_cs,
	rw           => cpu_rw,
	addr         => cpu_addr(0),
	data_in      => cpu_data_out(7 downto 0),
	data_out     => keyboard_data_out(7 downto 0),
	irq          => keyboard_irq,
	kbd_clk      => chanclk_io,
	kbd_data     => chan_io(15)
	);

----------------------------------------
--
-- Video Display Unit instantiation
--
----------------------------------------
my_vdu : vdu8 
  generic map(
--      VDU_CLK_FREQ           => CPU_CLK_FREQ, -- HZ
      VGA_CLK_FREQ           => VGA_CLK_FREQ, -- HZ
	   VGA_HOR_CHARS          => 80, -- CHARACTERS
	   VGA_VER_CHARS          => 25, -- CHARACTERS
	   VGA_HOR_CHAR_PIXELS    => 8,  -- PIXELS
	   VGA_VER_CHAR_LINES     => 16, -- LINES
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


my_spi : spi_master
  port map (
    --
    -- CPU Interface Signals
    --
    clk                => cpu_clk,
    reset              => cpu_rst,
    cs                 => spi_cs,
    rw                 => cpu_rw,
    addr               => cpu_addr(1 downto 0),
    data_in            => cpu_data_out,
    data_out           => spi_data_out,
    irq                => spi_irq,
    --
    -- SPI Interface Signals
    --
    spi_miso           => miso_i,
    spi_mosi           => mosi_o,
    spi_clk            => sclk_o,
    spi_cs_n           => spi_cs_n
    );

  ------------------------------------------------------------------------
  -- Instantiate the SDRAM controller that connects to the memory tester
  -- module and interfaces to the external SDRAM chip.
  ------------------------------------------------------------------------

my_sdram : SDRAMCntl
  generic map (
--    FREQ                 => MEM_CLK_FREQ, -- operating frequency in KHz
--    CLK_DIV              => CLK_DIV,      -- divisor for FREQ (can only be 1.0, 1.5, 2.0, 2.5, 3.0, 4.0, 5.0, 8.0 or 16.0)
--    PIPE_EN              => PIPE_EN,      -- if true, enable pipelined read operations
--    MAX_NOP              => MAX_NOP,      -- number of NOPs before entering self-refresh
--    MULTIPLE_ACTIVE_ROWS => MULTIPLE_ACTIVE_ROWS,  -- if true, allow an active row in each bank
--    DATA_WIDTH           => DATA_WIDTH,   -- host & SDRAM data width
--    NROWS                => NROWS,        -- number of rows in SDRAM array
--    NCOLS                => NCOLS,        -- number of columns in SDRAM array
--    HADDR_WIDTH          => HADDR_WIDTH,  -- host-side address width
--    SADDR_WIDTH          => SADDR_WIDTH   -- SDRAM-side address width


    FREQ_G                 => MEM_CLK_FREQ, -- operating frequency in MHz
    IN_PHASE_G             => true,
--    CLK_DIV_G              => CLK_DIV,      -- divisor for FREQ (can only be 1.0, 1.5, 2.0, 2.5, 3.0, 4.0, 5.0, 8.0 or 16.0)
    PIPE_EN_G              => PIPE_EN,      -- if true, enable pipelined read operations
    MAX_NOP_G              => MAX_NOP,      -- number of NOPs before entering self-refresh
    ENABLE_REFRESH_G       => true,         -- If true, row refreshes are automatically inserted.
    MULTIPLE_ACTIVE_ROWS_G => MULTIPLE_ACTIVE_ROWS,  -- if true, allow an active row in each bank
    DATA_WIDTH_G           => DATA_WIDTH,   -- host & SDRAM data width
    NROWS_G                => NROWS,        -- number of rows in SDRAM array
    NCOLS_G                => NCOLS,        -- number of columns in SDRAM array
    HADDR_WIDTH_G          => HADDR_WIDTH,  -- host-side address width
    SADDR_WIDTH_G          => SADDR_WIDTH   -- SDRAM-side address width
    )
  port map (
      -- Host side.
      clk_i                => sdClkfb_i,    -- Master clock.
--      bufclk_o             => open,       -- buffered master clock
--      clk1x_o              => open,       -- host clock sync'ed to master clock (and divided if CLK_DIV>1)
--      clk2x_o              => open,       -- double-speed host clock
--      lock_o               => lock_o,     -- True if clock is stable.
      lock_i               => lock_o,     -- True if clock is stable.
      rst_i                => rst_i,      -- Reset.
      rd_i                 => hRd,        -- Initiate read operation.
      wr_i                 => hWr,        -- Initiate write operation.
      uds_i                => hUds,       -- host-side SDRAM upper data strobe
      lds_i                => hLds,       -- host-side SDRAM lower data strobe
      earlyOpBegun_o       => earlyBegun, -- Read/write/self-refresh op has begun (async).
      opBegun_o            => opBegun,    -- Read/write/self-refresh op has begun (clocked).
      rdPending_o          => rdPending,  -- True if read operation(s) are still in the pipeline.
      done_o               => ramDone,    -- Read or write operation is done_o.
      rdDone_o             => rdDone,     -- Read operation is done_o and data is available.
      addr_i               => hAddr,      -- Address from host to SDRAM.
      data_i               => hDIn,       -- Data from host to SDRAM.
      data_o               => hDout,      -- Data from SDRAM to host.
      status_o             => open,       -- Diagnostic status of the FSM .

      -- SDRAM side.
--      sdclkfb_i            => SDClkfb_i,  -- clock from SDRAM after PCB delays
--      sdclk_o              => SDclk_o,    -- SDRAM clock sync'ed to master clock
      sdCke_o              => SDcke_o,    -- Clock-enable to SDRAM.
      sdCe_bo              => SDce_bo,    -- Chip-select to SDRAM.
      sdRas_bo             => SDras_bo,   -- SDRAM row address strobe.
      sdCas_bo             => SDcas_bo,   -- SDRAM column address strobe.
      sdWe_bo              => SDwe_bo,    -- SDRAM write enable.
      sdBs_o               => SDbs_o,     -- SDRAM bank address.
      sdAddr_o             => SDAddr_o,   -- SDRAM row/column address.
      sdData_io            => SDData_io,  -- Data to/from SDRAM.
      sdDqmh_o             => SDdqmh_o,   -- Enable upper-byte of SDRAM databus if true.
      sdDqml_o             => SDdqml_o    -- Enable lower-byte of SDRAM databus if true.
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
							timer_data_out,
							trap_data_out,
							spi_data_out,
							ram_data_out
							)
begin
      cpu_data_in <= (others=>'0');
      dat_cs      <= '0';
      rom_cs      <= '0';
	   acia_cs     <= '0';
	   keyboard_cs <= '0';
	   vdu_cs      <= '0';
	   timer_cs    <= '0';
	   trap_cs     <= '0';
	   spi_cs      <= '0';
      flex_cs     <= '0';
	   ram_cs      <= '0';
		
      if cpu_addr( 15 downto 8 ) = "11111111" then
	      cpu_data_in <= rom_data_out;
         dat_cs      <= cpu_vma and not cpu_rw;              -- write DAT
         rom_cs      <= cpu_vma and     cpu_rw;              -- read  ROM
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
           -- SPI Master $E0A0 - $E0AF
			  --
			  when "1010" => -- $E0A0
             cpu_data_in <= spi_data_out;
			    spi_cs     <= cpu_vma;

			  --
			  -- Remaining 6 slots reserved for non SWTPc Peripherals
			  --
			  when others => -- $E0B0 to $E0FF
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
-- SDRAM Address and data bus assignments
--
my_sdram_addr_data : process( cpu_addr, dat_addr,
                                cpu_data_out, hDout )
begin
  hAddr(22 downto 19)  <= "0000";
  hAddr(18 downto 11)  <= dat_addr;
  hAddr(10 downto 0)   <= cpu_addr(11 downto 1);
  if cpu_addr(0) = '0' then
     hUds              <= '1';
     hLds              <= '0';
     hDin( 7 downto 0) <= (others=>'0');
     hDin(15 downto 8) <= cpu_data_out;
     ram_data_out      <= hDout(15 downto 8);
  else
     hUds              <= '0';
     hLds              <= '1';
     hDin( 7 downto 0) <= cpu_data_out;
     hDin(15 downto 8) <= (others=>'0');
     ram_data_out      <= hDout( 7 downto 0);
  end if;
end process;

--
-- SDRAM read write control
--
my_sdram_rw : process( sdClkfb_i, cpu_rst, 
                       opBegun, ramDone,
							  ram_state,
                       ram_rd_req, ram_wr_req )
begin

  if( cpu_rst = '1' ) then
	   hRd        <= '0';
      hWr        <= '0';
	   ram_hold   <= '0';
		ram_state  <= ram_state_0;
  elsif falling_edge(sdClkfb_i) then
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

  end if; -- clk
end process;

--
-- CPU read data request on rising CPU clock edge
-- CPU write data to request on rising CPU clock edge
--
ram_read_write_req: process( hRd, hWr, cpu_rst, cpu_clk, ram_cs, cpu_rw, ram_release )
begin
  if hRd = '1' or cpu_rst = '1' then
	   ram_rd_req <= '0';
  elsif rising_edge(cpu_clk) then
	   if (ram_cs = '1') and (cpu_rw = '1') and (ram_release = '1') then
		   ram_rd_req <= '1';
      end if; -- hRd
  end if;

  if hWr = '1' or cpu_rst = '1' then
      ram_wr_req <= '0';
  elsif rising_edge(cpu_clk) then
	   if (ram_cs = '1') and (cpu_rw = '0') and (ram_release = '1') then
		  ram_wr_req <= '1';
      end if; -- hWr		 
  end if; -- cpu_clk
end process;

--
-- Hold RAM until falling CPU clock edge
--
ram_bus_hold: process( cpu_rst, cpu_clk, ram_hold )
begin
    if cpu_rst = '1' then
	    ram_release   <= '1';
	 elsif falling_edge(cpu_clk) then
		 ram_release   <= not ram_hold;
	 end if;
end process;

--
-- Reset button and reset timer
--
my_rst_assignments : process( lock_o, fpgaclk_i )
begin
  cpu_rst <= (not lock_o);
  if( fpgaclk_i'event and fpgaclk_i='0' ) then
    if count /= "0000000000000000" then
      count <= count - "000000000000001";
      rst_i <= '1';
	 else
	   rst_i <= '0';
    end if;
  end if;
end process;

--
-- Interrupts and other bus control signals
--
my_irq_assignments : process( ram_hold,
                              acia_irq, 
							         keyboard_irq, 
							         trap_irq,
                              spi_irq,							 
							         timer_irq
							        )
begin
    cpu_irq    <= acia_irq or keyboard_irq or spi_irq;
	 cpu_nmi    <= trap_irq;
	 cpu_firq   <= timer_irq;
	 cpu_halt   <= '0';
	 cpu_hold   <= ram_hold;
end process;

--
-- spi master chip select outputs
--
my_spi_assignments : process( spi_cs_n )
begin
  flashCs_bo    <= spi_cs_n(0);
  usdflashCs_bo <= spi_cs_n(1);
end process;

--
-- RS232 signals:
--
my_acia_assignments : process( chan_io, txd, rts_n )
begin
  rxd         <= '1'; -- chan_io(17);
  cts_n       <= '0'; -- chan_io(18);
  dcd_n       <= '0';
  chan_io(1)  <= txd;
  chan_io(3)  <= rts_n;
end process;

--
-- VGA ouputs
--
my_vga_assignments : process( vga_red_o, vga_green_o, vga_blue_o, vga_vsync_n, vga_hsync_n )
begin
  chan_io(28)  <= vga_red_o;   -- R4
  chan_io(26)  <= vga_red_o;   -- R3
  chan_io(7)   <= vga_red_o;   -- R2
  chan_io(22)  <= vga_red_o;   -- R1
  chan_io(11)  <= vga_green_o; -- G4
  chan_io(25)  <= vga_green_o; -- G3
  chan_io(23)  <= vga_green_o; -- G2
  chan_io(5)   <= vga_green_o; -- G1
  chan_io(4)   <= vga_green_o; -- G0
  chan_io(10)  <= vga_blue_o;  -- B4
  chan_io(8)   <= vga_blue_o;  -- B3
  chan_io(6)   <= vga_blue_o;  -- B2
  chan_io(21)  <= vga_blue_o;  -- B1
  chan_io(20)  <= vga_blue_o;  -- B0
  chan_io(13)  <= vga_vsync_n; -- VS_N
  chan_io(14)  <= vga_hsync_n; -- HS_N
end process;

end rtl; --===================== End of architecture =======================--

