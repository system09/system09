--===========================================================================--
--                                                                           --
-- Synthesizable 6809 SoC Top Level File For Digilent XC3S1000 Starter Board --
--                                                                           --
--===========================================================================--
--
--
-- File name      : System09_Xess_XSA-3S1000.vhd
--
-- Purpose        : Top level file for 6809 compatible system on a chip
--                  Designed with Xilinx XC3S1000 Spartan 3 FPGA.
--                  Implemented With XESS XSA-3S1000 FPGA board.
--                  *** Note ***
--                  This configuration can run Flex9 however it only has
--                  32k bytes of user memory and the VDU is monochrome
--                  The design needs to be updated to use the SDRAM on 
--                  the XSA-3S1000 board.
--                  This configuration also lacks a DAT so cannot use
--                  the RAM Disk features of SYS09BUG.
--
-- Dependencies   : ieee.std_logic_1164
--                  ieee.std_logic_unsigned
--                  ieee.std_logic_arith
--                  ieee.numeric_std
--                  unisim.vcomponents
--
-- Uses           : mon_rom    (sys09bug_rom4k_b16.vhd) Sys09Bug Monitor ROM
--                  cpu09      (cpu09.vhd)          CPU core
--                  ACIA_6850  (ACIA_6850.vhd)      ACIA / UART
--                             (ACIA_RX.vhd)
--                             (ACIA_TX.vhd)
--                  ACIA_Clock (ACIA_Clock.vhd)      ACIA clock.
--                  keyboard   (keyboard.vhd)        PS/2 Keyboard interface
--                             (ps2_keyboard.vhd)
--                             (keymap_rom_slice.vhd) Key map table 
--                  vdu8_mono  (vdu8_mono.vhd)        Monochrome VDU
--                             (char_rom2k_b16.vhd)
--                             (ram2k_b16.vhd)
--                  timer      (timer.vhd)            Interrupt timer
--                  trap       (trap.vhd)             Bus condition trap logic
--                  flex_ram   (flex9_ram8k_b16.vhd)  Flex operating system
--                  ram_32K    (ram32k_b16.vhd)       32 KBytes of Block RAM
--                  
-- 
-- Author         : John E. Kent      
--                  dilbert57@opencores.org      
--
-- Memory Map     :
--
-- $0000 - User program RAM (32K Bytes)
-- $C000 - Flex Operating System memory (8K Bytes)
-- $E000 - ACIA (SWTPc)
-- $E010 - Reserved for FD1771 FDC (SWTPc)
-- $E020 - Keyboard
-- $E030 - VDU
-- $E040 - IDE / Compact Flash interface
-- $E050 - Timer
-- $E060 - Bus trap
-- $E070 - Reserced for Parallel I/O (B5-X300)
-- $E080 - Reserved for 6821 PIA (?) (SWTPc)
-- $E090 - Reserved for 6840 PTM (?) (SWTPc)
-- $F000 - Sys09Bug monitor Program (4K Bytes)
--
--  File name      : System09_Digilent_3S1000.vhd
--
--  Purpose        : Implements the top level of a Quad core 6809 instruction 
--                   System on a Chip (SoC) for the Digilent Spartan 3 Starter board 
--                   fitted with the XC3S1000 FPGA.
--                   Memory mapping of peripherals and memory are similar to the 
--                   SWTPc 6809 MP-09. 
--
--  Status:        : *** Currently under development ***
--                   The version for the Digilenet Spartan 3 Starter board using the
--                   XC3S200 FPGA should also work on the XC3S1000 if the FPGA type
--                   is changed in the Xilinx ISE project file.
--                  
--  Dependencies   : ieee.Std_Logic_1164
--                   ieee.std_logic_unsigned
--                   ieee.std_logic_arith
--                   ieee.numeric_std
--
--  Uses           : mon_rom   (sys09bug_rom4k_b16.vhd) Monitor ROM
--                   quadcpu09 (quadcpu09.vhd)          Quad CPU core
--                   dat_ram   (datram.vhd)             Dynamic Address Translation
--                   acia_6850 (acia_6850.vhd)          ACIA (UART)
--                             (acia_rx.vhd)
--                             (acia_tx.vhd)
--                   keyboard  (keyboard.vhd)           PS/2 Keyboard
--                             (ps2_keyboard.vhd)
--                             (keymap_rom)
--                   vdu8      (vdu8.vhd)		          Video Display Unit
--                             (char_rom2K_b16.vhd)
--                             (ram2k_b16.vhd)
--                   seven_segment (SevenSegment.vhd)   Seven Segment Display
--
--  Author         : John E. Kent
--
--  Email          : dilbert57@opencores.org      
--
--  Web            : http://opencores.org/project,system09
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
--                                                                           --
--                              Revision  History                            --
--                                                                           --
--===========================================================================--
--
-- Version       Date          Author     Changes
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

entity my_system09 is
  port(
    CLKA       : in  Std_Logic;  -- 100MHz Clock input
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
--	 S            : out std_logic_vector(7 downto 0);

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
    
	 -- Disable Flash
	 FLASH_CE_N   : out std_logic
	 );
end My_System09;

-------------------------------------------------------------------------------
-- Architecture for System09
-------------------------------------------------------------------------------
architecture rtl of my_system09 is

  -----------------------------------------------------------------------------
  -- constants
  -----------------------------------------------------------------------------
  constant SYS_CLK_FREQ  : integer := 50000000;  -- FPGA System Clock
  constant VGA_CLK_FREQ  : integer := 25000000;  -- VGA Pixel Clock
  constant CPU_CLK_FREQ  : integer := 25000000;  -- CPU Clock
  constant BAUD_Rate     : integer := 57600;	  -- Baud Rate
  constant ACIA_CLK_FREQ : integer := BAUD_Rate * 16;

  type hold_state_type is ( hold_release_state, hold_request_state );

  -----------------------------------------------------------------------------
  -- Signals
  -----------------------------------------------------------------------------
  signal rst_n          :  Std_logic;  -- Master Reset input (active low)
  signal nmi_n          :  Std_logic;  -- Non Maskable Interrupt input (active low)

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
  signal ram_hold       : std_logic; -- hold off slow accesses

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


-- SDRAM

  constant  FREQ                 :     natural := 100_000; -- operating frequency in KHz
  constant  CLK_DIV              :     real    := 2.0;    -- divisor for FREQ (can only be 1.0, 1.5, 2.0, 2.5, 3.0, 4.0, 5.0, 8.0 or 16.0)
  constant  PIPE_EN              :     boolean := false;  -- if true, enable pipelined read operations
  constant  MAX_NOP              :     natural := 10000;  -- number of NOPs before entering self-refresh
  constant  MULTIPLE_ACTIVE_ROWS :     boolean := false;  -- if true, allow an active row in each bank
  constant  DATA_WIDTH           :     natural := 16;     -- host & SDRAM data width
  constant  NROWS                :     natural := 8192;   -- number of rows in SDRAM array
  constant  NCOLS                :     natural := 512;    -- number of columns in SDRAM array
  constant  HADDR_WIDTH          :     natural := 24;     -- host-side address width
  constant  SADDR_WIDTH          :     natural := 13;     -- SDRAM-side address width

  signal   rst_i        : std_logic;     -- internal reset signal
  signal   clk_i        : std_logic;     -- internal master clock signal
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
  type ram_rd_type is (rd_state0, rd_state1, rd_state2, rd_state3);
  type ram_wr_type is (wr_state0, wr_state1, wr_state2, wr_state3, wr_state4);
  signal ram_rd_state   : ram_rd_type;
  signal ram_wr_state   : ram_wr_type;

--  signal BaudCount    : std_logic_vector(5 downto 0);
  signal CountL         : std_logic_vector(23 downto 0);
  signal clk_count      : std_logic_vector(0 downto 0);
  signal Clk25          : std_logic;
  signal pix_clk        : std_logic;

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
    address:  out	std_logic_vector(15 downto 0);
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
    data_in  : in  std_logic_vector (7 downto 0);
    data_out : out std_logic_vector (7 downto 0)
    );
end component;

-----------------------------------------------------------------
--
-- 6850 Compatible ACIA / UART
--
-----------------------------------------------------------------

component ACIA_6850
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
     SYS_CLK_FREQ  : integer := SYS_CLK_FREQ;
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
    rst      => cpu_reset,
    cs       => rom_cs,
    rw       => '1',
    addr     => cpu_addr(11 downto 0),
    data_in  => cpu_data_out,
    data_out => rom_data_out
    );

my_flex : flex_ram port map (
    clk       => cpu_clk,
    rst       => cpu_reset,
	 cs        => flex_cs,
	 rw        => cpu_rw,
    addr      => cpu_addr(12 downto 0),
    data_in   => cpu_data_out,
    data_out  => flex_data_out
    );

my_acia  : ACIA_6850 port map (
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
    SYS_CLK_FREQ  => SYS_CLK_FREQ,
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
      VGA_CLK_FREQ           => VGA_CLK_FREQ, -- HZ
	   VGA_HOR_CHARS          => 80, -- CHARACTERS
	   VGA_HOR_CHAR_PIXELS    =>  8, -- PIXELS
	   VGA_HOR_FRONT_PORCH    => 16, -- PIXELS
	   VGA_HOR_SYNC           => 96, -- PIXELS
	   VGA_HOR_BACK_PORCH     => 48, -- PIXELS
	   VGA_VER_CHARS          => 25, -- CHARACTERS
	   VGA_VER_CHAR_LINES     => 16, -- LINES
	   VGA_VER_FRONT_PORCH    => 10, -- LINES
	   VGA_VER_SYNC           =>  2, -- LINES
	   VGA_VER_BACK_PORCH     => 34  -- LINES
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
      vga_clk       => pix_clk,					 -- 25 MHz VDU pixel clock
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
      FREQ                 => FREQ,
      PIPE_EN              => PIPE_EN,
      DATA_WIDTH           => DATA_WIDTH,
      MULTIPLE_ACTIVE_ROWS => MULTIPLE_ACTIVE_ROWS,
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

pix_clk_buffer : BUFG port map(
    i => Clk25,
	 o => pix_clk
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
peripheral_bus: process( clk_i, cpu_reset, cpu_rw, cpu_addr, cpu_data_out )
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
peripheral_bus_hold: process( cpu_clk, cpu_reset, pb_rdu, pb_wrl, ether_rdy )
begin
    if cpu_reset = '1' then
		 pb_release    <= '0';
		 pb_count      <= "0000";
	    pb_hold_state <= hold_release_state;
	 elsif cpu_clk'event and cpu_clk='1' then
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
            if ether_rdy = '1' then
              pb_release    <= '1';
				  pb_hold_state <= hold_release_state;
            end if;
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
interrupts : process( lock, rst_n, nmi_n,
							 pb_cs, pb_hold, pb_release,
							 ram_cs, ram_hold,
							 ether_irq, 
                      acia_irq, 
							 keyboard_irq, 
							 trap_irq, 
							 timer_irq
							 )
begin
 	 cpu_reset <= (not rst_n) or (not lock); -- CPU reset is active high
    pb_hold   <= pb_cs and (not pb_release);
    cpu_irq   <= acia_irq or keyboard_irq;
	 cpu_nmi   <= trap_irq or not( nmi_n );
	 cpu_firq  <= timer_irq;
	 cpu_halt  <= '0';
	 cpu_hold  <= pb_hold or ram_hold;
end process;


--
-- Flash 7 segment LEDS
--
my_led_flasher: process( Clk_i, rst_n, CountL )
begin
    if rst_n = '0' then
		   CountL <= "000000000000000000000000";
    elsif(Clk_i'event and Clk_i = '1') then
		   CountL <= CountL + 1;
    end if;
--	 S(7 downto 0) <= CountL(23 downto 16);
end process;

--
-- Generate a 25 MHz Clock from 50 MHz
--
my_prescaler : process( Clk_i, clk_count )
begin
  if Clk_i'event and Clk_i = '1' then
    clk_count(0) <= not clk_count(0);
  end if;
  Clk25 <= clk_count(0);
end process;

--
-- Push buttons
--
my_switch_assignments : process( SW2_N, SW3_N, rst_n )
begin
  rst_n    <= SW2_N;
  rst_i    <= not rst_n; 
  nmi_n    <= SW3_N;
  --
  -- Disable Flash memory
  --
  FLASH_CE_N    <= '1';
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
-- SDRAM assignments
--
my_sdram_assignments : process( cpu_clk, clk_i, cpu_reset, 
                                opBegun, rdDone, wrDone,
										  ram_rd_state, ram_wr_state,
                                cpu_addr, dat_addr,
                                cpu_data_out, hDout,
										  ram_cs, cpu_rw, ram_hold )
begin
  if( cpu_reset = '1' ) then
    hWr    <= '0';
	 hRd    <= '0';
	 wrDone <= '0';
	 ram_wr_state <= wr_state0;
	 ram_rd_state <= rd_state0;

  elsif( clk_i'event and clk_i='0' ) then
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
  --
  -- Strobe host RD and WR signals high on RAM select
  -- Return low when cycle has started
  --
  if( cpu_reset = '1' ) then
	 ram_hold     <= '0';
  elsif( cpu_clk'event and cpu_clk='1' ) then
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

  hAddr(23 downto 20) <= "0000";
  hAddr(19 downto 12) <= dat_addr;
  hAddr(11 downto 0)  <= cpu_addr(11 downto 0);
  hDin(7 downto 0)    <= cpu_data_out;
  hDin(15 downto 8)   <= (others => '0');
  ram_data_out        <= hDout(7 downto 0);

end process;

end rtl; --===================== End of architecture =======================--

