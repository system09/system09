--=============================================================================--
--                                                                             --
--  System09 - Synthesizable System On a Chip - VHDL FPGA core top level file. --
--                                                                             --
--=============================================================================--
--
--
-- File name      : System09_Terasic_DE1.vhd
--
-- Entity name    : System09
--
-- Purpose        : Top level file for 6809 compatible system on a chip
--                  Designed with Alera EP2C20 Cyclone 2 FPGA.
--                  Implemented with Terasic DE1 FPGA board,
--
-- Dependencies   : ieee.Std_Logic_1164
--                  ieee.std_logic_unsigned
--                  ieee.std_logic_arith
--                  ieee.numeric_std
--
-- Uses           : cpu09         (..\VHDL\cpu09.vhd)          CPU core
--                  dat_ram       (..\VHDL\datram.vhd)         Dynamic Address Translation
--                  mon_rom       (..\Cyclone2\mon_rom.vhd)    Monitor ROM
--                  acia6850      (..\VHDL\acia6850.vhd)       ACIA (UART)
--                  ACIA_Clock    (..\VHDL\ACIA_Clock.vhd)     ACIA Baud Rate Clock Divider
--                  keyboard      (..\VHDL\keyboard.vhd)       PS/2 Keyboard register interface
--                  ps2_keyboard  (..\VHDL\ps2_keyboard.vhd)   PS/2 Keyboard interface logic
--                  keymap_rom    (..\Cyclone2\keymap_rom.vhd) PS/2 Keyboard key code look up table
--                  trap          (..\VHDL\trap.vhd)           Hardware breakpointy trap
--                  timer         (..\VHDL\timer.vhd)          Interrupt timer
--                  vdu8          (..\VHDL\vdu8.vhd)		   Video Display Unit
--                                (..\Cyclone2\char_rom.vhd)   Character Generator ROM (B16_RAM)
--                                (..\Cyclone2\ram_2k.vhd)     Text & Attribute RAM Buffer
--                  sprom         (..\Cyclone2\sprom.vhd)      Single port altsyncrom   
--                  spram         (..\Cyclone2\spram.vhd)      Single port altsyncram   
--                  bit_funcs     (..\VHDL\bit_funcs.vhd)      Bit manipulation Functions package   
-- 
-- Author         : John E. Kent      
--                  dilbert57@opencores.org      
--
-- Memory Map     :
--
-- $0000 - $DFFF System RAM (512K Mapped via DAT)
-- $E000 - ACIA (SWTPc)
-- $E010 - Reserved for SWTPc FD-01 FD1771 FDC
-- $E020 - Keyboard
-- $E030 - VDU
-- $E040 - Reserved for SWTPc MP-T (was Compact Flash)
-- $E050 - Timer
-- $E060 - Trap (Hardware Breakpoint)
-- $E070 - Reserved for Trace Buffer
-- $E080 - Reserved for SWTPc MP-ID 6821 PIA (?)
-- $E090 - Reserved for SWTPc MP-ID 6840 PTM (?)
-- $E0A0 - Switches in / LEDS out
-- $E0B0 - 7 Segment hex display
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
-- Inverted sys_clk
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
--	removed Compaact Flash and Trap Logic.
-- Replaced SBUG with KBug9s
--
-- Version 2.1 - 21 November 2006 - John Kent
-- Replaced KBug9s with Sys09bug 1.0
-- Inverted bottom nybble of DAT register outputs
-- Changed ROM & I/O decoding to be compatible with SWTPc
-- Upped the serial baud rate to 115.2 KBd
-- added multiple global clock buffers
-- (Uart would not operate correctly)
--
-- Version 2.2 - 22 December 2006 - John Kent
-- Increased CPU clock from 12.5MHz to 25 MHz.
-- Removed some of the global clock buffers
-- Added LED output register
-- Changed address decoding to 4K Blocks
--
-- Version 2.3 - 1 June 2007 - John Kent
-- Updated VDU & ACIA
-- Changed decoding for Sys09Bug
--
-- Version 2.4 - 31 January 2008 - John Kent
--	ACIA does not appear to work.
-- Made RAM OE and WE strobes synchonous to sys_clk
--
-- Version 2.5 - 23rd Feburary 2009 - John Kent
-- Implemented recommendation to remove vga_clk clock buffer
--
-- Version 2.6 - 5th september 2010 - John Kent
-- Renamed ACIA_6850 to acia6850
-- Updated generics on VDU8
-- Shortened the "keyboard" label
-- Fixed up address label on CPU09
-- Removed Flex RAM
-- Map RAM at $F000 - $F7FF
--
-- Vesrion 2.7 - 22nd October 2010 - John Kent
-- Ported from Digilent XC3S200 Starter to 
-- Terasic EP2C20 DE1 board.
--   
--===========================================================================--
library ieee;
   use ieee.std_logic_1164.all;
   use IEEE.STD_LOGIC_ARITH.ALL;
   use IEEE.STD_LOGIC_UNSIGNED.ALL;
   use ieee.numeric_std.all;

entity System09_Terasic_DE1 is
  port(
	-- clocks
	clock_24    : in    std_logic_vector(1 downto 0);
	clock_27    : in    std_logic; 
    clock_50    : in    std_logic;  -- System Clock input
    ext_clock   : in    std_logic;
    
    -- push button keys
	key         : in    std_logic_vector(3 downto 0);  -- RST, NMI

    -- SRAM interface signals

    sram_ce_n   : out   std_logic;
    sram_we_n   : out   std_logic;
    sram_oe_n   : out   std_logic;
	sram_ub_n   : out   std_logic;
	sram_lb_n   : out   std_logic;
    sram_addr   : out   std_logic_vector(17 downto 0);
    sram_dq     : inout std_logic_vector(15 downto 0);

	-- PS/2 Keyboard
	ps2_clk     : inout Std_logic;
	ps2_dat     : inout Std_Logic;

	-- ACIA/UART Interface
    uart_rxd    : in  Std_Logic;
	uart_txd    : out Std_Logic;

	-- CRTC output signals
    vga_r       : out std_logic_vector(3 downto 0);
    vga_g       : out std_logic_vector(3 downto 0);
    vga_b       : out std_logic_vector(3 downto 0);
    vga_hs      : out Std_Logic;
	vga_vs      : out Std_Logic;

	-- LEDS & Switches
	ledg        : out std_logic_vector(7 downto 0);
	ledr        : out std_logic_vector(9 downto 0);
	sw          : in  std_logic_vector(9 downto 0);

	-- hexadecimal display
	hex0        : out std_logic_vector(6 downto 0);
	hex1        : out std_logic_vector(6 downto 0);
	hex2        : out std_logic_vector(6 downto 0);
	hex3        : out std_logic_vector(6 downto 0);
	 
	-- gnereal purpose I/O
	gpio_0      : in  std_logic_vector(35 downto 0);
	gpio_1      : in  std_logic_vector(35 downto 0);
	 
	-- i2c interface
	i2c_sclk    : in std_logic;
	i2c_sdat    : in std_logic;
	 
	-- audio codec
	aud_adcdat  : in std_logic;
	aud_adclrck : in std_logic;
	aud_bclk    : in std_logic;
	aud_dacdat  : in std_logic;
	aud_daclrck : in std_logic;
	aud_xck     : in std_logic;
	  
	-- dram
	dram_addr  : out std_logic_vector(11 downto 0);
	dram_dq    : in  std_logic_vector(15 downto 0);
	dram_cs_n  : out std_logic;
	dram_ba_0  : in std_logic;
	dram_ba_1  : in std_logic;
	dram_ldqm  : in std_logic;
	dram_udqm  : in std_logic;
	dram_ras_n : out std_logic;
	dram_cas_n : out std_logic;
	dram_we_n  : out std_logic;
	dram_cke   : in std_logic;
	dram_clk   : in std_logic;
	 
	 -- Flash memory
	fl_addr    : out std_logic_vector(21 downto 0);
	fl_dq      : in  std_logic_vector(7 downto 0);
	fl_rst_n   : in  std_logic;
	fl_oe_n    : out std_logic;
	fl_we_n    : out std_logic;
	
	-- JTAG
	tck        : in std_logic;
	tcs        : in std_logic;
	tdi        : in std_logic;
	tdo        : in std_logic
	);
end System09_Terasic_DE1;

-------------------------------------------------------------------------------
-- Architecture for System09
-------------------------------------------------------------------------------
architecture my_computer of System09_Terasic_DE1 is
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

  signal sys_clk       : std_logic;
  signal vga_clk       : std_logic;
  signal pll_locked    : std_logic;
  
  -- CPU Interface signals
  signal cpu_clk       : std_logic;
  signal cpu_rst       : std_logic;
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

  -- Dynamic Address Translation
  signal dat_cs        : std_logic;
  signal dat_addr      : std_logic_vector(7 downto 0);

  -- BOOT ROM
  signal rom_cs        : Std_logic;
  signal rom_data_out  : Std_Logic_Vector(7 downto 0);

  -- ACIA Interface signals
  signal acia_clk      : std_logic;
  signal acia_cs       : Std_Logic;
  signal acia_data_out : Std_Logic_Vector(7 downto 0);  
  signal acia_irq      : Std_Logic;
  signal acia_rxd      : Std_Logic;
  signal acia_txd      : Std_Logic;
  signal acia_dcd_n    : Std_Logic;
--  signal acia_rts_n    : Std_Logic;
  signal acia_cts_n    : Std_Logic;

  -- keyboard port
  signal kbd_data_out : std_logic_vector(7 downto 0);
  signal kbd_cs       : std_logic;
  signal kbd_irq      : std_logic;
  
  -- LEDs
  signal leds_data_out : std_logic_vector(7 downto 0);
  signal leds_cs       : std_logic;

  -- Video Display Unit
  signal vdu_cs       : std_logic;
  signal vdu_data_out : std_logic_vector(7 downto 0);
  signal vga_red      : std_logic;
  signal vga_green    : std_logic;
  signal vga_blue     : std_logic;

  -- LEDs
  signal led_cs       : std_logic;
  signal led_data_out : std_logic_vector(7 downto 0);

  -- 7 Segment Display
  signal hex_cs       : std_logic;
  signal hex_data_out : std_logic_vector(7 downto 0);
  signal hex0_reg     : std_logic_vector(7 downto 0);
  signal hex1_reg     : std_logic_vector(7 downto 0);
  signal hex2_reg     : std_logic_vector(7 downto 0);
  signal hex3_reg     : std_logic_vector(7 downto 0);
  
  -- interrupt timer
  signal tmr_data_out : std_logic_vector(7 downto 0);
  signal tmr_cs       : std_logic;
  signal tmr_irq      : std_logic;

  -- hardware break point (trap)
  signal hbp_data_out : std_logic_vector(7 downto 0);
  signal hbp_cs       : std_logic;
  signal hbp_irq      : std_logic;

  -- RAM
  signal sram_cs       : std_logic; -- memory chip select
  signal sram_data_out : std_logic_vector(7 downto 0);
  signal sram_ce       : std_logic;
  signal sram_ub       : std_logic;
  signal sram_lb       : std_logic;
  signal sram_we       : std_logic;
  signal sram_oe       : std_logic;

  -- System Clock Prescaler
  signal clk_count     : std_logic;

-----------------------------------------------------------------
--
-- CPU09 CPU core
--
-----------------------------------------------------------------

component cpu09
  port (    
	 clk      :	in  std_logic;
    rst      : in  std_logic;
    vma      :	out std_logic;
    addr     : out std_logic_vector(15 downto 0);
    rw       :	out std_logic;
    data_in  : in	 std_logic_vector(7 downto 0);
	 data_out : out std_logic_vector(7 downto 0);
	 halt     : in  std_logic;
	 hold     : in  std_logic;
	 irq      : in  std_logic;
	 nmi      : in  std_logic;
	 firq     : in  std_logic
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
-- 4KByte Block RAM Monitor ROM
--
----------------------------------------
component mon_rom
  Port (
    clk      : in  std_logic;
    rst      : in  std_logic;
    cs       : in  std_logic;
    rw       : in  std_logic;
    addr     : in  std_logic_vector (10 downto 0);
    data_in  : in  std_logic_vector (7 downto 0);
    data_out : out std_logic_vector (7 downto 0)
    );
end component;

-----------------------------------------------------------------
--
-- 6850 ACIA
--
-----------------------------------------------------------------

component acia6850
  port (
    clk      : in  Std_Logic;  -- System Clock
    rst      : in  Std_Logic;  -- Reset input (active high)
    cs       : in  Std_Logic;  -- ACIA Chip Select
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
    RTS_n    : out Std_Logic   -- Request To send
  );
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
	  acia_clk : out Std_logic   -- ACIA Clock output
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
    addr            : in    std_logic;
    rw              : in    std_logic;
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
    vdu_clk      : in  std_logic;	 -- CPU Clock - 12.5MHz
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
-- Interrupt timer
--
----------------------------------------

component timer is
	port (	
		clk        : in  std_logic;
		rst        : in  std_logic;
		cs         : in  std_logic;
		addr       : in  std_logic;
		rw         : in  std_logic;
		data_in    : in  std_logic_vector(7 downto 0);
		data_out   : out std_logic_vector(7 downto 0);
		irq        : out std_logic
	);
end component;

----------------------------------------
--
-- hardware break point (bus trap)
--
----------------------------------------

component trap is
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


component pll IS
	PORT
	(
		areset		: IN STD_LOGIC  := '0';
		inclk0		: IN STD_LOGIC  := '0';
		c0			: OUT STD_LOGIC ;
		c1			: OUT STD_LOGIC ;
		c2			: OUT STD_LOGIC ;
		locked		: OUT STD_LOGIC 
	);
END component;

begin
  -----------------------------------------------------------------------------
  -- Instantiation of internal components
  -----------------------------------------------------------------------------

my_cpu : cpu09  port map (    
	 clk	     => cpu_clk,
    rst       => cpu_rst,
    vma       => cpu_vma,
    addr      => cpu_addr(15 downto 0),
    rw	     => cpu_rw,
    data_in   => cpu_data_in,
	 data_out  => cpu_data_out,
	 halt      => cpu_halt,
	 hold      => cpu_hold,
	 irq       => cpu_irq,
	 nmi       => cpu_nmi,
	 firq      => cpu_firq
    );

my_dat : dat_ram port map (
    clk       => cpu_clk,
	rst       => cpu_rst,
	cs        => dat_cs,
	addr_hi   => cpu_addr(15 downto 12),
	addr_lo   => cpu_addr(3 downto 0),
	rw        => cpu_rw,
	data_in   => cpu_data_out,
	data_out  => dat_addr(7 downto 0)
	 );

my_rom : mon_rom port map (
    clk       => cpu_clk,
    rst       => cpu_rst,
	cs        => rom_cs,
	rw        => '1',
    addr      => cpu_addr(10 downto 0),
    data_in   => cpu_data_out,
    data_out  => rom_data_out
    );

my_acia  : acia6850 port map (
	clk	      => cpu_clk,
	rst       => cpu_rst,
    cs        => acia_cs,
    addr      => cpu_addr(0),
	rw        => cpu_rw,
	data_in   => cpu_data_out,
	data_out  => acia_data_out,
    irq       => acia_irq,
	RxC       => acia_clk,
	TxC       => acia_clk,
	RxD       => acia_rxd,
	TxD       => acia_txd,
	DCD_n     => acia_dcd_n,
	CTS_n     => acia_cts_n,
	RTS_n     => open
    );


----------------------------------------
--
-- ACIA Clock
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
	  cs           => kbd_cs,
	  addr         => cpu_addr(0),
	  rw           => cpu_rw,
	  data_in      => cpu_data_out(7 downto 0),
	  data_out     => kbd_data_out(7 downto 0),
	  irq          => kbd_irq,
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

    -- Control Registers
    vdu_clk       => cpu_clk,					 -- 12.5 MHz System Clock in
    vdu_rst       => cpu_rst,
    vdu_cs        => vdu_cs,
    vdu_addr      => cpu_addr(2 downto 0),
    vdu_rw        => cpu_rw,
    vdu_data_in   => cpu_data_out,
    vdu_data_out  => vdu_data_out,

    -- vga port connections
    vga_clk       => vga_clk,					 -- 25 MHz pixel clock
    vga_red_o     => vga_red,
    vga_green_o   => vga_green,
    vga_blue_o    => vga_blue,
    vga_hsync_o   => vga_hs,
    vga_vsync_o   => vga_vs
  );

----------------------------------------
--
-- Interrupt timer
--
----------------------------------------

my_timer : timer
	port map (	
		clk        => cpu_clk,
		rst        => cpu_rst,
		cs         => tmr_cs,
		addr       => cpu_addr(0),
		rw         => cpu_rw,
		data_in    => cpu_data_out,
		data_out   => tmr_data_out,
		irq        => tmr_irq
	);

----------------------------------------
--
-- hardware break point (bus trap)
--
----------------------------------------

my_hw_bp : trap
	port map (	
		clk        => cpu_clk,
		rst        => cpu_rst,
		cs         => hbp_cs,
		rw         => cpu_rw,
		vma        => cpu_vma,
		addr       => cpu_addr,
		data_in    => cpu_data_out,
		data_out   => hbp_data_out,
		irq        => hbp_irq
	);

----------------------------------------
--
-- Phase Locked Loop Clock divider
--

my_pll : pll
	PORT MAP
	(
		areset		=> '0',
		inclk0		=> clock_50,
		c0			=> sys_clk,
		c1			=> cpu_clk,
		c2			=> vga_clk,
		locked		=> pll_locked 
	);
	 
----------------------------------------------------------------------
--
-- Process to decode memory map
--
----------------------------------------------------------------------

mem_decode: process( cpu_addr, cpu_rw, cpu_vma,
					dat_cs, dat_addr,
					rom_data_out,
					acia_data_out,
					kbd_data_out,
					vdu_data_out,
					hex_data_out,
					led_data_out,
					tmr_data_out,
					hbp_data_out,
					sram_data_out
					)
begin
      cpu_data_in <= (others=>'0');
      dat_cs      <= '0';
      rom_cs      <= '0';
      acia_cs     <= '0';
      kbd_cs      <= '0';
      vdu_cs      <= '0';
      hex_cs      <= '0';
      led_cs      <= '0';
      sram_cs     <= '0';
	  tmr_cs      <= '0';
      hbp_cs      <= '0';
--	    pb_cs       <= '0';
--      ide_cs      <= '0';
--      ether_cs    <= '0';
--	    slot1_cs    <= '0';
--      slot2_cs    <= '0';

      if cpu_addr( 15 downto 8 ) = "11111111" then
	    cpu_data_in <= rom_data_out;
        dat_cs      <= cpu_vma;              -- write DAT
        rom_cs      <= cpu_vma;              -- read  ROM
	    --
		-- Sys09Bug Monitor ROM $F000 - $FFFF
		--
	   elsif dat_addr(3 downto 0) = "1111" then -- $XF000 - $XFFFF
          if cpu_addr(11) = '1' then
            --
		    -- Monitor ROM $F800 - $FFFF
		    --
            cpu_data_in <= rom_data_out;
            rom_cs      <= cpu_vma;          -- read  ROM
          else
            --
		    -- SRAM $F000 - $F7FF
		    --
            cpu_data_in <= sram_data_out;
            sram_cs      <= cpu_vma;
          end if;
        --
		-- IO Devices $E000 - $EFFF
		--
		elsif dat_addr(3 downto 0) = "1110" then -- $XE000 - $XEFFF
			case cpu_addr(11 downto 8) is
			--
			-- SWTPC peripherals from $E000 to $E0FF
			--
			when "0000" =>
		      case cpu_addr(7 downto 4) is
			  --
			  -- ACIA ($E000 - $E00F)
			  --
			  when "0000" =>
		        cpu_data_in <= acia_data_out;
			    acia_cs      <= cpu_vma;

	          --
			  -- Reserved - FD1771 FDC ($E010 - $E01F) (SWTPC)
              --

			  --
              -- Keyboard port ($E020 - $E02F)
			  --
			  when "0010" =>
                cpu_data_in <= kbd_data_out;
			    kbd_cs      <= cpu_vma;

              --
              -- VDU port ($E030 - $E03F)
			  --
			  when "0011" =>
                cpu_data_in <= vdu_data_out;
			    vdu_cs      <= cpu_vma;

              --
			  -- Reserved - SWTPc MP-T ($E040 - $E04F)
			  --

              --
              -- Reserved - Timer ($E050 - $E05F) (B5-X300)
			  --
			  when "0101" =>
			    cpu_data_in <= tmr_data_out;
			    tmr_cs      <= cpu_vma;
			    
              --
              -- Reserved - hardware break point (Bus Trap) ($E060 - $E06F)
			  --
			  when "0110" =>
			    cpu_data_in <= hbp_data_out;
			    hbp_cs      <= cpu_vma;
			    
              --
              -- Reserved - I/O port ($E070 - $E07F) (B5-X300)
			  --

			  --
			  -- Reserved - PTM 6840 ($E080 - $E08F) (SWTPC)
			  --

			  --
			  -- Reserved - PIA Timer ($E090 - $E09F) (SWTPC)
			  --

              --
			  -- Read Switched port ($E0A0 - $E0AF)
			  -- Write LEDS
			  --
			  when "1010" =>
                cpu_data_in <= led_data_out;
			    led_cs     <= cpu_vma;

              --
              -- 7 segment display port ($E0B0 - $E0BF)
			  --
			  when "1011" =>
                cpu_data_in <= hex_data_out;
			    hex_cs      <= cpu_vma;


			  when others => -- $EXC0 to $EXFF
			    null;
		      end case;
            --
			--	$E100 to $EFFF reserved for future use
			--
        	when others =>
			  null;
         end case;
		--
		-- Everything else is RAM
		--
		else
		  cpu_data_in <= sram_data_out;
		  sram_cs      <= cpu_vma;
    end if;
end process;


--
-- 1M byte SRAM Control
-- Processes to read and write memory based on bus signals
--
sram_process: process( sys_clk, cpu_rst,
                      cpu_addr, cpu_rw, cpu_vma, cpu_data_out,
					  dat_addr, sram_cs,
                      sram_ce,  sram_ub, sram_lb, sram_dq,
					  sram_we,  sram_oe )
begin
    --
	-- Clock Hold on rising edge
	--
    if( sys_clk'event and sys_clk='1' ) then
		--
		-- sram_hold signal helps 
		--
		if( cpu_rst = '1' ) then
			sram_we   <= '0';
			sram_oe   <= '0';
		else
			if (sram_cs = '1') and (sram_we = '0') and (sram_oe = '0') then
				sram_we   <= not cpu_rw;
				sram_oe   <=     cpu_rw;
			else
				sram_we   <= '0';
				sram_oe   <= '0';
			end if;
		end if;
    end if;

	sram_we_n  <= not sram_we;
	sram_oe_n  <= not sram_oe;

    sram_ce   <= sram_cs;
    sram_ub   <= not cpu_addr(0);
    sram_lb   <= cpu_addr(0);
    sram_ce_n <= not sram_ce;
    sram_ub_n <= not sram_ub;
    sram_lb_n <= not sram_lb;

	sram_addr(17 downto 11) <= dat_addr(6 downto 0);
	sram_addr(10 downto 0) <= cpu_addr(11 downto 1);

    if sram_we = '1' and sram_ce = '1' and sram_lb = '1' then
		sram_dq(7 downto 0) <= cpu_data_out;
	else
        sram_dq(7 downto 0)  <= "ZZZZZZZZ";
	end if;

    if sram_we = '1' and sram_ce = '1' and sram_ub = '1' then
		sram_dq(15 downto 8) <= cpu_data_out;
	else
        sram_dq(15 downto 8)  <= "ZZZZZZZZ";
	end if;

 	if cpu_addr(0) = '0' then
        sram_data_out <= sram_dq(15 downto 8);
	else
        sram_data_out <= sram_dq(7 downto 0);
    end if;
    
end process;

--
-- LEDS output register
--
led_output : process( cpu_clk, cpu_rst, led_cs, cpu_rw, cpu_addr, cpu_data_out, sw )
begin
	if cpu_clk'event and cpu_clk='0' then
		if cpu_rst = '1' then
			ledr <= (others=>'0');
			ledg <= (others=>'0');
		else
			if led_cs = '1' and cpu_rw = '0' then
				if cpu_addr(0) = '0' then
					ledr(7 downto 0) <= cpu_data_out;
				else
					ledg(7 downto 0) <= cpu_data_out;
				end if;
			end if;
		end if;
	end if;
	led_data_out <= sw(7 downto 0);
end process;

--
-- 7 segment HEX display output register
--
hex_output : process( cpu_clk, cpu_rst, hex_cs, cpu_rw, cpu_addr, cpu_data_out,
						hex0_reg, hex1_reg, hex2_reg, hex3_reg )
begin
	if cpu_clk'event and cpu_clk='0' then
		if cpu_rst = '1' then
			hex0_reg <= (others=>'0');
			hex1_reg <= (others=>'0');
			hex2_reg <= (others=>'0');
			hex3_reg <= (others=>'0');
		else
			if	hex_cs = '1' and cpu_rw = '0' then
				case cpu_addr(1 downto 0) is
				when "00" => 
					hex0_reg <= cpu_data_out;
				when "01" => 
					hex1_reg <= cpu_data_out;
				when "10" => 
					hex2_reg <= cpu_data_out;
				when "11" => 
					hex3_reg <= cpu_data_out;
				end case;
			end if;
		end if;
	end if;

	case cpu_addr(1 downto 0) is
	when "00" => 
		hex_data_out <= hex0_reg;
	when "01" => 
		hex_data_out <= hex1_reg;
	when "10" => 
		hex_data_out <= hex2_reg;
	when "11" => 
		hex_data_out <= hex3_reg;
	end case;

    hex0 <= not hex0_reg(6 downto 0);
    hex1 <= not hex1_reg(6 downto 0);
    hex2 <= not hex2_reg(6 downto 0);
    hex3 <= not hex3_reg(6 downto 0);
    	
end process;

--
-- Interrupts and other bus control signals
--
interrupts : process(   key, pll_locked, 
						acia_irq, kbd_irq, hbp_irq, tmr_irq )
begin
	cpu_rst    <= not key(0); -- CPU reset is active high
	cpu_firq   <= kbd_irq;
	cpu_nmi    <= (not key(1)) or hbp_irq;
	cpu_irq    <= acia_irq or tmr_irq;
	cpu_halt   <= '0';
	cpu_hold   <= '0';
end process;

--
-- ACIA pin assignments
--
acia_assignments : process( uart_rxd, acia_txd )
begin
	acia_dcd_n <= '0';
	acia_cts_n <= '0';
	acia_rxd   <= uart_rxd;
	uart_txd   <= acia_txd;
end process;

--
-- assign vga colour bits to single bit RGB output of VDU
--
vga_assignments : process( vga_red, vga_green, vga_blue )
begin
	vga_r <= (others=>vga_red);
	vga_g <= (others=>vga_green);
	vga_b <= (others=>vga_blue);
end process;

--
-- assign dram signals
--
dram_assign : process( all )
begin
	dram_addr  <= (others=>'0');
	dram_cs_n  <= '1';
	dram_ras_n <= '1';
	dram_cas_n <= '1';
	dram_we_n  <= '1';
end process;

--
-- assign flash memory signals
--
flash_assign : process( all )
begin
	fl_addr    <= (others=>'0');
	fl_oe_n    <= '1';
	fl_we_n    <= '1';
	
end process;

end my_computer; --===================== End of architecture =======================--

