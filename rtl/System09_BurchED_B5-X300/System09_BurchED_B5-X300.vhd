--===========================================================================
--
--  System09 - SoC for the BurchED B5-X300 Spartan2 FPGA board.
--
--===========================================================================
--
-- File name      : System09_BurchED_B5-X300.vhd
--
-- Entity name    : my_system09
--
-- Purpose        : Top level file for 6809 compatible system on a chip
--                  Designed with Xilinx XC2S300e Spartan 2+ FPGA.
--                  Implemented With BurchED B5-X300 FPGA board,
--                  B5-SRAM module, B5-CF module and B5-FPGA-CPU-IO module
--
-- Dependencies   : ieee.Std_Logic_1164
--                  ieee.std_logic_unsigned
--                  ieee.std_logic_arith
--                  ieee.numeric_std
--
-- Uses           : clock_div     (../vhdl/clock_div.vhd)       System clock divider
--                  flasher       (../vhdl/flasher.vhd)         LED flasher
--                  BED_SRAM      (../vhdl/BED_SRAM.vhd)        BurchED SRAM interface
--                  cpu09         (../vhdl/cpu09.vhd)           CPU core
--                  SYS09BUG_F800 (../spartan2/sys09b5x_b4.vhd) Monitor ROM
--                  dat_ram       (../vhdl/datram.vhd)          Dynamic Address Translation
--                  acia6850      (../vhdl/acia6850.vhd)        ACIA
--                  ACIA_Clock    (../vhdl/ACIA_Clock.vhd)      ACIA Baud Clock Divider
--                  keyboard      (../vhdl/keyboard.vhd)        PS/2 Keyboard Interface
--                  vdu8          (../vhdl/vdu8.vhd)            80 x 25 Video Display
--                  timer         (../vhdl/timer.vhd)           Timer module
--                  trap	       (../vhdl/trap.vhd)            Bus Trap interrupt
--                  ioport        (../vhdl/ioport.vhd)          Parallel I/O port.
-- 
-- Author         : John E. Kent      
--                  dilbert57@opencores.org      
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
-- $E070 - Parallel I/O
-- $E080 - Reserved for SWTPc MP-ID 6821 PIA (?)
-- $E090 - Reserved for SWTPc MP-ID 6840 PTM (?)
-- $E0A0 - Reserved SPP Printer Port
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
--===========================================================================
--
--                             Revision History:
--
--===========================================================================
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
-- $E000 - ACIA
-- $E010 - Keyboard
-- $E020 - VDU
-- $E030 - Compact Flash
-- $E040 - Timer
-- $E050 - Bus trap
-- $E060 - Parallel I/O
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
-- Version 1.8 - 23rd February 2009 - John Kent
-- Renamed mon_rom to SYS09BUG_F800
--
-- Version 1.9 - 5th sepember 2010 - John Kent
-- Added Peripheral bus interface
-- Made the clock divider an external module
-- Rearranged VDU generic signals
-- Changed address decoding
-- Made the SRAM an external module
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

entity my_system09 is
  port(
    clk_in      : in  Std_Logic;  -- System Clock input
	 rst_n       : in  Std_logic;  -- Master Reset input (active low)
    LED         : out std_logic;  -- Diagnostic LED Flasher

    -- Memory Interface signals
    ram_csn     : out Std_Logic;
    ram_wrln    : out Std_Logic;
    ram_wrun    : out Std_Logic;
    ram_addr    : out Std_Logic_Vector(16 downto 0);
    ram_data    : inout Std_Logic_Vector(15 downto 0);

	 -- Stuff on the peripheral board

 	 -- PS/2 Keyboard
	 kb_clock    : inout Std_logic;
	 kb_data     : inout Std_Logic;

	 -- PS/2 Mouse interface
--	 mouse_clock : in  Std_Logic;
--	 mouse_data  : in  Std_Logic;

	 -- Uart Interface
    rxbit       : in  Std_Logic;
	 txbit       : out Std_Logic;
    rts_n       : out Std_Logic;
    cts_n       : in  Std_Logic;

	 -- CRTC output signals
	 v_drive     : out Std_Logic;
    h_drive     : out Std_Logic;
    blue_lo     : out std_logic;
    blue_hi     : out std_logic;
    green_lo    : out std_logic;
    green_hi    : out std_logic;
    red_lo      : out std_logic;
    red_hi      : out std_logic;
--	   buzzer      : out std_logic;

-- Compact Flash
    cf_rst_n     : out std_logic;
    cf_cs0_n     : out std_logic;
    cf_cs1_n     : out std_logic;
    cf_rd_n      : out std_logic;
    cf_wr_n      : out std_logic;
    cf_a         : out std_logic_vector(2 downto 0);
    cf_d         : inout std_logic_vector(15 downto 0);

-- Parallel I/O port
    porta        : inout std_logic_vector(7 downto 0);
    portb        : inout std_logic_vector(7 downto 0);

-- CPU bus
	 bus_clk      : out std_logic;
	 bus_reset    : out std_logic;
	 bus_rw       : out std_logic;
	 bus_cs       : out std_logic;
    bus_addr     : out std_logic_vector(15 downto 0);
	 bus_data     : inout std_logic_vector(7 downto 0)
	 );
end my_system09;

-------------------------------------------------------------------------------
-- Architecture for System09
-------------------------------------------------------------------------------
architecture rtl of my_system09 is
  -----------------------------------------------------------------------------
  -- constants
  -----------------------------------------------------------------------------
  constant SYS_CLK_FREQ  : integer := 50000000;  -- FPGA System Clock
  constant VGA_CLK_FREQ  : integer := 25000000;  -- VGA Pixel Clock
  constant CPU_CLK_FREQ  : integer := 12500000;  -- CPU Clock
  constant BAUD_RATE     : integer := 57600;	  -- Baud Rate
  constant ACIA_CLK_FREQ : integer := BAUD_RATE * 16;

  type hold_state_type is ( hold_release_state, hold_request_state );

  -----------------------------------------------------------------------------
  -- Signals
  -----------------------------------------------------------------------------
  -- Clock signals
  signal sys_clk       : std_logic;
  signal vga_clk       : std_logic;

  -- CPU Interface signals
  signal cpu_rst       : Std_Logic;
  signal cpu_clk       : Std_Logic;
  signal cpu_vma       : std_logic;
  signal cpu_addr      : std_logic_vector(15 downto 0);
  signal cpu_rw        : std_logic;
  signal cpu_data_in   : std_logic_vector(7 downto 0);
  signal cpu_data_out  : std_logic_vector(7 downto 0);
  signal cpu_firq      : std_logic;
  signal cpu_irq       : std_logic;
  signal cpu_nmi       : std_logic;
  signal cpu_halt      : std_logic;
  signal cpu_hold      : std_logic;

  -- Dynamic address translation
  signal dat_cs       : std_logic;
  signal dat_addr     : std_logic_vector(7 downto 0);

  -- Monitor ROM
  signal rom_data_out  : Std_Logic_Vector(7 downto 0);
  signal rom_cs        : std_logic;

  -- UART/ACIA Interface signals
  signal uart_data_out : Std_Logic_Vector(7 downto 0);  
  signal uart_cs       : Std_Logic;
  signal uart_irq      : Std_Logic;
  signal uart_clk      : Std_Logic;
  signal DCD_n         : Std_Logic;

  -- Keyboard port
  signal kbd_data_out  : std_logic_vector(7 downto 0);
  signal kbd_cs        : std_logic;
  signal kbd_irq       : std_logic;

  -- Video Display Unit
  signal vdu_cs        : std_logic;
  signal vdu_data_out  : std_logic_vector(7 downto 0);
  signal vga_red       : std_logic;
  signal vga_green     : std_logic;
  signal vga_blue      : std_logic;

  -- Timer
  signal tmr_data_out  : std_logic_vector(7 downto 0);
  signal tmr_cs        : std_logic;
  signal tmr_irq       : std_logic;

  -- Trap Hardware break point
  signal trap_cs       : std_logic;
  signal trap_data_out : std_logic_vector(7 downto 0);
  signal trap_irq      : std_logic;

  -- Parallel I/O port
  signal pio_data_out  : std_logic_vector(7 downto 0);
  signal pio_cs        : std_logic;

  -- Peripheral bus
  signal pb_data_out   : std_logic_vector(7 downto 0);
  signal pb_cs         : std_logic;
  signal pb_hold       : std_logic;

  -- Compact Flash on peripheral bus
  signal cf_cs         : std_logic;

  -- SRAM
  signal ram_cs        : std_logic; -- memory chip select
  signal ram_data_out  : std_logic_vector(7 downto 0);

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

------------------------------------------------------------
--
--           B5 SRAM interface ($0000 - $DFFF)
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
    -- BED_SRAM Interface Signals
    --
    ram_csn   : out   Std_Logic;
    ram_wrln  : out   Std_Logic;
    ram_wrun  : out   Std_Logic;
    ram_addr  : out   Std_Logic_Vector(16 downto 0);
    ram_data  : inout Std_Logic_Vector(15 downto 0)

    );
end component;


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
    rw       :	out std_logic;		-- Asynchronous memory interface
    data_in  : in  std_logic_vector(7 downto 0);
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
    clk:      in  std_logic;
	 rst:      in  std_logic;
	 cs:       in  std_logic;
	 rw:       in  std_logic;
	 addr_lo:  in  std_logic_vector(3 downto 0);
	 addr_hi:  in  std_logic_vector(3 downto 0);
    data_in:  in  std_logic_vector(7 downto 0);
	 data_out: out std_logic_vector(7 downto 0)
	 );
end component;

----------------------------------------
--
-- SBUG Block RAM Monitor ROM
--
----------------------------------------
component SYS09BUG_F800
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
-- 6850 ACIA/UART
--
-----------------------------------------------------------------

component acia6850
  port (
     clk      : in  Std_Logic;  -- System Clock
     rst      : in  Std_Logic;  -- Reset input (active high)
     cs       : in  Std_Logic;  -- miniUART Chip Select
     addr     : in  Std_Logic;  -- Register Select
     rw       : in  Std_Logic;  -- Read / Not Write
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
    VGA_CLK_FREQ           : integer := VGA_CLK_FREQ; -- 25MHz
	 VGA_HOR_CHARS          : integer := 80; -- CHARACTERS 25.6us
	 VGA_HOR_CHAR_PIXELS    : integer := 8;  -- PIXELS 0.32us
	 VGA_HOR_FRONT_PORCH    : integer := 16; -- PIXELS 0.64us (0.94us)
	 VGA_HOR_SYNC           : integer := 96; -- PIXELS 3.84us (3.77us)
	 VGA_HOR_BACK_PORCH     : integer := 48; -- PIXELS 1.92us (1.89us)
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
-- Timer module
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
-- Dual 8 bit Parallel I/O module
--
----------------------------------------
component ioport
  port (	
    clk       : in  std_logic;
    rst       : in  std_logic;
    cs        : in  std_logic;
    rw        : in  std_logic;
    addr      : in  std_logic_vector(1 downto 0);
    data_in   : in  std_logic_vector(7 downto 0);
    data_out  : out std_logic_vector(7 downto 0);
    porta_io  : inout std_logic_vector(7 downto 0);
    portb_io  : inout std_logic_vector(7 downto 0)
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


component BUFG 
  port (
		i: in  std_logic;
		o: out std_logic
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

----------------------------------------
--
-- Dynamic Address Translation Registers
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
-- SBUG / KBUG / SYS09BUG Monitor ROM
--
----------------------------------------
my_rom : SYS09BUG_F800 port map (
       clk      => cpu_clk,
		 rst      => cpu_rst,
		 cs       => rom_cs,
       addr     => cpu_addr(10 downto 0),
		 rw       => '1',
		 data_in  => cpu_data_out,
       data_out => rom_data_out
    );

----------------------------------------
--
-- ACIA/UART Serial interface
--
----------------------------------------
my_acia  : acia6850 port map (
	 clk	     => cpu_clk,
	 rst       => cpu_rst,
    cs        => uart_cs,
    addr      => cpu_addr(0),
	 rw        => cpu_rw,
	 data_in   => cpu_data_out,
	 data_out  => uart_data_out,
    irq       => uart_irq,
	 RxC       => uart_clk,
	 TxC       => uart_clk,
	 RxD       => rxbit,
	 TxD       => txbit,
	 DCD_n     => dcd_n,
	 CTS_n     => cts_n,
	 RTS_n     => rts_n
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
    acia_clk   => uart_clk
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
	  kbd_clk      => kb_clock,
	  kbd_data     => kb_data
	);


------------------------------------------------
--
-- Video Display Unit instantiation ($E030-$E03F)
--
-------------------------------------------------
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
    vga_red_o     => vga_red,
    vga_green_o   => vga_green,
    vga_blue_o    => vga_blue,
    vga_hsync_o   => h_drive,
    vga_vsync_o   => v_drive
  );

----------------------------------------
--
-- Timer Module
--
----------------------------------------
my_timer  : timer port map (
    clk       => cpu_clk,
	 rst       => cpu_rst,
    cs        => tmr_cs,
	 rw        => cpu_rw,
    addr      => cpu_addr(0),
	 data_in   => cpu_data_out,
	 data_out  => tmr_data_out,
    irq       => tmr_irq
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
-- Parallel I/O Port
--
----------------------------------------
my_ioport  : ioport port map (
	 clk       => cpu_clk,
    rst       => cpu_rst,
    cs        => pio_cs,
    rw        => cpu_rw,
    addr      => cpu_addr(1 downto 0),
    data_in   => cpu_data_out,
	 data_out  => pio_data_out,
	 porta_io  => porta,
	 portb_io  => portb
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
    pb_rd_n   => cf_rd_n,
    pb_wr_n   => cf_wr_n,
    pb_addr(2 downto 0) => cf_a,
    pb_addr(4 downto 3) => open,
    pb_data   => cf_d,

    -- Peripheral chip selects on Peripheral Bus 
    ide_cs    => cf_cs,
    eth_cs    => open,
    sl1_cs    => open,
    sl2_cs    => open
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
    -- BED_SRAM Interface Signals
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

mem_decode: process( dat_addr,
                     cpu_addr, cpu_rw, cpu_vma,
					      rom_data_out, 
						   uart_data_out,
							kbd_data_out,
							vdu_data_out,
						   tmr_data_out, 
							trap_data_out, 
							pio_data_out,
							bus_data,
                     pb_data_out,
							ram_data_out )
begin
  rom_cs   <= '0';              -- read ROM
  dat_cs   <= '0';              -- write DAT
  ram_cs   <= '0';
  uart_cs  <= '0';
  kbd_cs   <= '0';
  vdu_cs   <= '0';
  tmr_cs   <= '0';
  trap_cs  <= '0';
  pio_cs   <= '0';
  bus_cs   <= '0';
  pb_cs    <= '0';

  --
  -- ROM / DAT $FF00 - $FFFF
  -- 
  if cpu_addr( 15 downto 8 ) = "11111111" then
    cpu_data_in <= rom_data_out;
    rom_cs      <= cpu_vma;              -- read ROM
    dat_cs      <= cpu_vma;              -- write DAT
  else
    --
    -- Decode on 4K Byte boundaries
    --
    case dat_addr(3 downto 0) is
    when "1111" => -- $F000 - $FFFF
      if cpu_addr(11) = '1' then
        --
        -- SBUG/KBUG/SYS09BUG Monitor ROM $F800 - $FFFF
        --
        cpu_data_in <= rom_data_out;
        rom_cs      <= cpu_vma;
      else
        --
        -- SRAM $F000 - $F7FF
        -- Future use DMAF-2 Floppy Disk controller
        -- 
        cpu_data_in <= ram_data_out;
        ram_cs      <= cpu_vma;
      end if;
    --
    -- IO Devices $EXXX - $EXXX
    --
    when "1110" =>
      --
      -- Decode on 256 Byte boundaries
      -- IO device $E0XX - $E7XX
      --
      case cpu_addr(10 downto 8) is
      when "000" =>
         --
         -- Decode I/O Devices on 16 byte boundaries
         -- IO device $E00X - $E0FX
         --
		   case cpu_addr(7 downto 4) is
			--
			-- UART / ACIA $E000
			--
			when "0000" => -- $E000
		     cpu_data_in <= uart_data_out;
			  uart_cs     <= cpu_vma;

			--
			-- WD1771 FDC sites at $E010-$E01F
			--

         --
         -- Keyboard port $E020 - $E02F
			--
			when "0010" => -- $E020
           cpu_data_in <= kbd_data_out;
			  kbd_cs      <= cpu_vma;

         --
         -- VDU port $E030 - $E03F
			--
			when "0011" => -- $E030
           cpu_data_in <= vdu_data_out;
			  vdu_cs      <= cpu_vma;

         --
			-- Reserved $E040 - $E04F
         --

         --
         -- Timer $E050 - $E05F
			--
			when "0101" => -- $E050
           cpu_data_in <= tmr_data_out;
           tmr_cs    <= cpu_vma;

         --
         -- Bus Trap Logic $E060 - $E06F
			--
			when "0110" => -- $E060
           cpu_data_in <= trap_data_out;
			  trap_cs     <= cpu_vma;

         --
         -- Parallel I/O port $E070 - $E07F
			--
			when "0111" => -- $E070
           cpu_data_in <= pio_data_out;
			  pio_cs       <= cpu_vma;

         --
         -- Undefined / Extension Bus $E080 - $E0FF
         --
			when others => -- $E080 to $E0FF
           cpu_data_in <= bus_data;
           bus_cs      <= cpu_vma;
		   end case;

      --
      -- Peripheral Bus $E100 - $E1FF
      --
      when "001" =>
        cpu_data_in <= pb_data_out;
        pb_cs       <= cpu_vma;

      --
      -- Map RAM at $E200 - $EFFF Just in case we need driver space.
      --
      when others =>
         cpu_data_in <= ram_data_out;
         ram_cs      <= cpu_vma;
      end case;
    --
    -- Everything else is RAM $0000 - $DFFF
    --
    when others =>
      cpu_data_in <= ram_data_out;
      ram_cs      <= cpu_vma;
    end case;
  end if;
end process;

--
-- Interrupts and other bus control signals
--
interrupts : process( rst_n, pb_hold,
                      uart_irq, trap_irq, tmr_irq, kbd_irq )
begin
 	 cpu_rst   <= not rst_n; -- CPU reset is active high
    cpu_irq   <= uart_irq or kbd_irq;
	 cpu_nmi   <= trap_irq;
	 cpu_firq  <= tmr_irq;
	 cpu_halt  <= '0';
	 cpu_hold  <= pb_hold;
end process;

--
-- CPU bus signals
--
my_bus : process( cpu_clk, cpu_rst, cpu_rw, cpu_addr, cpu_data_out )
begin
	bus_clk   <= cpu_clk;
   bus_reset <= cpu_rst;
	bus_rw    <= cpu_rw;
   bus_addr  <= cpu_addr;
	if( cpu_rw = '1' ) then
	   bus_data <= (others => 'Z');
   else
	   bus_data <= cpu_data_out;
   end if;
end process;

--
-- Assign VDU VGA colour output
-- only 8 colours are handled.
--
my_vga_out: process( vga_red, vga_green, vga_blue )
begin
	   red_lo   <= vga_red;
      red_hi   <= vga_red;
      green_lo <= vga_green;
      green_hi <= vga_green;
      blue_lo  <= vga_blue;
      blue_hi  <= vga_blue;
end process;

--
-- CF card chip selects ($E100 - $E13F)
-- Located on peripheral bus
--
my_cf_decode: process( cpu_rst, cpu_addr, cf_cs )
begin

  cf_cs0_n  <= not( cf_cs and not cpu_addr(4));
  cf_cs1_n  <= not( cf_cs and     cpu_addr(4));
  cf_rst_n  <= not cpu_rst;

end process;

end rtl; --===================== End of architecture =======================--

