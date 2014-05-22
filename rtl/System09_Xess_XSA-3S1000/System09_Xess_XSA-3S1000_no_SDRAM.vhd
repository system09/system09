--===========================================================================----
--
--  S Y N T H E Z I A B L E    System09 - SOC.
--
--  www.OpenCores.Org - February 2007
--  This core adheres to the GNU public license  
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
-- Dependencies   : ieee.Std_Logic_1164
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
--    CLKA         : in  Std_Logic;  -- 100MHz Clock input
    CLKB         : in  Std_Logic;  -- 50MHz Clock input
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
end My_System09;

-------------------------------------------------------------------------------
-- Architecture for System09
-------------------------------------------------------------------------------
architecture rtl of my_system09 is

  -----------------------------------------------------------------------------
  -- constants
  -----------------------------------------------------------------------------

  constant SDRAM                : boolean := false;

  -- SDRAM
  constant SYS_FREQ             : natural := 50_000_000;  -- FPGA System Clock
  constant CPU_FREQ             : natural := 25_000_000;  -- CPU Clock (Hz)
  constant CPU_DIV              : natural := (SYS_FREQ/CPU_FREQ);
  constant PIX_FREQ             : natural := 25_000_000;  -- VGA Pixel Clock
  constant PIX_DIV              : natural := (SYS_FREQ/PIX_FREQ);
  constant BAUD_RATE            : integer := 57600;	  -- Baud Rate
  constant ACIA_FREQ            : integer := BAUD_Rate * 16;

  constant Treset               : natural := 300;      -- min initialization interval (us)
  constant RST_CYCLES           : natural := 1+(Treset*(SYS_FREQ/1_000_000));  -- SDRAM power-on initialization interval

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

  -- CPU Interface signals
  signal cpu_reset      : Std_Logic;
  signal cpu_clk       : Std_Logic;
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
  signal slot1_cs      : std_logic;	-- Expansion slot 1
  signal slot2_cs      : std_logic;	-- Expansion slot 2

  signal rst_i         : std_logic;     -- internal reset signal
  signal clk_i         : std_logic;     -- internal master clock signal
  signal lock          : std_logic;     -- SDRAM clock DLL lock indicator

  type ram_type is (ram_state_0, 
                    ram_state_rd1, ram_state_rd2,
                    ram_state_wr1,
						  ram_state_3 );
  signal ram_state     : ram_type;


--  signal BaudCount   : std_logic_vector(5 downto 0);
  signal CountL        : std_logic_vector(23 downto 0);
  signal clk_count     : std_logic;
  signal Clk25         : std_logic;
  signal vga_clk       : std_logic;
  signal pix_count     : std_logic_vector(1 downto 0);
  signal rst_timer     : natural range 0 to RST_CYCLES;  -- current SDRAM op time

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
       clk   : in  std_logic;
		 rst   : in  std_logic;
		 cs    : in  std_logic;
		 rw    : in  std_logic;
       addr  : in  std_logic_vector (11 downto 0);
       rdata : out std_logic_vector (7 downto 0);
       wdata : in  std_logic_vector (7 downto 0)
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
    rdata    : out std_logic_vector (7 downto 0);
    wdata    : in  std_logic_vector (7 downto 0)
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
     irq      : out Std_Logic;  -- Interrupt
     Addr     : in  Std_Logic;  -- Register Select
     DataIn   : in  Std_Logic_Vector(7 downto 0); -- Data Bus In 
     DataOut  : out Std_Logic_Vector(7 downto 0); -- Data Bus Out
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
     SYS_Clock_Frequency  : integer :=  SYS_FREQ;
	  ACIA_Clock_Frequency : integer := ACIA_FREQ
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
  KBD_Clock_Frequency : integer := CPU_FREQ
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
        VDU_CLOCK_FREQUENCY    : integer := CPU_FREQ; -- HZ
        VGA_CLOCK_FREQUENCY    : integer := PIX_FREQ; -- HZ
	     VGA_HOR_CHARS          : integer := 80; -- CHARACTERS
	     VGA_VER_CHARS          : integer := 25; -- CHARACTERS
	     VGA_PIXELS_PER_CHAR    : integer := 8;  -- PIXELS
	     VGA_LINES_PER_CHAR     : integer := 16; -- LINES
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
    address   => cpu_addr(15 downto 0),
    data_in   => cpu_data_in,
	 data_out  => cpu_data_out,
	 halt      => cpu_halt,
	 hold      => cpu_hold,
	 irq       => cpu_irq,
	 nmi       => cpu_nmi,
	 firq      => cpu_firq
  );

my_rom : mon_rom port map (
       clk   => cpu_clk,
		 rst   => cpu_reset,
		 cs    => rom_cs,
		 rw    => '1',
       addr  => cpu_addr(11 downto 0),
       wdata => cpu_data_out,
       rdata => rom_data_out
    );

my_flex : flex_ram port map (
    clk       => cpu_clk,
    rst       => cpu_reset,
	 cs        => flex_cs,
	 rw        => cpu_rw,
    addr      => cpu_addr(12 downto 0),
    rdata     => flex_data_out,
    wdata     => cpu_data_out
    );

my_acia  : ACIA_6850 port map (
	 clk	     => cpu_clk,
	 rst       => cpu_reset,
    cs        => acia_cs,
	 rw        => cpu_rw,
    irq       => acia_irq,
    Addr      => cpu_addr(0),
	 Datain    => cpu_data_out,
	 DataOut   => acia_data_out,
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
    SYS_Clock_Frequency  =>  SYS_FREQ,
	 ACIA_Clock_Frequency => ACIA_FREQ
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
	KBD_Clock_Frequency => CPU_FREQ
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
      VDU_CLOCK_FREQUENCY    => CPU_FREQ, -- HZ
      VGA_CLOCK_FREQUENCY    => PIX_FREQ, -- HZ
	   VGA_HOR_CHARS          => 80, -- CHARACTERS
	   VGA_VER_CHARS          => 25, -- CHARACTERS
	   VGA_PIXELS_PER_CHAR    => 8,  -- PIXELS
	   VGA_LINES_PER_CHAR     => 16, -- LINES
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


cpu_clk_buffer : BUFG port map(
    i => Clk25,
	 o => cpu_clk
    );	 

reset_buffer : BUFG port map(
    i => rst_i,
	 o => cpu_reset
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
  elsif clk_i'event and clk_i = '1' then
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
  elsif clk_i'event and clk_i = '1' then
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
peripheral_bus_hold: process( cpu_clk, cpu_reset, pb_rdu, pb_wrl )
begin
    if cpu_reset = '1' then
		 pb_release    <= '0';
		 pb_count      <= "0000";
	    pb_hold_state <= hold_release_state;
	 elsif clk_i'event and clk_i = '1' then
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
              pb_release    <= '1';
				  pb_hold_state <= hold_release_state;
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
							 pb_cs, pb_hold, pb_release,
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
	 cpu_hold   <= pb_hold;
end process;


--
-- Flash 7 segment LEDS
--
my_led_flasher: process( clk_i, rst_i, CountL )
begin
    if rst_i = '1' then
		   CountL <= "000000000000000000000000";
    elsif clk_i'event and clk_i = '1' then
		   CountL <= CountL + 1;
    end if;
--	 S(7 downto 0) <= CountL(23 downto 16);
end process;

--
-- Generate CPU & Pixel Clock from Memory Clock
--
my_prescaler : process( clk_i, clk_count )
begin
  if clk_i'event and clk_i = '1' then
     clk_count <= not clk_count;
  end if;
  clk25 <= clk_count;
end process;

--
-- Reset button and reset timer
--
my_switch_assignments : process( SW2_N )
begin
  rst_i <= not SW2_N;
  FLASH_CE_N <= '1';
  ether_cs_n <= '1';
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


sdram_stub : process(CLKB)
begin
  ram_data_out <= (others => '0');
  clk_i <= CLKB;
  lock <= '1';
end process;

status_leds : process( rst_i, cpu_reset, lock )
begin
    S(0) <= rst_i;
	 S(1) <= cpu_reset;
	 S(2) <= lock;
	 S(3)	<= countL(23);
	 S(7 downto 4) <= "0000";
end process;

end rtl; --===================== End of architecture =======================--

