--===========================================================================--
--                                                                           --
-- Synthesizable 6809 SoC Top Level File For Digilent XC3S1000 Starter Board --
--                                                                           --
--===========================================================================--
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
--                   acia6850  (acia6850.vhd)           ACIA (UART)
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
-- Version 0.1 - 20 March 2003 - John Kent
-- Version 0.2 - 30 March 2003 - John Kent
-- Version 0.3 - 29 April 2003 - John Kent
-- Version 0.4 - 29 June 2003  - John Kent
--
-- Version 0.5 - 19 July 2003 - John Kent
-- prints out "Hello World"
--
-- Version 0.6 - 5 September 2003 - John Kent
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
-- Version 3.0 - 16 June 2010 - John Kent
-- *** Under development ***
-- Updated GPL notice.  
-- Work under way to implementing a quad core version.
--
--===========================================================================--
library ieee;
   use ieee.std_logic_1164.all;
   use ieee.std_logic_arith.all;
   use ieee.std_logic_unsigned.all;
   use ieee.numeric_std.all;

entity my_system09 is
  port(
    sys_clk     : in  Std_Logic;  -- System Clock input
	 rst_sw      : in  Std_logic;  -- Master Reset input (active high)
	 nmi_sw      : in  Std_logic;

    -- Memory Interface signals
    ram_addr    : out Std_Logic_Vector(17 downto 0);
    ram_wen     : out Std_Logic;
    ram_oen     : out Std_Logic;

    ram1_cen    : out Std_Logic;
	 ram1_ubn    : out Std_Logic;
	 ram1_lbn    : out Std_Logic;
    ram1_data   : inout Std_Logic_Vector(15 downto 0);

    ram2_cen    : out Std_Logic;
	 ram2_ubn    : out Std_Logic;
	 ram2_lbn    : out Std_Logic;
    ram2_data   : inout Std_Logic_Vector(15 downto 0);

 	 -- PS/2 Keyboard
	 ps2c        : inout Std_logic;
	 ps2d        : inout Std_Logic;

	 -- ACIA Interface
    rxd         : in  Std_Logic;
	 txd         : out Std_Logic;

	 -- CRTC output signals
	 vs          : out Std_Logic;
    hs          : out Std_Logic;
    blue        : out std_logic;
    green       : out std_logic;
    red         : out std_logic;

	 -- LEDS & Switches
	 switches    : in  std_logic_vector(7 downto 0);
	 leds        : out std_logic_vector(7 downto 0);

	 -- seven segment display
	 segments    : out std_logic_vector(7 downto 0);
	 digits      : out std_logic_vector(3 downto 0)
	 );
end my_system09;

-------------------------------------------------------------------------------
-- Architecture for System09
-------------------------------------------------------------------------------
architecture my_computer of my_system09 is
  -----------------------------------------------------------------------------
  -- constants
  -----------------------------------------------------------------------------
  constant SYS_CLK_FREQ  : integer := 50000000;  -- FPGA System Clock
  constant VGA_CLK_FREQ  : integer := 25000000;  -- VGA Pixel Clock
  constant CPU_CLK_FREQ  : integer := 25000000;  -- CPU Clock
  constant BAUD_RATE     : integer := 57600;	    -- Baud Rate
  constant ACIA_CLK_FREQ : integer := BAUD_RATE * 16;

  type hold_state_type is ( hold_release_state, hold_request_state );

  -----------------------------------------------------------------------------
  -- Signals
  -----------------------------------------------------------------------------

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
  signal kbd_cs       : std_logic;
  signal kbd_data_out : std_logic_vector(7 downto 0);
  signal kbd_irq      : std_logic;
  
  -- LEDs
  signal led_cs       : std_logic;
  signal led_data_out : std_logic_vector(7 downto 0);

  -- RAM
  signal ram_cs       : std_logic; -- memory chip select
  signal ram_data_out : std_logic_vector(7 downto 0);
  signal ram1_ce      : std_logic;
  signal ram1_ub      : std_logic;
  signal ram1_lb      : std_logic;
  signal ram2_ce      : std_logic;
  signal ram2_ub      : std_logic;
  signal ram2_lb      : std_logic;
  signal ram_we       : std_logic;
  signal ram_oe       : std_logic;

  -- CPU Interface signals
  signal cpu_rst      : Std_Logic;
  signal cpu_clk      : Std_Logic;
  signal cpu_vma      : std_logic;
  signal cpu_addr     : std_logic_vector(19 downto 0);
  signal cpu_rw       : std_logic;
  signal cpu_data_in  : std_logic_vector(7 downto 0);
  signal cpu_data_out : std_logic_vector(7 downto 0);
  signal cpu_firq     : std_logic;
  signal cpu_irq      : std_logic;
  signal cpu_nmi      : std_logic;
  signal cpu_halt     : std_logic;
  signal cpu_hold     : std_logic;

  -- Video Display Unit
  signal vdu_cs       : std_logic;
  signal vdu_data_out : std_logic_vector(7 downto 0);
  signal vga_clk      : std_logic;

  -- 7 Segment Display
  signal seg_cs       : std_logic;
  signal seg_data_out : std_logic_vector(7 downto 0);

  -- System Clock Prescaler
  signal clk_count    : std_logic;

-----------------------------------------------------------------
--
-- Quad CPU09 CPU core
--
-----------------------------------------------------------------

component quadcpu09
  port (    
	 clk      :	in  std_logic;
    rst      : in  std_logic;
    vma      :	out std_logic;
    addr     : out std_logic_vector(19 downto 0);
    rw       :	out std_logic;
	 data_out : out std_logic_vector(7 downto 0);
    data_in  : in	 std_logic_vector(7 downto 0);
	 irq      : in  std_logic;
	 nmi      : in  std_logic;
	 firq     : in  std_logic;
	 halt     : in  std_logic;
	 hold     : in  std_logic
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
     CLK      : in  std_logic;  -- System Clock Input
	  ACIA_CLK : out std_logic   -- ACIA Clock output
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
-- Seven Segment Display driver
--
----------------------------------------

component seven_segment is
	port (	
	  clk         : in  std_logic;
     rst         : in  std_logic;
     cs          : in  std_logic;
     addr        : in  std_logic_vector(1 downto 0);
     rw          : in  std_logic;
     data_in     : in  std_logic_vector(7 downto 0);
	  data_out    : out std_logic_vector(7 downto 0);
	  segments    : out std_logic_vector(7 downto 0);
	  digits	     : out std_logic_vector(3 downto 0)
	);
end component;

component BUFG 
  port (
    i            : in  std_logic;
    o            : out std_logic
  );
end component;

begin
  -----------------------------------------------------------------------------
  -- Instantiation of internal components
  -----------------------------------------------------------------------------

my_quadcpu : quadcpu09  port map (    
	 clk	     => cpu_clk,
    rst       => cpu_rst,
    vma       => cpu_vma,
    addr      => cpu_addr(19 downto 0),
    rw	     => cpu_rw,
	 data_out  => cpu_data_out,
    data_in   => cpu_data_in,
	 irq       => cpu_irq,
	 nmi       => cpu_nmi,
	 firq      => cpu_firq,
	 halt      => cpu_halt,
	 hold      => cpu_hold
    );

my_acia  : acia6850 port map (
	 clk	     => cpu_clk,
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
	kbd_clk      => ps2c,
	kbd_data     => ps2d
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
	   VGA_PIXELS_PER_CHAR    => 8,  -- PIXELS
	   VGA_LINES_PER_CHAR     => 16, -- LINES
	   VGA_HOR_BACK_PORCH     => 40, -- PIXELS
	   VGA_HOR_SYNC           => 96, -- PIXELS
	   VGA_HOR_FRONT_PORCH    => 24, -- PIXELS
	   VGA_VER_BACK_PORCH     => 13, -- LINES
	   VGA_VER_SYNC           => 1,  -- LINES
	   VGA_VER_FRONT_PORCH    => 36  -- LINES
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
      vga_clk       => vga_clk,					 -- 25 MHz VDU pixel clock
      vga_red_o     => red,
      vga_green_o   => green,
      vga_blue_o    => blue,
      vga_hsync_o   => hs,
      vga_vsync_o   => vs
   );


----------------------------------------
--
-- Seven Segment Display instantiation
--
----------------------------------------

my_seg : seven_segment port map (
    clk        => cpu_clk,
	 rst        => cpu_rst,
	 cs         => seg_cs,
	 addr       => cpu_addr(1 downto 0),
	 rw         => cpu_rw,
    data_in    => cpu_data_out,
	 data_out   => seg_data_out,
	 segments   => segments,
	 digits     => digits
	 );


vga_clk_buffer : BUFG port map(
    i => clk_count,
	 o => vga_clk
    );
	 	 
cpu_clk_buffer : BUFG port map(
    i => clk_count,
	 o => cpu_clk
    );	 

--
-- Clock divider
-- Assumes 50 MHz system clock
-- 25MHz pixel clock
-- 25MHz CPU clock
--
sys09_clock : process( sys_clk, clk_count )
begin
	if sys_clk'event and sys_clk='1' then
	   clk_count <= not clk_count;
   end if;
end process;
	 
----------------------------------------------------------------------
--
-- Process to decode memory map
--
----------------------------------------------------------------------

mem_decode: process( cpu_addr, cpu_rw, cpu_vma,
						   acia_data_out,
							kbd_data_out,
							vdu_data_out,
							seg_data_out,
							led_data_out,
							ram_data_out
							)
begin
      cpu_data_in <= (others=>'0');
      dat_cs      <= '0';
      acia_cs     <= '0';
      kbd_cs      <= '0';
      vdu_cs      <= '0';
      seg_cs      <= '0';
      led_cs     <= '0';
      ram_cs      <= '0';
--	     timer_cs    <= '0';
--      trap_cs     <= '0';
--	     pb_cs       <= '0';
--      ide_cs      <= '0';
--      ether_cs    <= '0';
--	     slot1_cs    <= '0';
--      slot2_cs    <= '0';

      --
		-- IO Devices $E000 - $EFFF
		--
		if cpu_addr(15 downto 12) = "1110" then -- $XE000 - $XEFFF
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
			    acia_cs     <= cpu_vma;
		       cpu_data_in <= acia_data_out;

	        --
			  -- Reserved - FD1771 FDC ($E010 - $E01F) (SWTPC)
           --

			  --
           -- Keyboard port ($E020 - $E02F)
			  --
			  when "0010" =>
			    kbd_cs      <= cpu_vma;
             cpu_data_in <= kbd_data_out;

           --
           -- VDU port ($E030 - $E03F)
			  --
			  when "0011" =>
			    vdu_cs      <= cpu_vma;
             cpu_data_in <= vdu_data_out;

           --
			  -- Reserved - SWTPc MP-T ($E040 - $E04F)
			  --

           --
           -- Reserved - Timer ($E050 - $E05F) (B5-X300)
			  --

           --
           -- Reserved - Bus Trap Logic ($E060 - $E06F) (B5-X300)
			  --

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
			    led_cs      <= cpu_vma;
             cpu_data_in <= led_data_out;

           --
           -- 7 segment display port ($E0B0 - $E0BF)
			  --
			  when "1011" =>
			    seg_cs      <= cpu_vma;
             cpu_data_in <= seg_data_out;


			  when others => -- $EXC0 to $EXFF
			    null;
		     end case;
			--
			-- XST-3.0 Peripheral Bus goes here
			--	$E100 to $E1FF
			--	Four devices
			-- IDE, Ethernet, Slot1, Slot2
			--
--			when "0001" =>
--			  cpu_data_in <= pb_data_out;
--			  pb_cs       <= cpu_vma;
--		     case cpu_addr(7 downto 6) is
			  --
			  -- IDE Interface $E100 to $E13F
			  --
--			  when "00" =>
--			    ide_cs   <= cpu_vma;
			  --
			  -- Ethernet Interface $E140 to $E17F
			  --
--			  when "01" =>
--			    ether_cs <= cpu_vma;
			  --
			  -- Slot 1 Interface $E180 to $E1BF
			  --
--			  when "10" =>
--			    slot1_cs <= cpu_vma;
			  --
			  -- Slot 2 Interface $E1C0 to $E1FF
			  --
--			  when "11" =>
--			    slot2_cs <= cpu_vma;
           --
			  -- Nothing else
			  --
--         when others =>
--           null;
--         end case;
         --
			--	$E200 to $EFFF reserved for future use
			--
        	when others =>
			  null;
         end case;
		--
		-- Everything else is RAM
		--
		else
		  ram_cs      <= cpu_vma;
		  cpu_data_in <= ram_data_out;
      end if;
end process;


--
-- 1M byte SRAM Control
-- Processes to read and write memory based on bus signals
--
ram_process: process( cpu_rst, sys_clk,
                      cpu_addr, cpu_rw, cpu_vma, cpu_data_out,
					       ram_cs,
                      ram1_ce, ram1_ub, ram1_lb, ram1_data,
                      ram2_ce, ram2_ub, ram2_lb, ram2_data,
							 ram_we, ram_oe )
begin
    --
    -- ram_hold signal helps 
    --
    if( cpu_rst = '1' ) then
	   ram_we   <= '0';
	   ram_oe   <= '0';
    --
	 -- Clock Hold on rising edge
	 --
    elsif( sys_clk'event and sys_clk='1' ) then
	   if (ram_cs = '1') and (ram_we = '0') and (ram_oe = '0') then
	     ram_we   <= not cpu_rw;
	     ram_oe   <=     cpu_rw;
      else
	     ram_we   <= '0';
	     ram_oe   <= '0';
      end if;
    end if;

	 ram_wen  <= not ram_we;
	 ram_oen  <= not ram_oe;

    ram1_ce   <= ram_cs and (not cpu_addr(1));
    ram1_ub   <= not cpu_addr(0);
    ram1_lb   <= cpu_addr(0);
    ram1_cen  <= not ram1_ce;
    ram1_ubn  <= not ram1_ub;
    ram1_lbn  <= not ram1_lb;

    ram2_ce   <= ram_cs and cpu_addr(1);
    ram2_ub   <= not cpu_addr(0);
    ram2_lb   <= cpu_addr(0);
    ram2_cen  <= not ram2_ce;
    ram2_ubn  <= not ram2_ub;
    ram2_lbn  <= not ram2_lb;

	 ram_addr(17 downto 0) <= cpu_addr(19 downto 2);

    if ram_we = '1' and ram1_ce = '1' and ram1_lb = '1' then
		ram1_data(7 downto 0) <= cpu_data_out;
	 else
      ram1_data(7 downto 0)  <= "ZZZZZZZZ";
	 end if;

    if ram_we = '1' and ram1_ce = '1' and ram1_ub = '1' then
		ram1_data(15 downto 8) <= cpu_data_out;
	 else
      ram1_data(15 downto 8)  <= "ZZZZZZZZ";
	 end if;

    if ram_we = '1' and ram2_ce = '1' and ram2_lb = '1' then
		ram2_data(7 downto 0) <= cpu_data_out;
	 else
      ram2_data(7 downto 0)  <= "ZZZZZZZZ";
	 end if;

    if ram_we = '1' and ram2_ce = '1' and ram2_ub = '1' then
		ram2_data(15 downto 8) <= cpu_data_out;
	 else
      ram2_data(15 downto 8)  <= "ZZZZZZZZ";
	 end if;

	 case cpu_addr(1 downto 0) is
	 when "00" =>
      ram_data_out <= ram1_data(15 downto 8);
	 when "01" =>
      ram_data_out <= ram1_data(7 downto 0);
	 when "10" =>
      ram_data_out <= ram2_data(15 downto 8);
    when others =>
      ram_data_out <= ram2_data(7 downto 0);
    end case;
end process;

--
-- LEDS output register
--
led_output : process( cpu_clk, cpu_rst, switches )
begin
	if cpu_rst = '1' then
		leds <= (others=>'0');
	elsif cpu_clk'event and cpu_clk='0' then
		if	led_cs = '1' and cpu_rw = '0' then
			leds <= cpu_data_out;
		end if;
	end if;
	led_data_out <= switches;
end process;

--
-- Interrupts and other bus control signals
--
interrupts : process(	rst_sw, 
								acia_irq,
								kbd_irq,
								nmi_sw
							 )
begin
   if sys_clk'event and sys_clk = '1' then
	  cpu_rst  <= rst_sw; -- CPU reset is active high
   end if;
	cpu_firq   <= kbd_irq;
	cpu_nmi    <= nmi_sw;
	cpu_irq    <= acia_irq;
	cpu_halt   <= '0';
	cpu_hold   <= '0';
end process;

--
-- ACIA pin assignments
--
acia_assignments : process( rxd, acia_txd )
begin
	acia_dcd_n <= '0';
	acia_cts_n <= '0';
	acia_rxd   <= rxd;
	txd        <= acia_txd;
end process;


end my_computer; --===================== End of architecture =======================--

