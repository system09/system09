-- SECD Front End Processor derived from System09 written by John E. Kent
-- This core adheres to the GNU public license  

library ieee;
use ieee.std_logic_1164.all;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use ieee.numeric_std.all;
--use config.all;

entity System09_trenz is
  port(
	 -- the following output assignments are required so that
    -- the GT3200 USB phy generates a 30 MHz clock
    utmi_databus16_8 : out std_logic;
    utmi_reset       : out std_logic;
    utmi_xcvrselect  : out std_logic;
    utmi_termselect  : out std_logic;
    utmi_opmode1     : out std_logic;
    utmi_txvalid     : out std_logic;

		-- this is the 30 MHz clock input (clkout is the utmi name)
    utmi_clkout      : in std_logic;

    reset_sw    : in    Std_logic;  -- Master Reset input (active low)

    -- PS/2 Keyboard
    ps2_clk1    : inout Std_logic;
    ps2_data1   : inout Std_Logic;

    -- acia Interface
    fpga_rxd    : in    Std_Logic;
    fpga_txd    : out   Std_Logic;
    fpga_cts    : in    Std_Logic;
    fpga_rts    : out   Std_Logic;

    -- CRTC output signals
    vsync_b     : out   Std_Logic;
    hsync_b     : out   Std_Logic;
    fpga_b      : out   Std_Logic_Vector(2 downto 0);
    fpga_g      : out   Std_Logic_Vector(2 downto 0);
    fpga_r      : out   Std_Logic_Vector(2 downto 0);

    -- LEDS & Switches
    mm_led      : out   Std_Logic;
    led         : out   Std_Logic_Vector(3 downto 0);

    joy_down    : in    Std_Logic;
    joy_fire    : in    Std_Logic;
    joy_left    : in    Std_Logic;
    joy_right   : in    Std_Logic;
    joy_up      : in    Std_Logic;

    -- LCD Display
    lcd_e       : out   Std_Logic;
    lcd_rw      : out   Std_Logic;
    lcd_rs      : out   Std_Logic;
    lcd_d       : inout Std_Logic_Vector(3 downto 0);

    -- Audio
    aud_out     : out   std_logic_vector(4 downto 1);

    --ir remote control
    ir_data     : in    std_logic;

    -- Memory interface
    ram_a       : out   std_logic_vector(20 downto 0);
    ram_io      : inout std_logic_vector(15 downto 0);
    ram_bhen    : out   std_logic;
    ram_blen    : out   std_logic;
    ram_cen     : out   std_logic;
    ram_oen     : out   std_logic;
    ram_wen     : out   std_logic;

	 -- Flash interface
	 fl_resetn   : out   std_logic;
	 fl_cen      : out   std_logic;
	 fl_oen      : out   std_logic;
	 fl_byten    : out   std_logic;
	 fl_busyn    : in    std_logic;

    -- Compact flash
	 cf_we       : out   std_logic;		  -- all these signals are active low
	 cf_reg      : out   std_logic;		  -- for more details see the specification
	 cf_cs0      : out   std_logic;		  -- of compact flash
	 cf_cs1      : out   std_logic;
	 cf_reset    : out   std_logic;
	 cf_iord     : out   std_logic;
	 cf_iowr     : out   std_logic;
	 cf_irq      : in    std_logic;
	 cf_wait     : in    std_logic;
	 cf_cd1      : in    std_logic;
	 cf_cd2      : in    std_logic;
	 iois16      : in    std_logic;
	 cf_oe       : out   std_logic;
	 cf_dasp     : inout std_logic;
	 cf_pdiag    : inout std_logic;

		--cf power enable (active low)
	 cf_pwr_en   : out   std_logic
    );
end System09_trenz;

-------------------------------------------------------------------------------
-- Architecture for System09
-------------------------------------------------------------------------------
architecture rtl of System09_trenz is
  -----------------------------------------------------------------------------
  -- constants
  -----------------------------------------------------------------------------
  constant SYS_Clock_Frequency  : integer := 50_000_000;  -- FPGA System Clock
  constant VGA_Clock_Frequency  : integer := 25_000_000;  -- VGA Pixel Clock
  constant CPU_Clock_Frequency  : integer := 25_000_000;  -- CPU Clock
  constant BAUD_Rate            : integer := 57600;	  -- Baud Rate
  constant ACIA_Clock_Frequency : integer := BAUD_Rate * 16;

  type hold_state_type is ( hold_release_state, hold_request_state );

  -----------------------------------------------------------------------------
  -- ChipScope Pro components and signals
  -----------------------------------------------------------------------------

--  component icon
--    port(control0    :   out std_logic_vector(35 downto 0));
--  end component;

--  component ila
--    port(control     : in    std_logic_vector(35 downto 0);
--         clk         : in    std_logic;
--         trig0       : in    std_logic_vector(39 downto 0));
--  end component;

--  signal chipscope_control : std_logic_vector(35 downto 0);
--  signal ila_clock : std_logic;

  -----------------------------------------------------------------------------
  -- Signals
  -----------------------------------------------------------------------------

  -- Clocks
  attribute buffer_type            : string;
  attribute period                 : string;

  signal    sys_clk                : std_logic;      -- 50 Mhz
  attribute period of sys_clk      : signal is "20 ns";
  attribute buffer_type of sys_clk : signal is "BUFG";

  signal    cpu_clk                : std_logic;      -- 25 Mhz
  attribute period of cpu_clk      : signal is "40 ns";
  attribute buffer_type of cpu_clk : signal is "BUFG";

  signal    vga_clk                : std_logic;      -- 25 Mhz
  attribute period of vga_clk      : signal is "40 ns";
  attribute buffer_type of vga_clk : signal is "BUFG";

  -- System Reset (generated by key press)
  signal cpu_reset     : std_logic;

  -- Dynamic Address Translation
  signal dat_cs        : std_logic;
  signal dat_addr      : std_logic_vector(7 downto 0);

  -- BOOT ROM
  signal rom_cs        : Std_logic;
  signal rom_data_out  : Std_Logic_Vector(7 downto 0);

  -- FLEX9 RAM
  signal flex_cs       : Std_logic;
  signal flex_data_out : Std_Logic_Vector(7 downto 0);

  -- acia Interface signals
  signal acia_data_out : Std_Logic_Vector(7 downto 0);  
  signal acia_cs       : Std_Logic;
  signal acia_irq      : Std_Logic;
  signal baudclk       : Std_Logic;
  signal DCD_n         : Std_Logic;
  signal RTS_n         : Std_Logic;
  signal CTS_n         : Std_Logic;

  -- keyboard port
  signal keyboard_data_out : std_logic_vector(7 downto 0);
  signal keyboard_cs       : std_logic;
  signal keyboard_irq      : std_logic;

  -- CPU Interface signals
  signal cpu_rw       : std_logic;
  signal cpu_vma      : std_logic;
  signal cpu_halt     : std_logic;
  signal cpu_hold     : std_logic;
  signal cpu_firq     : std_logic;
  signal cpu_irq      : std_logic;
  signal cpu_nmi      : std_logic;
  signal cpu_addr     : std_logic_vector(15 downto 0);
  signal cpu_data_in  : std_logic_vector(7 downto 0);
  signal cpu_data_out : std_logic_vector(7 downto 0);

  -- Compact Flash port
  -- CF data bus shared with RAM
--  signal cf_data_out : std_logic_vector(7 downto 0);
  signal cf_cs0x     : std_logic;
  signal cf_cs1x     : std_logic;
  signal cf_rd       : std_logic;
  signal cf_wr       : std_logic;
  signal cf_hold     : std_logic;
  signal cf_release  : std_logic;
  signal cf_count    : std_logic_vector(3 downto 0);
  signal cf_hold_state : hold_state_type;

  -- Video Display Unit
  signal vdu_cs       : std_logic;
  signal vdu_data_out : std_logic_vector(7 downto 0);

  -- VGA output signals (distributed to VGA DAC)
  signal red          : std_logic;
  signal green        : std_logic;
  signal blue         : std_logic;

  -- LCD register select
  signal lcd_cs       : std_logic;
  signal lcd_data_in  : std_logic_vector(7 downto 0);
  signal lcd_data_out : std_logic_vector(7 downto 0);

  -- LED register select
  signal leds_cs       : std_logic;
  signal leds_data_in  : std_logic_vector(7 downto 0) := (others => '0');
  signal leds_data_out : std_logic_vector(7 downto 0) := (others => '0');

  -- Joystick buffer
  signal joy_cs       : std_logic;
  signal joy_data_out : std_logic_vector(7 downto 0);

  -- External RAM interface
  signal ram_cs       : std_logic := '0';
  signal ram_data_out : std_logic_vector(7 downto 0);
  signal ram_oe       : std_logic;
  signal ram_we       : std_logic;

  -- Locked signal of clock synthesizer
  signal clock_locked : std_logic;
  signal ila_clock    : std_logic;

  -- LED Flasher
  signal blink_count  : std_logic_vector(25 downto 0) := (others => '0');

  -- System Clock Prescaler
  signal clk_count    : std_logic;
  

-----------------------------------------------------------------
--
-- CPU09 CPU core
--
-----------------------------------------------------------------

  component cpu09
    port (    
      clk      : in  std_logic;
      rst      : in  std_logic;
      rw       : out std_logic;		-- Asynchronous memory interface
      vma      : out std_logic;
      address  : out std_logic_vector(15 downto 0);
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
-- 4KByte Block RAM Monitor ROM
--
----------------------------------------
component mon_rom
  Port (
    clk      : in  std_logic;
    rst      : in  std_logic;
    cs       : in  std_logic;
    rw       : in  std_logic;
    addr     : in  std_logic_vector (11 downto 0);
    rdata    : out std_logic_vector (7 downto 0);
    wdata    : in  std_logic_vector (7 downto 0)
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


-----------------------------------------------------------------
--
-- 6850 ACIA / UART
--
-----------------------------------------------------------------

  component ACIA_6850
    port (
      clk      : in  Std_Logic;  -- System Clock
      rst      : in  Std_Logic;  -- Reset input (active high)
      cs       : in  Std_Logic;  -- ACIA Chip Select
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
      SYS_Clock_Frequency  : integer :=  SYS_Clock_Frequency;
      ACIA_Clock_Frequency : integer := ACIA_Clock_Frequency
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
      KBD_Clock_Frequency : integer := CPU_Clock_Frequency
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
      VDU_CLOCK_FREQUENCY    : integer := CPU_Clock_Frequency; -- HZ
      VGA_CLOCK_FREQUENCY    : integer := VGA_Clock_Frequency; -- HZ
      VGA_HOR_CHARS          : integer := 80; -- CHARACTERS
      VGA_VER_CHARS          : integer := 25; -- CHARACTERS
      VGA_PIXELS_PER_CHAR    : integer := 8;  -- PIXELS
      VGA_LINES_PER_CHAR     : integer := 16; -- LINES
      VGA_HOR_BACK_PORCH     : integer := 40; -- PIXELS
      VGA_HOR_SYNC           : integer := 96; -- PIXELS
      VGA_HOR_FRONT_PORCH    : integer := 24; -- PIXELS
      VGA_VER_BACK_PORCH     : integer := 13; -- LINES
      VGA_VER_SYNC           : integer := 1;  -- LINES
      VGA_VER_FRONT_PORCH    : integer := 36  -- LINES
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

--  component ram_controller
--  port(
--    reset      : in  std_logic;
--    clk        : in  std_logic;
--    cs_ram     : in  std_logic;
--    rw         : in  std_logic;
--    din        : in  std_logic_vector(7 downto 0);
--    dout       : out std_logic_vector(7 downto 0);
--    addr       : in  std_logic_vector(19 downto 0);

    -- External interface
--    ram_oen    : out   std_logic;
--    ram_cen    : out   std_logic;
--    ram_wen    : out   std_logic;
--    ram_io     : inout std_logic_vector(15 downto 0);
--    ram_a      : out   std_logic_vector(20 downto 1);
--    ram_bhen   : out   std_logic;
--    ram_blen   : out   std_logic
--  );
--  end component;

component BUFG 
  port (
    i            : in  std_logic;
    o            : out std_logic
  );
end component;

begin

  -----------------------------------------------------------------
  --
  -- ChipsScope Pro cores
  --
  -----------------------------------------------------------------

--  i_icon : icon
--    port map(control0  => chipscope_control);
--
--  i_ila : ila
--    port map(control => chipscope_control,
--             clk => ila_clock,
--             trig0(15 downto 8) => cpu_data_in,
--             trig0(23 downto 16) => cpu_data_out,
--             trig0(39 downto 24) => cpu_addr,
--             trig0(0) => cpu_clk,
--             trig0(1) => cpu_vma,
--             trig0(2) => ram_bhenx,
--             trig0(3) => ram_blenx,
--             trig0(4) => ram_cenx,
--             trig0(5) => ram_oenx,
--             trig0(6) => ram_wenx,
--             trig0(7) => vga_clk);


  
  -----------------------------------------------------------------
  --
  -- CPU09 CPU core
  --
  -----------------------------------------------------------------

  my_cpu : entity cpu09 port map (    
    clk	     => cpu_clk,
    rst       => cpu_reset,
    rw	     => cpu_rw,
    vma       => cpu_vma,
    address   => cpu_addr,
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
  -- Sys09Bug ROM (Xilinx Block RAM, 4k)
  --
  ----------------------------------------

my_rom : mon_rom port map (
    clk       => cpu_clk,
    rst       => cpu_reset,
	 cs        => rom_cs,
	 rw        => '1',
    addr      => cpu_addr(11 downto 0),
    rdata     => rom_data_out,
    wdata     => cpu_data_out
    );

  ----------------------------------------
  --
  -- Flex Operating System (Xilinx Block RAM, 8k)
  --
  ----------------------------------------

my_flex : flex_ram port map (
    clk       => cpu_clk,
    rst       => cpu_reset,
	 cs        => flex_cs,
	 rw        => cpu_rw,
    addr      => cpu_addr(12 downto 0),
    rdata     => flex_data_out,
    wdata     => cpu_data_out
    );

  ----------------------------------------
  --
  -- Dynamic Address Translation
  --
  ----------------------------------------

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


  -----------------------------------------------------------------
  --
  -- 6850 ACIA
  --
  -----------------------------------------------------------------

  my_acia  : entity acia_6850 port map (
    clk	     => cpu_clk,
    rst       => cpu_reset,
    cs        => acia_cs,
    rw        => cpu_rw,
    irq       => acia_irq,
    Addr      => cpu_addr(0),
    Datain    => cpu_data_out,
    DataOut   => acia_data_out,
    RxC       => baudclk,
    TxC       => baudclk,
    RxD       => fpga_rxd,
    TxD       => fpga_txd,
    DCD_n     => dcd_n,
    CTS_n     => fpga_cts,
    RTS_n     => fpga_rts
    );


----------------------------------------
--
-- PS/2 Keyboard Interface
--
----------------------------------------
  my_keyboard : keyboard
    generic map (
      KBD_Clock_Frequency => CPU_Clock_frequency
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
      kbd_clk      => ps2_clk1,
      kbd_data     => ps2_data1
      );

----------------------------------------
--
-- Video Display Unit instantiation
--
----------------------------------------
  my_vdu : vdu8 
    generic map(
      VDU_CLOCK_FREQUENCY    => CPU_Clock_Frequency, -- HZ
      VGA_CLOCK_FREQUENCY    => VGA_Clock_Frequency, -- HZ
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
      vdu_rst       => cpu_reset,
      vdu_cs        => vdu_cs,
      vdu_rw        => cpu_rw,
      vdu_addr      => cpu_addr(2 downto 0),
      vdu_data_in   => cpu_data_out,
      vdu_data_out  => vdu_data_out,

      -- vga port connections
      vga_clk       => vga_clk,					 -- 25 MHz VDU pixel clock
      vga_red_o     => red,
      vga_green_o   => green,
      vga_blue_o    => blue,
      vga_hsync_o   => hsync_b,
      vga_vsync_o   => vsync_b
      );

  ----------------------------------------
  --
  -- Clock Synthesis instantiation
  --
  ----------------------------------------

  my_clock_synthesis : entity clock_synthesis port map (
    clk_30mhz       => utmi_clkout,
    sys_clk_out     => sys_clk,
    locked          => clock_locked );


vga_clk_buffer : BUFG port map(
    i => clk_count,
	 o => vga_clk
    );
	 	 
cpu_clk_buffer : BUFG port map(
    i => clk_count,
	 o => cpu_clk
    );	 
  
  ----------------------------------------
  --
  -- RAM Controller instantiation
  --
  ----------------------------------------

--  my_external_ram : entity ram_controller port map (
--    reset              => cpu_reset,
--    clk                => cpu_clk,
--    cs_ram             => ram_cs,
--    rw                 => cpu_rw,
--    din                => cpu_data_out,
--    dout               => ram_data_out,
--    addr(19 downto 12) => dat_addr( 7 downto 0),
--    addr(11 downto 0)  => cpu_addr(11 downto 0),

    -- external interface
--    ram_oen 		=> ram_oenx,
--    ram_cen 		=> ram_cenx,
--    ram_wen 		=> ram_wenx,
--    ram_io        => ram_io,
--    ram_a         => ram_a,
--    ram_bhen 		=> ram_bhenx,
--    ram_blen		=> ram_blenx
--    );


----------------------------------------
--
-- ACIA Clock
--
----------------------------------------
  my_ACIA_Clock : ACIA_Clock
    generic map(
      SYS_Clock_Frequency  => SYS_Clock_Frequency,
      ACIA_Clock_Frequency => ACIA_Clock_Frequency
      ) 
    port map(
      clk        => sys_clk,
      acia_clk   => baudclk
      ); 


--
-- Generate a 25 MHz Clock from 50 MHz
--
my_sys09_clk : process( sys_clk, clk_count )
begin
  if sys_clk'event and sys_clk = '1' then
    clk_count <= not clk_count;
  end if;
end process;

----------------------------------------------------------------------
--
-- Process to decode memory map
--
----------------------------------------------------------------------

mem_decode: process( cpu_addr, cpu_rw, cpu_vma,
					      dat_cs, dat_addr,
					      rom_data_out,
						   acia_data_out,
							keyboard_data_out,
							vdu_data_out,
							joy_data_out,
							lcd_data_out,
							leds_data_out,
							flex_data_out,
							ram_data_out
							)
begin
	 cpu_data_in <= (others => '0');
    dat_cs      <= '0';
    rom_cs      <= '0';
    acia_cs     <= '0';
    keyboard_cs <= '0';
    vdu_cs      <= '0';
	 cf_cs0x     <= '0';
	 cf_cs1x     <= '0';
	 joy_cs      <= '0';
    lcd_cs      <= '0';
    leds_cs     <= '0';
    flex_cs     <= '0';
    ram_cs      <= '0';

    if cpu_addr( 15 downto 8 ) = "11111111" then
	     cpu_data_in <= rom_data_out;
        dat_cs      <= cpu_vma;              -- write DAT
        rom_cs      <= cpu_vma;              -- read  ROM
	 --
    -- Sys09Bug Monitor ROM $F000 - $FFFF
	 --
	 elsif dat_addr(3 downto 0) = "1111" then -- $XF000 - $XFFFF
	   --
		-- Monitor ROM $F000 - $FFFF
		--
      cpu_data_in <= rom_data_out;
      rom_cs      <= cpu_vma;          -- read  ROM

    --
	 -- IO Devices $E000 - $EFFF
	 --
	 elsif dat_addr(3 downto 0) = "1110" then -- $XE000 - $XEFFF
		   dat_cs      <= '0';
			rom_cs      <= '0';
			case cpu_addr(11 downto 8) is
			when "0000" =>
		     case cpu_addr(7 downto 4) is
			  --
			  -- UART / ACIA ($E000 - $E00F)
			  --
			  when "0000" =>
		       cpu_data_in <= acia_data_out;
			    acia_cs     <= cpu_vma;

			  --
			  -- Reserved - FD1771 FDC ($E010 - $E01F) (SWTPC)
           --

			  --
           -- Keyboard port ($E020 - $E02F)
			  --
			  when "0010" =>
             cpu_data_in <= keyboard_data_out;
			    keyboard_cs <= cpu_vma;

           --
           -- VDU port ($E030 - $E03F)
			  --
			  when "0011" =>
             cpu_data_in <= vdu_data_out;
			    vdu_cs      <= cpu_vma;

           --
			  -- Reserved SWTPc MP-T ($E040 - $E04F)
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
			  -- Read LED port ($E0A0 - $E0AF)
			  -- Write LEDS
			  --
			  when "1010" =>
             cpu_data_in <= leds_data_out;
			    leds_cs     <= cpu_vma;

           --
           -- LCD display port ($E0B0 - $E0BF)
			  --
			  when "1011" =>
             cpu_data_in <= lcd_data_out;
			    lcd_cs      <= cpu_vma;

           --
			  -- Read Joy Stick port ($E0D0 - $E0DF)
			  -- Write LEDS
			  --
			  when "1101" =>
             cpu_data_in <= joy_data_out;
			    joy_cs     <= cpu_vma;

           --
			  -- Read LED port ($E0E0 - $E0EF)
			  -- Write LEDS
			  --
			  when "1110" =>
             cpu_data_in <= leds_data_out;
			    leds_cs     <= cpu_vma;

           --
           -- LCD display port ($E0F0 - $E0BF)
			  --
			  when "1111" =>
             cpu_data_in <= lcd_data_out;
			    lcd_cs      <= cpu_vma;

			  when others => -- $EXC0 to $EXFF
			     null;

		     end case;
			--
			-- XST-3.0 Peripheral Bus goes here
			--	$E100 to $E1FF
			--	Four devices
			-- IDE, Ethernet, Slot1, Slot2
			--
			when "0001" =>
           cpu_data_in <= ram_data_out;

		     case cpu_addr(7 downto 4) is
			  --
			  -- CF Interface $E100 to $E1FF
			  --
			  when "0000" =>
			    cf_cs0x     <= cpu_vma;

			  when "0001" =>
			    cf_cs1x     <= cpu_vma;
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
			  cpu_data_in <= (others => '0');
         end case;
	   --
		-- FLEX RAM $0C000 - $0DFFF
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
-- 1M byte SRAM Control
-- Processes to read and write memory based on bus signals
-- using bhe/ble controlled read and write.
-- Can't gate the write pulse with the clock
-- because the bus is shared with the CF 
-- which uses clock stretching.
--
ram_process: process( cpu_reset, sys_clk, cpu_addr, cpu_rw, cpu_data_out, dat_addr,
                      ram_cs, ram_io, ram_we, ram_oe )
begin
    --
    -- ram_hold signal helps 
    --
    if( cpu_reset = '1' ) then
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

    ram_cen  <= not ram_cs;
    ram_bhen <=     cpu_addr(0);
    ram_blen <= not cpu_addr(0);
	 ram_wen  <= not ram_we;
	 ram_oen  <= not ram_oe;

	 ram_a(20) <= '0';
	 ram_a(19 downto 0) <= dat_addr(7 downto 0) & cpu_addr(11 downto 0);

    if (cpu_rw = '0') and (cpu_addr(0) = '0') then
		ram_io(15 downto 8) <= cpu_data_out;
	 else
      ram_io(15 downto 8)  <= "ZZZZZZZZ";
	 end if;

    if (cpu_rw = '0') and (cpu_addr(0) = '1') then
		ram_io(7 downto 0) <= cpu_data_out;
	 else
      ram_io(7 downto 0)  <= "ZZZZZZZZ";
	 end if;

	 if cpu_addr(0) = '0' then
      ram_data_out <= ram_io(15 downto 8);
    else
      ram_data_out <= ram_io(7 downto 0);
    end if;
end process;

--
-- Compact Flash Control
-- Configure compact flash for TRUE IDE mode
--
  compact_flash: process( reset_sw,
                          cpu_addr, cpu_rw, cpu_vma, cpu_data_out,
                          cf_cs0x, cf_cs1x, cf_rd, cf_wr, cf_cd1, cf_cd2 )
  begin
    cf_reset  <= reset_sw;
    cf_pwr_en <= (cf_cd1) or (cf_cd2);	  -- power enable when card detect
    cf_oe     <= '0';					  -- TRUE IDE mode
    cf_we     <= '1';
    cf_reg    <= '1';					  
    cf_cs0    <= not cf_cs0x;
    cf_cs1    <= not cf_cs1x;
    cf_wr     <= (cf_cs0x or cf_cs1x) and (not cpu_rw);
    cf_rd     <= (cf_cs0x or cf_cs1x) and      cpu_rw;
    cf_iowr   <= not cf_wr;
    cf_iord   <= not cf_rd;
  end process;


--
-- Hold CF access	for a few cycles
--
cf_hold_proc: process( cpu_clk, cpu_reset )
begin
    if cpu_reset = '1' then
		 cf_release    <= '0';
		 cf_count      <= "0000";
	    cf_hold_state <= hold_release_state;
	 elsif falling_edge( cpu_clk ) then
	    case cf_hold_state is
		 when hold_release_state =>
          cf_release <= '0';
		    if (cf_cs0x = '1') or (cf_cs1x = '1') then
			    cf_count      <= "0011";
				 cf_hold_state <= hold_request_state;
			 end if;

		 when hold_request_state =>
		    cf_count <= cf_count - "0001";
			 if cf_count = "0000" then
             cf_release    <= '1';
				 cf_hold_state <= hold_release_state;
			 end if;
       when others =>
		    null;
       end case;
	 end if;

end process;


--
-- Interrupts and other bus control signals
--
  interrupts : process( acia_irq, keyboard_irq, joy_up, cf_cs0x, cf_cs1x, cf_hold, cf_release )
  begin
    cf_hold   <= (cf_cs0x or cf_cs1x) and (not cf_release);
    cpu_irq   <= keyboard_irq;
    cpu_nmi   <= not joy_up;
    cpu_firq  <= acia_irq;
    cpu_halt  <= '0';
    cpu_hold  <= cf_hold;
  end process;

  --
  -- LCD write register
  -- LCD_data_in and LCD_data_out 
  -- are relative to the CPU
  -- Not the LCD display
  --
  lcd_control : process(cpu_reset, cpu_clk, lcd_data_in, lcd_d )
  begin
    if cpu_reset = '1' then
	   lcd_data_in <= (others => '0');
    elsif falling_edge(cpu_clk) then
      if lcd_cs = '1' and cpu_rw = '0' then
         lcd_data_in <= cpu_data_out;
      end if;
    end if;
	 if lcd_data_in(4) = '1' and lcd_data_in(5) = '0' then
       lcd_d  <= lcd_data_in(3 downto 0);
    else
	    lcd_d  <= (others => 'Z');
    end if;
    lcd_e     <= lcd_data_in(4);
    lcd_rw    <= lcd_data_in(5);
    lcd_rs    <= lcd_data_in(6);
	 -- read back control signals
	 lcd_data_out(7 downto 4) <= lcd_data_in(7 downto 4); 
	 -- read back 4 bit data bus
	 lcd_data_out(3 downto 0) <= lcd_d(3 downto 0); 
  end process;

  --
  -- LED write register
  --
  led_control : process(cpu_reset, cpu_clk, leds_data_in)
  begin
    if cpu_reset = '1' then
      leds_data_in <= (others => '1');
    elsif falling_edge(cpu_clk) then
      if leds_cs = '1' and cpu_rw = '0' then
        leds_data_in <= cpu_data_out;
      end if;
    end if;
    led <= leds_data_in(3 downto 0);
	 -- read back output state
	 leds_data_out <= leds_data_in;
  end process;


  --
  -- Joystick register
  --
  read_joystick : process(cpu_clk, joy_up, joy_right, joy_down, joy_left, joy_fire)
  begin
    if rising_edge(cpu_clk) then
      joy_data_out(0) <= joy_up;
      joy_data_out(1) <= joy_right;
      joy_data_out(2) <= joy_down;
      joy_data_out(3) <= joy_left;
      joy_data_out(4) <= joy_fire;
      joy_data_out(7 downto 5) <= (others => '0');
    end if;
  end process;
  
--
-- LED Flasher
--
  my_led_flasher: process(vga_clk, cpu_reset, blink_count)
  begin
    if cpu_reset = '1' then
      blink_count <= (others => '0');
    elsif rising_edge(vga_clk) then
      blink_count <= blink_count + 1;
    end if;

    mm_led <= blink_count(25);

  end process;


-- Set acia DCD to always true
  DCD_n <= '0';

--
-- configure utmi for 30MHz clock
--
  utmi_databus16_8 <= '1';
  utmi_reset		 <= '0';
  utmi_xcvrselect	 <= '1';
  utmi_termselect	 <= '1';
  utmi_opmode1		 <= '0';
  utmi_txvalid		 <= '0'; 

--
-- Feed RGB DAC
--
  fpga_r(0) <= red;
  fpga_r(1) <= red;
  fpga_r(2) <= red;
  fpga_g(0) <= green;
  fpga_g(1) <= green;
  fpga_g(2) <= green;
  fpga_b(0) <= blue;
  fpga_b(1) <= blue;
  fpga_b(2) <= blue;

  -- Hold system in reset until the clock is locked or when the reset
  -- key is pressed.
  cpu_reset <= (not reset_sw ) or (not clock_locked);

  --
  -- Terminate Audio Output signals
  --
  aud_out <= (others => '0');

  --
  -- Terminate Flash memory controls
  --
  fl_resetn <= '1';
  fl_cen    <= '1';
  fl_oen    <= '1';
  fl_byten  <= '1';

-- debug output
--	input detected from an IR remote control is forwarded to the LED on the micromodule.
-- there it is easily accessible by a scope and you can also see some flicker when pressing a button.
--  mm_led <= ir_data;


end;

