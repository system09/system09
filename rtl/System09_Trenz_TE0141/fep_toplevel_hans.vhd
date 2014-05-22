-- SECD Front End Processor derived from System09 written by John E. Kent
-- This core adheres to the GNU public license  

library ieee;
use ieee.std_logic_1164.all;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use ieee.numeric_std.all;
--use config.all;

entity secd_fep_trenz is
  port(
    utmi_clkout      : in  Std_Logic;  -- UTMI Clock input
    utmi_databus16_8 : out Std_Logic;  -- UTMI configuration input

    reset_sw    : in  Std_logic;  -- Master Reset input (active low)

    -- PS/2 Keyboard
    ps2_clk1    : inout Std_logic;
    ps2_data1   : inout Std_Logic;

    -- Uart Interface
    fpga_rxd    : in  Std_Logic;
    fpga_txd    : out Std_Logic;
    fpga_cts    : in  Std_Logic;
    fpga_rts    : out Std_Logic;

    -- CRTC output signals
    vsync_b     : out Std_Logic;
    hsync_b     : out Std_Logic;
    fpga_b      : out Std_Logic_Vector(2 downto 0);
    fpga_g      : out Std_Logic_Vector(2 downto 0);
    fpga_r      : out Std_Logic_Vector(2 downto 0);

    -- LEDS & Switches
    mm_led      : out Std_Logic;
    led         : out Std_Logic_Vector(3 downto 0);

    joy_down    : in Std_Logic;
    joy_fire    : in Std_Logic;
    joy_left    : in Std_Logic;
    joy_right   : in Std_Logic;
    joy_up      : in Std_Logic;

    -- LCD Display
    lcd_e       : out Std_Logic;
    lcd_rw      : out Std_Logic;
    lcd_rs      : out Std_Logic;
    lcd_d       : out Std_Logic_Vector(3 downto 0);

    -- Audio
    aud_out     : out std_logic_vector(4 downto 1);

    -- Memory interface
    ram_a       : out std_logic_vector(20 downto 1);
    ram_io      : inout std_logic_vector(15 downto 0);
    ram_bhen    : out std_logic;
    ram_blen    : out std_logic;
    ram_cen     : out std_logic;
    ram_oen     : out std_logic;
    ram_wen     : out std_logic;

    -- Compact flash
    cf_reset    : out std_logic;
--    cf_irq      : in std_logic;
    cf_iord     : out std_logic;
    cf_iowr     : out std_logic;
--    cf_wait     : in std_logic;
--    cf_dasp     : in std_logic;
--    cf_pdiag    : in std_logic;
--    cf_cd1      : in std_logic;
--    cf_cd2      : in std_logic;
--    iois16      : in std_logic;
--    cf_oe       : out std_logic;
    cf_pwr_en   : out std_logic;
    cf_cs0      : out std_logic;
    cf_cs1      : out std_logic
--    cf_we       : out std_logic;
--    cf_rew      : out std_logic
    );
end secd_fep_trenz;

-------------------------------------------------------------------------------
-- Architecture for System09
-------------------------------------------------------------------------------
architecture rtl of secd_fep_trenz is
  -----------------------------------------------------------------------------
  -- constants
  -----------------------------------------------------------------------------
  constant fep_only             : integer := 1;

  constant SYS_Clock_Frequency  : integer := 50000000;  -- FPGA System Clock
  constant VGA_Clock_Frequency  : integer := 25000000;  -- VGA Pixel Clock
  constant CPU_Clock_Frequency  : integer := 12500000;  -- CPU Clock
  constant BAUD_Rate            : integer := 57600;	  -- Baud Rate
  constant ACIA_Clock_Frequency : integer := BAUD_Rate * 16;

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
  attribute buffer_type                 : string;
  attribute period                      : string;

  signal vdu_clk                        : std_logic;      -- 25 Mhz
  attribute period of vdu_clk           : signal is "40 ns";
  attribute buffer_type of vdu_clk      : signal is "BUFG";

  signal cpu_clk      			: std_logic;      -- 12.5 Mhz
  attribute buffer_type of cpu_clk      : signal is "BUFG";

  -- BOOT ROM
  signal rom_cs        : Std_logic;
  signal rom_data_out  : Std_Logic_Vector(7 downto 0);

  -- RAM
  signal user_ram0_cs   : std_logic;
  signal user_ram0_dout : std_logic_vector(7 downto 0);
  signal user_ram1_cs   : std_logic;
  signal user_ram1_dout : std_logic_vector(7 downto 0);

  -- UART Interface signals
  signal uart_data_out : Std_Logic_Vector(7 downto 0);  
  signal uart_cs       : Std_Logic;
  signal uart_irq      : Std_Logic;
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
  signal cf_data_out : std_logic_vector(7 downto 0);
  signal cf_cs       : std_logic;
  signal cf_rd       : std_logic;
  signal cf_wr       : std_logic;

  -- Video Display Unit
  signal vdu_cs       : std_logic;
  signal vdu_data_out : std_logic_vector(7 downto 0);

  -- VGA output signals (distributed to VGA DAC)
  signal red          : std_logic;
  signal green        : std_logic;
  signal blue         : std_logic;

  -- System Reset (generated by key press)
  signal reset        : std_logic;

  -- LCD register select
  signal lcd_cs       : std_logic;

  -- LED register select
  signal led_cs       : std_logic;
  signal led_reg      : std_logic_vector(7 downto 0) := (others => '0');

  -- Joystick buffer
  signal joystick     : std_logic_vector(7 downto 0);

  -- LED Flasher
  signal blink_count  : std_logic_vector(25 downto 0) := (others => '0');
  
  -- SECD interface
  signal secd_button           : std_logic := '0';
  signal secd_stop             : std_logic := '1';
  signal secd_stopped          : std_logic := '1';
  signal secd_state            : std_logic_vector(1 downto 0);
  signal secd_ram_addr_hi      : std_logic_vector(7 downto 0) := (others => '0');
  signal secd_ram_addr_high_cs : std_logic := '0';
  signal secd_ram_cs           : std_logic := '0';
  signal secd_control_cs       : std_logic := '0';

  -- SECD RAM Controller interface
  signal secd_ram_busy          : std_logic;

  -- RAM signal taps
  signal ram_bhenx : std_logic;
  signal ram_blenx : std_logic;
  signal ram_cenx : std_logic;
  signal ram_oenx : std_logic;
  signal ram_wenx : std_logic;

  -- Interface signals for SECD
  signal secd_ram_din32         : std_logic_vector(31 downto 0);
  signal secd_ram_dout32        : std_logic_vector(31 downto 0);
  signal secd_ram_addr32        : std_logic_vector(13 downto 0);
  signal secd_ram_read32        : std_logic;
  signal secd_ram_write32       : std_logic;

  -- Interface signals for 6809
  signal secd_ram_dout8         : std_logic_vector(7 downto 0);
  signal secd_ram_hold          : std_logic;

  -- Locked signal of clock synthesizer
  signal clock_locked           : std_logic;
  signal ila_clock              :std_logic;
-----------------------------------------------------------------
--
-- CPU09 CPU core
--
-----------------------------------------------------------------

  component cpu09
    port (    
      clk      :	in  std_logic;
      rst      : in  std_logic;
      rw       :	out std_logic;		-- Asynchronous memory interface
      vma      :	out std_logic;
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
-- 16KByte Block RAM Mais Forth ROM
--
----------------------------------------
  component maisforth_rom_16k
    Port (
      clk      : in  std_logic;
      rst      : in  std_logic;
      cs       : in  std_logic;
      rw       : in  std_logic;
      addr     : in  std_logic_vector (13 downto 0);
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
  component ram_2k
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
-- Open Cores Mini UART
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
      SYS_Clock_Frequency  : integer :=  VGA_Clock_Frequency;
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
--             trig0(7) => vdu_clk);
  
  -----------------------------------------------------------------
  --
  -- CPU09 CPU core
  --
  -----------------------------------------------------------------

  my_cpu : entity cpu09 port map (    
    clk	      => cpu_clk,
    rst       => reset,
    rw	      => cpu_rw,
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
  -- Maisforth ROM (Xilinx Block RAM, 16k)
  --
  ----------------------------------------

  my_maisforth_rom_16k : entity maisforth_rom_16k port map (
    clk   => cpu_clk,
    rst   => reset,
    cs    => rom_cs,
    rw    => '1',
    addr  => cpu_addr(13 downto 0),
    rdata => rom_data_out,
    wdata => cpu_data_out
    );

  -----------------------------------------------------------------------------
  --
  -- Internal RAM (Xilinx Block RAM, 4k)
  --
  -----------------------------------------------------------------------------

  my_user_ram0_2k : entity ram_2k port map (
    clk   => cpu_clk,
    rst   => reset,
    cs    => user_ram0_cs,
    rw    => cpu_rw,
    addr  => cpu_addr(10 downto 0),
    rdata => user_ram0_dout,
    wdata => cpu_data_out
    );

  my_user_ram1_2k : entity ram_2k port map (
    clk   => cpu_clk,
    rst   => reset,
    cs    => user_ram1_cs,
    rw    => cpu_rw,
    addr  => cpu_addr(10 downto 0),
    rdata => user_ram1_dout,
    wdata => cpu_data_out
    );

  -----------------------------------------------------------------
  --
  -- 6850 ACIA
  --
  -----------------------------------------------------------------

  my_uart  : entity acia_6850 port map (
    clk	     => cpu_clk,
    rst       => reset,
    cs        => uart_cs,
    rw        => cpu_rw,
    irq       => uart_irq,
    Addr      => cpu_addr(0),
    Datain    => cpu_data_out,
    DataOut   => uart_data_out,
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
      rst          => reset,
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
      vdu_rst       => reset,
      vdu_cs        => vdu_cs,
      vdu_rw        => cpu_rw,
      vdu_addr      => cpu_addr(2 downto 0),
      vdu_data_in   => cpu_data_out,
      vdu_data_out  => vdu_data_out,

      -- vga port connections
      vga_clk       => vdu_clk,					 -- 25 MHz VDU pixel clock
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
    vdu_clk         => vdu_clk,
    cpu_clk         => cpu_clk,
    locked          => clock_locked );
--    clk_60mhz       => ila_clock);

--make_secd: if fep_only /= '1' generate
--  ----------------------------------------
--  --
--  -- SECD CPU instantiation
--  --
--  ----------------------------------------
--
--  my_secd_system : entity secd_system port map (
--    clk         => cpu_clk,
--    reset       => reset,
--    button      => secd_button,
--    ram_read    => secd_ram_read32,
--    ram_in      => secd_ram_dout32,
--    ram_write   => secd_ram_write32,
--    ram_out     => secd_ram_din32,
--    ram_a       => secd_ram_addr32,
--    ram_busy    => secd_ram_busy,
--    stop_input  => secd_stop,
--    stopped     => secd_stopped,
--    state       => secd_state
--    );
--end generate;
  
  ----------------------------------------
  --
  -- SECD RAM Controller instantiation
  --
  ----------------------------------------

  my_secd_ram : entity secd_ram_controller port map (
    clk                 => vdu_clk,
    reset               => reset,
    secd_stopped        => secd_stopped,

    -- SECD interface
    din32               => secd_ram_din32,
    dout32              => secd_ram_dout32,
    addr32              => secd_ram_addr32,
    read32_enable       => secd_ram_read32,
    write32_enable      => secd_ram_write32,
    busy32              => secd_ram_busy,

    -- 6809 interface
    clk8                => cpu_clk,
    din8                => cpu_data_out,
    dout8               => secd_ram_dout8,
    addr8(15 downto 8)  => secd_ram_addr_hi,
    addr8(7 downto 0)   => cpu_addr(7 downto 0),
    cs8_ram             => secd_ram_cs,
    rw8                 => cpu_rw,
    hold8               => secd_ram_hold,

    -- Compact Flash interface
    cs8_cf              => cf_cs,

    -- external interface
    ram_oen 		=> ram_oenx,
    ram_cen 		=> ram_cenx,
    ram_wen 		=> ram_wenx,
    ram_io        => ram_io,
    ram_a         => ram_a,
    ram_bhen 		=> ram_bhenx,
    ram_blen		=> ram_blenx
    );


----------------------------------------
--
-- ACIA Clock
--
----------------------------------------
  my_ACIA_Clock : ACIA_Clock
    generic map(
      SYS_Clock_Frequency  => VGA_Clock_Frequency,
      ACIA_Clock_Frequency => ACIA_Clock_Frequency
      ) 
    port map(
      clk        => vdu_clk,
      acia_clk   => baudclk
      ); 

  ----------------------------------------------------------------------
  --
  -- Process to decode memory map
  --
  ----------------------------------------------------------------------

  mem_decode : process( cpu_addr, cpu_rw, cpu_vma,
                        rom_data_out,
                        user_ram0_dout,
                        user_ram1_dout,
                        uart_data_out,
                        keyboard_data_out,
                        joystick,
                        vdu_data_out,
                        cf_data_out,
                        cpu_data_out,
                        secd_state, secd_stopped, secd_ram_dout8, secd_ram_addr_hi )

  begin
    user_ram0_cs     <= '0';
    user_ram1_cs     <= '0';
    rom_cs           <= '0';
    uart_cs          <= '0';
    keyboard_cs      <= '0';
    vdu_cs           <= '0';
    cf_cs            <= '0';
    lcd_cs           <= '0';
    led_cs           <= '0';
    cpu_data_in      <= X"00";

    secd_control_cs       <= '0';
    secd_ram_cs           <= '0';
    secd_ram_addr_high_cs <= '0';

    case cpu_addr(15 downto 14) is

      -- Maisforth ROM - $C000 - $FFFF
      when "11" =>
        cpu_data_in     <= rom_data_out;
        rom_cs          <= cpu_vma;              -- read  ROM

        -- RAM - $0000-$3FFF
      when "00" =>
        case cpu_addr(13 downto 11) is
          when "000" =>
            cpu_data_in <= user_ram0_dout;
            user_ram0_cs <= cpu_vma;

          when "001" =>
            cpu_data_in <= user_ram1_dout;
            user_ram1_cs <= cpu_vma;

          when others =>
            cpu_data_in <= (others => '0');

        end case;

        -- Unmapped - $4000-$7FFF, read as FF
      when "01" =>
        cpu_data_in <= X"FF";

        -- I/O - $8000-$BFFF - Do additional decoding
      when "10" =>
        case cpu_addr(13 downto 8) is

          -- Real I/O $B000 - $B0FF
          when "110000" =>
            case cpu_addr(7 downto 4) is

              -- UART / ACIA $B000
              when X"0" =>
                cpu_data_in <= uart_data_out;
                uart_cs     <= cpu_vma;

                -- Keyboard port $B010 - $B01F
                -- Note in latest System09
                -- I have moved the Keyboard
                -- to $E020 to make way for the
                -- Floppy Disk Controller at $E01X
                -- JK. 10th Aug 07
              when X"1" =>
                cpu_data_in <= keyboard_data_out;
                keyboard_cs <= cpu_vma;

                -- VDU port $B020 - $B02F
                -- Note in latest System09
                -- I have moved the VDU to
                -- $E030 - JK. 10th Aug 07
              when X"2" =>
                cpu_data_in <= vdu_data_out;
                vdu_cs      <= cpu_vma;

                -- CF port $B040 - $B05F
                -- Note in latest System09
                -- I have moved the CF to
                -- $E040 - JK. 10th Aug 07
					 -- However the Trenz TE0141
					 -- must map the CF on 16 bit 
					 -- word boundaries, so it has 
					 -- to take 2 I/O slots
              when X"4" | X"5" =>
                cpu_data_in <= secd_ram_dout8;
                cf_cs       <= cpu_vma;

                -- Joystick $B0D0 (read only)
              when X"D" =>
                if cpu_addr(3 downto 0) = "0000" then
                  cpu_data_in <= joystick;
                end if;

                -- LED $B0E0 (write only)
              when X"E" =>
                if cpu_addr(3 downto 0) = "0000" then
                  led_cs <= cpu_vma;
                  cpu_data_in <= led_reg;
                end if;

                -- LCD Display $B0F0 (write only)
              when X"F" =>
                if cpu_addr(3 downto 0) = "0000" then
                  lcd_cs <= cpu_vma;
                end if;

              when others =>
                null;
            end case;

            -- SECD Control registers - $B100
          when "110001" =>

            case cpu_addr(7 downto 0) is

              -- $B140 -> SECD Status
              when X"40" =>
                secd_control_cs         <= cpu_vma;
                cpu_data_in(0)          <= secd_stopped;
                cpu_data_in(2 downto 1) <= secd_state;

                -- $B141 -> SECD Address High
              when X"41" =>
                secd_ram_addr_high_cs   <= cpu_vma;
                cpu_data_in             <= secd_ram_addr_hi;

              when others =>
                null;

            end case;

            -- SECD mapped memory page - $B200
          when "110010" =>
            cpu_data_in                 <= secd_ram_dout8;
            secd_ram_cs                 <= cpu_vma;

          when others =>
            null;

        end case;

      when others =>
        null;

    end case;
  end process;

--
-- Compact Flash Control
--
  compact_flash: process( reset_sw,
                          cpu_addr, cpu_rw, cpu_vma, cpu_data_out,
                          cf_cs, cf_rd, cf_wr )
  begin
    cf_reset  <= reset_sw;
    cf_cs0    <= not( cf_cs ) or cpu_addr(4);
    cf_cs1    <= not( cf_cs and cpu_addr(4));
    cf_wr     <= cf_cs and (not cpu_rw);
    cf_rd     <= cf_cs and cpu_rw;
    cf_iowr   <= not cf_wr;
    cf_iord   <= not cf_rd;
    cf_pwr_en <= '0';
  end process;


--
-- Interrupts and other bus control signals
--
  interrupts : process( reset_sw, uart_irq, keyboard_irq, reset, joy_up, secd_ram_hold, secd_ram_cs )
  begin
    cpu_irq   <= keyboard_irq;
    cpu_nmi   <= not joy_up;
    cpu_firq  <= uart_irq;
    cpu_halt  <= '0';
    cpu_hold  <= secd_ram_hold;
  end process;

  --
  -- LCD write register
  --
  lcd_control : process(lcd_cs, cpu_clk, cpu_data_out)
  begin
    if falling_edge(cpu_clk) then
      if lcd_cs = '1' and cpu_rw = '0' then
        lcd_d     <= cpu_data_out(3 downto 0);
        lcd_e     <= cpu_data_out(4);
        lcd_rw    <= cpu_data_out(5);
        lcd_rs    <= cpu_data_out(6);
      end if;
    end if;
  end process;

  --
  -- LED write register
  --
  led_control : process(led_reg, led_cs, cpu_clk, cpu_data_out)
  begin
    if reset = '1' then
      led_reg <= (others => '1');
    elsif falling_edge(cpu_clk) then
      if led_cs = '1' and cpu_rw = '0' then
        led_reg <= cpu_data_out;
      end if;
    end if;

  end process;

  led <= led_reg(3 downto 0);

  -- SECD control register
  --
  secd_control : process(secd_control_cs, cpu_clk, cpu_data_out)
  begin
    if falling_edge(cpu_clk) then
      if secd_control_cs = '1' and cpu_rw = '0' then
        secd_stop   <= cpu_data_out(0);
        secd_button <= cpu_data_out(1);
      end if;
    end if;
  end process;

  --
  -- SECD RAM Adressing
  --

  secd_ram_addressing_high : process(cpu_clk, cpu_rw, cpu_data_out, secd_ram_addr_high_cs)
  begin
    if falling_edge(cpu_clk) then
      if cpu_rw = '0' and secd_ram_addr_high_cs = '1' then
        secd_ram_addr_hi <= cpu_data_out;
      end if;
    end if;
  end process;

  --
  -- Joystick register
  --
  read_joystick : process(cpu_clk, joy_up, joy_right, joy_down, joy_left, joy_fire)
  begin
    if rising_edge(cpu_clk) then
      joystick(0) <= joy_up;
      joystick(1) <= joy_right;
      joystick(2) <= joy_down;
      joystick(3) <= joy_left;
      joystick(4) <= joy_fire;
      joystick(7 downto 5) <= (others => '0');
    end if;
  end process;
  
--
-- LED Flasher
--
  my_led_flasher: process(vdu_clk, reset, blink_count)
  begin
    if reset = '1' then
      blink_count <= (others => '0');
    elsif rising_edge(vdu_clk) then
      blink_count <= blink_count + 1;
    end if;

    mm_led <= blink_count(25);

  end process;

-- Set UART DCD to always true
  DCD_n <= '0';

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

  -- set USB PHY to 16 bit mode so that it generates a 30 Mhz Clock
  utmi_databus16_8 <= '1';

  -- Hold system in reset until the clock is locked or when the reset
  -- key is pressed.
  reset     <= not reset_sw or not clock_locked;

  aud_out <= (others => '0');

  ram_bhen <= ram_bhenx;
  ram_blen <= ram_blenx;
  ram_cen <= ram_cenx;
  ram_oen <= ram_oenx;
  ram_wen <= ram_wenx;

  secd_ram_din32 <= (others => '0');
  secd_ram_addr32	<= (others => '0');
  secd_ram_read32 <= '0';
  secd_ram_write32 <= '0';

end;

