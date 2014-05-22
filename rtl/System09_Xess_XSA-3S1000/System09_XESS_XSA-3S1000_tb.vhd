--===========================================================================----
--
--  S Y N T H E Z I A B L E    System09 - SOC.
--
--  www.OpenCores.Org - February 2007
--  This core adheres to the GNU public license  
--
-- File name      : System09_Xess_XSA-3S1000_tb.vhd
--
-- Purpose        : Test bench file for 6809 compatible system on a chip
--                  Designed with Xilinx XC3S1000 Spartan 3 FPGA.
--                  Implemented With XESS XSA-3S1000 FPGA board.
--
-- Dependencies   : ieee.Std_Logic_1164
--                  ieee.std_logic_unsigned
--                  ieee.std_logic_arith
--                  ieee.numeric_std
--                  unisim.vcomponents
--
-- Author         : John E. Kent      
--                  dilbert57@opencores.org      
--
--
--===========================================================================----
--
-- Revision History:
--===========================================================================--
--
-- Version 0.1 - 30 July 2008
--
library ieee;
   use ieee.std_logic_1164.all;
   use IEEE.STD_LOGIC_ARITH.ALL;
   use IEEE.STD_LOGIC_UNSIGNED.ALL;
   use ieee.numeric_std.all;
library unisim;
   use unisim.vcomponents.all;

entity my_system09_tb is
end my_System09_tb;

architecture behavior of my_system09_tb is

constant  CAS_LATENCY  : integer := 3;

signal    CLKA         : Std_Logic; -- in  -- 100MHz Clock input
signal	 SW2_N        : Std_logic; -- in  -- Master Reset input (active low)
signal	 SW3_N        : Std_logic; -- in  -- Non Maskable Interrupt input (active low)

 	 -- PS/2 Keyboard
signal	 ps2_clk      : Std_logic;	-- inout
signal	 ps2_dat      : Std_Logic;	-- inout

	 -- CRTC output signals
signal	 vga_vsync_n  : Std_Logic; -- out
signal    vga_hsync_n  : Std_Logic; -- out
signal    vga_blue     : std_logic_vector(2 downto 0); -- out
signal    vga_green    : std_logic_vector(2 downto 0); -- out
signal    vga_red      : std_logic_vector(2 downto 0); -- out

    -- RS232 Port
signal	 RS232_RXD    : Std_Logic; -- in
signal	 RS232_TXD    : Std_Logic; -- out
signal    RS232_CTS    : Std_Logic; -- in
signal    RS232_RTS    : Std_Logic; -- out

    -- SDRAM side
signal    SDRAM_clk    : std_logic; -- out -- in     -- feedback SDRAM clock after PCB delays
signal    SDRAM_CKE    : std_logic; -- out           -- clock-enable to SDRAM
signal    SDRAM_CS_N   : std_logic; -- out           -- chip-select to SDRAM
signal    SDRAM_RAS_N  : std_logic; -- out           -- SDRAM row address strobe
signal    SDRAM_CAS_N  : std_logic; -- out           -- SDRAM column address strobe
signal    SDRAM_WE_N   : std_logic; -- out           -- SDRAM write enable
signal    SDRAM_BA     : std_logic_vector(1 downto 0);  --out    -- SDRAM bank address
signal    SDRAM_A      : std_logic_vector(12 downto 0); --out    -- SDRAM row/column address
signal    SDRAM_D      : std_logic_vector(15 downto 0); -- inout -- data from SDRAM
signal    SDRAM_DQMH   : std_logic; -- out           -- enable upper-byte of SDRAM databus if true
signal    SDRAM_DQML   : std_logic; -- out           -- enable lower-byte of SDRAM databus if true
signal    SDATA_I      : std_logic_vector(15 downto 0); -- in    -- data to SDRAM
signal    SDATA_O      : std_logic_vector(15 downto 0); -- out   -- data from SDRAM

	 -- Peripheral I/O bus $E100 - $E1FF
signal    PB_RD_N      : std_logic; -- out
signal    PB_WR_N      : std_logic; -- out
signal    PB_A         : std_logic_vector(4 downto 0); -- out
signal    PB_D         : std_logic_vector(15 downto 0); -- inout

    -- IDE Compact Flash $E100 - $E13F
signal    ide_dmack_n  : std_logic; -- out
signal    ide_cs0_n    : std_logic; -- out
signal    ide_cs1_n    : std_logic; -- out

    -- Ethernet $E140 - $E17F
signal    ether_cs_n   : std_logic; -- out
signal    ether_aen    : std_logic; -- out -- Ethernet address enable not 
signal    ether_bhe_n  : std_logic; -- out -- Ethernet bus high enable 
-- signal    ether_clk    : std_logic; -- in -- Ethernet clock 
-- signal    ether_rdy    : std_logic; -- in -- Ethernet ready
-- signal    ether_irq    : std_logic; -- in -- Ethernet irq - Shared with BAR6

    -- Slot 1 $E180 - $E1BF
signal	 slot1_cs_n   : std_logic; -- out
--signal	 slot1_irq    : std_logic;	-- in

    -- Slot 2 $E1C0 - $E1FF
signal    slot2_cs_n   : std_logic;	-- out
--signal	 slot2_irq    : std_logic; -- in

  -- CPU Debug Interface signals
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
    
	 -- Disable Flash
signal    FLASH_CE_N   : std_logic;	-- out

type      pipe_type is array(0 to CAS_LATENCY-1) of std_logic_vector(15 downto 0);
signal    data_pipe : pipe_type;

component my_system09 is
  port(
    CLKA         : in  Std_Logic;  -- 100MHz Clock input
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
--    ether_clk    : in  std_logic; -- Ethernet clock 
--    ether_rdy    : in  std_logic; -- Ethernet ready
--    ether_irq    : in  std_logic; -- Ethernet irq - Shared with BAR6

    -- Slot 1 $E180 - $E1BF
	 slot1_cs_n   : out std_logic;
--	 slot1_irq    : in  std_logic;

    -- Slot 2 $E1C0 - $E1FF
	 slot2_cs_n   : out std_logic;
--	 slot2_irq    : in  std_logic;
-- CPU Debug Interface signals
    cpu_reset_o     : out Std_Logic;
    cpu_clk_o       : out Std_Logic;
    cpu_rw_o        : out std_logic;
    cpu_vma_o       : out std_logic;
    cpu_halt_o      : out std_logic;
    cpu_hold_o      : out std_logic;
    cpu_firq_o      : out std_logic;
    cpu_irq_o       : out std_logic;
    cpu_nmi_o       : out std_logic;
    cpu_addr_o      : out std_logic_vector(15 downto 0);
    cpu_data_in_o   : out std_logic_vector(7 downto 0);
    cpu_data_out_o  : out std_logic_vector(7 downto 0);
    
	 -- Disable Flash
	 FLASH_CE_N   : out std_logic
	 );
end component;

begin

my_system09_inst : my_system09 port map (
    CLKA         => CLKA,    -- 100MHz Clock input
	 SW2_N        => SW2_N, -- Master Reset input (active low)
	 SW3_N        => SW3_N,  -- Non Maskable Interrupt input (active low)

 	 -- PS/2 Keyboard
	 ps2_clk      => ps2_clk,
	 ps2_dat      => ps2_dat,

	 -- CRTC output signals
	 vga_vsync_n  => vga_vsync_n,
    vga_hsync_n  => vga_hsync_n,
    vga_blue     => vga_blue,
    vga_green    => vga_green,
    vga_red      => vga_red,

    -- RS232 Port
	 RS232_RXD    => RS232_RXD,
	 RS232_TXD    => RS232_TXD,
    RS232_CTS    => RS232_CTS,
    RS232_RTS    => RS232_RTS,

    -- SDRAM side
    SDRAM_clkfb  => SDRAM_clk,  -- feedback SDRAM clock after PCB delays
    SDRAM_clkout => SDRAM_clk, -- clock to SDRAM
    SDRAM_CKE    => SDRAM_CKE,    -- clock-enable to SDRAM
    SDRAM_CS_N   => SDRAM_CS_N,   -- chip-select to SDRAM
    SDRAM_RAS_N  => SDRAM_RAS_N,  -- SDRAM row address strobe
    SDRAM_CAS_N  => SDRAM_CAS_N,  -- SDRAM column address strobe
    SDRAM_WE_N   => SDRAM_WE_N,   -- SDRAM write enable
    SDRAM_BA     => SDRAM_BA,     -- SDRAM bank address
    SDRAM_A      => SDRAM_A,      -- SDRAM row/column address
    SDRAM_D      => SDRAM_D,      -- data from SDRAM
    SDRAM_DQMH   => SDRAM_DQMH,   -- enable upper-byte of SDRAM databus if true
    SDRAM_DQML   => SDRAM_DQML,   -- enable lower-byte of SDRAM databus if true

	 -- Peripheral I/O bus $E100 - $E1FF
    PB_RD_N      => PB_RD_N,
    PB_WR_N      => PB_WR_N,
    PB_A         => PB_A,
    PB_D         => PB_D,

    -- IDE Compact Flash $E100 - $E13F
    ide_dmack_n  => ide_dmack_n,
	 ide_cs0_n    => ide_cs0_n,
	 ide_cs1_n    => ide_cs1_n,

    -- Ethernet $E140 - $E17F
    ether_cs_n   => ether_cs_n,
    ether_aen    => ether_aen, -- Ethernet address enable not 
    ether_bhe_n  => ether_bhe_n, -- Ethernet bus high enable 
--    ether_clk    => ether_clk,   -- Ethernet clock 
--    ether_rdy    => ether_rdy,   -- Ethernet ready
--    ether_irq    => ether_irq,   -- Ethernet irq - Shared with BAR6

    -- Slot 1 $E180 - $E1BF
	 slot1_cs_n   => slot1_cs_n,
--	 slot1_irq   => slot1_irq,

    -- Slot 2 $E1C0 - $E1FF
	 slot2_cs_n   => slot2_cs_n,
--	 slot2_irq   => slot2_irq,

  cpu_reset_o    => cpu_reset,
  cpu_clk_o      => cpu_clk,
  cpu_rw_o       => cpu_rw,
  cpu_vma_o      => cpu_vma,
  cpu_halt_o     => cpu_halt,
  cpu_hold_o     => cpu_hold,
  cpu_firq_o     => cpu_firq,
  cpu_irq_o      => cpu_irq,
  cpu_nmi_o      => cpu_nmi,
  cpu_addr_o     => cpu_addr,
  cpu_data_out_o => cpu_data_out,
  cpu_data_in_o  => cpu_data_in,
    
	 -- Disable Flash
	 FLASH_CE_N   => FLASH_CE_N
	 );



   sdram : process( SDRAM_CS_N, SDRAM_CAS_N, SDRAM_WE_N, SDRAM_DQMH, SDRAM_DQML )
	begin
	  if( SDRAM_CS_N = '0') and (SDRAM_CAS_N = '0') and (SDRAM_WE_N = '1') then
	    if SDRAM_DQMH = '0' then
		   SDATA_I(15 downto 8) <= "11001100";
       else
		   SDATA_I(15 downto 8) <= "ZZZZZZZZ";
       end if;
	    if SDRAM_DQML = '0' then
		   SDATA_I( 7 downto 0) <= "00001111";
       else
		   SDATA_I( 7 downto 0) <= "ZZZZZZZZ";
       end if;
     else
		 SDATA_I(15 downto 8) <= "ZZZZZZZZ";
		 SDATA_I( 7 downto 0) <= "ZZZZZZZZ";
	  end if;
   end process;

	--
	-- data out of SDRAM occurs 2 clock cycles after READ Command
	--	CAS Latency = 3 on XESS controller
	-- i.e. data appear 3 clocks after CAS strobe.
	--
   pipe : process( SDRAM_clk, data_pipe, SDATA_I, SDRAM_D, data_pipe )
	variable i : integer;
	begin
	  if rising_edge( SDRAM_clk ) then
		 for i in CAS_LATENCY-1 downto 1 loop
	      data_pipe(i) <= data_pipe(i-1);
       end loop;
	    data_pipe(0) <= SDATA_I;
	  end if;
	  SDRAM_D <= data_pipe(CAS_LATENCY-1);
	  SDATA_O <= SDRAM_D;
   end process;

   tb : PROCESS
	variable count : integer;
   BEGIN

	SW2_N <= '1';  -- reset_n
	SW3_N <= '1';	-- nmi_n
	CLKA  <= '0';
	RS232_RXD <= '1';
	RS232_CTS <= '1';
--   ether_clk <= '0'; -- Ethernet clock 
--   ether_rdy <= '0'; -- Ethernet ready
--   ether_irq <= '0'; -- Ethernet irq - Shared with BAR6

		for count in 0 to 50000 loop
			CLKA <= '0';
			wait for 5 ns;
			if count = 0 then
				SW2_N <= '0';
			elsif count = 8 then
				SW2_N <= '1';
			end if;
			CLKA <= '1';
			wait for 5 ns;
		end loop;

      wait; -- will wait forever
   END PROCESS;
-- *** End Test Bench - User Defined Section ***


end architecture;