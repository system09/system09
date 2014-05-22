--===========================================================================----
--
--  S Y N T H E Z I A B L E    System09 - SOC.
--
--  www.OpenCores.Org - February 2007
--  This core adheres to the GNU public license  
--
-- File name      : xsasdramcntl_tb.vhd
--
-- Purpose        : Test bench file for XESS XSA-3S1000 SDRAM controller
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

entity my_xsasdramcntl_tb is
end my_xsasdramcntl_tb;

architecture behavior of my_xsasdramcntl_tb is

  constant  FREQ                 :     natural := 100_000; -- operating frequency in KHz
  constant  CLK_DIV              :     real    := 2.0;    -- divisor for FREQ (can only be 1.0, 1.5, 2.0, 2.5, 3.0, 4.0, 5.0, 8.0 or 16.0)
  constant  PIPE_EN              :     boolean := true;  -- if true, enable pipelined read operations
  constant  MAX_NOP              :     natural := 10000;  -- number of NOPs before entering self-refresh
  constant  MULTIPLE_ACTIVE_ROWS :     boolean := false;  -- if true, allow an active row in each bank
  constant  DATA_WIDTH           :     natural := 16;     -- host & SDRAM data width
  constant  NROWS                :     natural := 8192;   -- number of rows in SDRAM array
  constant  NCOLS                :     natural := 512;    -- number of columns in SDRAM array
  constant  HADDR_WIDTH          :     natural := 24;     -- host-side address width
  constant  SADDR_WIDTH          :     natural := 13;     -- SDRAM-side address width

  constant  CAS_LATENCY          :     integer := 3;

signal    clk          : std_logic;  -- in  -- master clock
signal    bufclk       : std_logic;  -- out -- buffered master clock
signal    clk1x        : std_logic;  -- out -- host clock sync'ed to master clock (and divided if CLK_DIV>1)
signal    clk2x        : std_logic;  -- out -- double-speed host clock
signal    lock         : std_logic;  -- out -- true when host clock is locked to master clock
signal    rst          : std_logic;  -- in  -- reset
signal    rd           : std_logic;  -- in  -- initiate read operation
signal    wr           : std_logic;  -- in  -- initiate write operation
signal    uds          : std_logic;  -- in  -- upper data strobe
signal    lds          : std_logic;  -- in  -- lower data strobe
signal    earlyOpBegun : std_logic;  -- out -- read/write/self-refresh op begun     (async)
signal    opBegun      : std_logic;  -- out -- read/write/self-refresh op begun (clocked)
signal    rdPending    : std_logic;  -- out -- read operation(s) are still in the pipeline
signal    done         : std_logic;  -- out -- read or write operation is done
signal    rdDone       : std_logic;  -- out -- read done and data is available
signal    hAddr        : std_logic_vector(HADDR_WIDTH-1 downto 0); -- in  -- address from host
signal    hDIn         : std_logic_vector(DATA_WIDTH-1 downto 0);  -- in  -- data from host
signal    hDOut        : std_logic_vector(DATA_WIDTH-1 downto 0);  -- out -- data to host
signal    status       : std_logic_vector(3 downto 0);  -- out -- diagnostic status of the FSM         

    -- SDRAM side
signal    Sclk    : std_logic; -- out -- in     -- feedback SDRAM clock after PCB delays
signal    CKE     : std_logic; -- out           -- clock-enable to SDRAM
signal    CS_N    : std_logic; -- out           -- chip-select to SDRAM
signal    RAS_N   : std_logic; -- out           -- SDRAM row address strobe
signal    CAS_N   : std_logic; -- out           -- SDRAM column address strobe
signal    WE_N    : std_logic; -- out           -- SDRAM write enable
signal    BA      : std_logic_vector(1 downto 0);  --out    -- SDRAM bank address
signal    SADDR   : std_logic_vector(12 downto 0); --out    -- SDRAM row/column address
signal    SDATA   : std_logic_vector(15 downto 0); -- inout -- data from SDRAM
signal    SDATA_I : std_logic_vector(15 downto 0); -- in    -- data to SDRAM
signal    SDATA_O : std_logic_vector(15 downto 0); -- out   -- data from SDRAM
signal    DQMH    : std_logic; -- out           -- enable upper-byte of SDRAM databus if true
signal    DQML    : std_logic; -- out           -- enable lower-byte of SDRAM databus if true

type      pipe_type is array(0 to CAS_LATENCY-1) of std_logic_vector(15 downto 0);
signal    data_pipe : pipe_type;

component XSASDRAMCntl is
  generic(
    FREQ                 :     natural := 100_000; -- operating frequency in KHz
    CLK_DIV              :     real    := 2.0;     -- divisor for FREQ (can only be 1.0, 1.5, 2.0, 2.5, 3.0, 4.0, 5.0, 8.0 or 16.0)
    PIPE_EN              :     boolean := false;   -- if true, enable pipelined read operations
    MAX_NOP              :     natural := 10000;   -- number of NOPs before entering self-refresh
    MULTIPLE_ACTIVE_ROWS :     boolean := false;   -- if true, allow an active row in each bank
    DATA_WIDTH           :     natural := 16;      -- host & SDRAM data width
    NROWS                :     natural := 8192;    -- number of rows in SDRAM array
    NCOLS                :     natural := 512;     -- number of columns in SDRAM array
    HADDR_WIDTH          :     natural := 24;      -- host-side address width
    SADDR_WIDTH          :     natural := 13       -- SDRAM-side address width
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
    uds                  : in  std_logic;  -- upper data strobe
    lds                  : in  std_logic;  -- lower data strobe
    earlyOpBegun         : out std_logic;  -- read/write/self-refresh op begun     (async)
    opBegun              : out std_logic;  -- read/write/self-refresh op begun (clocked)
    rdPending            : out std_logic;  -- read operation(s) are still in the pipeline
    done                 : out std_logic;  -- read or write operation is done
    rdDone               : out std_logic;  -- read done and data is available
    hAddr                : in  std_logic_vector(HADDR_WIDTH-1 downto 0); -- address from host
    hDIn                 : in  std_logic_vector(DATA_WIDTH-1 downto 0);  -- data from host
    hDOut                : out std_logic_vector(DATA_WIDTH-1 downto 0);  -- data to host
    status               : out std_logic_vector(3 downto 0);  -- diagnostic status of the FSM         

    -- SDRAM side
    sclkfb               : in    std_logic; -- clock from SDRAM after PCB delays
    sclk                 : out   std_logic; -- SDRAM clock sync'ed to master clock
    cke                  : out   std_logic; -- clock-enable to SDRAM
    cs_n                 : out   std_logic; -- chip-select to SDRAM
    ras_n                : out   std_logic; -- SDRAM row address strobe
    cas_n                : out   std_logic; -- SDRAM column address strobe
    we_n                 : out   std_logic; -- SDRAM write enable
    ba                   : out   std_logic_vector(1 downto 0);             -- SDRAM bank address bits
    sAddr                : out   std_logic_vector(SADDR_WIDTH-1 downto 0); -- SDRAM row/column address
    sData                : inout std_logic_vector(DATA_WIDTH-1 downto 0);  -- SDRAM in/out databus
    dqmh                 : out   std_logic; -- high databits I/O mask
    dqml                 : out   std_logic  -- low databits I/O mask
    );
end component;


begin

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
      clk                  => clk,      -- master clock from external clock source (unbuffered)
      bufclk               => bufclk,   -- buffered master clock output
      clk1x                => clk1x,    -- synchronized master clock (accounts for delays to external SDRAM)
      clk2x                => clk2x,    -- synchronized doubled master clock
      lock                 => lock,     -- DLL lock indicator
      rst                  => rst,      -- reset
      rd                   => rd,       -- host-side SDRAM read control from memory tester
      wr                   => wr,       -- host-side SDRAM write control from memory tester
      uds                  => uds,      -- host-side SDRAM upper data strobe from memory tester
      lds                  => lds,      -- host-side SDRAM lower data strobe from memory tester
      rdPending            => rdPending,-- read operation to SDRAM is in progress
      opBegun              => opBegun,  -- indicates memory read/write has begun
      earlyOpBegun         => earlyOpBegun,  -- early indicator that memory operation has begun
      rdDone               => rdDone,   -- indicates SDRAM memory read operation is done
      done                 => done,     -- indicates SDRAM memory read or write operation is done
      hAddr                => hAddr,    -- host-side address from memory tester to SDRAM
      hDIn                 => hDIn,     -- test data pattern from memory tester to SDRAM
      hDOut                => hDOut,    -- SDRAM data output to memory tester
      status               => status,   -- SDRAM controller state (for diagnostics)

    -- SDRAM side
      sclkfb => Sclk,   -- feedback SDRAM clock after PCB delays
      sclk   => Sclk,   -- clock to SDRAM
      CKE    => CKE,    -- clock-enable to SDRAM
      CS_N   => CS_N,   -- chip-select to SDRAM
      RAS_N  => RAS_N,  -- SDRAM row address strobe
      CAS_N  => CAS_N,  -- SDRAM column address strobe
      WE_N   => WE_N,   -- SDRAM write enable
      BA     => BA,     -- SDRAM bank address
      SADDR  => SADDR,  -- SDRAM row/column address
      SDATA  => SDATA,  -- data to and from SDRAM
      DQMH   => DQMH,   -- enable upper-byte of SDRAM databus if true
      DQML   => DQML    -- enable lower-byte of SDRAM databus if true
	 );

   sdram : process( CS_N, CAS_N, WE_N, DQMH, DQML, SADDR )
	begin
	  if( CS_N = '0') and (CAS_N = '0') and (WE_N = '1') then
	    if DQMH = '0' then
		   SDATA_I(15 downto 8) <= "110" & SADDR(12 downto 8);
       else
		   SDATA_I(15 downto 8) <= "ZZZZZZZZ";
       end if;
	    if DQML = '0' then
		   SDATA_I( 7 downto 0) <= SADDR(7 downto 0);
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
	-- i.e. data appears on the output of the SDRAM 3 clocks after the CAS strobe.
	--
   pipe : process( Sclk, data_pipe, SDATA_I, SDATA )
	variable i : integer;
	begin
	  if rising_edge( Sclk ) then
		 for i in CAS_LATENCY-1 downto 1 loop
	      data_pipe(i) <= data_pipe(i-1);
       end loop;
	    data_pipe(0) <= SDATA_I;
	  end if;
	  SDATA <= data_pipe(CAS_LATENCY-1);
	  SDATA_O <= SDATA;
   end process;

--
-- Activate outputs on rising clkx1
--
events : PROCESS(Sclk)
variable count : integer := 0;
variable burst : integer := 8;
BEGIN
		if rising_edge(Sclk) then
			if count = 0 then
            rd    <= '0';
            wr    <= '0';
            uds   <= '0';
            lds   <= '0';
				rst   <= '1';
			elsif count = 8 then
				rst   <= '0';
         elsif count = 30000 then
			   uds   <= '1';
			   rd    <= '1';
				burst := 0;
         elsif count = 30100 then
			   lds   <= '1';
			   rd    <= '1';
				burst := 0;
         elsif count = 30200 then
			   uds   <= '1';
			   lds   <= '1';
			   rd    <= '1';
				burst := 0;
         elsif count = 30300 then
			   uds   <= '1';
			   wr    <= '1';
				burst := 0;
         elsif count = 30400 then
			   lds   <= '1';
			   wr    <= '1';
 				burst := 0;
        elsif count = 30500 then
			   uds   <= '1';
			   lds   <= '1';
			   wr    <= '1';
				burst := 0;
         elsif count = 31000 then
			   uds <= '1';
			   lds <= '1';
			   rd  <= '1';
				burst := 7;
         elsif count = 32000 then
			   uds <= '1';
			   lds <= '1';
			   wr  <= '1';
				burst := 7;
			end if;

			if count = 0 then
			  hAddr <= "111100001111000000000000";
			  hDIn  <= "0000111100110011";
			elsif earlyOpBegun = '1' then
			  if burst = 0 then
			    uds <= '0';
			    lds <= '0';
			    wr  <= '0';
			    rd  <= '0';
           else
			    hAddr <= hAddr + "000000000000000000000001";
			    hDIn  <= hDin  + "0000000000000001";
			    burst := burst - 1;
           end if;
         end if;

			count := count + 1;
		end if;

   END PROCESS;

--
-- Generate a master clock for the SDRAM controller
--
   tb : PROCESS
	variable i : integer;
   BEGIN
		for i in 0 to 80000 loop
			clk <= '0';
			wait for 5 ns;
			clk <= '1';
			wait for 5 ns;
      end loop;
      wait; -- will wait forever
   end process;

-- *** End Test Bench - User Defined Section ***


end architecture;