--------------------------------------------------------------------
-- Company       : XESS Corp.
-- Engineer      : Dave Vanden Bout
-- Creation Date : 05/17/2005
-- Copyright     : 2005, XESS Corp
-- Tool Versions : WebPACK 6.3.03i
--
-- Description:
--    Customizes the generic SDRAM controller module for the XSA Board.
--
-- Revision:
--    1.3.0
--
-- Additional Comments:
--    1.3.0:
--        adapted for revised XuLA2 SDRAMCntl
--        John Keent 2013-02-23
--    1.2.0:
--        added upper and lower data strobe signals
--        John Kent 2008-03-23
--    1.1.0:
--        Added CLK_DIV generic parameter to allow stepping-down the clock frequency.
--        Added MULTIPLE_ACTIVE_ROWS generic parameter to enable/disable keeping an active row in each bank.
--    1.0.0:
--        Initial release.
--
-- License:
--    This code can be freely distributed and modified as long as
--    this header is not removed.
--------------------------------------------------------------------


library IEEE, UNISIM;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use UNISIM.VComponents.all;
use WORK.CommonPckg.all;

entity XSASDRAMCntl is
  generic(
    FREQ                 : natural := 100_000; -- operating frequency in KHz
    CLK_DIV              : real    := 2.0;     -- divisor for FREQ (can only be 1.0, 1.5, 2.0, 2.5, 3.0, 4.0, 5.0, 8.0 or 16.0)
    PIPE_EN              : boolean := false;   -- if true, enable pipelined read operations
    MAX_NOP              : natural := 10000;   -- number of NOPs before entering self-refresh
    MULTIPLE_ACTIVE_ROWS : boolean := false;   -- if true, allow an active row in each bank
    DATA_WIDTH           : natural := 16;      -- host & SDRAM data width
    NROWS                : natural := 4096;    -- number of rows in SDRAM array
    NCOLS                : natural := 512;     -- number of columns in SDRAM array
    HADDR_WIDTH          : natural := 23;      -- host-side address width
    SADDR_WIDTH          : natural := 12       -- SDRAM-side address width
    );
  port(
    -- host side
    clk_i                : in  std_logic;  -- master clock
    bufclk_o             : out std_logic;  -- buffered master clock
    clk1x_o              : out std_logic;  -- host clock sync'ed to master clock (and divided if CLK_DIV>1)
    clk2x_o              : out std_logic;  -- double-speed host clock
    lock_o               : out std_logic;  -- true when host clock is locked to master clock
    rst_i                : in  std_logic;  -- reset
    rd_i                 : in  std_logic;  -- initiate read operation
    wr_i                 : in  std_logic;  -- initiate write operation
    uds_i                : in  std_logic;  -- upper data strobe
    lds_i                : in  std_logic;  -- lower data strobe
    earlyOpBegun_o       : out std_logic;  -- read/write/self-refresh op begun     (async)
    opBegun_o            : out std_logic;  -- read/write/self-refresh op begun (clocked)
    rdPending_o          : out std_logic;  -- read operation(s) are still in the pipeline
    done_o               : out std_logic;  -- read or write operation is done
    rdDone_o             : out std_logic;  -- read done and data is available
    addr_i               : in  std_logic_vector(HADDR_WIDTH-1 downto 0); -- address from host
    data_i               : in  std_logic_vector(DATA_WIDTH-1 downto 0);  -- data from host
    data_o               : out std_logic_vector(DATA_WIDTH-1 downto 0);  -- data to host
    status_o             : out std_logic_vector(3 downto 0);  -- diagnostic status of the FSM         

    -- SDRAM side
    sdClkfb_i            : in    std_logic;      -- clock from SDRAM after PCB delays
    sdClk_o              : out   std_logic;      -- SDRAM clock sync'ed to master clock
    sdCke_o              : out   std_logic;      -- Clock-enable to SDRAM.
    sdCe_bo              : out   std_logic;      -- Chip-select to SDRAM.
    sdRas_bo             : out   std_logic;      -- SDRAM row address strobe.
    sdCas_bo             : out   std_logic;      -- SDRAM column address strobe.
    sdWe_bo              : out   std_logic;      -- SDRAM write enable.
    sdBs_o               : out   std_logic_vector(1 downto 0);  -- SDRAM bank address.
    sdAddr_o             : out   std_logic_vector(SADDR_WIDTH-1 downto 0);  -- SDRAM row/column address.
    sdData_io            : inout std_logic_vector(DATA_WIDTH-1 downto 0);  -- Data to/from SDRAM.
    sdDqmh_o             : out   std_logic;      -- Enable upper-byte of SDRAM databus if true.
    sdDqml_o             : out   std_logic       -- Enable lower-byte of SDRAM databus if true.
    );
end XSASDRAMCntl;



architecture arch of XSASDRAMCntl is

  component SdramCntl is
    generic(
      FREQ_G                 : real    := real(FREQ/1000);  -- Operating frequency in MHz.
      IN_PHASE_G             : boolean := true;  -- SDRAM and controller work on same or opposite clock edge.
      PIPE_EN_G              : boolean := false;  -- If true, enable pipelined read operations.
      MAX_NOP_G              : natural := 10000;  -- Number of NOPs before entering self-refresh.
      ENABLE_REFRESH_G       : boolean := true;  -- If true, row refreshes are automatically inserted.
      MULTIPLE_ACTIVE_ROWS_G : boolean := false;  -- If true, allow an active row in each bank.
      DATA_WIDTH_G           : natural := 16;   -- Host & SDRAM data width.
      -- Parameters for Winbond W9812G6JH-75 (all times are in nanoseconds).
      NROWS_G                : natural := 4096;  -- Number of rows in SDRAM array.
      NCOLS_G                : natural := 512;  -- Number of columns in SDRAM array.
      HADDR_WIDTH_G          : natural := 23;   -- Host-side address width.
      SADDR_WIDTH_G          : natural := 12;   -- SDRAM-side address width.
      T_INIT_G               : real    := 200_000.0;  -- min initialization interval (ns).
      T_RAS_G                : real    := 45.0;  -- min interval between active to precharge commands (ns).
      T_RCD_G                : real    := 20.0;  -- min interval between active and R/W commands (ns).
      T_REF_G                : real    := 64_000_000.0;  -- maximum refresh interval (ns).
      T_RFC_G                : real    := 65.0;  -- duration of refresh operation (ns).
      T_RP_G                 : real    := 20.0;  -- min precharge command duration (ns).
      T_XSR_G                : real    := 75.0  -- exit self-refresh time (ns).
    );
  port(
      -- Host side.
      clk_i          : in  std_logic;         -- Master clock.
      lock_i         : in  std_logic := YES;  -- True if clock is stable.
      rst_i          : in  std_logic := NO;   -- Reset.
      rd_i           : in  std_logic := NO;   -- Initiate read operation.
      wr_i           : in  std_logic := NO;   -- Initiate write operation.
      uds_i          : in  std_logic;
      lds_i          : in  std_logic;
      earlyOpBegun_o : out std_logic;         -- Read/write/self-refresh op has begun (async).
      opBegun_o      : out std_logic;         -- Read/write/self-refresh op has begun (clocked).
      rdPending_o    : out std_logic;         -- True if read operation(s) are still in the pipeline.
      done_o         : out std_logic;         -- Read or write operation is done_o.
      rdDone_o       : out std_logic;         -- Read operation is done_o and data is available.
      addr_i         : in  std_logic_vector(HADDR_WIDTH_G-1 downto 0) := (others => ZERO);  -- Address from host to SDRAM.
      data_i         : in  std_logic_vector(DATA_WIDTH_G-1 downto 0)  := (others => ZERO);  -- Data from host to SDRAM.
      data_o         : out std_logic_vector(DATA_WIDTH_G-1 downto 0);  -- Data from SDRAM to host.
      status_o       : out std_logic_vector(3 downto 0);  -- Diagnostic status of the FSM         .

      -- SDRAM side.
      sdCke_o        : out   std_logic;      -- Clock-enable to SDRAM.
      sdCe_bo        : out   std_logic;      -- Chip-select to SDRAM.
      sdRas_bo       : out   std_logic;      -- SDRAM row address strobe.
      sdCas_bo       : out   std_logic;      -- SDRAM column address strobe.
      sdWe_bo        : out   std_logic;      -- SDRAM write enable.
      sdBs_o         : out   std_logic_vector(1 downto 0);  -- SDRAM bank address.
      sdAddr_o       : out   std_logic_vector(SADDR_WIDTH_G-1 downto 0);  -- SDRAM row/column address.
      sdData_io      : inout std_logic_vector(DATA_WIDTH_G-1 downto 0);  -- Data to/from SDRAM.
      sdDqmh_o       : out   std_logic;  -- Enable upper-byte of SDRAM databus if true.
      sdDqml_o       : out   std_logic  -- Enable lower-byte of SDRAM databus if true.
      );
  end component;

  -- The SDRAM controller and external SDRAM chip will clock on the same edge
  -- if the frequency and divided frequency are both greater than the minimum DLL lock frequency.
  -- Otherwise the DLLs cannot be used so the SDRAM controller and external SDRAM clock on opposite edges
  -- to try and mitigate the clock skew between the internal FPGA logic and the external SDRAM.
  constant MIN_LOCK_FREQ : real    := 25_000.0;
  constant IN_PHASE      : boolean := real(FREQ)/CLK_DIV >= MIN_LOCK_FREQ;
  -- Calculate the frequency of the clock for the SDRAM.
--  constant SDRAM_FREQ    : natural := int_select(IN_PHASE, (FREQ*integer(2.0*CLK_DIV))/2, FREQ);
  constant SDRAM_FREQ    : natural := intselect(IN_PHASE, (FREQ*2)/integer(2.0*CLK_DIV), FREQ);
  -- Compute the CLKDV_DIVIDE generic paramter for the DLL modules.  It defaults to 2 when CLK_DIV=1
  -- because the DLL does not support a divisor of 1 on the CLKDV output.  We use the CLK0 output
  -- when CLK_DIV=1 so we don't care what is output on thr CLK_DIV output of the DLL.
  constant CLKDV_DIVIDE  : real    := realselect(CLK_DIV = 1.0, 2.0, CLK_DIV);

  signal int_clkin                      : std_logic; -- signals for internal logic clock DLL
  signal int_clk1x, int_clk1x_b         : std_logic;
  signal int_clk2x, int_clk2x_b         : std_logic;
  signal int_clkdv, int_clkdv_b         : std_logic;
  signal ext_clkin                      : std_logic; -- signals for external logic clock DLL
  signal ext_clk1x, ext_clk1x_b         : std_logic;
  signal dllext_rst, dllext_rst_n       : std_logic; -- external DLL reset signal
  signal clkfb_b                        : std_logic; -- clock for SDRAM controller logic
  signal clk_int                        : std_logic;
  signal ext_sclk, ext_sclk_n           : std_logic;
  signal int_lock, ext_lock, lock_i     : std_logic; -- DLL lock signals


begin

  -----------------------------------------------------------
  -- setup the DLLs for clock generation 
  -----------------------------------------------------------

  -- master clock must come from a dedicated clock pin
  --  clkin_buf : BUFG port map (I => clk, O => int_clkin);
  --
  -- clk already buffered externally
  --
  int_clkin <= clk_i;
  -- The external DLL is driven from the same source as the internal DLL
  -- if the clock divisor is 1.  If CLK_DIV is greater than 1, then the external DLL 
  -- is driven by the divided clock from the internal DLL.  Otherwise, the SDRAM will be
  -- clocked on the opposite edge if the internal and external logic are not in-phase.
  ext_clkin <= int_clkin    when (IN_PHASE and (CLK_DIV = 1.0)) else
                int_clkdv_b when (IN_PHASE and (CLK_DIV/=1.0))  else
                not int_clkin;

  -- Generate the DLLs for sync'ing the clocks as long as the clocks
  -- have a frequency high enough for the DLLs to lock
  gen_dlls : if IN_PHASE generate

    -- generate an internal clock sync'ed to the master clock
    dllint : DCM_SP
      generic map(
        CLKDV_DIVIDE => CLKDV_DIVIDE
        )
      port map(
        CLKIN        => int_clkin,
        CLKFB        => int_clk1x_b,
        RST          => '0',
		  PSEN         => '0',
		  PSINCDEC     => '0',
		  PSCLK        => int_clkin,
        CLK0         => int_clk1x,
        CLK90        => open,
        CLK180       => open,
        CLK270       => open,
        CLK2X        => int_clk2x,
        CLKDV        => int_clkdv,
        LOCKED       => int_lock
        );

    -- sync'ed single, doubled and divided clocks for use by internal logic
    int_clk1x_buf : BUFG port map(I => int_clk1x, O => int_clk1x_b);
    int_clk2x_buf : BUFG port map(I => int_clk2x, O => int_clk2x_b);
    int_clkdv_buf : BUFG port map(I => int_clkdv, O => int_clkdv_b);

    -- The external DLL is held in a reset state until the internal DLL locks.
    -- Then the external DLL reset is released after a delay set by this shift register.
    -- This keeps the external DLL from locking onto the internal DLL clock signal
    -- until it is stable.
    SRL16_inst : SRL16
      generic map (
        INIT => X"0000"
        )
      port map (
        CLK  => clk_int,
        A0   => '1',
        A1   => '1',
        A2   => '1',
        A3   => '1',
        D    => int_lock,
        Q    => dllext_rst_n
        );
--    Error ???
--    dllext_rst <= not dllext_rst when CLK_DIV/=1.0 else ZERO;
    dllext_rst <= not dllext_rst_n when CLK_DIV/=1.0 else ZERO;

    -- generate an external SDRAM clock sync'ed to the master clock
    sclkfb_buf     : IBUFG port map(I => sdclkfb_i, O => clkfb_b);  -- SDRAM clock with PCB delays
    ext_clk1x_bufg :  BUFG port map (I => ext_clk1x, O => ext_clk1x_b);
	 
    dllext     : DCM_SP port map(
        CLKIN        => ext_clkin,  -- this is either the master clock or the divided clock from the internal DLL
        CLKFB        => clkfb_b,
        RST          => dllext_rst,
		  PSEN         => '0',
		  PSINCDEC     => '0',
		  PSCLK        => int_clkin,
        CLK0         => ext_clk1x,
        CLK90        => open,
        CLK180       => open,
        CLK270       => open,
        CLK2X        => open,
        CLKDV        => open,
        LOCKED       => ext_lock
      );

  end generate;

  -- The buffered clock is just a buffered version of the master clock.
  bufclk_bufg : BUFG port map (I => int_clkin, O => bufclk_o);
  
  -- The host-side clock comes from the CLK0 output of the internal DLL if the clock divisor is 1.
  -- Otherwise it comes from the CLKDV output if the clock divisor is greater than 1.
  -- Otherwise it is just a copy of the master clock if the DLLs aren't being used.
  clk_int    <= int_clk1x_b when (IN_PHASE and (CLK_DIV  = 1.0)) else
                int_clkdv_b when (IN_PHASE and (CLK_DIV /= 1.0))  else
                int_clkin;
  clk1x_o    <= clk_int;      -- This is the output of the host-side clock
  clk2x_o    <= int_clk2x_b when IN_PHASE else 
                int_clkin;  -- this is the doubled master clock
  ext_sclk   <= ext_clk1x_b when IN_PHASE else 
                ext_clkin;  -- this is the clock for the external SDRAM
  ext_sclk_n <= not ext_sclk;
  
  ODDR2_inst : ODDR2
   generic map(
      DDR_ALIGNMENT => "NONE",
      INIT => '0',
      SRTYPE => "SYNC")
   port map (
      Q =>  sdclk_o,       -- 1-bit output data
      C0 => ext_sclk,   -- 1-bit clock input
      C1 => ext_sclk_n, -- 1-bit clock input
      CE => '1',        -- 1-bit clock enable input
      D0 => '1',
      D1 => '0',
      R  => '0',    -- 1-bit reset input
      S  => '0'     -- 1-bit set input
   );

  -- indicate the lock status of the internal and external DLL
  lock_i <= int_lock and ext_lock when IN_PHASE else YES;
  lock_o <= lock_i;                     -- lock signal for the host logic


  -- SDRAM memory controller module
  u1 : sdramCntl
    generic map(
      FREQ_G                 => real(SDRAM_FREQ/1000),
      IN_PHASE_G             => IN_PHASE,
      PIPE_EN_G              => PIPE_EN,
      MAX_NOP_G              => MAX_NOP,
      ENABLE_REFRESH_G       => true,  -- If true, row refreshes are automatically inserted.
      MULTIPLE_ACTIVE_ROWS_G => MULTIPLE_ACTIVE_ROWS,
      DATA_WIDTH_G           => DATA_WIDTH,
      NROWS_G                => NROWS,
      NCOLS_G                => NCOLS,
      HADDR_WIDTH_G          => HADDR_WIDTH,
      SADDR_WIDTH_G          => SADDR_WIDTH
      )
    port map(
      -- Host side.
      clk_i                => clk_int,    -- master clock from external clock source (unbuffered)
      lock_i               => lock_i,     -- valid synchronized clocks indicator
      rst_i                => rst_i,      -- reset
      rd_i                 => rd_i,       -- host-side SDRAM read control from memory tester
      wr_i                 => wr_i,       -- host-side SDRAM write control from memory tester
      uds_i                => uds_i,      -- host-side SDRAM upper data strobe
      lds_i                => lds_i,      -- host-side SDRAM lower data strobe
      earlyOpBegun_o       => earlyOpBegun_o,  -- SDRAM memory read/write done indicator
      opBegun_o            => opBegun_o,  -- SDRAM memory read/write done indicator
      rdPending_o          => rdPending_o,
      rdDone_o             => rdDone_o,   -- SDRAM memory read/write done indicator
      done_o               => done_o,
      Addr_i               => addr_i,     -- host-side address from memory tester
      data_i               => data_i,     -- test data pattern from memory tester
      data_o               => data_o,     -- SDRAM data output to memory tester
      status_o             => status_o,   -- SDRAM controller state (for diagnostics)

      -- SDRAM side.
      sdcke_o              => sdcke_o,    -- SDRAM clock enable
      sdce_bo              => sdce_bo,    -- SDRAM chip-select
      sdras_bo             => sdras_bo,   -- SDRAM RAS
      sdcas_bo             => sdcas_bo,   -- SDRAM CAS
      sdwe_bo              => sdwe_bo,    -- SDRAM write-enable
      sdbs_o               => sdbs_o,     -- SDRAM bank address
      sdaddr_o             => sdAddr_o,   -- SDRAM address
      sddata_io            => sdData_io,  -- SDRAM data i/o
      sddqmh_o             => sddqmh_o,   -- SDRAM DQMH
      sddqml_o             => sddqml_o    -- SDRAM DQML
      );

end arch;
