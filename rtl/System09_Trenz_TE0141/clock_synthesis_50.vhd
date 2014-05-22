--
-- Clock synthesis
--
-- This module generates the 50 Mhz System Clock
-- from the Trenz 30MHz clock using a DCM.  
-- The outputs are fed into BUFGs.
--
library ieee;
use ieee.std_logic_1164.ALL;
use ieee.numeric_std.ALL;
-- synopsys translate_off
library UNISIM;
use UNISIM.Vcomponents.ALL;
-- synopsys translate_on

entity clock_synthesis is
  port ( clk_30mhz   : in  std_logic; 
         sys_clk_out : out std_logic;
         locked      : out std_logic);
end clock_synthesis;

architecture BEHAVIORAL of clock_synthesis is

  signal clk_30mhz_ibufg : std_logic;

  signal sys_clkfb_in    : std_logic;
  signal sys_clkfb_out   : std_logic;
  signal sys_clk_in      : std_logic;

  signal gnd1            : std_logic;

  component BUFG
    port ( I : in    std_logic; 
           O : out   std_logic);
  end component;
  
  component IBUFG
    port ( I : in    std_logic; 
           O : out   std_logic);
  end component;
  
  -- Period Jitter with noise (unit interval) for block DCM_INST = 0.04 UI
  -- Period Jitter with noise (Peak-to-Peak) for block DCM_INST = 0.86 ns
  component DCM
    generic( CLK_FEEDBACK          : string     :=  "1X";
             CLKDV_DIVIDE          : real       :=  2.000000;
             CLKFX_DIVIDE          : integer    :=  1;
             CLKFX_MULTIPLY        : integer    :=  4;
             CLKIN_DIVIDE_BY_2     : boolean    :=  FALSE;
             CLKIN_PERIOD          : real       :=  10.000000;
             CLKOUT_PHASE_SHIFT    : string     :=  "NONE";
             DESKEW_ADJUST         : string     :=  "SYSTEM_SYNCHRONOUS";
             DFS_FREQUENCY_MODE    : string     :=  "LOW";
             DLL_FREQUENCY_MODE    : string     :=  "LOW";
             DUTY_CYCLE_CORRECTION : boolean    :=  TRUE;
             FACTORY_JF            : bit_vector :=  x"C080";
             PHASE_SHIFT           : integer    :=  0;
             STARTUP_WAIT          : boolean    :=  TRUE;
             DSS_MODE              : string     :=  "NONE");
    port ( CLKIN    : in    std_logic; 
           CLKFB    : in    std_logic; 
           RST      : in    std_logic; 
           PSEN     : in    std_logic; 
           PSINCDEC : in    std_logic; 
           PSCLK    : in    std_logic; 
           DSSEN    : in    std_logic; 
           CLK0     : out   std_logic; 
           CLK90    : out   std_logic; 
           CLK180   : out   std_logic; 
           CLK270   : out   std_logic; 
           CLKDV    : out   std_logic; 
           CLK2X    : out   std_logic; 
           CLK2X180 : out   std_logic; 
           CLKFX    : out   std_logic; 
           CLKFX180 : out   std_logic; 
           STATUS   : out   std_logic_vector (7 downto 0); 
           LOCKED   : out   std_logic; 
           PSDONE   : out   std_logic);
  end component;
  
begin

  GND1 <= '0';

  sys_clkin_ibufg_inst : ibufg
    port map (i => clk_30mhz,
              o => clk_30mhz_ibufg);
  
  sys_clk_bufg_inst : bufg
    port map (i => sys_clk_in,
              o => sys_clk_out);
  

  sys_fb_bufg_inst : bufg
    port map (i => sys_clkfb_in,
              o => sys_clkfb_out);

  sys_clk_dcm : dcm
    generic map( clk_feedback          =>  "1X",
                 clkfx_divide          =>  6,
                 clkfx_multiply        =>  10,
                 clkin_divide_by_2     =>  FALSE,
                 clkin_period          =>  33.333300,
                 clkout_phase_shift    =>  "NONE",
                 deskew_adjust         =>  "SYSTEM_SYNCHRONOUS",
                 dfs_frequency_mode    =>  "LOW",
                 dll_frequency_mode    =>  "LOW",
                 duty_cycle_correction =>  TRUE,
                 factory_jf            =>  x"C080",
                 phase_shift           =>  0,
                 startup_wait          =>  FALSE)

    port map (clkfb    => sys_clkfb_out,
              clkin    => clk_30mhz_ibufg,
              dssen    => gnd1,
              psclk    => gnd1,
              psen     => gnd1,
              psincdec => gnd1,
              rst      => gnd1,
              clkdv    => open,
              clkfx    => sys_clk_in,
              clkfx180 => open,
              clk2x    => open,
              clk2x180 => open,
              clk0     => sys_clkfb_in,
              clk90    => open,
              clk180   => open,
              clk270   => open,
              locked   => locked,
              psdone   => open,
              status   => open);

end;
