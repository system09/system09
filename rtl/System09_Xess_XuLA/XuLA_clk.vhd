--=============================================================================--
--                                                                             --
--  System09 - Synthesizable System On a Chip - XuLA System Clock DCM          --
--                                                                             --
--=============================================================================--
--
--
-- File name      : XuLA_clk.vhd
--
-- Entity name    : XuLA_clk
--
-- Purpose        : Clock module to generate 48MHz SDRAM clock
--                  and 24MHz CPU and VDU pixel clock 
--                  from the 12MHz PIC FPGA Clock
--
-- Dependencies   : ieee.Std_Logic_1164
--                  ieee.std_logic_unsigned
--                  ieee.std_logic_arith
--                  ieee.numeric_std
--
-- Uses           : 
--
-- Author         : John E. Kent      
--                  dilbert57@opencores.org      
--
--  Copyright (C) 2011 John Kent
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
-- Version 0.1 - 30 April 2011 - John Kent
-- Initial version
--
library ieee;
   use ieee.std_logic_1164.all;
   use IEEE.STD_LOGIC_ARITH.ALL;
   use IEEE.STD_LOGIC_UNSIGNED.ALL;
   use ieee.numeric_std.all;
library unisim;
   use unisim.vcomponents.all;

Entity XuLA_clk is
  generic(
        FPGA_CLK_FREQ          : integer := 12000000; -- HZ
	     CPU_CLK_FREQ           : integer := 24000000; -- Hz
	     VDU_CLK_FREQ           : integer := 24000000; -- Hz
		  RAM_CLK_FREQ           : integer := 48000000 -- Hz
  );
  port(
    fpga_clk     : in  std_logic;      -- 12MHz FPGA Clock
    cpu_clk      : out std_logic;      -- 24MHz CPU clock
    vdu_clk      : out std_logic;      -- 24MHz VDU clock
	 ram_clk      : out std_logic       -- 48MHz RAM clock
    );
end XuLA_clk;

Architecture RTL of XuLA_clk is

signal clk12_dcm : std_logic;
signal clk24_dcm : std_logic;
signal clk48_dcm : std_logic;

component BUFG 
  port (
    I : in  std_logic;
    O : out std_logic
  );
end component;

begin
 
  DCM_XuLA_inst : DCM
   generic map (
     DLL_FREQUENCY_MODE    => "LOW", -- "LOW" or "HIGH" 
     CLKIN_PERIOD          => 83.3,  -- in nsec 
     CLKFX_DIVIDE          => 1,
     CLKFX_MULTIPLY        => 4,
	  CLKDV_DIVIDE          => 2,
     CLKIN_DIVIDE_BY_2     => FALSE,
     CLKOUT_PHASE_SHIFT    => "NONE",
     CLK_FEEDBACK          => "1X",
     DESKEW_ADJUST         => "SYSTEM_SYNCHRONOUS",
     DFS_FREQUENCY_MODE    => "LOW",
     DUTY_CYCLE_CORRECTION => TRUE,
     FACTORY_JF            => X"8080",
     PHASE_SHIFT           => 0,
     STARTUP_WAIT          => FALSE)
   port map (
     CLKIN      => fpga_clk,     -- input 12MHz
     CLKFB      => clk12_dcm,    -- feedback input 
     CLK0       => clk12_dcm,    -- Feedback output (phase, freq = input) 
     CLK90      => open,         -- Feedback output +  90deg
     CLK180     => open,         -- Feedback output + 180deg 
     CLK270     => open,         -- Feedback output + 270deg
     CLK2X      => clk24_dcm,    -- 2 x input Freq Output 
     CLK2X180   => open,         -- 2 x input Freq Output + 180 deg
     CLKDV      => open,         -- Fclkdv = Fclkin/CLKDV_DIVIDE 
     CLKFX      => clk48_dcm,    -- Fclkfx = Fclkin*CLKFX_MULIPLY/CLKFX_DIVIDE 
     CLKFX180   => open,         -- CLKFX180 = CLKFX + 180 degrees 
     LOCKED     => open,         -- DCM in lock 
     PSDONE     => open,
     STATUS     => open,
     PSCLK      => open,         -- Clock input to dynamic phase shifter 
     PSEN       => open,
     PSINCDEC   => open,
     RST        => '0'
   );

  bufram : BUFG port map(
               I => clk48_dcm,
               O => ram_clk
              );

  bufcpu : BUFG port map (
               I => clk24_dcm,
               O => cpu_clk
              );

  bufvdu : BUFG port map (
               I => clk24_dcm,
               O => vdu_clk
              );

end RTL;
