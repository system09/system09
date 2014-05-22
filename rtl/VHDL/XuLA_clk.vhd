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
        FPGA_CLK_FREQ          : integer := 12_000_000; -- HZ
	     CPU_CLK_FREQ           : integer := 24_000_000; -- Hz
	     VDU_CLK_FREQ           : integer := 24_000_000; -- Hz
		  SYS_CLK_FREQ           : integer := 48_000_000; -- Hz
		  RAM_CLK_FREQ           : integer := 96_000_000  -- Hz
  );
  port(
    fpga_clk     : in  std_logic;      -- 12MHz FPGA Clock
    cpu_clk      : out std_logic;      -- 24MHz CPU clock
    vdu_clk      : out std_logic;      -- 24MHz VDU clock
	 sys_clk      : out std_logic;      -- 48MHz SYS clock
	 ram_clk      : out std_logic;      -- 96MHz RAM clock
	 ram_clk_n    : out std_logic;      -- 96MHz RAM clock
	 lock         : out std_logic
    );
end XuLA_clk;

Architecture RTL of XuLA_clk is

signal clk12_dcm1   : std_logic;
signal clk24_dcm1   : std_logic;
signal clk24_dcm2   : std_logic;
signal clk48_dcm2   : std_logic;
signal clk96_dcm2   : std_logic;
signal clk96_dcm2_n : std_logic;
signal lock1        : std_logic;
signal lock2        : std_logic;

component BUFG 
  port (
    I : in  std_logic;
    O : out std_logic
  );
end component;

begin
 
  DCM_XuLA_inst1 : DCM
   generic map (
     DLL_FREQUENCY_MODE    => "LOW", -- "LOW" or "HIGH" 
     CLKIN_PERIOD          => 83.3333333,  -- in nsec 
     CLKFX_DIVIDE          => 1,
     CLKFX_MULTIPLY        => 4,
	  CLKDV_DIVIDE          => 2.0,
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
     CLKFB      => clk12_dcm1,    -- feedback input 
     CLK0       => clk12_dcm1,    -- Feedback output (phase, freq = input) 
     CLK90      => open,         -- Feedback output +  90deg
     CLK180     => open,         -- Feedback output + 180deg 
     CLK270     => open,         -- Feedback output + 270deg
     CLK2X      => clk24_dcm1,   -- 2 x input Freq Output 
     CLK2X180   => open,         -- 2 x input Freq Output + 180 deg
     CLKDV      => open,         -- Fclkdv = Fclkin/CLKDV_DIVIDE 
     CLKFX      => open,         -- Fclkfx = Fclkin*CLKFX_MULIPLY/CLKFX_DIVIDE 
     CLKFX180   => open,         -- CLKFX180 = CLKFX + 180 degrees 
     LOCKED     => lock1,        -- DCM in lock 
     PSDONE     => open,
     STATUS     => open,
     PSCLK      => open,         -- Clock input to dynamic phase shifter 
     PSEN       => open,
     PSINCDEC   => open,
     RST        => '0'
   );

  bufcpu : BUFG port map (
               I => clk24_dcm1,
               O => cpu_clk
              );

  bufvdu : BUFG port map (
               I => clk24_dcm1,
               O => vdu_clk
              );

  DCM_XuLA_inst2 : DCM
   generic map (
     DLL_FREQUENCY_MODE    => "LOW", -- "LOW" or "HIGH" 
     CLKIN_PERIOD          => 83.3333333,  -- in nsec 
     CLKFX_DIVIDE          => 1,
     CLKFX_MULTIPLY        => 4,
	  CLKDV_DIVIDE          => 2.0,
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
     CLKIN      => clk24_dcm1,   -- input 48MHz
     CLKFB      => clk24_dcm2,   -- feedback input 
     CLK0       => clk24_dcm2,   -- Feedback output (phase, freq = input) 
     CLK90      => open,         -- Feedback output +  90deg
     CLK180     => open,         -- Feedback output + 180deg 
     CLK270     => open,         -- Feedback output + 270deg
     CLK2X      => clk48_dcm2,   -- 2 x input Freq Output 
     CLK2X180   => open,         -- 2 x input Freq Output + 180 deg
     CLKDV      => open,         -- Fclkdv = Fclkin/CLKDV_DIVIDE 
     CLKFX      => clk96_dcm2,   -- Fclkfx = Fclkin*CLKFX_MULIPLY/CLKFX_DIVIDE 
     CLKFX180   => clk96_dcm2_n,         -- CLKFX180 = CLKFX + 180 degrees 
     LOCKED     => lock2,        -- DCM in lock 
     PSDONE     => open,
     STATUS     => open,
     PSCLK      => open,         -- Clock input to dynamic phase shifter 
     PSEN       => open,
     PSINCDEC   => open,
     RST        => '0'
   );

  bufsys : BUFG port map(
               I => clk48_dcm2,
               O => sys_clk
              );

  bufram : BUFG port map(
               I => clk96_dcm2,
               O => ram_clk
              );

  bufram_n : BUFG port map(
               I => clk96_dcm2_n,
               O => ram_clk_n
              );

  lock <= lock1 and lock2;
  
end RTL;
