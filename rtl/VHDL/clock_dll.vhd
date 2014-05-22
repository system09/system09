--===========================================================================--
--                                                                           --
--  clock_dll.vhd - Synthesible System Clock Divider for Xilinx Spartan 3    --
--                                                                           --
--===========================================================================--
--
--  File name      : clock_dll.vhd
--
--  Purpose        : Implements a a system clock divider for System09. 
--                   For Xilinx Spartan 3 and 3E FPGA boards
--                   Assumes a 12.5 MHz system clock input
--                   Generates a x1 (12.5 MHz) CPU clock 
--                   Generates a x2 (25.0 MHz) VGA clock 
--                   Generates a x4 (50.0 MHz) MEM clock 
--                  
--  Dependencies   : ieee.std_logic_1164
--                   ieee.std_logic_arith
--                   ieee.std_logic_unsigned
--                   ieee.numeric_std
--                   unisim.vcomponents
--
--  Author         : John E. Kent
--
--  Email          : dilbert57@opencores.org      
--
--  Web            : http://opencores.org/project,system09
--
--  clock_dll.vhd is a system clock divider for system09. 
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
-- Revision Name          Date             Description
-- 0.1      John E. Kent  7th September 2008 Initial version
-- 1.0      John E. Kent  30th May 2010      Added GPL Header
--
library ieee;
   use ieee.std_logic_1164.all;
   use ieee.std_logic_arith.all;
   use ieee.std_logic_unsigned.all;
   use ieee.numeric_std.all;
library unisim;
	use unisim.vcomponents.all;

entity clock_dll is
  port(
    clk_in      : in  std_Logic;  -- System Clock input
    clk_cpu     : out std_logic;  -- CPU Clock Out       (x1)
	 clk_vga     : out std_logic;  -- VGA Pixel Clock Out (x2)
	 clk_mem     : out std_logic;  -- Memory Clock Out    (x4)
	 locked      : out std_logic   -- DLL in lock
  );
end entity;

architecture RTL of clock_dll is

  signal CPU_CLK0    : std_ulogic;
  signal CPU_CLK90   : std_ulogic;
  signal CPU_CLK180  : std_ulogic;
  signal CPU_CLK270  : std_ulogic;
  signal CPU_CLK2X   : std_ulogic;
  signal CPU_CLKDV   : std_ulogic;
  signal CPU_LOCKED  : std_ulogic;
  signal CPU_CLKFB   : std_ulogic;
  signal CPU_CLKIN   : std_ulogic;
  signal CPU_RESET   : std_ulogic;

  signal VGA_CLK0    : std_ulogic;
  signal VGA_CLK90   : std_ulogic;
  signal VGA_CLK180  : std_ulogic;
  signal VGA_CLK270  : std_ulogic;
  signal VGA_CLK2X   : std_ulogic;
  signal VGA_CLKDV   : std_ulogic;
  signal VGA_LOCKED  : std_ulogic;
  signal VGA_CLKFB   : std_ulogic;
  signal VGA_CLKIN   : std_ulogic;
  signal VGA_RESET   : std_ulogic;
  signal VGA_RESET_N : std_ulogic;

-- Component Declaration for CLKDLL should be placed
-- after architecture statement but before begin keyword

component CLKDLL
  -- synthesis translate_off
  generic (
    CLKDV_DIVIDE          : real    := 2.0;  -- (1.5, 2.0, 2.5, 3.0, 4.0, 5.0, 8.0, 16.0)
    DUTY_CYCLE_CORRECTION : Boolean := TRUE; -- (TRUE, FALSE)
    STARTUP_WAIT          : boolean := FALSE -- (TRUE, FALSE)
  );
  -- synthesis translate_on
  port (
    CLK0   : out STD_ULOGIC;
    CLK180 : out STD_ULOGIC;
    CLK270 : out STD_ULOGIC;
    CLK2X  : out STD_ULOGIC;
    CLK90  : out STD_ULOGIC;
    CLKDV  : out STD_ULOGIC;
    LOCKED : out STD_ULOGIC;
    CLKFB  : in  STD_ULOGIC;
    CLKIN  : in  STD_ULOGIC;
    RST    : in  STD_ULOGIC
  );
end component;

component IBUFG
  port (
		i: in  std_logic;
		o: out std_logic
  );
end component;

component BUFG 
  port (
		i: in  std_logic;
		o: out std_logic
  );
end component;

component SRL16 
  port (
    Q   : out std_logic;
    D   : in  std_logic;
    CLK : in  std_logic;
    A0  : in  std_logic;
    A1  : in  std_logic;
    A2  : in  std_logic;
    A3  : in  std_logic
  );
end component;

--
-- Start instantiation
--
begin

--
-- 12.5MHz CPU clock input
--
cpu_clkin_buffer : IBUFG 
  port map(
    i => clk_in,
	 o => CPU_CLKIN
  );

--
-- 12.5MHz CPU clock input
--
cpu_clkout_buffer : BUFG 
  port map(
    i => CPU_CLKIN,
	 o => clk_cpu
  );

--
-- 25 MHz VGA clock input
--
cpu_clkfb_buffer : BUFG 
  port map(
    i => CPU_CLK2X,
	 o => CPU_CLKFB
  );

CLKDLL_CPU : CLKDLL
  -- synthesis translate_off
  generic map (
    CLKDV_DIVIDE          => 2.0,  -- (1.5,2,2.5,3,4,5,8,16)
    DUTY_CYCLE_CORRECTION => TRUE, -- (TRUE, FALSE)
    STARTUP_WAIT          => FALSE  -- (TRUE, FALSE)
  );
  -- synthesis translate_on
  port map (
    CLK0   => CPU_CLK0,
    CLK90  => CPU_CLK90,
    CLK180 => CPU_CLK180,
    CLK270 => CPU_CLK270,
    CLK2X  => CPU_CLK2X,
    CLKDV  => CPU_CLKDV,
    LOCKED => CPU_LOCKED,
    CLKFB  => CPU_CLKFB,
    CLKIN  => CPU_CLKIN,
    RST    => CPU_RESET 
  );
  
--
-- 25 MHz VGA clock output
--
vga_clkfb_buffer : BUFG 
  port map(
    i => VGA_CLK2X,
	 o => VGA_CLKFB
  );

CLKDLL_VGA : CLKDLL
  -- synthesis translate_off
  generic map (
    CLKDV_DIVIDE          => 2.0,    -- (1.5,2,2.5,3,4,5,8,16)
    DUTY_CYCLE_CORRECTION => TRUE, -- (TRUE, FALSE)
    STARTUP_WAIT          => FALSE  -- (TRUE, FALSE)
  );
  -- synthesis translate_on
  port map (
    CLK0   => VGA_CLK0,
    CLK90  => VGA_CLK90,
    CLK180 => VGA_CLK180,
    CLK270 => VGA_CLK270,
    CLK2X  => VGA_CLK2X,
    CLKDV  => VGA_CLKDV,
    LOCKED => VGA_LOCKED,
    CLKFB  => VGA_CLKFB,
    CLKIN  => VGA_CLKIN,
    RST    => VGA_RESET 
  );
  
my_srl16 : SRL16 port map (
  Q   => VGA_RESET_N,
  D   => CPU_LOCKED,
  CLK => CPU_CLKFB,
  A0  => '1',
  A1  => '1',
  A2  => '1',
  A3  => '1'
  );

clock_dll_assign : process( VGA_RESET_N, VGA_LOCKED,
                            clk_in, CPU_CLKFB, VGA_CLKFB )
begin
  VGA_RESET <= not VGA_RESET_N;
  VGA_CLKIN <= CPU_CLKFB;
  CPU_RESET <= '0';
  clk_vga   <= CPU_CLKFB;
  clk_mem   <= VGA_CLKFB;
  locked    <= VGA_LOCKED;
end process;

end architecture;
 