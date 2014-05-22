--===========================================================================--
--                                                                           --
--                  N input Multiplexer Test Bench                           --
--                                                                           --
--===========================================================================--
--
--  File name      : n_mux_tb.vhd
--
--  Entity name    : n_mux_tb
--
--  Purpose        : Implements an n input multiplexer Test bench
--
--  Dependencies   : ieee.std_logic_1164
--                   ieee.numeric_std
--                   ieee.std_logic_unsigned
--
--  Author         : John E. Kent
--
--  Email          : dilbert57@opencores.org      
--
--  Web            : http://opencores.org/project,system09
--
--
--  Copyright (C) 2012 John Kent
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
-- Version Author        Date         Changes
--
-- 0.1     John Kent     2012-04-13   New model
--


library ieee;
   use ieee.std_logic_1164.all;
--   use IEEE.STD_LOGIC_ARITH.ALL;
   use IEEE.STD_LOGIC_UNSIGNED.ALL;
   use ieee.numeric_std.all;
   use work.bit_funcs.all;

entity n_mux_testbench is
end n_mux_testbench;

-------------------------------------------------------------------------------
-- Architecture for memio Controller Unit
-------------------------------------------------------------------------------
architecture behavior of n_mux_testbench is
  -----------------------------------------------------------------------------
  -- Signals
  -----------------------------------------------------------------------------
  constant N_WIDTH   : integer := 24;
  constant C_WIDTH   : integer := log2(N_WIDTH-1)+1;
  -- MUX Interface signals
  signal mux_ctrl    : Std_Logic_Vector(C_WIDTH-1 downto 0)   := (others => '0');
  signal mux_in      : Std_Logic_Vector(N_WIDTH-1 downto 0) := (others => '0');
  signal mux_out     : Std_Logic;

component n_mux is
  generic (
    N_WIDTH : integer := 24
    );                      
  port (
    c  : in  STD_LOGIC_VECTOR(C_WIDTH-1 downto 0);
    d  : in  STD_LOGIC_VECTOR(N_WIDTH-1 downto 0);
    q  : out STD_LOGIC
    );
end component;

begin

 my_n_mux : n_mux 
 generic map (
   N_WIDTH => N_WIDTH
   )
 port map (
   c => mux_ctrl,
   d => mux_in,
   q => mux_out
  );

  -- *** Test Bench - User Defined Section ***
   tb : PROCESS
	variable count : integer;
   BEGIN

      mux_ctrl <= "00000";
      mux_in <= "111111110000111100110101";

		for count in 0 to 31 loop
			wait for 100 ns;
         mux_ctrl <= mux_ctrl + "00001";
		end loop;

      wait; -- will wait forever
   END PROCESS;
-- *** End Test Bench - User Defined Section ***

end behavior; --===================== End of architecture =======================--
