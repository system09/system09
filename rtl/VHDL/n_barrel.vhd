--===========================================================================--
--                                                                           --
--                  Synthesizable N * 2 Input Barrel Multiplexer             --
--                                                                           --
--===========================================================================--
--
--  File name      : n_barrel.vhd
--
--  Entity name    : n_barrel
--
--  Purpose        : Implements an N * 2 input barrel multiplexer
--                   The control input selects the number of bit places 
--                   to be shifted down. 0 is no shift
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
library IEEE;
   use ieee.std_logic_1164.all;
   use IEEE.STD_LOGIC_ARITH.ALL;
   use IEEE.STD_LOGIC_UNSIGNED.ALL;
   use ieee.numeric_std.all;
   use work.bit_funcs.all;

entity n_barrel is
  generic (
    N_WIDTH : integer := 24
    );                      
  port (
    c  : in  STD_LOGIC_VECTOR(LOG2(N_WIDTH-1) downto 0);
    d  : in  STD_LOGIC_VECTOR((N_WIDTH-1)*2 downto 0);
    q  : out STD_LOGIC_VECTOR(N_WIDTH-1 downto 0)
    );
end n_barrel;

architecture rtl of n_barrel is

component n_mux is
  generic (
    N_WIDTH : integer := N_WIDTH
    );                      
  port (
    c  : in  STD_LOGIC_VECTOR(LOG2(N_WIDTH-1) downto 0);
    d  : in  STD_LOGIC_VECTOR(N_WIDTH-1 downto 0);
    q  : out STD_LOGIC
    );
end component;

begin

barrel_mux : for i in N_WIDTH-1 downto 0 generate
begin
--  n_mux_gen: generate
--  begin
    n_mux_inst: n_mux 
    generic map ( N_WIDTH => N_WIDTH ) 
    port map (c => c, d => d(N_WIDTH-1+i downto i), q => q(i) );
--  end generate n_mux_gen;
end generate barrel_mux;

end rtl;