--===========================================================================--
--                                                                           --
--                  Synthesizable N input Multiplexer                        --
--                                                                           --
--===========================================================================--
--
--  File name      : n_mux.vhd
--
--  Entity name    : n_mux
--
--  Purpose        : Implements an n input multiplexer
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

entity n_mux is
  generic (
    N_WIDTH : integer := 24
    );                      
  port (
    c  : in  STD_LOGIC_VECTOR(LOG2(N_WIDTH-1) downto 0);
    d  : in  STD_LOGIC_VECTOR(N_WIDTH-1 downto 0);
    q  : out STD_LOGIC
    );
end n_mux;

architecture rtl of n_mux is

begin

n_mux_p : process( c, d )
variable prod_v : std_logic;
variable sum_v  : std_logic;
begin
  --
  -- zero sum
  --
  sum_v := '0';
  --
  -- loop for all inputs
  --
  for i in 0 to N_WIDTH-1 loop
    --
    -- product initially set
    --
    prod_v  := '1';
    --
    -- loop for product of control terms
    --
    for j in 0 to log2(N_WIDTH-1) loop
      --
      -- products of control inputs
      --
      if (i mod (2**(j+1))) >= 2**j then
        prod_v := prod_v and c(j);
      else
        prod_v := prod_v and not c(j);
      end if;
    end loop;
    --
    -- sum of products terms including input
    --
    sum_v := sum_v or (prod_v and d(i)); 
  end loop;
  --
  -- assign sum of products to output
  --
  q <= sum_v;
end process;

end rtl;