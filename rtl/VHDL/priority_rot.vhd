--===========================================================================--
--                                                                           --
--  priority_rot.vhd - Synthesizable Rotating Priority Encoder               --
--                                                                           --
--===========================================================================--
--
--  File name      : priority_rot.vhd
--
--  Purpose        : Implements a rotating priority encoder
--                  
--  Dependencies   : ieee.std_logic_1164
--                   ieee.std_logic_unsigned
--                   ieee.std_logic_arith
--                   unisim.vcomponents
--
--  Author         : John E. Kent
--
--  Email          : dilbert57@opencores.org      
--
--  Web            : http://opencores.org/project,system09
--
--  Priority_rot.vhd is a rotating priority encoder written in VHDL.
-- 
--  Copyright (C) 2010 John Kent
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
-- Version  Author        Date               Description
-- 0.1      John Kent     30th May 2010      Initial version
--
library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_unsigned.all;
  use ieee.std_logic_arith.all;

library unisim;
  use unisim.vcomponents.all;

entity priority_rot is
   generic (
      WIDTH = 8
   )
	port (
		clk      : in  std_logic;
		rst      : in  std_logic;
	   input    : in  std_logic_vector(WIDTH-1 downto 0);
	   output   : out std_logic_vector(log2(WIDTH)-1 downto 0);
      valid    : out std_logic
   )
		);
end priority_rot;

architecture rtl of priority_rot is

constant OUT_WIDTH : integer := log2(WIDTH);

begin

  for i in 1 to WIDTH loop
    j := log2( i );
    if( input(i-1) = '1' ) then

  end loop;
end architecture;

