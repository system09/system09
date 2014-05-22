--===========================================================================--
--                                                                           --
--  priority_enc.vhd - Synthesizable Priority Encoder                        --
--                                                                           --
--===========================================================================--
--
--  File name      : priority_enc.vhd
--
--  Purpose        : Implements a priority encoder
--                   with generic input size
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
--  Priority_enc.vhd is a priority encoder written in VHDL.
--
--  Q(0) = D7 + D6*.(D5 + D4*.(D3 + D2*.(D1 + D0*.0)))
--  Q(1) = D7 + D6 + D5*. D4*.(D3 + D2)+ D1*. D0*.0
--  Q(2) = D7 + D6 + D5 + D4 + D3*. D2*. D1*. D0*.0
--
--  http://www.electronics-tutorials.ws/combination/comb_4.html
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
  use work.bit_funcs.all;

library unisim;
  use unisim.vcomponents.all;

entity priority_enc is
   generic (
      D_WIDTH : integer := 128
   );
	port (
	   d    : in  std_logic_vector(D_WIDTH-1 downto 0);
	   q    : out std_logic_vector(log2(D_WIDTH)-1 downto 0);
      v    : out std_logic
   );
end priority_enc;

architecture rtl of priority_enc is

constant Q_WIDTH : integer := log2(D_WIDTH);

begin

pri_enc : process( d )
variable qvar : std_logic_vector(Q_WIDTH-1 downto 0);
variable vvar : std_logic := '0';
variable i    : integer := 0;
variable j    : integer := 0;
begin
  for i in 0 to Q_WIDTH-1 loop
    qvar(i) := '0';
    for j in 0 to D_WIDTH-1 loop
      if (j mod (2**(i+1))) >= 2**i then
        qvar(i) := qvar(i) or d(j);
      else
        qvar(i) := qvar(i) and not(d(j));
      end if;
    end loop;
    q(i) <= qvar(i);
  end loop;
  vvar := '0';
  for j in 0 to D_WIDTH-1 loop
    vvar := vvar or d(j);
  end loop;
  v <= vvar;
end process;

end architecture;

