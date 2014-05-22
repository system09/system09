--===========================================================================--
--                                                                           --
--  log2.vhd - log base 2 function package                                   --
--                                                                           --
--===========================================================================--
--
--  File name      : log2.vhd
--
--  Purpose        : log base 2 functions package
--                  
--  Dependencies   : ieee.std_logic_1164
--                   ieee.std_logic_arith
--                   ieee.std_logic_unsigned
--                   unisim.vcomponents
--
--  Author         : Original posted by Ray Andraka, modified by editor Edwin Naroska
--                   Header added "John E. Kent <dilbert57@opencores.org>"
--
--  Web            : http://www.vhdl.org/comp.lang.vhdl/FAQ1.html
--                   The web site states that it is a monthly posting to 
--                   comp.lang.vhdl containing general information.
--                   Section 4.10 "Frequently Requested" Models/Packages
--                   I could find no Copyright notice on the web site
-- 
--  Web Editor     : "Edwin Naroska <edwin@ds.e-technik.uni-dortmund.de>"
--
--  log2.vhd is a log base 2 function package
-- 
--  Copyright (C) 2010 Ray Andraka and Edwin Naroska
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
--
library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_unsigned.all;
  use ieee.std_logic_arith.all;

library unisim;
  use unisim.vcomponents.all;

function log2 (x : positive) return natural is
variable temp, log: natural;
begin
  temp := x / 2;
  log := 0;
  while (temp /= 0) loop
    temp := temp/2;
    log := log + 1;
  end loop;
  return log;
end function log2;

