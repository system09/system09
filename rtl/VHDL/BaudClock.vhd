--===========================================================================--
--                                                                           --
--  BaudClock.vhd - Synthesizable Baud Rate Clock Divider                    --
--                                                                           --
--===========================================================================--
--
--  File name      : BaudClock.vhd
--
--  Purpose        : Implements a baud rate clock divider for a 6850 compatible
--                   Asynchronous Communications Interface Adapter 
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
--  BaudClock.vhd is baud rate clock divider for a 6850 compatible ACIA core.
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
-- 0.1      John E. Kent  unknown          Initial version
-- 1.0      John E. Kent  30th May 2010    Added GPL Header
--
library ieee;
   use ieee.std_logic_1164.all;
   use ieee.std_logic_arith.all;
   use ieee.std_logic_unsigned.all;
   use ieee.numeric_std.all;
library unisim;
	use unisim.vcomponents.all;

entity ACIA_Clock is
  generic (
     SYS_Clock_Frequency  : integer;
	  BAUD_Clock_Frequency : integer
  );   
  port(
    clk      : in  Std_Logic;  -- System Clock input
	 ACIA_Clk : out Std_Logic   -- ACIA Clock output
  );
end ACIA_Clock;

-------------------------------------------------------------------------------
-- Architecture for ACIA_Clock
-------------------------------------------------------------------------------
architecture rtl of ACIA_Clock is

constant full_cycle : integer :=  (SYS_Clock_Frequency / BAUD_Clock_Frequency) - 1;
constant half_cycle : integer :=  (full_cycle / 2) - 1;
--
-- Baud Rate Clock Divider
--
-- 25MHz / 27  = 926,000 KHz = 57,870Bd * 16
-- 50MHz / 54  = 926,000 KHz = 57,870Bd * 16
--
my_baud_clock: process( SysClk )
begin
    if(SysClk'event and SysClk = '0') then
		if( BaudCount = 53 )	then
			baudclk <= '0';
		   BaudCount <= "000000";
		else
		   if( BaudCount = 26 )	then
				baudclk <='1';
			else
				baudclk <=baudclk;
			end if;
		   BaudCount <= BaudCount + 1;
		end if;			 
    end if;
end process;
