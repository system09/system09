--===========================================================================--
--                                                                           --
--  SevenSegment.vhd - Synthesizable Multiplex Seven Segment LED Driver      --
--                                                                           --
--===========================================================================--
--
--  File name      : SevenSegment.vhd
--
--  Entity name    : SevenSegment
--
--  Purpose        : 4 x 8 bit lathes to display 7 segments
--                   Multiplexes segment registers across 4 displays.
--                   For use on the Digilent Spartan 3 Starter Board
--                  
--  Dependencies   : ieee.std_logic_1164
--                   ieee.std_logic_unsigned
--                   unisim.vcomponents
--
--  Author         : John E. Kent
--
--  Email          : dilbert57@opencores.org      
--
--  Web            : http://opencores.org/project,system09
--
--  SevenSegment.vhd is a multiplexed seven segment LED display driver written in VHDL
-- 
--  Copyright (C) 2004 - 2010 John Kent
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
-- 0.1      John Kent     19 Oct 2004        Initial version
-- 0.2      John Kent     21 Nov 2006        Inverted segment registers 
--                                           so '0' in segment registers 
--                                           switches segment OFF
-- 0.3      John Kent     31 May 2010        Updated Header and GPL.
--

library ieee;
   use ieee.std_logic_1164.all;
   use ieee.std_logic_unsigned.all;

library unisim;
   use unisim.vcomponents.all;

entity seven_segment is
	port (	
	 clk       : in  std_logic;
    rst       : in  std_logic;
    cs        : in  std_logic;
    rw        : in  std_logic;
    addr      : in  std_logic_vector(1 downto 0);
    data_in   : in  std_logic_vector(7 downto 0);
	 data_out  : out std_logic_vector(7 downto 0);
	 segments  : out std_logic_vector(7 downto 0);
	 digits	  : out std_logic_vector(3 downto 0)
	 );
end;

architecture rtl of seven_segment is
signal seg_reg0     : std_logic_vector(7 downto 0);
signal seg_reg1     : std_logic_vector(7 downto 0);
signal seg_reg2     : std_logic_vector(7 downto 0);
signal seg_reg3     : std_logic_vector(7 downto 0);

signal ClockDivider : std_logic_vector(13 downto 0);
signal WhichDigit   : std_logic_vector(1 downto 0);

begin

---------------------------------
--
-- Write Segment registers
--
---------------------------------

seg_write : process( clk, rst, addr, cs, rw, data_in )
begin
  if clk'event and clk = '0' then
    if rst = '1' then
      seg_reg0 <= "00000000";
      seg_reg1 <= "00000000";
      seg_reg2 <= "00000000";
      seg_reg3 <= "00000000";
    else
	   if cs = '1' and rw = '0' then
        case addr is
	     when "00" =>
		    seg_reg0 <= data_in;
	     when "01" =>
		    seg_reg1 <= data_in;
	     when "10" =>
		    seg_reg2 <= data_in;
	     when "11" =>
		    seg_reg3 <= data_in;
        when others =>
		    null;
		  end case;
	   end if;
	 end if;
  end if;
end process;

---------------------------------
--
-- Read Segment registers
--
---------------------------------

seg_read : process(  addr,
                     seg_reg0, seg_reg1, seg_reg2, seg_reg3 )
begin
      case addr is
	     when "00" =>
		    data_out <= seg_reg0;
	     when "01" =>
		    data_out <= seg_reg1;
	     when "10" =>
		    data_out <= seg_reg2;
	     when "11" =>
		    data_out <= seg_reg3;
        when others =>
		    null;
		end case;
end process;

---------------------------------
--
-- Output Segment registers
--
---------------------------------

seg_out : process( rst, Clk)
begin
		if rst = '1' then
			ClockDivider <= (others => '0');
			WhichDigit   <= "00";
			Segments     <= "00000000";
			Digits	    <= "1111";
		elsif Clk'Event and Clk = '0' then
			if ClockDivider = "11000011010011" then
				ClockDivider <= (others => '0');
				case WhichDigit is	-- note that everything is pipelined
					when "00" => 
						Digits   <= "1110"; 
						Segments <= not( seg_reg0 );
					when "01" => 
						Digits   <= "1101"; 
						Segments <= not( seg_reg1 );
					when "10" => 
						Digits   <= "1011"; 
						Segments <= not( seg_reg2 );
					when "11" => 
						Digits   <= "0111"; 
						Segments <= not( seg_reg3 );
					when others => 
					   null;
				end case;
				WhichDigit <= WhichDigit + 1;
			else
				ClockDivider <= ClockDivider + 1;
			end if;
		end if;
end process;

end rtl;
	
