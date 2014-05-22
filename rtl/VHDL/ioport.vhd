--===========================================================================--
--                                                                           --
--  ioport.vhd - Synthesizable Dual Bidirectionsal I/O Port                  --
--                                                                           --
--===========================================================================--
--
--  File name      : ioport.vhd
--
--  Purpose        : Implements a dual 8 bit bidirectional I/O port
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
--  ioport.vhd is a dual bi-directional 8 bit I/O port written in VHDL.
-- 
--  Copyright (C) 2002 - 2010 John Kent
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
-- 0.1      John E. Kent  11 October 2002    Used a loop counter for 
--                                           data direction & read port signals
-- 0.2      John E. Kent  5 September 2003   Reduced to 2 x 8 bit ports
-- 1.0      John E. Kent  6 September 2003   Changed Clock Edge
-- 1.1      John E. Kent  25 Februrary 2007  Modified sensitivity lists
-- 1.2      John E. Kent  30 May 2010        Updated Header, added unisim library
--
--===========================================================================
--

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_unsigned.all;
--library unisim;
--  use unisim.vcomponents.all;

entity ioport is
	port (	
	 clk       : in  std_logic;
    rst       : in  std_logic;
    cs        : in  std_logic;
    rw        : in  std_logic;
    addr      : in  std_logic_vector(1 downto 0);
    data_in   : in  std_logic_vector(7 downto 0);
	 data_out  : out std_logic_vector(7 downto 0);
	 porta_io  : inout std_logic_vector(7 downto 0);
	 portb_io  : inout std_logic_vector(7 downto 0)
	 );
end;

architecture rtl of ioport is

signal porta_ddr : std_logic_vector(7 downto 0);
signal portb_ddr : std_logic_vector(7 downto 0);
signal porta_data : std_logic_vector(7 downto 0);
signal portb_data : std_logic_vector(7 downto 0);

begin


--------------------------------
--
-- read I/O port
--
--------------------------------

ioport_read : process( addr,
                     porta_ddr, portb_ddr,
							porta_data, portb_data,
						   porta_io, portb_io )
variable count : integer;
begin
      case addr is
	     when "00" =>
		    for count in 0 to 7 loop
            if porta_ddr(count) = '1' then
              data_out(count) <= porta_data(count);
            else
              data_out(count) <= porta_io(count);
            end if;
			 end loop;

		  when "01" =>
		    for count in 0 to 7 loop
            if portb_ddr(count) = '1' then
              data_out(count) <= portb_data(count);
            else
              data_out(count) <= portb_io(count);
            end if;
			 end loop;

	     when "10" =>
		    data_out <= porta_ddr;
		  when "11" =>
		    data_out <= portb_ddr;
		  when others =>
		    null;
		end case;
end process;

---------------------------------
--
-- Write I/O ports
--
---------------------------------

ioport_write : process( clk, rst, addr, cs, rw, data_in,
                        porta_data, portb_data,
								porta_ddr, portb_ddr )
begin
  if clk'event and clk = '0' then
    if rst = '1' then
      porta_data <= "00000000";
      portb_data <= "00000000";
      porta_ddr <= "00000000";
      portb_ddr <= "00000000";
    elsif cs = '1' and rw = '0' then
      case addr is
	     when "00" =>
		    porta_data <= data_in;
		    portb_data <= portb_data;
		    porta_ddr  <= porta_ddr;
		    portb_ddr  <= portb_ddr;
		  when "01" =>
		    porta_data <= porta_data;
		    portb_data <= data_in;
		    porta_ddr  <= porta_ddr;
		    portb_ddr  <= portb_ddr;
		  when "10" =>
		    porta_data <= porta_data;
		    portb_data <= portb_data;
		    porta_ddr  <= data_in;
		    portb_ddr  <= portb_ddr;
		  when "11" =>
		    porta_data <= porta_data;
		    portb_data <= portb_data;
		    porta_ddr  <= porta_ddr;
		    portb_ddr  <= data_in;
		  when others =>
		    null;
		end case;
	 else
		    porta_data <= porta_data;
		    portb_data <= portb_data;
		    porta_ddr  <= porta_ddr;
		    portb_ddr  <= portb_ddr;
	 end if;
  end if;
end process;

---------------------------------
--
-- direction control port a
--
---------------------------------
porta_direction : process ( porta_data, porta_ddr )
variable count : integer;
begin
  for count in 0 to 7 loop
    if porta_ddr(count) = '1' then
      porta_io(count) <= porta_data(count);
    else
      porta_io(count) <= 'Z';
    end if;
  end loop;
end process;
---------------------------------
--
-- direction control port b
--
---------------------------------
portb_direction : process ( portb_data, portb_ddr )
variable count : integer;
begin
  for count in 0 to 7 loop
    if portb_ddr(count) = '1' then
      portb_io(count) <= portb_data(count);
    else
      portb_io(count) <= 'Z';
    end if;
  end loop;
end process;
---------------------------------

end rtl;
	
