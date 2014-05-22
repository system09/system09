--===========================================================================--
--                                                                           --
--  xula_ioport.vhd - Synthesizable Dual Bidirectionsal I/O Port             --
--                                                                           --
--===========================================================================--
--
--  File name      : xula_ioport.vhd
--
--  Purpose        : Implements a dual 8 bit bidirectional I/O port
--                   modified for the XuLA implementation of System09
--                   Port A supports a full 8 bit bidirectional port
--                   Port B supports a 5 bit bidirectional port with 3 inputs 
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
--  xula_ioport.vhd is a dual bi-directional 8 bit I/O port written in VHDL.
--
--  address  function
--  =======  ========
--  base+0   port a data register
--           bits 0 - 7 = i/o
--  base+1   port b data register
--           bits 0 - 4 = i/o
--           bits 5 - 7 - inputs
--  base+2   port a direction register 
--           0 => port a bit = input
--           1 => port a bit = output
--  base+3   port b direction
--           For bits 0 to 4:
--           0 => port b bit = input
--           1 => port b bit = output
--           For bits 5 to 7:
--           0 => port b bit = interrupt disable
--           1 => port b bit = interrupt enable
--           interrupt inputs on port b bits 5 to 7 are active high
--
--  Copyright (C) 2002 - 2011 John Kent
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
-- 2.0      John E. Kent  30 April 2011      modified for XuLA System09 I/O
--===========================================================================
--

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_unsigned.all;
--library unisim;
--  use unisim.vcomponents.all;

entity xula_ioport is
  port (	
    clk       : in    std_logic;
    rst       : in    std_logic;
    cs        : in    std_logic;
    rw        : in    std_logic;
    addr      : in    std_logic_vector(1 downto 0);
    data_in   : in    std_logic_vector(7 downto 0);
    data_out  : out   std_logic_vector(7 downto 0);
    porta_io  : inout std_logic_vector(7 downto 0);
    portb_io  : inout std_logic_vector(4 downto 0);
    portc_in  : in    std_logic_vector(7 downto 5);
    irq       : out   std_logic
  );
end;

architecture rtl of xula_ioport is

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
		  if count < 5 then
          data_out(count) <= portb_io(count);
		  else
          data_out(count) <= portc_in(count);
		  end if;
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
      porta_data <= (others=>'0');
      portb_data <= (others=>'0');
      porta_ddr  <= (others=>'0');
      portb_ddr  <= (others=>'0');
    else
      if cs = '1' and rw = '0' then
        case addr is
        when "00" =>
           porta_data <= data_in;
        when "01" =>
           portb_data <= data_in;
        when "10" =>
           porta_ddr  <= data_in;
        when "11" =>
           portb_ddr  <= data_in;
        when others =>
           null;
        end case;
      end if;
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
portb_direction : process ( portb_data, portb_ddr, portb_io )
variable count : integer;
variable irq_temp : std_logic;
begin
  --
  -- For bit 0 to 4 DDR determines the direction of the port
  --
  for count in 0 to 4 loop
    if portb_ddr(count) = '1' then
      portb_io(count) <= portb_data(count);
    else
      portb_io(count) <= 'Z';
    end if;
  end loop;

  --
  -- For bit 5 to 7 DDR is an interrupt enable
  --
  irq_temp := '0';
  for count in 5 to 7 loop
    irq_temp := (portc_in(count) AND portb_ddr(count)) OR irq_temp;
  end loop;
  irq <= irq_temp;
end process;
---------------------------------

end rtl;
	
