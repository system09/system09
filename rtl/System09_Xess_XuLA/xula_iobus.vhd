--===========================================================================--
--                                                                           --
--  xula_iobus.vhd - Synthesizable Dual Bidirectionsal I/O Port             --
--                                                                           --
--===========================================================================--
--
--  File name      : xula_iobusi.vhd
--
--  Purpose        : Implements a dual 8 bit bidirectional I/O bus
--                   for the XuLA implementation of System09
--                   Allows the XuLA System09 port to talk to
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
-- Description
--
--  system09 clk  /-\_/-\_/-\_/-\_/-\_/-\_/-\_/-\_/-\_/-\_/-\_/-\_/-\_/-\
--
--  system09 cs   ---\_______________________________/--------------------
--
--  bus release   ______________________________/-------------------------
--
--  system09 hold ____/---------------------------\_______________________
--
--  bus cs_n      ----\______________________________/--------------------
--
--  bus rd_n      ---------------------\_____________/--------------------
--
--  bus ds        _____________________/-------------\____________________
--
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
-- 0.1      John E. Kent  1 May 2011         Initial version 
--===========================================================================
--

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_unsigned.all;
--library unisim;
--  use unisim.vcomponents.all;

entity xula_iobus is
  port (	
    clk        : in    std_logic;
    rst        : in    std_logic;
    cs         : in    std_logic;
    rw         : in    std_logic;
    addr       : in    std_logic_vector(4 downto 0);
    data_in    : in    std_logic_vector(7 downto 0);
    data_out   : out   std_logic_vector(7 downto 0);
	 hold       : out   std_logic;
    irq        : out   std_logic;
	 bus_cs     : out   std_logic;
    bus_ds_rdn : out   std_logic;
    bus_rw_wrn : out   std_logic;
    bus_addr   : out   std_logic_vector(3 downto 0);
    bus_data   : inout std_logic_vector(7 downto 0);
	 bus_irq    : in    std_logic;
  );
end;

architecture rtl of xula_iobus is

begin


--------------------------------
--
-- read I/O bus control registers
--
--------------------------------

iobus_read : process( addr, bus_data, bus_reg )
begin
  if addr(4) = '0' then
    data_out <= bus_data;
  else
    data_out <= bus_reg;
  end if;
end process;

---------------------------------
--
-- Write bus data / register
--
---------------------------------

iobus_write : process( clk, rst, addr, cs, rw, data_in )
begin
  if clk'event and clk = '0' then
    if rst = '1' then
      bus_data <= (others=>'Z');
      bus_reg <= (others=>'0');
    else
      if cs = '1' then
		  if addr(4) = '0' then
		    if bus_release = '0' and bus_hold = '0' then
			   bus_hold <= '1';
            if rw = '0' then
              bus_data <= data_in;
            else
              bus_data <= (other=>'Z');
            end if;
          else
			   if bus_release = '1' and bus_hold = '1' then
				  bus_hold <= '0';
				end if;
			 end if;
        else
		    if rw = '0' then
             bus_reg <= data_in;
          end if;
        end if;
      end if;
    end if;
  end if;
end process;


---------------------------------
--
-- Write bus register
--
---------------------------------

iobus_reg_write : process( clk, rst, addr, cs, rw, data_in )
begin
  if clk'event and clk = '0' then
    if rst = '1' then
    else
      if cs = '1' and addr(4) = '1' then
		  if rw = '0' then
           bus_reg <= data_in;
        else
           bus_data <= (other=>'Z');
        end if;
      end if;
    end if;
  end if;
end process;

---------------------------------
--
-- direction control port a
--
---------------------------------
iobus_ctrl : process ( clk, rst,  )
begin
  if clk'event and clk = '0' then
    if rst = '1' then
      bus_data <= (others=>'0');
      bus_reg  <= (others=>'0');
    else
end process;
---------------------------------
--
-- hold CPU for one external bus cycle
--
---------------------------------
iobus_hold : process ( portb_data, portb_ddr, portb_io )
begin
end process;
---------------------------------

----------------------------------------------------------
--
-- Generate a bus clock with half cycle period
-- equal to the cpu clock cycle count
-- in the bus register bits 6 downto 0.
--
-- Generate a time out signal for one cpu clock cycle
-- when the bus timer reaches zero
--
----------------------------------------------------------
iobus_clk : process ( clk, rst )
begin
  if clk'event and clk = '0' then
    if rst = '1' then
	   bus_timer <= (others=>'0');
		bus_clk   <= '0';
		bus_to    <= '0';
    else
	   if bus_timer = "0000000" then
		  bus_timer <= bus_reg(6 downto 0);
		  bus_clk   <= not bus_clk;
		  bus_to    <= '1';
		else
		  bus_timer <= bus_timer - "0000001";
		  bus_to    <= '0';
		end if;
	 end if;
  end if;
  
end process;

----------------------------------------------------------
--
--  Bus Request
--
--  Synchronize I/O bus cycle request to the start of the bus clock
--
--  The start of the io bus cycle is defined as there being
--  a bus timer timeout and the bus clock is low.
--
--  If there is a bus request wait for the start of the
--  io bus cycle before acknowledging
--
--  If the bus request is removed, wait for the start of the
--  io bus cycle before removing the acknowledge.
--
----------------------------------------------------------
iobus_req : process ( clk, rst, bus_req, bus_ack, bus_clk, bus_to )
begin
  if clk'event and clk = '0' then
    if rst = '1' then
	   bus_ack <= '0';
    else
	   if bus_req = '1' and bus_ack = '0' then
		  if bus_clk = '0' and bus_to = '1' then
		    bus_ack <= '1';
		  end if;
      elsif bus_req = '0' and bus_ack = '1' then
		  if bus_clk = '0' and bus_to = '1' then
		    bus_ack <= '0';
		  end if;
		end if;
	 end if;
  end if;
  
end process;

end rtl;
	
