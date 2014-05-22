--===========================================================================--
--                                                                           --
--              Synthesizable Simple Parallel Port                           --
--                                                                           --
--===========================================================================--
--
--  File name      : spp.vhd
--
--  Entity name    : spp
--
--  Purpose        : implements a Simple Parallel Port for System09
--
--  Dependencies   : ieee.Std_Logic_1164
--                   ieee.std_logic_unsigned
--
--  Uses           : None
--
--  Author         : John E. Kent      
--
--  Email          : dilbert57@opencores.org      
--
--  Web            : http://opencores.org/project,system09
--
--  Description    : Register Memory Map
--
--  Address                                MSB                         LSB
--                                    Bit:   7   6   5   4   3   2   1   0
--  Base+$00 (SPP Data port)    Write Pin:   9   8   7   6   5   4   3   2
--  Base+$01 (SPP Status port)  Read  Pin: ~11  10  12  13  15   -   -   -			
--  Base+$02 (SPP Control port) Write Pin:   -   -   -   - ~17  16 ~14  ~1
--  Base+$03 (EPP Address port) R/W
--  Base+$04 (EPP Data port)    R/W
-- 
--  ~ indicates a hardware inversion of the bit.
--  Parallel printer port pin assignment
-- 
--  Pin No (DB25) SPP Signal      EPP Signal    Direction Register  Bit Inverted
--  1             nStrobe         Write_n       Out       Control-0 Yes
--  2             Data0           Data0         In/Out    Data-0 	  No
--  3             Data1           Data1         In/Out    Data-1 	  No
--  4             Data2           Data2         In/Out    Data-2 	  No
--  5             Data3           Data3         In/Out    Data-3 	  No
--  6             Data4           Data4         In/Out    Data-4 	  No
--  7             Data5           Data5         In/Out    Data-5 	  No
--  8             Data6           Data6         In/Out    Data-6 	  No
--  9             Data7           Data7         In/Out    Data-7 	  No
--  10            nAck            Interrupt     In        Status-6  No
--  11            Busy            Wait          In        Status-7  Yes
--  12            Paper-Out       Spare         In        Status-5  No
--  13            Select          Spare         In        Status-4  No
--  14            Linefeed        Data_Strobe_n Out       Control-1 Yes
--  15            nError          Spare         In        Status-3  No
--  16            nInitialize     Reset         Out       Control-2 No
--  17            nSelect-Printer Addr_Strobe_n Out       Control-3 Yes
--  18-25         Ground          Ground        -         -         -
-- 
--  Copyright (C) 2008 - 2010 John Kent
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
--                    Revision History                                       --
--                                                                           --
--===========================================================================--
--
-- Version  Date       Author      Description
-- 0.1      2008-09-06 John Kent   Initial version generated from ioport.vhd
-- 0.2      2010-08-09 John Kent   Updated Header and added GPL
--
--===========================================================================

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_unsigned.all;

entity spp is
	port (	
	 clk       : in  std_logic;
    rst       : in  std_logic;
    cs        : in  std_logic;
    rw        : in  std_logic;
    addr      : in  std_logic_vector(2 downto 0);
    data_in   : in  std_logic_vector(7 downto 0);
	 data_out  : out std_logic_vector(7 downto 0);
	 spp_data  : out std_logic_vector(7 downto 0);
	 spp_stat  : in  std_logic_vector(7 downto 3);
	 spp_ctrl  : out std_logic_vector(3 downto 0);
	 hold      : out std_logic;
    irq       : out std_logic
	 );
end;

architecture rtl of spp is

signal spp_data_reg : std_logic_vector(7 downto 0);
signal spp_stat_reg : std_logic_vector(7 downto 3);
signal spp_ctrl_reg : std_logic_vector(3 downto 0);

begin


--------------------------------
--
-- read I/O port
--
--------------------------------

spp_read : process( addr,
                    spp_data_reg, spp_stat_reg, spp_ctrl_reg,
						  spp_stat )
begin
      spp_stat_reg(6 downto 3) <=     spp_stat(6 downto 3);
      spp_stat_reg(7)          <= not spp_stat(7);
      case addr is
	     when "000" =>
          data_out <= spp_data_reg;

		  when "001" =>
          data_out <= spp_stat_reg & "000";

	     when "010" =>
		    data_out <= "0000" & spp_ctrl_reg;

		  when others =>
		    data_out <= (others=> '0');
		end case;
      hold <= '0';
		irq  <= '0';
end process;

---------------------------------
--
-- Write I/O ports
--
---------------------------------

spp_write : process( clk, rst, addr, cs, rw, data_in,
                     spp_data_reg, spp_ctrl_reg )
begin
  if clk'event and clk = '0' then
    if rst = '1' then
      spp_data_reg <= "00000000";
      spp_ctrl_reg <= "0000";
    elsif cs = '1' and rw = '0' then
      case addr is
	     when "000" =>
		    spp_data_reg <= data_in;
		  when "010" =>
		    spp_ctrl_reg <= data_in(3 downto 0);
		  when others =>
		    null;
		end case;
	 end if;
  end if;
  spp_data    <=     spp_data_reg;
  spp_ctrl(0) <= not spp_ctrl_reg(0);
  spp_ctrl(1) <= not spp_ctrl_reg(1);
  spp_ctrl(2) <=     spp_ctrl_reg(2);
  spp_ctrl(3) <= not spp_ctrl_reg(3);
end process;

end rtl;
	
