--===========================================================================--
--                                                                           --
--                 Synthesizable Enhance Parallel Port                       --
--                                                                           --
--===========================================================================--
--
--  File name      : epp.vhd
--
--  Entity name    : epp
--
--  Purpose        : Implements an Enhanced Parallel Port Interface
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
--  Description    : Register Memory Map
--
--  Address                              MSB                         LSB
--                                   Bit:    7   6   5   4   3   2   1   0
--  Base+$00 (SPP Data port)    Write Pin:   9   8   7   6   5   4   3   2
--  Base+$01 (SPP Status port)  Read  Pin: ~11  10  12  13  15 				
--  Base+$02 (SPP Control port) Write Pin:                 ~17  16 ~14  ~1
--  Base+$03 (EPP Address port) R/W
--  Base+$04 (EPP Data port)    R/W
-- 
--  ~ indicates a hardware inversion of the bit.
--
--  Parallel printer port pin assignment
-- 
--  Pin No (DB25)  SPP Signal      EPP Signal    Direction Register  Bit Inverted
--  1              nStrobe 	     Write_n       Out       Control-0 Yes
--  2              Data0           Data0         In/Out    Data-0    No
--  3              Data1           Data1         In/Out    Data-1    No
--  4              Data2           Data2         In/Out    Data-2    No
--  5              Data3           Data3         In/Out    Data-3    No
--  6              Data4           Data4         In/Out    Data-4    No
--  7              Data5           Data5         In/Out    Data-5    No
--  8              Data6           Data6         In/Out    Data-6    No
--  9              Data7           Data7         In/Out    Data-7    No
--  10             nAck            Interrupt     In        Status-6  No
--  11             Busy            Wait          In        Status-7  Yes
--  12             Paper-Out       Spare         In        Status-5  No
--  13             Select          Spare         In        Status-4  No
--  14             Linefeed        Data_Strobe_n Out       Control-1 Yes
--  15             nError          Spare         In        Status-3  No
--  16             nInitialize     Reset         Out       Control-2 No
--  17             nSelect-Printer Addr_Strobe_n Out       Control-3 Yes
--  18-25          Ground          Ground        -         -         -
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
--
--===========================================================================--
--                                                                           --
--                              Revision  History                            --
--                                                                           --
--===========================================================================--
--
-- Revision Author        Date         Description
--
-- 0.1      John E. Kent  2008-09-06   initial version generated from ioport.vhd
-- 0.2      John E. Kent  2010-08-09   updated header and added GPL
--

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_unsigned.all;
library unisim;
  use unisim.vcomponents.all;

entity epp is
	port (
   --
   -- CPU Interface Signals
   --	
	 clk       : in  std_logic;
    rst       : in  std_logic;
    cs        : in  std_logic;
    rw        : in  std_logic;
    addr      : in  std_logic_vector(2 downto 0);
    data_in   : in  std_logic_vector(7 downto 0);
	 data_out  : out std_logic_vector(7 downto 0);
	 irq       : out std_logic;
	 hold      : out std_logic;
    --
    -- Parallel Port Interface Signals
    --
	 epp_stat  : in  std_logic_vector(7 downto 3);
	 epp_ctrl  : out std_logic_vector(3 downto 0);
	 epp_data  : out std_logic_vector(7 downto 0)
	 );
end;

architecture rtl of epp is

constant CTRL_RW_BIT : integer := 0;
constant CTRL_DS_BIT : integer := 1;
constant CTRL_RS_BIT : integer := 2;
constant CTRL_AS_BIT : integer := 3;

constant STAT_IR_BIT : integer := 6;
constant STAT_WT_BIT : integer := 7;

signal epp_ctrl_reg : std_logic_vector(3 downto 0);

begin

--
-- Read / Write control
--
epp_control : process( rst, clk, cs, rw, addr, epp_stat, epp_crl_reg, data_in )
begin
  if rst = '1' then
    epp_ctrl_reg(CTRL_RW_BIT) <= '1';
    epp_ctrl_reg(CTRL_AS_BIT) <= '1';
    epp_ctrl_reg(CTRL_RS_BIT) <= '0';
    epp_ctrl_reg(CTRL_DS_BIT) <= '1';
    epp_data <= (others=>'Z');
  --
  -- clock controls on rising edge
  --
  elsif clk'event and clk = '1' then
    epp_ctrl_reg(CTRL_RS_BIT) <= '1';

    if cs = '1' then
      case addr is
		--
		-- address register
		--
      when "011" =>
        --
        -- set Data port direction
        --
	     if rw = '1' then
          epp_ctrl_reg(CTRL_RW_BIT) <= '1';
          epp_data <= (others=>'Z');
        else
          epp_ctrl_reg(CTRL_RW_BIT) <= '0';
			 epp_data <= data_in;
        end if;
        --
		  -- initiale an address strobe
		  --
        if epp_stat(STAT_WT_BIT) = '0' then
          epp_ctrl_reg(CTRL_AS_BIT) <= '0';
        elsif epp_stat(STAT_WT_BIT) = '1' then
          epp_ctrl_reg(CTRL_AS_BIT) <= '1';
        end if;

		--
		-- data register
		--
      when "100" =>
		  --
		  -- set data port direction
		  --
	     if rw = '1' then
          epp_ctrl_reg(CTRL_RW_BIT) <= '1';
          epp_data <= (others=>'Z');
        else
          epp_ctrl_reg(CTRL_RW_BIT) <= '0';
			 epp_data <= data_in;
        end if;
		  --
		  -- initiate a data strobe
		  --
        if epp_stat(STAT_WT_BIT) = '0' then
          epp_ctrl_reg(CTRL_DS_BIT) <= '0';
        elsif epp_stat(STAT_WT_BIT) = '1' then
          epp_ctrl_reg(CTRL_DS_BIT) <= '1';
        end if;

      when others =>
        epp_ctrl_reg(CTRL_RW_BIT) <= '1';
        epp_ctrl_reg(CTRL_AS_BIT) <= '1';
        epp_ctrl_reg(CTRL_DS_BIT) <= '1';
        epp_data <= (others=>'Z');
		  null;

      end case; -- addr
    end if; -- cs
  end if; -- clk / reset
  irq      <= epp_stat(STAT_IR_BIT);
  hold     <= not( epp_ctrl_reg(CTRL_DS_BIT) ) or not( epp_ctrl_reg(CTRL_AS_BIT) );
  epp_ctrl <= epp_ctrl_reg;
  data_out <= epp_data;

end process;

end rtl;
	
