--===========================================================================--
--                                                                           --
--  Synthesizable 4K Sys09_bug ROM using Xilinx RAMB16_S9 Block RAM          --
--                                                                           --
--===========================================================================--
--
--  File name      : sys09s3s_b16.vhd
--
--  Entity name    : mon_rom
--
--  Purpose        : Implements a 4KByte Sys09_bug ROM 
--                   for the 200K gate Digilent spartan 3 starter board
--                   using two Xilinx RAMB16_S9 Block RAM
--
--  Dependencies   : ieee.std_logic_1164
--                   ieee.std_logic_arith
--                   unisim.vcomponents
--
--  Uses           : SYS09BUG_F000
--                   SYS09BUG_F800
--
--  Author         : John E. Kent
--
--  Email          : dilbert57@opencores.org      
--
--  Web            : http://opencores.org/project,system09
--
--  Description    : Block RAM instatiation
--
--  Copyright (C) 2006 - 2010 John Kent
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
-- Version Date        Author     Changes
--
-- 1.0     2006-11-21  John Kent  Initial version
-- 1.1     2006-12-22  John Kent  Made into 4K ROM/RAM.
-- 1.2     2010-06-17  John Kent  Added GPL and header
--                                Renamed data input and output signals
-- 
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
library unisim;
	use unisim.vcomponents.all;

entity mon_rom is
    Port (
       clk      : in  std_logic;
		 rst      : in  std_logic;
		 cs       : in  std_logic;
       addr     : in  std_logic_vector (11 downto 0);
		 rw       : in  std_logic;
       data_in  : in  std_logic_vector (7 downto 0);
       data_out : out std_logic_vector (7 downto 0)
    );
end mon_rom;

architecture rtl of mon_rom is

  signal we        : std_logic;
  signal cs0       : std_logic;
  signal cs1       : std_logic;
  signal dp0       : std_logic;
  signal dp1       : std_logic;
  signal data_out0 : std_logic_vector(7 downto 0);
  signal data_out1 : std_logic_vector(7 downto 0);

component SYS09BUG_F000
    Port (
       clk      : in  std_logic;
       rst      : in  std_logic;
       cs       : in  std_logic;
       addr     : in  std_logic_vector (10 downto 0);
       rw       : in  std_logic;
       data_in  : in  std_logic_vector (7 downto 0);
       data_out : out std_logic_vector (7 downto 0)
    );
end component;

component SYS09BUG_F800
    Port (
       clk      : in  std_logic;
       rst      : in  std_logic;
       cs       : in  std_logic;
       addr     : in  std_logic_vector (10 downto 0);
       rw       : in  std_logic;
       data_in  : in  std_logic_vector (7 downto 0);
       data_out : out std_logic_vector (7 downto 0)
    );
end component;

begin

   addr_f000 : SYS09BUG_F000 port map (
       clk      => clk,
       rst      => rst,
       cs       => cs0,
       addr     => addr(10 downto 0),
       rw       => rw,
       data_in  => data_in,
       data_out => data_out0
    );

   addr_f800 : SYS09BUG_F800 port map (
       clk      => clk,
       rst      => rst,
       cs       => cs1,
       addr     => addr(10 downto 0),
       rw       => rw,
       data_in  => data_in,
       data_out => data_out1
    );

my_mon : process ( rw, addr, cs, data_out0, data_out1 )
begin
	 we    <= not rw;
    cs0   <= '0';
    cs1   <= '0';
	 case addr(11) is
	 when '0' =>
	   cs0   <= cs;
		data_out <= data_out0;
    when '1' =>
		cs1   <= cs;
		data_out <= data_out1;
    when others =>
      null;
    end case;		
		
end process;

end architecture rtl;

