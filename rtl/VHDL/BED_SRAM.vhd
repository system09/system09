--===========================================================================--
--                                                                           --
--                    BurchED SRAM Interface                              --
--                                                                           --
--===========================================================================--
--
--  File name      : BED_SRAM.vhd
--
--  Entity name    : BED_SRAM
--
--  Purpose        : Implements a 256KByte SRAM interface 
--                   for the BurchED B3 Spartan 2 board
--                   The clock should be twice the CPU clock.
--
--  Dependencies   : ieee.std_logic_1164
--                   ieee.numeric_std
--                   unisim.vcomponents
--
--  Author         : John E. Kent
--
--  Email          : dilbert57@opencores.org      
--
--  Web            : http://opencores.org/project,system09
--
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
-- Version Author        Date         Changes
--
-- 0.1     John Kent     2010-08-27   Separated SRAM interface 
--                                    from the top level design file
--

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;
  use ieee.std_logic_unsigned.all;
library unisim;
  use unisim.vcomponents.all;

-----------------------------------------------------------------------
--                    Entity for BED_SRAM                             --
-----------------------------------------------------------------------

entity BED_SRAM is
  port (
    --
    -- CPU Interface signals
    --
    clk      : in  std_logic;                     -- System Clock (twice the CPU clock)
    rst      : in  std_logic;                     -- Reset input (active high)
    cs       : in  std_logic;                     -- RAM Chip Select
    addr     : in  std_logic_vector(17 downto 0); -- RAM address bus
    rw       : in  std_logic;                     -- Read / Not Write
    data_in  : in  std_logic_vector(7 downto 0);  -- Data Bus In 
    data_out : out std_logic_vector(7 downto 0);  -- Data Bus Out
    --
    -- BED_SRAM Interface Signals
    --
    ram_csn     : out Std_Logic;
    ram_wrln    : out Std_Logic;
    ram_wrun    : out Std_Logic;
    ram_addr    : out Std_Logic_Vector(16 downto 0);
    ram_data    : inout Std_Logic_Vector(15 downto 0)

    );
end BED_SRAM;

--================== End of entity ==============================--

-------------------------------------------------------------------------------
-- Architecture for BED_SRAM
-------------------------------------------------------------------------------

architecture rtl of  BED_SRAM is

  signal ram_we      : std_logic; -- memory write strobe
  signal ram_wrl     : std_logic; -- memory write lower
  signal ram_wru     : std_logic; -- memory write upper

begin

--
-- BED-SRAM Control
-- Processes to read and write memory based on bus signals
--
BED_sram_process: process( clk, rst, cs, addr, rw, data_in,
                           ram_we, ram_wrl, ram_wru, ram_data )
begin

    ram_csn  <=  not( cs ) or  rst;
	 ram_addr(16 downto 0)  <= addr(17 downto 1);
    --
    -- clock write strobe 
    --
    if falling_edge( clk ) then
      if( rst = '1' ) then
        ram_we   <= '0';
	   elsif (cs = '1') and (ram_we = '0') then
	     ram_we   <= not rw;
      else
        ram_we   <= '0';
      end if;
    end if;

	 ram_wrl  <=      addr(0)  and ram_we;
    ram_wru  <= (not addr(0)) and ram_we;
	 ram_wrln <= not (ram_wrl);
	 ram_wrun <= not (ram_wru);

    if ram_wrl = '1' then
		ram_data(7 downto 0) <= data_in;
	 else
      ram_data(7 downto 0) <= "ZZZZZZZZ";
	 end if;

	 if ram_wru = '1' then
		ram_data(15 downto 8) <= data_in;
	 else
      ram_data(15 downto 8) <= "ZZZZZZZZ";
    end if;

	 if addr(0) = '0' then
      data_out <= ram_data(15 downto 8);
	 else
      data_out <= ram_data(7 downto 0);
    end if;
end process;

end rtl;