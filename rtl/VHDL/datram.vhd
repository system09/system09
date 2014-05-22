--===========================================================================--
--                                                                           --
--         Synthesizable SWTPc 6809 Dynamic Address Translation Table        --
--                                                                           --
--===========================================================================--
--
--  File name      : datram.vhd
--
--  Entity name    : dat_ram
--
--  Purpose        : Implements a Dynamic Address Translation RAM module
--                   as found in the SWTPc MP-09 CPU card.
--                   Maps the high order 4 address bits to 8 address lines
--                   extending the memory addressing range from 64K to 1MByte
--                   Memory segments are mapped on 4 KByte boundaries
--                   The DAT registers are mapped at the the top of memory 
--                   ($FFF0 - $FFFF) and are write only so can map behind ROM.
--                   Since the DAT is not supported by SWTBUG for the 6800,
--                   the resgisters reset state map the bottom 64K of RAM. 
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
--  Description    :
--
--  DAT is initializedas follows:
--
--  DAT    Dat           Logical Physical
--  Reg    Val           Addr    Addr
--	 fff0 - 0f - page 0 - $0xxx = $00xxx (RAM)
--	 fff1 - 0e - page 1 - $1xxx = $01xxx (RAM) 
--	 fff2 - 0d - page 2 - $2xxx = $02xxx (RAM)
--	 fff3 - 0c - page 3 - $3xxx = $03xxx (RAM)
--	 fff4 - 0b - page 4 - $4xxx = $04xxx (RAM)
--	 fff5 - 0a - page 5 - $5xxx = $05xxx (RAM)
--	 fff6 - 09 - page 6 - $6xxx = $06xxx (RAM)
--	 fff7 - 08 - page 7 - $7xxx = $07xxx (RAM)
--	 fff8 - 07 - page 8 - $8xxx = $08xxx (RAM)
--	 fff9 - 06 - page 9 - $9xxx = $09xxx (RAM)
--	 fffa - 05 - page A - $axxx = $0axxx (RAM)
--	 fffb - 04 - page B - $bxxx = $0bxxx (RAM)
--	 fffc - 03 - page C - $cxxx = $0cxxx (RAM)
--	 fffd - 02 - page D - $dxxx = $0dxxx (RAM)
--	 fffe - f1 - page E - $exxx = $fexxx (I/O)
--	 ffff - f0 - page F - $fxxx = $ffxxx (ROM/DMFA2)
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
-- Version Date        Author     Changes
--
-- 0.1     2002-11-10  John Kent  Initial version
--
-- 0.2     2006-11-21  John Kent  Inverted bottom 4 bits of dat_addr
--                                so that it is compatible with SWTPc MP-09 card.
--
-- 0.3     2007-02-25  John Kent  Modify the sensitivity lists
--
-- 0.4     2010-06-17  John Kent  Update header and added GPL
--
-- 0.5     2010-12-10  John Kent  Correction of pages in header documentation
-- 

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_unsigned.all;
--library unisim;
--  use unisim.vcomponents.all;

entity dat_ram is
	port (	
	 clk       : in  std_logic;
    rst       : in  std_logic;
    cs        : in  std_logic;
    addr_hi   : in  std_logic_vector(3 downto 0);
    addr_lo   : in  std_logic_vector(3 downto 0);
    rw        : in  std_logic;
    data_in   : in  std_logic_vector(7 downto 0);
	 data_out  : out std_logic_vector(7 downto 0));
end dat_ram;

architecture rtl of dat_ram is
signal dat_reg0  : std_logic_vector(7 downto 0);
signal dat_reg1  : std_logic_vector(7 downto 0);
signal dat_reg2  : std_logic_vector(7 downto 0);
signal dat_reg3  : std_logic_vector(7 downto 0);
signal dat_reg4  : std_logic_vector(7 downto 0);
signal dat_reg5  : std_logic_vector(7 downto 0);
signal dat_reg6  : std_logic_vector(7 downto 0);
signal dat_reg7  : std_logic_vector(7 downto 0);
signal dat_reg8  : std_logic_vector(7 downto 0);
signal dat_reg9  : std_logic_vector(7 downto 0);
signal dat_reg10 : std_logic_vector(7 downto 0);
signal dat_reg11 : std_logic_vector(7 downto 0);
signal dat_reg12 : std_logic_vector(7 downto 0);
signal dat_reg13 : std_logic_vector(7 downto 0);
signal dat_reg14 : std_logic_vector(7 downto 0);
signal dat_reg15 : std_logic_vector(7 downto 0);

begin

---------------------------------
--
-- Write DAT RAM
--
---------------------------------

--dat_write : process( clk, rst, addr_lo, cs, rw, data_in )
dat_write : process( clk )
begin
  if clk'event and clk = '0' then
    if rst = '1' then
      dat_reg0  <= "00001111";
      dat_reg1  <= "00001110";
      dat_reg2  <= "00001101";
      dat_reg3  <= "00001100";
      dat_reg4  <= "00001011";
      dat_reg5  <= "00001010";
      dat_reg6  <= "00001001";
      dat_reg7  <= "00001000";
      dat_reg8  <= "00000111";
      dat_reg9  <= "00000110";
      dat_reg10 <= "00000101";
      dat_reg11 <= "00000100";
      dat_reg12 <= "00000011";
      dat_reg13 <= "00000010";
      dat_reg14 <= "11110001";
      dat_reg15 <= "11110000";
    else
	   if cs = '1' and rw = '0' then
        case addr_lo is
	     when "0000" =>
		    dat_reg0 <= data_in;
	     when "0001" =>
		    dat_reg1 <= data_in;
	     when "0010" =>
		    dat_reg2 <= data_in;
	     when "0011" =>
		    dat_reg3 <= data_in;
	     when "0100" =>
		    dat_reg4 <= data_in;
	     when "0101" =>
		    dat_reg5 <= data_in;
	     when "0110" =>
		    dat_reg6 <= data_in;
	     when "0111" =>
		    dat_reg7 <= data_in;
	     when "1000" =>
		    dat_reg8 <= data_in;
	     when "1001" =>
		    dat_reg9 <= data_in;
	     when "1010" =>
		    dat_reg10 <= data_in;
	     when "1011" =>
		    dat_reg11 <= data_in;
	     when "1100" =>
		    dat_reg12 <= data_in;
	     when "1101" =>
		    dat_reg13 <= data_in;
	     when "1110" =>
		    dat_reg14 <= data_in;
	     when "1111" =>
		    dat_reg15 <= data_in;
        when others =>
		    null;
		  end case;
	   end if;
	 end if;
  end if;
end process;

dat_read : process(  addr_hi,
                     dat_reg0, dat_reg1, dat_reg2, dat_reg3,
                     dat_reg4, dat_reg5, dat_reg6, dat_reg7,
                     dat_reg8, dat_reg9, dat_reg10, dat_reg11,
                     dat_reg12, dat_reg13, dat_reg14, dat_reg15 )
variable phy_addr : std_logic_vector( 7 downto 0 );
begin
      case addr_hi is
	     when "0000" =>
		    phy_addr := dat_reg0;
	     when "0001" =>
		    phy_addr := dat_reg1;
	     when "0010" =>
		    phy_addr := dat_reg2;
	     when "0011" =>
		    phy_addr := dat_reg3;
	     when "0100" =>
		    phy_addr := dat_reg4;
	     when "0101" =>
		    phy_addr := dat_reg5;
	     when "0110" =>
		    phy_addr := dat_reg6;
	     when "0111" =>
		    phy_addr := dat_reg7;
	     when "1000" =>
		    phy_addr := dat_reg8;
	     when "1001" =>
		    phy_addr := dat_reg9;
	     when "1010" =>
		    phy_addr := dat_reg10;
	     when "1011" =>
		    phy_addr := dat_reg11;
	     when "1100" =>
		    phy_addr := dat_reg12;
	     when "1101" =>
		    phy_addr := dat_reg13;
	     when "1110" =>
		    phy_addr := dat_reg14;
	     when "1111" =>
		    phy_addr := dat_reg15;
        when others =>
		    null;
		end case;
      data_out( 7 downto 4 ) <= phy_addr( 7 downto 4 );
      data_out( 3 downto 0 ) <= not( phy_addr( 3 downto 0 ) );
		
end process;

end rtl;
	
