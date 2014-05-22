--===========================================================================--
--                                                                           --
--  Synthesizable Memory Management Unit Based on SWTPc MP-09 Dyn Addr Trans --
--                                                                           --
--===========================================================================--
--
--  File name      : mmu.vhd
--
--  Entity name    : mmu
--
--  Purpose        : Implements a MMU which is an extension of the Dynamic 
--                   Address Translation RAM module found in the SWTPc MP-09 CPU card.
--                   Maps the high order 4 address bits of the CPU to 16 address lines
--                   extending the memory addressing range from 64K to 256MByte
--                   Memory segments are mapped on 4 KByte boundaries.
--                   In order to maintain compatibility with the original SWTPc 
--                   MP-09 DAT the high order byte of the MMU address table is mapped
--                   in the 16 bytes below the the low bytes and the least significant
--                   nybble of the low address of the MMU is inverted. 
--                   The MMU registers are mapped at the the top of memory 
--                   ($FFE0 - $FFFF) and are write only so can map behind ROM.                   
--                   Since the MMU is not supported by SWTBUG for the 6800,
--                   the resgisters reset state map the bottom 64K of RAM.
--                   This also means the the SWTBug DAT initialization routine 
--                   will also be compatible with the extended MMU. 
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
--  MMU is initializedas follows:
--                        
--  MMU    MMU             Logical Physical
--  Reg    Val             Addr    Addr
--	 ffe0 - 00 - page 0 hi
--	 ffe1 - 00 - page 1 hi
--	 ffe2 - 00 - page 2 hi
--	 ffe3 - 00 - page 3 hi
--	 ffe4 - 00 - page 4 hi
--	 ffe5 - 00 - page 5 hi
--	 ffe6 - 00 - page 6 hi
--	 ffe7 - 00 - page 7 hi
--	 ffe8 - 00 - page 8 hi
--	 ffe9 - 00 - page 9 hi
--	 ffea - 00 - page A hi
--	 ffeb - 00 - page B hi
--	 ffec - 00 - page C hi
--	 ffed - 00 - page D hi
--	 ffee - FF - page E hi
--	 ffef - FF - page F hi
--	 fff0 - 0F - page 0 lo - $0xxx = $0000xxx (RAM)
--	 fff1 - 0E - page 1 lo - $1xxx = $0001xxx (RAM) 
--	 fff2 - 0D - page 2 lo - $2xxx = $0002xxx (RAM) 
--	 fff3 - 0C - page 3 lo - $3xxx = $0003xxx (RAM)
--	 fff4 - 0B - page 4 lo - $4xxx = $0004xxx (RAM)
--	 fff5 - 0A - page 5 lo - $5xxx = $0005xxx (RAM)
--	 fff6 - 09 - page 6 lo - $6xxx = $0006xxx (RAM)
--	 fff7 - 08 - page 7 lo - $7xxx = $0007xxx (RAM)
--	 ffd8 - 07 - page 8 lo - $8xxx = $0008xxx (RAM)
--	 fff9 - 06 - page 9 lo - $9xxx = $0009xxx (RAM)
--	 fffa - 05 - page A lo - $Axxx = $000Axxx (RAM)
--	 fffb - 04 - page B lo - $Bxxx = $000Bxxx (RAM)
--	 fffc - 03 - page C lo - $Cxxx = $000Cxxx (RAM)
--	 fffd - 02 - page D lo - $Dxxx = $000Dxxx (RAM)
--	 fffe - F1 - page E lo - $Exxx = $FFFExxx (IO)
--	 ffff - F0 - page F lo - $Fxxx = $FFFFxxx (ROM/DMAF2)
--
--  Copyright (C) 2010 John Kent
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
-- 0.1     2002-11-10  John Kent  DAT Initial version
--
-- 0.2     2006-11-21  John Kent  DAT Inverted bottom 4 bits of mmu_addr
--                                so that it is compatible with SWTPc MP-09 card.
--
-- 0.3     2007-02-25  John Kent  DAT Modify the sensitivity lists
--
-- 0.4     2010-06-17  John Kent  DAT Update header and added GPL
--
-- 1.0     2010-12-10  John Kent  MMU Extended DAT to create MMU
--

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_unsigned.all;
--library unisim;
--  use unisim.vcomponents.all;

entity mmu is
	port (	
	 clk       : in  std_logic;
    rst       : in  std_logic;
    cs        : in  std_logic;
    addr      : in  std_logic_vector(15 downto 0);
    rw        : in  std_logic;
    data_in   : in  std_logic_vector( 7 downto 0);
	 addr_out  : out std_logic_vector(27 downto 0));
end mmu;

architecture rtl of mmu is
signal mmu_reg0  : std_logic_vector(15 downto 0);
signal mmu_reg1  : std_logic_vector(15 downto 0);
signal mmu_reg2  : std_logic_vector(15 downto 0);
signal mmu_reg3  : std_logic_vector(15 downto 0);
signal mmu_reg4  : std_logic_vector(15 downto 0);
signal mmu_reg5  : std_logic_vector(15 downto 0);
signal mmu_reg6  : std_logic_vector(15 downto 0);
signal mmu_reg7  : std_logic_vector(15 downto 0);
signal mmu_reg8  : std_logic_vector(15 downto 0);
signal mmu_reg9  : std_logic_vector(15 downto 0);
signal mmu_reg10 : std_logic_vector(15 downto 0);
signal mmu_reg11 : std_logic_vector(15 downto 0);
signal mmu_reg12 : std_logic_vector(15 downto 0);
signal mmu_reg13 : std_logic_vector(15 downto 0);
signal mmu_reg14 : std_logic_vector(15 downto 0);
signal mmu_reg15 : std_logic_vector(15 downto 0);

begin

---------------------------------
--
-- Write DAT RAM
--
---------------------------------

--mmu_write : process( clk, rst, addr_lo, cs, rw, data_in )
mmu_write : process( clk )
begin
  if clk'event and clk = '0' then
    if rst = '1' then
      mmu_reg0  <= "0000000000001111";
      mmu_reg1  <= "0000000000001110";
      mmu_reg2  <= "0000000000001101";
      mmu_reg3  <= "0000000000001100";
      mmu_reg4  <= "0000000000001011";
      mmu_reg5  <= "0000000000001010";
      mmu_reg6  <= "0000000000001001";
      mmu_reg7  <= "0000000000001000";
      mmu_reg8  <= "0000000000000111";
      mmu_reg9  <= "0000000000000110";
      mmu_reg10 <= "0000000000000101";
      mmu_reg11 <= "0000000000000100";
      mmu_reg12 <= "0000000000000011";
      mmu_reg13 <= "0000000000000010";
      mmu_reg14 <= "1111111111110001";
      mmu_reg15 <= "1111111111110000";
    else
	   if cs = '1' and rw = '0' then
        case addr(4 downto 0) is
	     when "00000" =>
		    mmu_reg0(15 downto 8) <= data_in;
	     when "00001" =>
		    mmu_reg1(15 downto 8) <= data_in;
	     when "00010" =>
		    mmu_reg2(15 downto 8) <= data_in;
	     when "00011" =>
		    mmu_reg3(15 downto 8) <= data_in;
	     when "00100" =>
		    mmu_reg4(15 downto 8) <= data_in;
	     when "00101" =>
		    mmu_reg5(15 downto 8) <= data_in;
	     when "00110" =>
		    mmu_reg6(15 downto 8) <= data_in;
	     when "00111" =>
		    mmu_reg7(15 downto 8) <= data_in;
	     when "01000" =>
		    mmu_reg8(15 downto 8) <= data_in;
	     when "01001" =>
		    mmu_reg9(15 downto 8) <= data_in;
	     when "01010" =>
		    mmu_reg10(15 downto 8) <= data_in;
	     when "01011" =>
		    mmu_reg11(15 downto 8) <= data_in;
	     when "01100" =>
		    mmu_reg12(15 downto 8) <= data_in;
	     when "01101" =>
		    mmu_reg13(15 downto 8) <= data_in;
	     when "01110" =>
		    mmu_reg14(15 downto 8) <= data_in;
	     when "01111" =>
		    mmu_reg15(15 downto 8) <= data_in;
	     when "10000" =>
		    mmu_reg0(7 downto 0) <= data_in;
	     when "10001" =>
		    mmu_reg1(7 downto 0) <= data_in;
	     when "10010" =>
		    mmu_reg2(7 downto 0) <= data_in;
	     when "10011" =>
		    mmu_reg3(7 downto 0) <= data_in;
	     when "10100" =>
		    mmu_reg4(7 downto 0) <= data_in;
	     when "10101" =>
		    mmu_reg5(7 downto 0) <= data_in;
	     when "10110" =>
		    mmu_reg6(7 downto 0) <= data_in;
	     when "10111" =>
		    mmu_reg7(7 downto 0) <= data_in;
	     when "11000" =>
		    mmu_reg8(7 downto 0) <= data_in;
	     when "11001" =>
		    mmu_reg9(7 downto 0) <= data_in;
	     when "11010" =>
		    mmu_reg10(7 downto 0) <= data_in;
	     when "11011" =>
		    mmu_reg11(7 downto 0) <= data_in;
	     when "11100" =>
		    mmu_reg12(7 downto 0) <= data_in;
	     when "11101" =>
		    mmu_reg13(7 downto 0) <= data_in;
	     when "11110" =>
		    mmu_reg14(7 downto 0) <= data_in;
	     when "11111" =>
		    mmu_reg15(7 downto 0) <= data_in;
        when others =>
		    null;
		  end case;
	   end if;
	 end if;
  end if;
end process;

mmu_read : process(  addr,
                     mmu_reg0, mmu_reg1, mmu_reg2, mmu_reg3,
                     mmu_reg4, mmu_reg5, mmu_reg6, mmu_reg7,
                     mmu_reg8, mmu_reg9, mmu_reg10, mmu_reg11,
                     mmu_reg12, mmu_reg13, mmu_reg14, mmu_reg15 )
variable phy_addr : std_logic_vector( 15 downto 0 );
begin
      case addr(15 downto 12) is
	     when "0000" =>
		    phy_addr := mmu_reg0;
	     when "0001" =>
		    phy_addr := mmu_reg1;
	     when "0010" =>
		    phy_addr := mmu_reg2;
	     when "0011" =>
		    phy_addr := mmu_reg3;
	     when "0100" =>
		    phy_addr := mmu_reg4;
	     when "0101" =>
		    phy_addr := mmu_reg5;
	     when "0110" =>
		    phy_addr := mmu_reg6;
	     when "0111" =>
		    phy_addr := mmu_reg7;
	     when "1000" =>
		    phy_addr := mmu_reg8;
	     when "1001" =>
		    phy_addr := mmu_reg9;
	     when "1010" =>
		    phy_addr := mmu_reg10;
	     when "1011" =>
		    phy_addr := mmu_reg11;
	     when "1100" =>
		    phy_addr := mmu_reg12;
	     when "1101" =>
		    phy_addr := mmu_reg13;
	     when "1110" =>
		    phy_addr := mmu_reg14;
	     when "1111" =>
		    phy_addr := mmu_reg15;
        when others =>
		    null;
		end case;
      addr_out( 27 downto 16 ) <=      phy_addr( 15 downto 4 );
      addr_out( 15 downto 12 ) <= not( phy_addr(  3 downto 0 ) );
		addr_out( 11 downto  0 ) <=          addr( 11 downto 0 );
end process;

end rtl;
	
