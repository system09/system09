--===========================================================================--
--                                                                           --
--       2K Byte RAM Block using 4KBit Block RAMs found in the Spartan 2     --
--                                                                           --
--===========================================================================--
--
-- File name      : ram2k_b4.vhd
--
-- Entity name    : ram_2k
--
-- Purpose        : 2KB RAM block used for a character text buffer for vdu8 
--                  using 4 x 4KBit Block RAMs
--
-- Dependencies   : ieee.Std_Logic_1164
--                  ieee.std_logic_arith
--                  ieee.std_logic_unsigned
--                  unisim.vcomponents
-- 
-- Author         : John E. Kent      
--                  dilbert57@opencores.org
--
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
--                             Revision History:                             --
--===========================================================================--
--
-- Version Date       Author      Comments
--
-- 0.1     2004-02-11 John Kent   Initial Version
-- 0.2     2010-08-27 John Kent   Added header
--                                Changed data input & output signals
--
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
library unisim;
	use unisim.all;

entity ram_2k is
    Port (
       clk      : in  std_logic;
       rst      : in  std_logic;
       cs       : in  std_logic;
       rw       : in  std_logic;
       addr     : in  std_logic_vector (10 downto 0);
       data_in  : in  std_logic_vector (7 downto 0);
       data_out : out std_logic_vector (7 downto 0)
    );
end ram_2k;

architecture rtl of ram_2k is

   signal we        : std_logic;
   signal data_out0 : std_logic_vector (7 downto 0);
   signal data_out1 : std_logic_vector (7 downto 0);
   signal data_out2 : std_logic_vector (7 downto 0);
   signal data_out3 : std_logic_vector (7 downto 0);
   signal ena0      : std_logic;
   signal ena1      : std_logic;
   signal ena2      : std_logic;
   signal ena3      : std_logic;

   component RAMB4_S8
    generic (
      INIT_00, INIT_01, INIT_02, INIT_03,
      INIT_04, INIT_05, INIT_06, INIT_07,
      INIT_08, INIT_09, INIT_0A, INIT_0B,
      INIT_0C, INIT_0D, INIT_0E, INIT_0F : bit_vector (255 downto 0) :=
      x"0000000000000000000000000000000000000000000000000000000000000000"
    );

    port (
      clk, we, en, rst : in std_logic;
      addr :  in std_logic_vector(8 downto 0);
      di   :  in std_logic_vector(7 downto 0);
      do   : out std_logic_vector(7 downto 0)
    );
  end component;

begin

  MY_RAM0 : RAMB4_S8
    generic map ( 
INIT_00 => x"000000FF0000001010101010101010003E1C7F7F3E1C08000000FF0000000000",
INIT_01 => x"202020202020200000FF0000000000000000000000FF000000000000FF000000",
INIT_02 => x"0000E0100808080000000304080808080810E000000000040404040404040420",
INIT_03 => x"808080808080FF80402010080402010102040810204080FF8080808080808000",
INIT_04 => x"081C3E7F7F7F3600FF000000000000003C7E7E7E7E3C0001010101010101FF80",
INIT_05 => x"3C424242423C0081422418182442810808040300000000404040404040404000",
INIT_06 => x"0808FF0808080800081C3E7F3E1C0802020202020202020008082A772A1C0800",
INIT_07 => x"03070F1F3F7FFF001414543E010000080808080808080850A050A050A050A008",
INIT_08 => x"24247E247E242400000000002424240008000008080808000000000000000000",
INIT_09 => x"00000000100804003A444A30484830004626100864620000083C0A1C281E0800",
INIT_0A => x"0008083E08080000082A1C3E1C2A080020100808081020000408101010080400",
INIT_0B => x"402010080402000018180000000000000000007E000000100808000000000000",
INIT_0C => x"3C42021C02423C007E40300C02423C003E080808281808003C42625A46423C00",
INIT_0D => x"1010100804427E003C42427C40201C003844020478407E0004047E24140C0400",
INIT_0E => x"080800000800000000080000080000003804023E42423C003C42423C42423C00",
INIT_0F => x"1000100C02423C0070180C060C18700000007E007E0000000E18306030180E10"
    )

    port map ( clk => clk,
	            en  => ena0,
				   we  => we,
				   rst => rst,
				   addr(8 downto 0) => addr(8 downto 0),
               di(7 downto 0)   => data_in(7 downto 0),
				   do(7 downto 0)   => data_out0(7 downto 0)
	);

  MY_RAM1 : RAMB4_S8
    generic map ( 

INIT_00 => x"001C22404040221C007C22223C22227C004242427E422418001E204C564A221C",
INIT_01 => x"001C22424E40221C004040407840407E007E40407840407E0078242222222478",
INIT_02 => x"0042444870484442003844040404040E001C08080808081C004242427E424242",
INIT_03 => x"0018244242422418004242464A526242004242425A5A6642007E404040404040",
INIT_04 => x"003C42023C40423C004244487C42427C001A244A42422418004040407C42427C",
INIT_05 => x"0042665A5A4242420018182424424242003C424242424242000808080808083E",
INIT_06 => x"003C20202020203C007E40201804027E000808081C2222220042422418244242",
INIT_07 => x"0010207F20100000080808082A1C0800003C04040404043C006E70103C10100C",
INIT_08 => x"003C4240423C0000005C6242625C4040003A443C04380000001E204C564A221C",
INIT_09 => x"3C023A46463A0000001010107C10120C003C407E423C0000003A4642463A0202",
INIT_0A => x"004468504844404038440404040C0004001C08080818000800424242625C4040",
INIT_0B => x"003C4242423C000000424242625C00000049494949760000001C080808080818",
INIT_0C => x"007C023C403E000000404040625C000002023A46463A000040405C62625C0000",
INIT_0D => x"00364949494100000018244242420000003A464242420000000C1210107C1010",
INIT_0E => x"003C20202020203C007E2018047E00003C023A46424200000042241824420000",
INIT_0F => x"0010207F20100000080808082A1C0800003C04040404043C006E70103C10100C"
    )

    port map ( clk => clk,
	            en  => ena1,
				   we  => we,
				   rst => rst,
				   addr(8 downto 0) => addr(8 downto 0),
               di(7 downto 0)   => data_in(7 downto 0),
				   do(7 downto 0)   => data_out1(7 downto 0)
	);

  MY_RAM2 : RAMB4_S8
    generic map ( 
INIT_00 => x"000000FF0000001010101010101010003E1C7F7F3E1C08000000FF0000000000",
INIT_01 => x"202020202020200000FF0000000000000000000000FF000000000000FF000000",
INIT_02 => x"0000E0100808080000000304080808080810E000000000040404040404040420",
INIT_03 => x"808080808080FF80402010080402010102040810204080FF8080808080808000",
INIT_04 => x"081C3E7F7F7F3600FF000000000000003C7E7E7E7E3C0001010101010101FF80",
INIT_05 => x"3C424242423C0081422418182442810808040300000000404040404040404000",
INIT_06 => x"0808FF0808080800081C3E7F3E1C0802020202020202020008082A772A1C0800",
INIT_07 => x"03070F1F3F7FFF001414543E010000080808080808080850A050A050A050A008",
INIT_08 => x"24247E247E242400000000002424240008000008080808000000000000000000",
INIT_09 => x"00000000100804003A444A30484830004626100864620000083C0A1C281E0800",
INIT_0A => x"0008083E08080000082A1C3E1C2A080020100808081020000408101010080400",
INIT_0B => x"402010080402000018180000000000000000007E000000100808000000000000",
INIT_0C => x"3C42021C02423C007E40300C02423C003E080808281808003C42625A46423C00",
INIT_0D => x"1010100804427E003C42427C40201C003844020478407E0004047E24140C0400",
INIT_0E => x"080800000800000000080000080000003804023E42423C003C42423C42423C00",
INIT_0F => x"1000100C02423C0070180C060C18700000007E007E0000000E18306030180E10"
    )

    port map ( clk => clk,
	            en  => ena2,
				   we  => we,
				   rst => rst,
				   addr(8 downto 0) => addr(8 downto 0),
               di(7 downto 0)   => data_in(7 downto 0),
				   do(7 downto 0)   => data_out2(7 downto 0)
	);

  MY_RAM3 : RAMB4_S8
    generic map ( 

INIT_00 => x"001C22404040221C007C22223C22227C004242427E422418001E204C564A221C",
INIT_01 => x"001C22424E40221C004040407840407E007E40407840407E0078242222222478",
INIT_02 => x"0042444870484442003844040404040E001C08080808081C004242427E424242",
INIT_03 => x"0018244242422418004242464A526242004242425A5A6642007E404040404040",
INIT_04 => x"003C42023C40423C004244487C42427C001A244A42422418004040407C42427C",
INIT_05 => x"0042665A5A4242420018182424424242003C424242424242000808080808083E",
INIT_06 => x"003C20202020203C007E40201804027E000808081C2222220042422418244242",
INIT_07 => x"0010207F20100000080808082A1C0800003C04040404043C006E70103C10100C",
INIT_08 => x"003C4240423C0000005C6242625C4040003A443C04380000001E204C564A221C",
INIT_09 => x"3C023A46463A0000001010107C10120C003C407E423C0000003A4642463A0202",
INIT_0A => x"004468504844404038440404040C0004001C08080818000800424242625C4040",
INIT_0B => x"003C4242423C000000424242625C00000049494949760000001C080808080818",
INIT_0C => x"007C023C403E000000404040625C000002023A46463A000040405C62625C0000",
INIT_0D => x"00364949494100000018244242420000003A464242420000000C1210107C1010",
INIT_0E => x"003C20202020203C007E2018047E00003C023A46424200000042241824420000",
INIT_0F => x"0010207F20100000080808082A1C0800003C04040404043C006E70103C10100C"
    )

    port map ( clk => clk,
	            en  => ena3,
				   we  => we,
				   rst => rst,
				   addr(8 downto 0) => addr(8 downto 0),
               di(7 downto 0)   => data_in(7 downto 0),
				   do(7 downto 0)   => data_out3(7 downto 0)
	);

my_ram_2k : process ( cs, rw, addr, data_out0, data_out1, data_out2, data_out3 )
begin
    ena0 <= '0';
	 ena1 <= '0';
    ena2 <= '0';
	 ena3 <= '0';
	 case addr(10 downto 9) is
	 when "00" =>
      ena0     <= cs;
		data_out <= data_out0;
	 when "01" =>
	   ena1     <= cs;
		data_out <= data_out1;
	 when "10" =>
      ena2     <= cs;
		data_out <= data_out2;
	 when "11" =>
	   ena3     <= cs;
		data_out <= data_out3;
	 when others =>
      null;
	 end case;

	 we <= not rw;

end process;

end;

