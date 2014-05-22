--===========================================================================--
--                                                                           --
--  Character generator ROM using 4KBit Block RAMs found in the Spartan 2    --
--                                                                           --
--===========================================================================--
--
-- File name      : char_rom2k_b4.vhd
--
-- Entity name    : char_rom
--
-- Purpose        : 2KB Character Generator ROM for vdu8 
--                  using 4 x 4KBit Block RAMs
--                  8 dots across [data( 7 dwonto 0)]
--                  16 lines down [addr( 3 downto 0)]
--                  127 character [addr(10 downto 4)]
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
--                             Revision History:                             --
--===========================================================================--
--
-- Version Date       Author      Comments
--
-- 0.1     2007-02-03 John Kent   Initial Version
-- 0.2     2010-08-27 John Kent   Added header
--
--
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
library unisim;
	use unisim.vcomponents.all;

entity char_rom is
    Port (
       clk      : in  std_logic;
       rst      : in  std_logic;
       cs       : in  std_logic;
       addr     : in  std_logic_vector (10 downto 0);
       rw       : in  std_logic;
       data_in  : in  std_logic_vector (7 downto 0);
       data_out : out std_logic_vector (7 downto 0)
    );
end char_rom;

architecture rtl of char_rom is

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
    INIT_00 => x"0000000009090F09090038043840380000000000070404040400444C54644400",
    INIT_01 => x"00000000110A040A110078407040780000000000110A040A1100380438403800",
    INIT_02 => x"000000000D1215110E0078407040780000000000040404041F00784070407800",
    INIT_03 => x"000000000F080808080070487048700000000000090A0C0A0900487848483000",
    INIT_04 => x"00000000040404041F0044447C444400000000000E010E100E00704870487000",
    INIT_05 => x"00000000040404041F001028444444000000000010101E101F007C4040404000",
    INIT_06 => x"0000000011111E111E003C4040403C000000000008080E080F00404070407800",
    INIT_07 => x"00000000070202020700380438403800000000000E1111110E00380438403800",
    INIT_08 => x"00000000070202060200704848487000000000000F080E080F00704848487000",
    INIT_09 => x"000000000E0107020F00704848487000000000000F0806090700704848487000",
    INIT_0a => x"00000000090A0C0A0900444C546444000000000001010F090900704848487000",
    INIT_0b => x"000000000E090E090E0078407040780000000000111315191100380438403800",
    INIT_0c => x"000000001111151B110078407040780000000000111315191100384040403800",
    INIT_0d => x"000000000E1010100E00784070407800000000000E090E090E00380438403800",
    INIT_0e => x"000000000E010E100E00384858403800000000000E010E100E00404070407800",
    INIT_0f => x"000000000E010E100E00304848484800000000000E010E100E00485070487000"
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
    INIT_00 => x"0000000008080000080808080808080000000000000000000000000000000000",
    INIT_01 => x"000000002424247E2424247E2424240000000000000000000000001212121200",
    INIT_02 => x"0000000043434020100804020161610000000000083E4909093E4848493E0800",
    INIT_03 => x"00000000000000000000002010080C00000000003D4244444438444444443800",
    INIT_04 => x"0000000020100804040404040810200000000000020408101010101008040200",
    INIT_05 => x"0000000000000808087F0808080000000000000000004122147F142241000000",
    INIT_06 => x"0000000000000000007F00000000000000402010181800000000000000000000",
    INIT_07 => x"0000000040404020100804020101010000000000181800000000000000000000",
    INIT_08 => x"000000003E080808080808082818080000000000081422414141414122140800",
    INIT_09 => x"000000003E410101010E010101413E00000000007F4020100804020141423C00",
    INIT_0a => x"000000003E410101615E404040407F000000000002020202027F22120A060200",
    INIT_0b => x"00000000404020100804020101017F00000000001E214141615E404040211E00",
    INIT_0c => x"000000003C420101013D434141423C00000000003E414141413E414141413E00",
    INIT_0d => x"0000402010181818000000181818000000000000001818180000001818180000",
    INIT_0e => x"00000000000000007F00007F0000000000000000010204081020100804020100",
    INIT_0f => x"00000000080800080808060101413E0000000000402010080402040810204000"
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
    INIT_00 => x"0000000041414141417F414122140800000000001C224140404E494541221C00",
    INIT_01 => x"000000001E2141404040404041211E00000000007E212121213E212121217E00",
    INIT_02 => x"000000007F404040407C404040407F00000000007C2221212121212121227C00",
    INIT_03 => x"000000001E2141414147404040211E000000000040404040407C404040407F00",
    INIT_04 => x"000000003E0808080808080808083E000000000041414141417F414141414100",
    INIT_05 => x"00000000414244485060504844424100000000003C4202020202020202020700",
    INIT_06 => x"00000000414141414141494955634100000000007F4040404040404040404000",
    INIT_07 => x"000000003E4141414141414141413E0000000000414141434549495161414100",
    INIT_08 => x"000000003D4245494141414141413E000000000040404040407E414141417E00",
    INIT_09 => x"000000003E410101013E404040413E000000000041424448507E414141417E00",
    INIT_0a => x"000000003E414141414141414141410000000000080808080808080808087F00",
    INIT_0b => x"0000000022225555494941414141410000000000080814141422222241414100",
    INIT_0c => x"0000000008080808080814224141410000000000414141221408142241414100",
    INIT_0d => x"000000001E1010101010101010101E00000000007F4040201008040201017F00",
    INIT_0e => x"000000003C0404040404040404043C0000000000010101020408102040404000",
    INIT_0f => x"000000007F000000000000000000000000000000000000000000004122140800"
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
    INIT_00 => x"000000003F41413F01013E000000000000000000000000000000000204081800",
    INIT_01 => x"000000001E21404040211E0000000000000000005E61616141615E4040404000",
    INIT_02 => x"000000003E40407F41413E0000000000000000003D43414141433D0101010100",
    INIT_03 => x"003C4202023E424242423D0100000000000000001010101010107C1010110E00",
    INIT_04 => x"000000003E0808080808180000080800000000004141414141615E4040404000",
    INIT_05 => x"00000000414448704844414040404000003C4202020202020202020000020200",
    INIT_06 => x"00000000414141494955220000000000000000001C0808080808080808081800",
    INIT_07 => x"000000003E41414141413E0000000000000000004141414141615E0000000000",
    INIT_08 => x"00010101013D434343433D000000000000404040405E616161615E0000000000",
    INIT_09 => x"000000003E01013E40403E0000000000000000002020202020314E0000000000",
    INIT_0a => x"000000003D4242424242420000000000000000000C12101010107C1010101000",
    INIT_0b => x"0000000022554949414141000000000000000000081414222241410000000000",
    INIT_0c => x"003C4202023A4642424242000000000000000000412214081422410000000000",
    INIT_0d => x"00000000070808081020100808080700000000007F20100804027F0000000000",
    INIT_0e => x"0000000070080808040204080808700000000000080808080800080808080800",
    INIT_0f => x"0000000049224922492249224922490000000000000000000000000046493100"
    )

    port map ( clk => clk,
	            en  => ena3,
				   we  => we,
				   rst => rst,
				   addr(8 downto 0) => addr(8 downto 0),
               di(7 downto 0)   => data_in(7 downto 0),
				   do(7 downto 0)   => data_out3(7 downto 0)
	);

my_char_rom2k_b4 : process ( cs, rw, addr, data_out0, data_out1, data_out2, data_out3 )
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

