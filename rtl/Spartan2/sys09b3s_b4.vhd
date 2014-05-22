--===========================================================================--
--                                                                           --
--  Sys09bug Monitor ROM using 4KBit Block RAMs found in the Spartan 2       --
--                                                                           --
--===========================================================================--
--
-- File name      : Sys09b3s_b4.vhd
--
-- Entity name    : SYS09BUG_F8000
--
-- Purpose        : Sys09bug Monitor Program using 4KBit Block RAMs
--                  For the System09 6809 compatible System on a Chip 
--                  Used on the BurchED B3 and B5-X300 Spartan 2 boards.
--
-- Dependencies   : ieee.Std_Logic_1164
--                  ieee.std_logic_arith
-- 
-- Author         : John E. Kent      
--                  dilbert57@opencores.org
--
-- Memory Map     : Sys09Bug monitor ROM assumes the following memory map
--
-- $0000 - $DFFF System RAM (256K Mapped via DAT)
-- $E000 - $E00F ACIA (SWTPc)
-- $E020 - $E02F Keyboard
-- $E030 - $E03F VDU
-- $E0A0 - $E0AF SPP Printer Port
-- $E100 - $E13F IDE / Compact Flash Card
-- $F800 - $FFFF Sys09bug ROM (Read only)
-- $FFF0 - $FFFF DAT - Dynamic Address Translation (Write Only)
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
--===========================================================================----
--
-- Revision History:
--===========================================================================--
--
-- Version Date       Author      Comments
--
-- 0.1     2003-??-?? John Kent   Initial Version
-- 0.2     2010-08-27 John Kent   Added header
--

library IEEE;
   use IEEE.std_logic_1164.all;
   use IEEE.std_logic_arith.all;
library unisim;
   use unisim.vcomponents.all;

entity SYS09BUG_F800 is
  port(
    clk      : in  std_logic;
    rst      : in  std_logic;
    cs       : in  std_logic;
    addr     : in  std_logic_vector(10 downto 0);
    rw       : in  std_logic;
    data_in  : in  std_logic_vector(7 downto 0);
    data_out : out std_logic_vector(7 downto 0)
   );
end SYS09BUG_F800;

architecture rtl of SYS09BUG_F800 is

   type data_array is array(0 to 3) of std_logic_vector(7 downto 0);
   signal xdata : data_array;
   signal en : std_logic_vector(3 downto 0);
   signal we : std_logic;

component RAMB4_S8
generic (
   INIT_00, INIT_01, INIT_02, INIT_03,
   INIT_04, INIT_05, INIT_06, INIT_07,
   INIT_08, INIT_09, INIT_0A, INIT_0B,
   INIT_0C, INIT_0D, INIT_0E, INIT_0F : bit_vector (255 downto 0)
    );
   port (
      clk, we, en, rst : in std_logic;
      addr : in std_logic_vector(8 downto 0);
      di   : in std_logic_vector(7 downto 0);
      do   : out std_logic_vector(7 downto 0)
      );
     end component RAMB4_S8;

   begin

   ROM00: RAMB4_S8
      generic map (
         INIT_00 => x"A780A610C6C0DF8E106DFE8E2EFA1AFB1EFB8FFBDCFCC1FC97FC9DFC61F814F8",
         INIT_01 => x"17431FE4A7D0866AAFDD8C30FB265AE26F0CC65B0117E0DFBF00E08EF9265AA0",
         INIT_02 => x"03179EFE8E0C0417F62A5A19048B0327856D0DC64FD0DF8E4703177DFE8EB104",
         INIT_03 => x"17408B981F6104175E86092C2081891FF1270D817F84330417B30217A5FE8E2E",
         INIT_04 => x"20F00217A7FE8EF5266DFE8C02300F2780E137FE8E20C0022F60C15504175A04",
         INIT_05 => x"17A4A61D0417A50317211F650217ADFE8E121F2D296B03173B341FBC2094ADC0",
         INIT_06 => x"27A4A1A4A7390F260D8117275E81DD271881E127088111285E0317150417A503",
         INIT_07 => x"0B031705201F30C0DF8E321FA20217BE203F31C2202131F303173F86F6031708",
         INIT_08 => x"27A203170527E4AC011FF0C4201F0634F0C41000C3101F390124E1AC20340629",
         INIT_09 => x"265A9C03172C031780A610C6A403172E0317E4AEEE0117ADFE8E103439623203",
         INIT_0a => x"29B70217BC20EE265A8503172E8602237E810425208180A610C6E1AE940317F5",
         INIT_0b => x"3984A73F86A4AFA0A709273F8184A60F271035558DFFFF8E10341A24C0DF8C1E",
         INIT_0c => x"4AAF0427268D1F304AAE431F39FB265A188D08C6E3DF8E105403163F86570317",
         INIT_0d => x"A7A0A7A0A7FF8684A7A4A604263F8184A60A24C0DF8C21AEB9FE16480217068D",
         INIT_0e => x"E1FD0200CC1EE1FD0600CC393D3139F7265A0427A1ACA0A608C6E3DF8E1039A0",
         INIT_0f => x"178D0EE1FD20C60AE1FD08E1FD06E1FD5F04E1FD0100CC2E8D0CE1FDE000CC1E"
      )
      port map (
         clk     => clk,
         en      => en(0),
         we      => we,
         rst     => rst,
         addr    => addr(8 downto 0),
         di      => data_in,
         do      => xdata(0)
      );

   ROM01: RAMB4_S8
      generic map (
         INIT_00 => x"E1FCF92680C50EE1FC3B341F4AAF00C08EF42600C18C80E700E1FC218D00C08E",
         INIT_01 => x"54545454A6E6D0DF8E104444444462A6363439F92708C50EE1FC39F22740C50E",
         INIT_02 => x"FCBD8435FD265A20C60434B63562E762EA62A70F8462A65858585853A6E6E4E7",
         INIT_03 => x"0234A80117F12631813D2739811F0217F9265381260217E2DF7F7602171186F5",
         INIT_04 => x"E0EB02340C2904358E01170434E46AE46AE4EBE0EBE0E6103421299101172629",
         INIT_05 => x"0117E26F2002161386E2DF732802173F86BA27FFC102355FEB2080A70527E46A",
         INIT_06 => x"2320008310062762A3E4EC0702171286F5FCBDE4AF0130492562AC4D2930344A",
         INIT_07 => x"1780A684EB63EB62EB68011762AE750117981F03CB2F0017EEFE8E64E720C602",
         INIT_08 => x"10347120028D396532C501171486C326E4AC62AF5B0117981F53F526646A6501",
         INIT_09 => x"8D618D394AAF0229F68DF28D910017E50016F80016AF011690356900179FFE8E",
         INIT_0a => x"498D3944AF0229D58DD18D5E8D3946AF0229E08DDC8D728D3948AF0229EB8DE7",
         INIT_0b => x"8D3941A70229B18DB08D588D3942A70229BC8DBB8D6C8D3943A70229C78DC68D",
         INIT_0c => x"BF0016311FF48DB1FE8E39F726048180A64D011739C4A7808A0429A68DA58D5F",
         INIT_0d => x"8DC3FE8EE12044AED78DC9FE8EB4001643A6E18DCFFE8EF42048AEEA8DBDFE8E",
         INIT_0e => x"D02042A6B38DDAFE8ED92041A6BC8DD5FE8ECF204AAEC58DB7FE8ED82046AECE",
         INIT_0f => x"ADFE8EBF8DB88DB08DA98DA18D27FF17ADFE8E900016E6FE8EC4A6AA8DDFFE8E"
      )
      port map (
         clk     => clk,
         en      => en(1),
         we      => we,
         rst     => rst,
         addr    => addr(8 downto 0),
         di      => data_in,
         do      => xdata(1)
      );

   ROM02: RAMB4_S8
      generic map (
         INIT_00 => x"3C29088D011F42290E8DC600172D86121F4D29098DD520CE8DC78DC08D17FF17",
         INIT_01 => x"811D253081578D39E0AB04342829078D891F484848483229118D903561A71034",
         INIT_02 => x"3439021A39578003226681072561813937800322468112254181393080032239",
         INIT_03 => x"C602345F20078B022F3981308B0F840235048D4444444402340235028D023510",
         INIT_04 => x"8610343F207F84048D0627E2DF7D8235F1265A4D8D518D2D860225E46880A608",
         INIT_05 => x"3501A6E0DF9FA75186EE27018584A620E08E0926018584A6E0DFBEE0DF9FA711",
         INIT_06 => x"1234498D2086008D8235018520E0B605260185E0DF9FA6E0DF9FA71186023490",
         INIT_07 => x"84A7518684A70386E0DFBE138D903501A70235F6260885FA27028584A6E0DFBE",
         INIT_08 => x"DF7F01E702C6FDDFFD04E703E702A7FBDFFD0000CC30E08E39E2DFB7FF86016D",
         INIT_09 => x"1784A70520098D042420810D20608D0427FEDF7D30E08E16345986028D1B86FE",
         INIT_0a => x"24270C81890027100D81382716817C0027101A815A271B81342708819635AF00",
         INIT_0b => x"27FBDFB66D205A34275DFBDFFC8F0016792619C15CFBDFFC45260A810F270B81",
         INIT_0c => x"54816E27598114273DC1FEDFF656200000CC5820212750814CFBDFB662204A2C",
         INIT_0d => x"ED224F812080FEDF7F39FDDFB70426FDDF7D39FEDF7F39FEDFB704263D813127",
         INIT_0e => x"26508102A74C84E720C6FBDFB6168D0000CC1B20E12218C120C0FDDF7FFDDFF6",
         INIT_0f => x"5AEA2619C15C4FF02650814CFBDFFC3903E702A7FBDFFDFCDFF64F39FEDF7FF7"
      )
      port map (
         clk     => clk,
         en      => en(2),
         we      => we,
         rst     => rst,
         addr    => addr(8 downto 0),
         di      => data_in,
         do      => xdata(2)
      );

   ROM03: RAMB4_S8
      generic map (
         INIT_00 => x"FCDFF6F42650C15C84A702E7FBDFF72086FBDFF604E75F012519C15C04E6E78D",
         INIT_01 => x"7FFB0369FB0274FB0139FEDFF702E7FBDFF75FE4205F03E7FCDFF7082719C15C",
         INIT_02 => x"F84DBCFA505EFA4CA5F847FDF8455CF94248FB1953FB183DFB1531FB105EFB04",
         INIT_03 => x"000A0DFFFFFFFF94F9A7F8A7F8A7F8A7F894F9D5F94488F958F1F853EDFB52A8",
         INIT_04 => x"0D4B04202D20202B32532D334220524F4620362E312047554239305359530000",
         INIT_05 => x"552020043D43502020043D5053202004202D20043F54414857043E040000000A",
         INIT_06 => x"20043D422020043D412020043D50442020043D58492020043D59492020043D53",
         INIT_07 => x"00000000000000000000000000000004315343565A4E4948464504203A434320",
         INIT_08 => x"300B2784AC1084AF1084EEAA558E10A0D08E84A7F086FB264A80A70F86F0FF8E",
         INIT_09 => x"2DA7D0DF8E10C0DFCE10FDFFB74444444443101F84EFD620ED26A0F08C00F089",
         INIT_0a => x"1084AF10AA558E1084EE2227A0F08C00F08930FB2A4AA66F0C862FA7F0862E6F",
         INIT_0b => x"2EA7D0DF8E10F186D520A5A70F88891F44444444101FD0DF8E1084EFE92684AC",
         INIT_0c => x"8EF32D0C814C80E7A66F0427A6E6211F4F2CE7A66F1420F92A4A0526A6E60C86",
         INIT_0d => x"9F6EC6DF9F6EC4DF9F6EC0DF9F6E62F816E2DFF753F9265A80A7A0A610C6F0FF",
         INIT_0e => x"0822CEDFBC8B300F27FFFF8CCCDFBE49584F4AAF80E64AAE431FCADF9F6EC8DF",
         INIT_0f => x"00FFB2FFC2FFBEFFBAFFB6FFC6FFB2FFC2DF9F6E42EE1F37F16E44AEC4EC1034"
      )
      port map (
         clk     => clk,
         en      => en(3),
         we      => we,
         rst     => rst,
         addr    => addr(8 downto 0),
         di      => data_in,
         do      => xdata(3)
      );

   rom_glue: process (cs, rw, addr, xdata)
   begin
      en <= (others=>'0');
      case addr(10 downto 9) is
      when "00" =>
         en(0)  <= cs;
         data_out  <= xdata(0);
      when "01" =>
         en(1)  <= cs;
         data_out  <= xdata(1);
      when "10" =>
         en(2)  <= cs;
         data_out  <= xdata(2);
      when "11" =>
         en(3)  <= cs;
         data_out  <= xdata(3);
      when others =>
         null;
      end case;
      we <= not rw;
   end process;
end architecture rtl;
