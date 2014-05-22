--
-- sys09b3s_rom2k_b4.vhd
--
-- SYS09BUG Monitor ROM for the B3-S2+ 6809
-- Using 4 x RAMB4_S8 in the XC2S200
--
-- John Kent
-- 3rd February 2007
-- Has the same entity name as SBUG so
-- it can be easily exchanged.
--
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
library unisim;
	use unisim.vcomponents.all;

entity mon_rom is
    Port (
       clk   : in  std_logic;
       rst   : in  std_logic;
       cs    : in  std_logic;
       rw    : in  std_logic;
       addr  : in  std_logic_vector (10 downto 0);
       wdata : in  std_logic_vector (7 downto 0);
       rdata : out std_logic_vector (7 downto 0)
    );
end mon_rom;

architecture rtl of mon_rom is

   signal rdata0 : std_logic_vector (7 downto 0);
   signal rdata1 : std_logic_vector (7 downto 0);
   signal rdata2 : std_logic_vector (7 downto 0);
   signal rdata3 : std_logic_vector (7 downto 0);

   signal ena0 : std_logic;
   signal ena1 : std_logic;
   signal ena2 : std_logic;
   signal ena3 : std_logic;

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

  ROM0 : RAMB4_S8
    generic map ( 
    INIT_00 => x"A780A610C6C0DF8E106DFE8E2EFA1AFB1EFB8FFBCAFCB5FC97FC9DFC61F814F8",
    INIT_01 => x"17431FE4A7D0866AAFDD8C30FB265AE26F0CC65B0117E0DFBF00E08EF9265AA0",
    INIT_02 => x"03179EFE8E0C0417F62A5A19048B0327856D0DC64FD0DF8E4703177DFE8E9B04",
    INIT_03 => x"17408B981F4F04175E86092C2081891FF1270D817F84330417B30217A5FE8E2E",
    INIT_04 => x"20F00217A7FE8EF5266DFE8C02300F2780E137FE8E20C0022F60C14304174804",
    INIT_05 => x"17A4A60B0417A50317211F650217ADFE8E121F2D296B03173B341FBC2094ADC0",
    INIT_06 => x"27A4A1A4A7390F260D8117275E81DD271881E127088111285E0317030417A503",
    INIT_07 => x"0B031705201F30C0DF8E321FA20217BE203F31C2202131E103173F86E4031708",
    INIT_08 => x"279603170527E4AC011FF0C4201F0634F0C41000C3101F390124E1AC20340629",
    INIT_09 => x"265A8A03172C031780A610C69203172E0317E4AEEE0117ADFE8E103439623203",
    INIT_0a => x"29B70217BC20EE265A7303172E8602237E810425208180A610C6E1AE820317F5",
    INIT_0b => x"3984A73F86A4AFA0A709273F8184A60F271035558DFFFF8E10341A24C0DF8C1E",
    INIT_0c => x"4AAF0427268D1F304AAE431F39FB265A188D08C6E3DF8E104203163F86450317",
    INIT_0d => x"A7A0A7A0A7FF8684A7A4A604263F8184A60A24C0DF8C21AEB9FE16480217068D",
    INIT_0e => x"E1FD0200CC1EE1FD0600CC393D3139F7265A0427A1ACA0A608C6E3DF8E1039A0",
    INIT_0f => x"178D0EE1FD20C60AE1FD08E1FD06E1FD5F04E1FD0100CC2E8D0CE1FDE000CC1E"
    )

    port map ( 
		clk => clk,
		en => ena0,
		we => we,
		rst => rst,
		addr(8 downto 0) => addr(8 downto 0),
		di(7 downto 0)   => wdata,
		do(7 downto 0)   => rdata0
	);

  ROM1 : RAMB4_S8
    generic map ( 
    INIT_00 => x"E1FCF92680C50EE1FC3B341F4AAF00C08EF42600C18C80E700E1FC218D00C08E",
    INIT_01 => x"54545454A6E6D0DF8E104444444462A6363439F92708C50EE1FC39F22740C50E",
    INIT_02 => x"FCBD8435FD265A20C60434B63562E762EA62A70F8462A65858585853A6E6E4E7",
    INIT_03 => x"0234A80117F12631813D2739811F0217F9265381260217E2DF7F6402171186DF",
    INIT_04 => x"E0EB02340C2904358E01170434E46AE46AE4EBE0EBE0E6103421299101172629",
    INIT_05 => x"0117E26F0E02161386E2DF731602173F86BA27FFC102355FEB2080A70527E46A",
    INIT_06 => x"2320008310062762A3E4ECF501171286DFFCBDE4AF0130492562AC4D2930344A",
    INIT_07 => x"1780A684EB63EB62EB68011762AE750117981F03CB2F0017EEFE8E64E720C602",
    INIT_08 => x"10347120028D396532B301171486C326E4AC62AF5B0117981F53F526646A6501",
    INIT_09 => x"8D618D394AAF0229F68DF28D910017E50016F800169D011690356900179FFE8E",
    INIT_0a => x"498D3944AF0229D58DD18D5E8D3946AF0229E08DDC8D728D3948AF0229EB8DE7",
    INIT_0b => x"8D3941A70229B18DB08D588D3942A70229BC8DBB8D6C8D3943A70229C78DC68D",
    INIT_0c => x"BF0016311FF48DB1FE8E39F726048180A63B011739C4A7808A0429A68DA58D5F",
    INIT_0d => x"8DC3FE8EE12044AED78DC9FE8EB4001643A6E18DCFFE8EF42048AEEA8DBDFE8E",
    INIT_0e => x"D02042A6B38DDAFE8ED92041A6BC8DD5FE8ECF204AAEC58DB7FE8ED82046AECE",
    INIT_0f => x"ADFE8EBF8DB88DB08DA98DA18D27FF17ADFE8E900016E6FE8EC4A6AA8DDFFE8E"
    )

    port map ( 
		clk => clk,
		en => ena1,
		we => we,
		rst => rst,
		addr(8 downto 0) => addr(8 downto 0),
		di(7 downto 0)   => wdata,
		do(7 downto 0)   => rdata1
	);

  ROM2 : RAMB4_S8
    generic map ( 
    INIT_00 => x"3C29088D011F42290E8DB400172D86121F4D29098DD520CE8DC78DC08D17FF17",
    INIT_01 => x"811D253081578D39E0AB04342829078D891F484848483229118D903561A71034",
    INIT_02 => x"3439021A39578003226681072561813937800322468112254181393080032239",
    INIT_03 => x"C602344D20078B022F3981308B0F840235048D4444444402340235028D023510",
    INIT_04 => x"BE10342D207F84048D0627E2DF7D8235F1265A3B8D3F8D2D860225E46880A608",
    INIT_05 => x"B605260185E0DF9FA60234903501A6EE27018584A620E08E0926018584A6E0DF",
    INIT_06 => x"BE138D903501A70235FA27028584A6E0DFBE1234458D2086008D8235018520E0",
    INIT_07 => x"E703E702A7FBDFFD0000CC30E08E39E2DFB7FF86016D84A7118684A70386E0DF",
    INIT_08 => x"810D20748D0427FEDF7D30E08E16345986028D1B86FEDF7F01E702C6FDDFFD04",
    INIT_09 => x"8E0027101A816C0027101B814100271008819635C5001784A70520098D042420",
    INIT_0a => x"5CFBDFFC51260A81110027100B812C0027100C81990027100D81450027101681",
    INIT_0b => x"DFB66800164A3327FBDFB67400165A3C0027105DFBDFFC9900168300261019C1",
    INIT_0c => x"54816E002710598116273DC1FEDFF65800160000CC5B00162500271050814CFB",
    INIT_0d => x"ED224F812080FEDF7F39FDDFB70426FDDF7D39FEDF7F39FEDFB704263D813127",
    INIT_0e => x"26508102A74C84E720C6FBDFB6168D0000CC1B20E12218C120C0FDDF7FFDDFF6",
    INIT_0f => x"5AEA2619C15C4FF02650814CFBDFFC3903E702A7FBDFFDFCDFF64F39FEDF7FF7"
    )

    port map ( 
		clk => clk,
		en => ena2,
		we => we,
		rst => rst,
		addr(8 downto 0) => addr(8 downto 0),
		di(7 downto 0)   => wdata,
		do(7 downto 0)   => rdata2
	);

  ROM3 : RAMB4_S8
    generic map ( 
    INIT_00 => x"FCDFF6F42650C15C84A702E7FBDFF72086FBDFF604E75F012519C15C04E6E78D",
    INIT_01 => x"7FFB0369FB0274FB0139FEDFF702E7FBDFF75FE4205F03E7FCDFF7082719C15C",
    INIT_02 => x"F84DBCFA505EFA4CA5F847FDF8455CF94248FB1953FB183DFB1531FB105EFB04",
    INIT_03 => x"000A0DFFFFFFFF94F9A7F8A7F8A7F8A7F894F9D5F94488F958F1F853EDFB52A8",
    INIT_04 => x"0D4B04202D20202B32532D334220524F4620342E312047554239305359530000",
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
	clk => clk,
	en => ena3,
	we => we,
	rst => rst,
	addr(8 downto 0) => addr(8 downto 0),
	di(7 downto 0)   => wdata,
	do(7 downto 0)   => rdata3
	);

my_sys09bug_b4 : process ( cs, rw, addr, rdata0, rdata1, rdata2, rdata3 )
begin
	case addr(10 downto 9) is
	when "00" =>
		ena0  <= cs;
		ena1  <= '0';
		ena2  <= '0';
		ena3  <= '0';
		rdata <= rdata0;
	when "01" =>
		ena0  <= '0';
		ena1  <= cs;
		ena2  <= '0';
		ena3  <= '0';
		rdata <= rdata1;
	when "10" =>
		ena0  <= '0';
		ena1  <= '0';
		ena2  <= cs;
		ena3  <= '0';
		rdata <= rdata2;
	when "11" =>
		ena0  <= '0';
		ena1  <= '0';
		ena2  <= '0';
		ena3  <= cs;
		rdata <= rdata3;
	when others =>
		null;
	end case;

	we <= not rw;

end process;

end architecture rtl;

