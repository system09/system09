--
-- Flex9 O/S Initialised 8KByte RAM
--
-- v1.0 - 22 December 2006 - John Kent
-- 
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
library unisim;
	use unisim.vcomponents.all;

entity flex_ram is
    Port (
       clk   : in  std_logic;
       rst   : in  std_logic;
       cs    : in  std_logic;
       rw    : in  std_logic;
       addr  : in  std_logic_vector (12 downto 0);
       rdata : out std_logic_vector (7 downto 0);
       wdata : in  std_logic_vector (7 downto 0)
    );
end flex_ram;

architecture rtl of flex_ram is

  signal we     : std_logic;
  signal cs0    : std_logic;
  signal cs1    : std_logic;
  signal cs2    : std_logic;
  signal cs3    : std_logic;
  signal dp0    : std_logic;
  signal dp1    : std_logic;
  signal dp2    : std_logic;
  signal dp3    : std_logic;
  signal rdata0 : std_logic_vector(7 downto 0);
  signal rdata1 : std_logic_vector(7 downto 0);
  signal rdata2 : std_logic_vector(7 downto 0);
  signal rdata3 : std_logic_vector(7 downto 0);


begin


  FLEX0 : RAMB16_S9
    generic map ( 
    INIT_00 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_01 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_02 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_03 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_04 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_05 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_06 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_07 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_08 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_09 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_0a => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_0b => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_0c => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_0d => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_0e => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_0f => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_10 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_11 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_12 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_13 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_14 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_15 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_16 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_17 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_18 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_19 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_1a => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_1b => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_1c => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_1d => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_1e => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_1f => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_20 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_21 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_22 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_23 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_24 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_25 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_26 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_27 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_28 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_29 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_2a => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_2b => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_2c => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_2d => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_2e => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_2f => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_30 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_31 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_32 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_33 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_34 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_35 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_36 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_37 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_38 => x"3B0000000010C810C8000000000C1FC77E95C77E85C77E47C77E03C77E21C77E",
    INIT_39 => x"8E3B02EE1031CCBF0727846D34CC7CFCCC8E112634CC7D02EF1031CCBE101A12",
    INIT_3a => x"A702354088EDC0CA8E01EC023484A619C7BE29271BC77D101AF12034CC7FF8CC",
    INIT_3b => x"FCCC7FEDD39FADEF1CF620123F1129271EC77D3B886F22886F02A70186846F03",
    INIT_3c => x"27261DC77D39EF1C30CC7F3930CC7CF420123F11052730CC7D101AF420123F11",
    INIT_3d => x"03260D8102353C8D0A8602340F270A811CC77F0F271CC77D1F2606D4BDC0CA8E",
    INIT_3e => x"036A0527036D19C7BE1DC77F228D12C7B6278D0A862B8D0D86D420318D1CC7B7",
    INIT_3f => x"F620123F11052BD8CCBD47C77E1BC77A19C7BF10C88E032640C88C043047C77E"
    )

    port map (
	  do     => rdata0,
	  dop(0) => dp0,
	  addr   => addr(10 downto 0),
	  clk    => clk,
	  di     => wdata,
	  dip(0) => dp0,
	  en     => cs0,
	  ssr    => rst,
	  we     => we
	);

  FLEX1 : RAMB16_S9
    generic map ( 
    INIT_00 => x"0000000000000000000000000000000000000000000000000000000000E4CC7E",
    INIT_01 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_02 => x"00000000000000000000000000000000005458540050555452415453000000FF",
    INIT_03 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_04 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_05 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_06 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_07 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_08 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_09 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_0a => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_0b => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_0c => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_0d => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_0e => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_0f => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_10 => x"CDBFFBD3BEF1D39FAD06CABFA10089302BCCBEFDD3B73986A0C039ACCABD0620",
    INIT_11 => x"2684E11284E784A6B9C6A0008E4FCDBFF7D3BE13CDBF10CDBFF9D3BE0DCDBF0A",
    INIT_12 => x"BD81CEBDDCCA8E16CCBF03CD8E2BCCBF5FFF8930EC2606CABC0004893084A70B",
    INIT_13 => x"CCBF80C08E67CD7E00D2BD01A6082706D4BD84A7018640C88EB5CEBD9E8DB5CE",
    INIT_14 => x"80C08EF4D1BD40C88EE7260D8180A714CCBED7D27E3034052706D4BD40C88E14",
    INIT_15 => x"EC250D8D0ECCB7F325148D2BCEBD81CEBDECCA8E2ED37E43CCBF67CD8E14CCBF",
    INIT_16 => x"3930383639011A39FE1C03275D1CCCB60B2575D1BD3910CCB7E525068D0FCCB7",
    INIT_17 => x"000004203F2959592C44442C4D4D2820455441440431302E33562058454C4620",
    INIT_18 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_19 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_1a => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_1b => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_1c => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_1d => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_1e => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_1f => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_20 => x"0000000000000000000000000000000000000001001BFF0000400400193A1808",
    INIT_21 => x"00000000000000000000000000000000010000FFBF0000000000000000000000",
    INIT_22 => x"274E4143043F54414857043F3F3F042B2B2B0000000060000000000000000000",
    INIT_23 => x"20524F525245204B53494404444E554F4620544F4E04524546534E4152542054",
    INIT_24 => x"004BD3004E4F4D0FD20054454704594441455220544F4E205345564952440423",
    INIT_25 => x"0000000000000000000000000000000000000000000000000A006400E8031027",
    INIT_26 => x"000000000070D37E00000000000000000000000000000000000000000070D37E",
    INIT_27 => x"0000000000000001000000000000000000000000000000000070D37E00000000",
    INIT_28 => x"CE7E2BCE7E41CF7EFBCE7E12CD7E0FCD7E0CCD7E09CD7EB1CD7E67CD7E57CD7E",
    INIT_29 => x"7ED4CF7E86CF7E49D37EEBD07EA2D17E36D07EEACD7E0ED07EB5CE7EEDCF7E81",
    INIT_2a => x"D4BD11CC7F7FC0CE1005CE7E05CE7E4ECD7E29D37E75D17ED0CF7E2FD17E7FD2",
    INIT_2b => x"9FAFE7D3BEE9D39FAF00C78E16CCBF03CD8E18DEBD7FC0CE10FDD3BD28CC7F00",
    INIT_2c => x"28CC7D142015CC7C052602CCB111CCB65A8D4CCC7F34CC7F31CCBFF8CC8EEBD3",
    INIT_2d => x"D0BD0DCC7C40C88EE5270D81D4D0BD7A8D81CEBD4ECC8EB22603D4BD97052610",
    INIT_2e => x"CCB7158656CC8E2ED2BD01986E0326358D072712CCBE09273E8D93CC8E162536",
    INIT_2f => x"7F21CC7F22CC7F23CC7F0ACDBF0DCDBE10CDBF13CDBE67CD7E11CC7F81CEBD20",
    INIT_30 => x"806D0C27A46DF026846D082680A1208002235F81A0A644C88E103924CC7F26CC",
    INIT_31 => x"0D811F2700CCB11D2701CCB1FBCEBD14CCBF80C08E39FB1CDE26846D0230FC26",
    INIT_32 => x"80C08CCE20268D52CC8E39DC260D8180A7E227FFC08CE7231F8132270A810D27",
    INIT_33 => x"20208641CFBD0D86B92066CFBD07CCB666CFBD20860826088107CCB61F30F427",
    INIT_34 => x"CC7F5C260ACCB1E5D39FAD65274ECDBDF320013041CFBD6F27048184A6328DC9",
    INIT_35 => x"2703CCB6D48D272621CC7D16CC9F6E11CC7FF326038150270ACCB1E5D39FAD1A",
    INIT_36 => x"CC7C0435FB265A088D052708CCF60434CE8D022709CC7D1ACC7F18221ACCB120",
    INIT_37 => x"1A2623CC7D39FE1C0435FA265A4E8D4F062705CCF60434588D0A865C8D0D861A",
    INIT_38 => x"1ACC7F0CCDBD032009CDBD0820538D0C2724CC7D11272FCC7D1A8D102726CC7D",
    INIT_39 => x"CD7E7FD2BD24CC7F3947CCBE042606D4BD24CCBE47CCBF062026CCBE47CCBF39",
    INIT_3a => x"CEBD062429CCB10B2704CCB6023429CC7C172029CC7F05221F81202621CC7D03",
    INIT_3b => x"03200FCDBD082626CC7D0D20B58D042724CC7D132622CC7D0234023529CC7CB5",
    INIT_3c => x"1FF7264DCC7A02300B8DA0CC8E84EC4DCCB704861DCCF74ACC7F39023512CDBD",
    INIT_3d => x"CC7D0B264ACC7D10264BCCB60234F4204BCC7C84A3072584A3104BCC7F3D2098",
    INIT_3e => x"44444444042084A6048D84A60130028D8235128D4ACC7C0520238D20860B271D",
    INIT_3f => x"2561810E235A810C254181162339811425308141CF7E078B02233981308B0F84"
    )

    port map (
	  do     => rdata1,
	  dop(0) => dp1,
	  addr   => addr(10 downto 0),
	  clk    => clk,
	  di     => wdata,
	  dip(0) => dp1,
	  en     => cs1,
	  ssr    => rst,
	  we     => we
	);

  FLEX2 : RAMB16_S9
    generic map ( 
    INIT_00 => x"0D8118CCB780A619CCB718CCB614CCBE103439FE1C3911CCB7011A06237A8104",
    INIT_01 => x"046F03A7FF8601A715869035B98DE72784A10426208114CCBF0B2702CCB11027",
    INIT_02 => x"BE6823238D6C273FCCBC092628252E8D0F262E25348D4BCCB70886D4D0BD0C6F",
    INIT_03 => x"393FCCBE0DCC7F03A70CCCB603200BCCB605270DCC7D0F2A036D6127046D3FCC",
    INIT_04 => x"4BCCF639FE1C2E812F240ED0BD03A70384382A036D3FCCBE1522398143258C8D",
    INIT_05 => x"04272D8108240ED0BD5A013004A72080022549CCB104354BCCF705C00434252B",
    INIT_06 => x"0426208184A614CCBE3FCCBFF6205A0130046FCB275D39011AE4265D06265F81",
    INIT_07 => x"03C6A5313D03C610220B810BD18E1018260CE63034393FCCBE14CCBFF6200130",
    INIT_08 => x"5243534B4142535953534142444D435458544E4942B035F7265A01300CA7A0A6",
    INIT_09 => x"7804C604341825268D22250ED0BD1ED2BD54554F545250524944434142544144",
    INIT_0a => x"8039FE1C1BCCBE39FB240ED0BDDF205C1CCCB71CCCBB0435F7265A1BCC791CCC",
    INIT_0b => x"223981DC250ED0BD1ED2BD39011A39FE1C032B0A8B072A078B042A068B0F2A47",
    INIT_0c => x"5C04351BCCFD0089E0EB1BCCF31BCCF34958495849581BCCFC023404340F84D2",
    INIT_0d => x"E5201DCCB701861FCCB72E8D1ECCB7338DF6261681152702813D8D1DCC7FD620",
    INIT_0e => x"5A3DCCBF80A73DCCBE0D8DD0274D891F148D3DCCFD1BCCF3891E1E8D891F228D",
    INIT_0f => x"39FE1C0D2606D4BD84A7048662320E26088101A6112706D4BD40C88EC120F326",
    INIT_10 => x"5F4FF120868D4CCC7C078D0F25358D0086E4CD7E738D39011A0326048120CCB7",
    INIT_11 => x"1ECC9F6E04271DCCF6A2D1BDEA8D228D028603CD7EAEFB27104CCCF6391BCCFD",
    INIT_12 => x"A7018640C88EEBD0BD40C88E1A25023536D0BD40C88E0234DECD7E81865CCC8E",
    INIT_13 => x"3439011A5DFB261002CCB107270D8111CCB6393B88A7FF86BB002510E4D1BD84",
    INIT_14 => x"0927026D40C88E61D38E105827108108262DCCBE10EACDBD632720CCB701A630",
    INIT_15 => x"2606D4BD84A7018603A70BCCB640C88E6E8D0BC638C88E2E2606D4BD84A70486",
    INIT_16 => x"3FCCBE81CEBD75CC8E1E2706D4BD84A715862188A720886F4C47474A20CCB616",
    INIT_17 => x"844A20CCB640C88EB5CEBDF62081CEBD82CC8EB03586CFBD5F846F01A720CCB6",
    INIT_18 => x"3034CD2006D4BD84A70486F4260D8141CFBDC92606D4BD2288E704CB3D3FC603",
    INIT_19 => x"FE1028CC7FB8CD7E28CC7C20CC7F45CCFF1043CCFD0635E1CD7E6BCC8E00D17E",
    INIT_1a => x"CD7E7FD2BD01A71B8640C88EF3D39F6E0426FCCC7D393A43CC9F6E20CCF645CC",
    INIT_1b => x"08F89F6E00F89F6E04F89F6E70D37E3900000000535953000053524F52524567",
    INIT_1c => x"00000000000000000000000000000000000000000000000006F89F6E0AF89F6E",
    INIT_1d => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_1e => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_1f => x"00CA7E84D380D37CD371D378D370D370D370D3C8DFC2DF70D374D30000000000",
    INIT_20 => x"204D455453595320474E4954415245504F204B534944207BD47E59D47E36D47E",
    INIT_21 => x"118D0AC609D48E15DEBDFF594220303839312029432820544847495259504F43",
    INIT_22 => x"2709D4BE09C7BD0CC77EFB265A806F1AC61BD48E1AD47F15D4BF13D4BF05008E",
    INIT_23 => x"0327FCCC7D39FFC60CC7BD026F0BD4BEEC242035C3DABD20340BD4BFE48830EB",
    INIT_24 => x"CC7D26250BD4BEB4D5BD112702C11A2702E6222684E6016F0BD4BF243409C7BD",
    INIT_25 => x"BE95ADCED48E585A0C2001C6042316C1142012C6ED20CFD6BD3924355F2326FC",
    INIT_26 => x"41D676D838D81DD8E2D5C3DAF9DAD9D986D9392435016D0CC7BD01E702240BD4",
    INIT_27 => x"02C60526208DE5DCFBDCBDDD07DB90D56FD510D887DA99D62CDBD6DB69D99FD6",
    INIT_28 => x"C30BD4FC39FE1C84ED94EC39011A0DC605270E8D39016F846F84AE84ED39011A",
    INIT_29 => x"2FC6028D5F4F0BD4BEEE2084AE39012684A31039FB1C032684AE1009D48E1C00",
    INIT_2a => x"04A60BC60BD4BE39F6265A01302488A704A60BC60BD4BE39F8265A01301188A7",
    INIT_2b => x"0BD4BE00D67E2388E679245402E60BD4BE39F0265A01300526E0A12488A60234",
    INIT_2c => x"E602E780CA552603C103C402E60BD4BE39011A1F265C4088A73A22886C2288E6",
    INIT_2d => x"20863B886A07273C2B3B88A639011A0BC639FE1C4088A73A2388E60A2680C50F",
    INIT_2e => x"1CE3274DD8203B88A70BD4BE0D25238D0C260981F627152218811B25318D1D20",
    INIT_2f => x"22886C0A272288E60BD4BE39011A12C6A0D97E84A7052701850925B1DABD39FE",
    INIT_30 => x"1C270000831020886C032621886C4088EC0BD4BE39EA24038D39FE1C4088A63A",
    INIT_31 => x"011A08C6022009C6062010C6042780C51024138D02352288A7048602341E88ED",
    INIT_32 => x"D4BE39EE240435178D043439FE1C032600DEBD118D12250CDEBD0BD4BE258D39",
    INIT_33 => x"052707C15C11D4F6242680C5112610C53912D4B711D4B74F394088301E88EC0B",
    INIT_34 => x"8D39011A39FE1C09DEBD0BD4BE12D4F70C2704C15C12D4F611D47F142011D4F7",
    INIT_35 => x"2640C5322706DEBD372735D4B60A2603DEBDB08D0BD4BE20250CDEBD0BD4BEC7",
    INIT_36 => x"C13B88E75C0F2620813D2B3B88E60BD4BE39011A20C639E0240435AB8D04340B",
    INIT_37 => x"148D098610202086042601C1023439FE1CF62027275D39E6240B8D0D200C267F",
    INIT_38 => x"E6D1FE261002C102E60BD4BE390235038D3B886F3B88A60BD4BE02340F250235",
    INIT_39 => x"D4BE39FE1C2288E70BD4BE04C60A247DD5BD0F250235218D0234082604C12288",
    INIT_3a => x"2625E08D2A253F8D17886F44271788E622261288E627204288ED2088ED5F4F0B",
    INIT_3b => x"7E1A249FD6BD4088ED0BD4BE0E8D82DC7E1188EC1788E702C60BD4BE2225DC8D",
    INIT_3c => x"1A07C60526E78D39846D1BD4BF3A1DD48E3D068603E60BD4BE3984EC038DC1DB",
    INIT_3d => x"43DCBD0B2717886D15886C032616886C1188ED032612886D1388ED0BD4BE3901",
    INIT_3e => x"6F026F0A2684ED0635B08D06344088EC0BD4BED22520D6BD1388EC0BD4BEDD25",
    INIT_3f => x"304088A75F20886C032621886C0BD4BE4F04AF103F3104AE100820056F046F03"
    )

    port map (
	  do     => rdata2,
	  dop(0) => dp2,
	  addr   => addr(10 downto 0),
	  clk    => clk,
	  di     => wdata,
	  dip(0) => dp2,
	  en     => cs2,
	  ssr    => rst,
	  we     => we
	);

  FLEX3 : RAMB16_S9
    generic map (
    INIT_00 => x"13D4F613D4BF15D4BE0E2003C604345F39FE1C4288ED2088EC0BD4BEF8265A01",
    INIT_01 => x"1D262288E60BD4BE392288E75F18D47F4088E704354188E70BD4BE14D4F60434",
    INIT_02 => x"2288A62F88ED1E88EC2288A7108618D4FD44DC052618D47D0BD4BE30250CD6BD",
    INIT_03 => x"C62288A73188A60BD4BE39FE1CF2265A013004A71435F5D5BD143418C63188A7",
    INIT_04 => x"1AD47D17D4B62388A703A60BD4BE9FD67EF2265A0130143514D7BD04A6143418",
    INIT_05 => x"88A60BD4BEEF2013D4BF18D4BE3723228D0C2705008C13D4BF15D4BE03A73126",
    INIT_06 => x"BD1DD8BD49D5BD1AD47F0BD4BEF220ADDDBD1C23078D3625BDDDBD0E2A03A723",
    INIT_07 => x"058D39FE1CE42659D5BD0F8D022A0C2704A60BD4BE39011A182708C1072438D8",
    INIT_08 => x"1525188D172687D7BD393488A73188A63288ED2F88EC0C263388A639FE1CFB1C",
    INIT_09 => x"D4BE08250CD6BD10D8BD39FE1CF6265A80A721315DA8A61BD4BE0BD4BE1006C6",
    INIT_0a => x"265A21315DA8A780A61BD4BE0BD4BE1006C6F825EA8D87D7BD392288E710C60B",
    INIT_0b => x"052476D8BD082541D6BD1E88ED2F88EC02A702860BD4BEC1DB7EE0249FD6BDF6",
    INIT_0c => x"292620850FA606271AD47D0BD4BE3B26382592D8BD3D25FAD4BD390AC6C1DB7E",
    INIT_0d => x"BEF4265A0C2504350CD6BD043413271788E676DABD4088ED1188EC2A25C0DCBD",
    INIT_0e => x"082A036D0BD4BE39011A04350CD5BD043404C6022011C639FE1C2288E75F0BD4",
    INIT_0f => x"CF2003C60426D52592D8BDDA2517D9BD37D5BDE225FAD4BD3910C60324BDDDBD",
    INIT_10 => x"A73488A62F88ED27273288EC0BD4BEF9265A01300F6F0AC60BD4BECA25C0DCBD",
    INIT_11 => x"39FE1C2288A704863E8D992569D9BDADDDBD1B88A710CCB61988ED0ECCFC3188",
    INIT_12 => x"D97EC1DBBD06249FD6BD082572D7BD0D2520D6BD2F88EC12886C17886F0BD4BE",
    INIT_13 => x"6F846F02A784A60BD4BE8A20EA2547D9BD3488A710863288ED1E88EC0BD4BECF",
    INIT_14 => x"8102A60BD4BE39FE1C2288E704C67AFB251044846F0E25288D392288A74F3B88",
    INIT_15 => x"12C6F023038102A60BD4BE0D25E78D39FE1C130125109FD6BD02A703860B2683",
    INIT_16 => x"25C88D1920A7DBBD05261288A60CD57E026F0BD4BE082702813125EC8D39011A",
    INIT_17 => x"D6BD282586D9BD39D32447D9BD052569D9BD0A2598DCBD052717886D0BD4BE17",
    INIT_18 => x"BE0286092520D6BD1388EC122680850FA60BD4BE1A2586D9BD1820038623250C",
    INIT_19 => x"3004A72488A60BC60BD4BE24272A2592D8BD358D39011A0BC639FE1C02A70BD4",
    INIT_1a => x"0CC639011A03C655200C8D09266085D62680850FA60BD4BE15254D8DF6265A01",
    INIT_1b => x"A60BD4BEEF2611D47A013004E73588A73588E604A611D4B70B860BD4BE39011A",
    INIT_1c => x"1C0BD4BE0626072592D8BDCE8D390BD4BEF6265A01300CA73D88A603C60C260C",
    INIT_1d => x"249FD6BD4088ED3902A700860BD4BE69D9BD04A7FF860BD4BE39011A04C639FE",
    INIT_1e => x"BE5A25B88D5E2517D9BD39011A0AC602200BC6062010C60A2780C5082640C514",
    INIT_1f => x"D4BE33271188EC0BD4BE0F2602EC1BD4BE87D7BD52266085522680850FA60BD4",
    INIT_20 => x"BE1388EC0BD4BE2025A28D1C271188EC0BD4BE2C2520D6BD0BD4BE142084ED1B",
    INIT_21 => x"0CC602200BC63947D9BD0325A7DBBD04ED04E31BD4BE1588EC0BD4BE02ED1BD4",
    INIT_22 => x"3788A74C0727FF813788A60E261388A3104C01C603233C88E15C1E88EC39011A",
    INIT_23 => x"EC39011A17C605271188A3101E88EC1626038B3A88A60BD4BE3025338D39FE1C",
    INIT_24 => x"F72520D6BD3888EC39FE1C3788A701863588ED1388EC3A88A704863888ED4088",
    INIT_25 => x"C1DB7EDA249FD6BDF3265A01304088A7213135A8A603C63A3A88E6121F0BD4BE",
    INIT_26 => x"BEF8265A013040886F5F3C88A76788A62088ED5F4F0BD4BE46250CD6BD10D8BD",
    INIT_27 => x"0E25B1DABD2088ED9ADD7E032A0100832088EC1D271788A60BD4BE39FE1C0BD4",
    INIT_28 => x"EE259FDDBD6A272088AE101188EC11D47F39011A12C605261788A6846F072446",
    INIT_29 => x"11D4B6023403300FD4BE2C242088A3100BD4BE0FD4BF008902EB7427026D5F4F",
    INIT_2a => x"023504353E25458D4088EC0BD4BE0434D2204C270235A8810827548111D4B74C",
    INIT_2b => x"E006233C88E105250BD4BE01EB84A6891F4AE0A0043402A60FD4BE2088A3C020",
    INIT_2c => x"BD39011A18C6022019C614272088A3104288EC0BD4BE142520D6BDF5204C3C88",
    INIT_2d => x"0BD4BE39F6265A013004A72488A60BC60BD4BE39FE1C3A44C60BD4BE082520D6",
    INIT_2e => x"000000000039011A10C639E82512DEBD03200FDEBD052603A70F2404814C03A6",
    INIT_2f => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_30 => x"CC0038DE7E52DE7E1FDE7EA4DE7EA4DE7E98DE7E33DE7E96DE7E74DE7E53DE7E",
    INIT_31 => x"E606E1FD4F02345A01C64F638D75200CE1FDE000CC1EE1FD0200CC1EE1FD0600",
    INIT_32 => x"1020344B8D0EE1FD2000CCE38D395F023504E1FD01C60AE1FD1EDEF608E1FDE4",
    INIT_33 => x"20342A8D0EE1FD3000CCC28D395F368D2035F5263F3180E700E1FC528D00018E",
    INIT_34 => x"B74F0123038103A6395F395F148D2035F5263F3100E1FD80E6308D4F00018E10",
    INIT_35 => x"0039F92708C50EE1FC39F22740C50EE1FCF92680C50EE1FC395F03A6395F1EDE",
    INIT_36 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_37 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_38 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_39 => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_3a => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_3b => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_3c => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_3d => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_3e => x"0000000000000000000000000000000000000000000000000000000000000000",
    INIT_3f => x"0000000000000000000000000000000000000000000000000000000000000000"
   )

    port map (
	  do     => rdata3,
	  dop(0) => dp3,
	  addr   => addr(10 downto 0),
	  clk    => clk,
	  di     => wdata,
	  dip(0) => dp3,
	  en     => cs3,
	  ssr    => rst,
	  we     => we
	);

my_flex : process ( rw, addr, cs, rdata0, rdata1, rdata2, rdata3 )
begin
	 we    <= not rw;
	 case addr(12 downto 11) is
	 when "00" =>
		cs0   <= cs;
		cs1   <= '0';
		cs2   <= '0';
		cs3   <= '0';
		rdata <= rdata0;
    when "01" =>
		cs0   <= '0';
		cs1   <= cs;
		cs2   <= '0';
		cs3   <= '0';
		rdata <= rdata1;
	 when "10" =>
		cs0   <= '0';
		cs1   <= '0';
		cs2   <= cs;
		cs3   <= '0';
		rdata <= rdata2;
    when "11" =>
		cs0   <= '0';
		cs1   <= '0';
		cs2   <= '0';
		cs3   <= cs;
		rdata <= rdata3;
    when others =>
		null;
    end case;		
		
end process;

end architecture rtl;

