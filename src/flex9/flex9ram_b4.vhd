library IEEE;
   use IEEE.std_logic_1164.all;
   use IEEE.std_logic_arith.all;
library unisim;
   use unisim.vcomponents.all;

entity FLEX9_C000 is
   port(
      clk    : in  std_logic;
      rst    : in  std_logic;
      cs     : in  std_logic;
      rw     : in  std_logic;
      addr   : in  std_logic_vector(10 downto 0);
      rdata  : out std_logic_vector(7 downto 0);
      wdata  : in  std_logic_vector(7 downto 0)
   );
end FLEX9_C000;

architecture rtl of FLEX9_C000 is

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
         INIT_0f => x"0000000000000000000000000000000000000000000000000000000000000000"
      )
      port map (
         clk     => clk,
         en      => en(0),
         we      => we,
         rst     => rst,
         addr    => addr(8 downto 0),
         di      => wdata,
         do      => xdata(0)
      );

   ROM01: RAMB4_S8
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
         INIT_0f => x"0000000000000000000000000000000000000000000000000000000000000000"
      )
      port map (
         clk     => clk,
         en      => en(1),
         we      => we,
         rst     => rst,
         addr    => addr(8 downto 0),
         di      => wdata,
         do      => xdata(1)
      );

   ROM02: RAMB4_S8
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
         INIT_0f => x"0000000000000000000000000000000000000000000000000000000000000000"
      )
      port map (
         clk     => clk,
         en      => en(2),
         we      => we,
         rst     => rst,
         addr    => addr(8 downto 0),
         di      => wdata,
         do      => xdata(2)
      );

   ROM03: RAMB4_S8
      generic map (
         INIT_00 => x"0000000000000000000000000000000000000000000000000000000000000000",
         INIT_01 => x"0000000000000000000000000000000000000000000000000000000000000000",
         INIT_02 => x"0000000000000000000000000000000000000000000000000000000000000000",
         INIT_03 => x"0000000000000000000000000000000000000000000000000000000000000000",
         INIT_04 => x"0000000000000000000000000000000000000000000000000000000000000000",
         INIT_05 => x"0000000000000000000000000000000000000000000000000000000000000000",
         INIT_06 => x"0000000000000000000000000000000000000000000000000000000000000000",
         INIT_07 => x"0000000000000000000000000000000000000000000000000000000000000000",
         INIT_08 => x"3B0000000010C810C8000000000C1FC77E95C77E85C77E47C77E03C77E21C77E",
         INIT_09 => x"8E3B02EE1031CCBF0727846D34CC7CFCCC8E112634CC7D02EF1031CCBE101A12",
         INIT_0a => x"A702354088EDC0CA8E01EC023484A619C7BE29271BC77D101AF12034CC7FF8CC",
         INIT_0b => x"FCCC7FEDD39FADEF1CF620123F1129271EC77D3B886F22886F02A70186846F03",
         INIT_0c => x"27261DC77D39EF1C30CC7F3930CC7CF420123F11052730CC7D101AF420123F11",
         INIT_0d => x"03260D8102353C8D0A8602340F270A811CC77F0F271CC77D1F2606D4BDC0CA8E",
         INIT_0e => x"036A0527036D19C7BE1DC77F228D12C7B6278D0A862B8D0D86D420318D1CC7B7",
         INIT_0f => x"F620123F11052BD8CCBD47C77E1BC77A19C7BF10C88E032640C88C043047C77E"
      )
      port map (
         clk     => clk,
         en      => en(3),
         we      => we,
         rst     => rst,
         addr    => addr(8 downto 0),
         di      => wdata,
         do      => xdata(3)
      );

   rom_glue: process (cs, rw, addr, xdata)
   begin
      en <= (others=>'0');
      case addr(10 downto 9) is
      when "00" =>
         en(0)  <= cs;
         rdata  <= xdata(0);
      when "01" =>
         en(1)  <= cs;
         rdata  <= xdata(1);
      when "10" =>
         en(2)  <= cs;
         rdata  <= xdata(2);
      when "11" =>
         en(3)  <= cs;
         rdata  <= xdata(3);
      when others =>
         null;
      end case;
      we <= not rw;
   end process;
end architecture rtl;

library IEEE;
   use IEEE.std_logic_1164.all;
   use IEEE.std_logic_arith.all;
library unisim;
   use unisim.vcomponents.all;

entity FLEX9_C800 is
   port(
      clk    : in  std_logic;
      rst    : in  std_logic;
      cs     : in  std_logic;
      rw     : in  std_logic;
      addr   : in  std_logic_vector(10 downto 0);
      rdata  : out std_logic_vector(7 downto 0);
      wdata  : in  std_logic_vector(7 downto 0)
   );
end FLEX9_C800;

architecture rtl of FLEX9_C800 is

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
         INIT_0f => x"0000000000000000000000000000000000000000000000000000000000000000"
      )
      port map (
         clk     => clk,
         en      => en(0),
         we      => we,
         rst     => rst,
         addr    => addr(8 downto 0),
         di      => wdata,
         do      => xdata(0)
      );

   ROM01: RAMB4_S8
      generic map (
         INIT_00 => x"CDBFFBD3BEF1D39FAD06CABFA10089302BCCBEFDD3B73986A0C039ACCABD0620",
         INIT_01 => x"2684E11284E784A6B9C6A0008E4FCDBFF7D3BE13CDBF10CDBFF9D3BE0DCDBF0A",
         INIT_02 => x"BD81CEBDDCCA8E16CCBF03CD8E2BCCBF5FFF8930EC2606CABC0004893084A70B",
         INIT_03 => x"CCBF80C08E67CD7E00D2BD01A6082706D4BD84A7018640C88EB5CEBD9E8DB5CE",
         INIT_04 => x"80C08EF4D1BD40C88EE7260D8180A714CCBED7D27E3034052706D4BD40C88E14",
         INIT_05 => x"EC250D8D0ECCB7F325148D2BCEBD81CEBDECCA8E2ED37E43CCBF67CD8E14CCBF",
         INIT_06 => x"3930383639011A39FE1C03275D1CCCB60B2575D1BD3910CCB7E525068D0FCCB7",
         INIT_07 => x"000004203F2959592C44442C4D4D2820455441440431302E33562058454C4620",
         INIT_08 => x"0000000000000000000000000000000000000000000000000000000000000000",
         INIT_09 => x"0000000000000000000000000000000000000000000000000000000000000000",
         INIT_0a => x"0000000000000000000000000000000000000000000000000000000000000000",
         INIT_0b => x"0000000000000000000000000000000000000000000000000000000000000000",
         INIT_0c => x"0000000000000000000000000000000000000000000000000000000000000000",
         INIT_0d => x"0000000000000000000000000000000000000000000000000000000000000000",
         INIT_0e => x"0000000000000000000000000000000000000000000000000000000000000000",
         INIT_0f => x"0000000000000000000000000000000000000000000000000000000000000000"
      )
      port map (
         clk     => clk,
         en      => en(1),
         we      => we,
         rst     => rst,
         addr    => addr(8 downto 0),
         di      => wdata,
         do      => xdata(1)
      );

   ROM02: RAMB4_S8
      generic map (
         INIT_00 => x"0000000000000000000000000000000000000001001BFF0000400400193A1808",
         INIT_01 => x"00000000000000000000000000000000010000FFBF0000000000000000000000",
         INIT_02 => x"274E4143043F54414857043F3F3F042B2B2B0000000060000000000000000000",
         INIT_03 => x"20524F525245204B53494404444E554F4620544F4E04524546534E4152542054",
         INIT_04 => x"004BD3004E4F4D0FD20054454704594441455220544F4E205345564952440423",
         INIT_05 => x"0000000000000000000000000000000000000000000000000A006400E8031027",
         INIT_06 => x"000000000070D37E00000000000000000000000000000000000000000070D37E",
         INIT_07 => x"0000000000000001000000000000000000000000000000000070D37E00000000",
         INIT_08 => x"CE7E2BCE7E41CF7EFBCE7E12CD7E0FCD7E0CCD7E09CD7EB1CD7E67CD7E57CD7E",
         INIT_09 => x"7ED4CF7E86CF7E49D37EEBD07EA2D17E36D07EEACD7E0ED07EB5CE7EEDCF7E81",
         INIT_0a => x"D4BD11CC7F7FC0CE1005CE7E05CE7E4ECD7E29D37E75D17ED0CF7E2FD17E7FD2",
         INIT_0b => x"9FAFE7D3BEE9D39FAF00C78E16CCBF03CD8E18DEBD7FC0CE10FDD3BD28CC7F00",
         INIT_0c => x"28CC7D142015CC7C052602CCB111CCB65A8D4CCC7F34CC7F31CCBFF8CC8EEBD3",
         INIT_0d => x"D0BD0DCC7C40C88EE5270D81D4D0BD7A8D81CEBD4ECC8EB22603D4BD97052610",
         INIT_0e => x"CCB7158656CC8E2ED2BD01986E0326358D072712CCBE09273E8D93CC8E162536",
         INIT_0f => x"7F21CC7F22CC7F23CC7F0ACDBF0DCDBE10CDBF13CDBE67CD7E11CC7F81CEBD20"
      )
      port map (
         clk     => clk,
         en      => en(2),
         we      => we,
         rst     => rst,
         addr    => addr(8 downto 0),
         di      => wdata,
         do      => xdata(2)
      );

   ROM03: RAMB4_S8
      generic map (
         INIT_00 => x"806D0C27A46DF026846D082680A1208002235F81A0A644C88E103924CC7F26CC",
         INIT_01 => x"0D811F2700CCB11D2701CCB1FBCEBD14CCBF80C08E39FB1CDE26846D0230FC26",
         INIT_02 => x"80C08CCE20268D52CC8E39DC260D8180A7E227FFC08CE7231F8132270A810D27",
         INIT_03 => x"20208641CFBD0D86B92066CFBD07CCB666CFBD20860826088107CCB61F30F427",
         INIT_04 => x"CC7F5C260ACCB1E5D39FAD65274ECDBDF320013041CFBD6F27048184A6328DC9",
         INIT_05 => x"2703CCB6D48D272621CC7D16CC9F6E11CC7FF326038150270ACCB1E5D39FAD1A",
         INIT_06 => x"CC7C0435FB265A088D052708CCF60434CE8D022709CC7D1ACC7F18221ACCB120",
         INIT_07 => x"1A2623CC7D39FE1C0435FA265A4E8D4F062705CCF60434588D0A865C8D0D861A",
         INIT_08 => x"1ACC7F0CCDBD032009CDBD0820538D0C2724CC7D11272FCC7D1A8D102726CC7D",
         INIT_09 => x"CD7E7FD2BD24CC7F3947CCBE042606D4BD24CCBE47CCBF062026CCBE47CCBF39",
         INIT_0a => x"CEBD062429CCB10B2704CCB6023429CC7C172029CC7F05221F81202621CC7D03",
         INIT_0b => x"03200FCDBD082626CC7D0D20B58D042724CC7D132622CC7D0234023529CC7CB5",
         INIT_0c => x"1FF7264DCC7A02300B8DA0CC8E84EC4DCCB704861DCCF74ACC7F39023512CDBD",
         INIT_0d => x"CC7D0B264ACC7D10264BCCB60234F4204BCC7C84A3072584A3104BCC7F3D2098",
         INIT_0e => x"44444444042084A6048D84A60130028D8235128D4ACC7C0520238D20860B271D",
         INIT_0f => x"2561810E235A810C254181162339811425308141CF7E078B02233981308B0F84"
      )
      port map (
         clk     => clk,
         en      => en(3),
         we      => we,
         rst     => rst,
         addr    => addr(8 downto 0),
         di      => wdata,
         do      => xdata(3)
      );

   rom_glue: process (cs, rw, addr, xdata)
   begin
      en <= (others=>'0');
      case addr(10 downto 9) is
      when "00" =>
         en(0)  <= cs;
         rdata  <= xdata(0);
      when "01" =>
         en(1)  <= cs;
         rdata  <= xdata(1);
      when "10" =>
         en(2)  <= cs;
         rdata  <= xdata(2);
      when "11" =>
         en(3)  <= cs;
         rdata  <= xdata(3);
      when others =>
         null;
      end case;
      we <= not rw;
   end process;
end architecture rtl;

library IEEE;
   use IEEE.std_logic_1164.all;
   use IEEE.std_logic_arith.all;
library unisim;
   use unisim.vcomponents.all;

entity FLEX9_D000 is
   port(
      clk    : in  std_logic;
      rst    : in  std_logic;
      cs     : in  std_logic;
      rw     : in  std_logic;
      addr   : in  std_logic_vector(10 downto 0);
      rdata  : out std_logic_vector(7 downto 0);
      wdata  : in  std_logic_vector(7 downto 0)
   );
end FLEX9_D000;

architecture rtl of FLEX9_D000 is

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
         INIT_0f => x"39FE1C0D2606D4BD84A7048662320E26088101A6112706D4BD40C88EC120F326"
      )
      port map (
         clk     => clk,
         en      => en(0),
         we      => we,
         rst     => rst,
         addr    => addr(8 downto 0),
         di      => wdata,
         do      => xdata(0)
      );

   ROM01: RAMB4_S8
      generic map (
         INIT_00 => x"5F4FF120868D4CCC7C078D0F25358D0086E4CD7E738D39011A0326048120CCB7",
         INIT_01 => x"1ECC9F6E04271DCCF6A2D1BDEA8D228D028603CD7EAEFB27104CCCF6391BCCFD",
         INIT_02 => x"A7018640C88EEBD0BD40C88E1A25023536D0BD40C88E0234DECD7E81865CCC8E",
         INIT_03 => x"3439011A5DFB261002CCB107270D8111CCB6393B88A7FF86BB002510E4D1BD84",
         INIT_04 => x"0927026D40C88E61D38E105827108108262DCCBE10EACDBD632720CCB701A630",
         INIT_05 => x"2606D4BD84A7018603A70BCCB640C88E6E8D0BC638C88E2E2606D4BD84A70486",
         INIT_06 => x"3FCCBE81CEBD75CC8E1E2706D4BD84A715862188A720886F4C47474A20CCB616",
         INIT_07 => x"844A20CCB640C88EB5CEBDF62081CEBD82CC8EB03586CFBD5F846F01A720CCB6",
         INIT_08 => x"3034CD2006D4BD84A70486F4260D8141CFBDC92606D4BD2288E704CB3D3FC603",
         INIT_09 => x"FE1028CC7FB8CD7E28CC7C20CC7F45CCFF1043CCFD0635E1CD7E6BCC8E00D17E",
         INIT_0a => x"CD7E7FD2BD01A71B8640C88EF3D39F6E0426FCCC7D393A43CC9F6E20CCF645CC",
         INIT_0b => x"08F89F6E00F89F6E04F89F6E70D37E3900000000535953000053524F52524567",
         INIT_0c => x"00000000000000000000000000000000000000000000000006F89F6E0AF89F6E",
         INIT_0d => x"0000000000000000000000000000000000000000000000000000000000000000",
         INIT_0e => x"0000000000000000000000000000000000000000000000000000000000000000",
         INIT_0f => x"00CA7E84D380D37CD371D378D370D370D370D3C8DFC2DF70D374D30000000000"
      )
      port map (
         clk     => clk,
         en      => en(1),
         we      => we,
         rst     => rst,
         addr    => addr(8 downto 0),
         di      => wdata,
         do      => xdata(1)
      );

   ROM02: RAMB4_S8
      generic map (
         INIT_00 => x"204D455453595320474E4954415245504F204B534944207BD47E59D47E36D47E",
         INIT_01 => x"118D0AC609D48E15DEBDFF594220303839312029432820544847495259504F43",
         INIT_02 => x"2709D4BE09C7BD0CC77EFB265A806F1AC61BD48E1AD47F15D4BF13D4BF05008E",
         INIT_03 => x"0327FCCC7D39FFC60CC7BD026F0BD4BEEC242035C3DABD20340BD4BFE48830EB",
         INIT_04 => x"CC7D26250BD4BEB4D5BD112702C11A2702E6222684E6016F0BD4BF243409C7BD",
         INIT_05 => x"BE95ADCED48E585A0C2001C6042316C1142012C6ED20CFD6BD3924355F2326FC",
         INIT_06 => x"41D676D838D81DD8E2D5C3DAF9DAD9D986D9392435016D0CC7BD01E702240BD4",
         INIT_07 => x"02C60526208DE5DCFBDCBDDD07DB90D56FD510D887DA99D62CDBD6DB69D99FD6",
         INIT_08 => x"C30BD4FC39FE1C84ED94EC39011A0DC605270E8D39016F846F84AE84ED39011A",
         INIT_09 => x"2FC6028D5F4F0BD4BEEE2084AE39012684A31039FB1C032684AE1009D48E1C00",
         INIT_0a => x"04A60BC60BD4BE39F6265A01302488A704A60BC60BD4BE39F8265A01301188A7",
         INIT_0b => x"0BD4BE00D67E2388E679245402E60BD4BE39F0265A01300526E0A12488A60234",
         INIT_0c => x"E602E780CA552603C103C402E60BD4BE39011A1F265C4088A73A22886C2288E6",
         INIT_0d => x"20863B886A07273C2B3B88A639011A0BC639FE1C4088A73A2388E60A2680C50F",
         INIT_0e => x"1CE3274DD8203B88A70BD4BE0D25238D0C260981F627152218811B25318D1D20",
         INIT_0f => x"22886C0A272288E60BD4BE39011A12C6A0D97E84A7052701850925B1DABD39FE"
      )
      port map (
         clk     => clk,
         en      => en(2),
         we      => we,
         rst     => rst,
         addr    => addr(8 downto 0),
         di      => wdata,
         do      => xdata(2)
      );

   ROM03: RAMB4_S8
      generic map (
         INIT_00 => x"1C270000831020886C032621886C4088EC0BD4BE39EA24038D39FE1C4088A63A",
         INIT_01 => x"011A08C6022009C6062010C6042780C51024138D02352288A7048602341E88ED",
         INIT_02 => x"D4BE39EE240435178D043439FE1C032600DEBD118D12250CDEBD0BD4BE258D39",
         INIT_03 => x"052707C15C11D4F6242680C5112610C53912D4B711D4B74F394088301E88EC0B",
         INIT_04 => x"8D39011A39FE1C09DEBD0BD4BE12D4F70C2704C15C12D4F611D47F142011D4F7",
         INIT_05 => x"2640C5322706DEBD372735D4B60A2603DEBDB08D0BD4BE20250CDEBD0BD4BEC7",
         INIT_06 => x"C13B88E75C0F2620813D2B3B88E60BD4BE39011A20C639E0240435AB8D04340B",
         INIT_07 => x"148D098610202086042601C1023439FE1CF62027275D39E6240B8D0D200C267F",
         INIT_08 => x"E6D1FE261002C102E60BD4BE390235038D3B886F3B88A60BD4BE02340F250235",
         INIT_09 => x"D4BE39FE1C2288E70BD4BE04C60A247DD5BD0F250235218D0234082604C12288",
         INIT_0a => x"2625E08D2A253F8D17886F44271788E622261288E627204288ED2088ED5F4F0B",
         INIT_0b => x"7E1A249FD6BD4088ED0BD4BE0E8D82DC7E1188EC1788E702C60BD4BE2225DC8D",
         INIT_0c => x"1A07C60526E78D39846D1BD4BF3A1DD48E3D068603E60BD4BE3984EC038DC1DB",
         INIT_0d => x"43DCBD0B2717886D15886C032616886C1188ED032612886D1388ED0BD4BE3901",
         INIT_0e => x"6F026F0A2684ED0635B08D06344088EC0BD4BED22520D6BD1388EC0BD4BEDD25",
         INIT_0f => x"304088A75F20886C032621886C0BD4BE4F04AF103F3104AE100820056F046F03"
      )
      port map (
         clk     => clk,
         en      => en(3),
         we      => we,
         rst     => rst,
         addr    => addr(8 downto 0),
         di      => wdata,
         do      => xdata(3)
      );

   rom_glue: process (cs, rw, addr, xdata)
   begin
      en <= (others=>'0');
      case addr(10 downto 9) is
      when "00" =>
         en(0)  <= cs;
         rdata  <= xdata(0);
      when "01" =>
         en(1)  <= cs;
         rdata  <= xdata(1);
      when "10" =>
         en(2)  <= cs;
         rdata  <= xdata(2);
      when "11" =>
         en(3)  <= cs;
         rdata  <= xdata(3);
      when others =>
         null;
      end case;
      we <= not rw;
   end process;
end architecture rtl;

library IEEE;
   use IEEE.std_logic_1164.all;
   use IEEE.std_logic_arith.all;
library unisim;
   use unisim.vcomponents.all;

entity FLEX9_D800 is
   port(
      clk    : in  std_logic;
      rst    : in  std_logic;
      cs     : in  std_logic;
      rw     : in  std_logic;
      addr   : in  std_logic_vector(10 downto 0);
      rdata  : out std_logic_vector(7 downto 0);
      wdata  : in  std_logic_vector(7 downto 0)
   );
end FLEX9_D800;

architecture rtl of FLEX9_D800 is

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
         INIT_0f => x"CF2003C60426D52592D8BDDA2517D9BD37D5BDE225FAD4BD3910C60324BDDDBD"
      )
      port map (
         clk     => clk,
         en      => en(0),
         we      => we,
         rst     => rst,
         addr    => addr(8 downto 0),
         di      => wdata,
         do      => xdata(0)
      );

   ROM01: RAMB4_S8
      generic map (
         INIT_00 => x"A73488A62F88ED27273288EC0BD4BEF9265A01300F6F0AC60BD4BECA25C0DCBD",
         INIT_01 => x"39FE1C2288A704863E8D992569D9BDADDDBD1B88A710CCB61988ED0ECCFC3188",
         INIT_02 => x"D97EC1DBBD06249FD6BD082572D7BD0D2520D6BD2F88EC12886C17886F0BD4BE",
         INIT_03 => x"6F846F02A784A60BD4BE8A20EA2547D9BD3488A710863288ED1E88EC0BD4BECF",
         INIT_04 => x"8102A60BD4BE39FE1C2288E704C67AFB251044846F0E25288D392288A74F3B88",
         INIT_05 => x"12C6F023038102A60BD4BE0D25E78D39FE1C130125109FD6BD02A703860B2683",
         INIT_06 => x"25C88D1920A7DBBD05261288A60CD57E026F0BD4BE082702813125EC8D39011A",
         INIT_07 => x"D6BD282586D9BD39D32447D9BD052569D9BD0A2598DCBD052717886D0BD4BE17",
         INIT_08 => x"BE0286092520D6BD1388EC122680850FA60BD4BE1A2586D9BD1820038623250C",
         INIT_09 => x"3004A72488A60BC60BD4BE24272A2592D8BD358D39011A0BC639FE1C02A70BD4",
         INIT_0a => x"0CC639011A03C655200C8D09266085D62680850FA60BD4BE15254D8DF6265A01",
         INIT_0b => x"A60BD4BEEF2611D47A013004E73588A73588E604A611D4B70B860BD4BE39011A",
         INIT_0c => x"1C0BD4BE0626072592D8BDCE8D390BD4BEF6265A01300CA73D88A603C60C260C",
         INIT_0d => x"249FD6BD4088ED3902A700860BD4BE69D9BD04A7FF860BD4BE39011A04C639FE",
         INIT_0e => x"BE5A25B88D5E2517D9BD39011A0AC602200BC6062010C60A2780C5082640C514",
         INIT_0f => x"D4BE33271188EC0BD4BE0F2602EC1BD4BE87D7BD52266085522680850FA60BD4"
      )
      port map (
         clk     => clk,
         en      => en(1),
         we      => we,
         rst     => rst,
         addr    => addr(8 downto 0),
         di      => wdata,
         do      => xdata(1)
      );

   ROM02: RAMB4_S8
      generic map (
         INIT_00 => x"BE1388EC0BD4BE2025A28D1C271188EC0BD4BE2C2520D6BD0BD4BE142084ED1B",
         INIT_01 => x"0CC602200BC63947D9BD0325A7DBBD04ED04E31BD4BE1588EC0BD4BE02ED1BD4",
         INIT_02 => x"3788A74C0727FF813788A60E261388A3104C01C603233C88E15C1E88EC39011A",
         INIT_03 => x"EC39011A17C605271188A3101E88EC1626038B3A88A60BD4BE3025338D39FE1C",
         INIT_04 => x"F72520D6BD3888EC39FE1C3788A701863588ED1388EC3A88A704863888ED4088",
         INIT_05 => x"C1DB7EDA249FD6BDF3265A01304088A7213135A8A603C63A3A88E6121F0BD4BE",
         INIT_06 => x"BEF8265A013040886F5F3C88A76788A62088ED5F4F0BD4BE46250CD6BD10D8BD",
         INIT_07 => x"0E25B1DABD2088ED9ADD7E032A0100832088EC1D271788A60BD4BE39FE1C0BD4",
         INIT_08 => x"EE259FDDBD6A272088AE101188EC11D47F39011A12C605261788A6846F072446",
         INIT_09 => x"11D4B6023403300FD4BE2C242088A3100BD4BE0FD4BF008902EB7427026D5F4F",
         INIT_0a => x"023504353E25458D4088EC0BD4BE0434D2204C270235A8810827548111D4B74C",
         INIT_0b => x"E006233C88E105250BD4BE01EB84A6891F4AE0A0043402A60FD4BE2088A3C020",
         INIT_0c => x"BD39011A18C6022019C614272088A3104288EC0BD4BE142520D6BDF5204C3C88",
         INIT_0d => x"0BD4BE39F6265A013004A72488A60BC60BD4BE39FE1C3A44C60BD4BE082520D6",
         INIT_0e => x"000000000039011A10C639E82512DEBD03200FDEBD052603A70F2404814C03A6",
         INIT_0f => x"0000000000000000000000000000000000000000000000000000000000000000"
      )
      port map (
         clk     => clk,
         en      => en(2),
         we      => we,
         rst     => rst,
         addr    => addr(8 downto 0),
         di      => wdata,
         do      => xdata(2)
      );

   ROM03: RAMB4_S8
      generic map (
         INIT_00 => x"0000C3F07EBFF07EA7F07EA3F07E9FF07E6CF07E63F07E5FF07E5BF07E57F07E",
         INIT_01 => x"0000000000000000000000000000FFFF40100302010000000000000000000000",
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
         INIT_0f => x"0000000000000000000000000000000000000000000000000000000000000000"
      )
      port map (
         clk     => clk,
         en      => en(3),
         we      => we,
         rst     => rst,
         addr    => addr(8 downto 0),
         di      => wdata,
         do      => xdata(3)
      );

   rom_glue: process (cs, rw, addr, xdata)
   begin
      en <= (others=>'0');
      case addr(10 downto 9) is
      when "00" =>
         en(0)  <= cs;
         rdata  <= xdata(0);
      when "01" =>
         en(1)  <= cs;
         rdata  <= xdata(1);
      when "10" =>
         en(2)  <= cs;
         rdata  <= xdata(2);
      when "11" =>
         en(3)  <= cs;
         rdata  <= xdata(3);
      when others =>
         null;
      end case;
      we <= not rw;
   end process;
end architecture rtl;

