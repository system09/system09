library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity char_rom is
    Port (
       clk		: in  std_logic;
       rst		: in  std_logic;
       cs		: in  std_logic;
       rw		: in  std_logic;
       addr 	: in  std_logic_vector (10 downto 0);
       data_in	: in  std_logic_vector (7 downto 0);
       data_out	: out std_logic_vector (7 downto 0)
    );
end char_rom;

architecture SYN of char_rom is
begin

	rom_inst : entity work.sprom
		generic map
		(
			INIT_FILE 	=> "char_rom.mif",
			WORD_COUNT	=> 2048,
			ADDR_WIDTH	=> 11
		)
		port map
		(
			clk			=> clk,
			addr		=> addr,
			data_in		=> data_in,
			data_out	=> data_out
		);

end SYN;

