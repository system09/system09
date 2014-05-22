library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity keymap_rom is
    Port (
       clk		: in  std_logic;
       rst		: in  std_logic;
       cs		: in  std_logic;
       rw		: in  std_logic;
       addr 	: in  std_logic_vector (8 downto 0);
       data_in	: in  std_logic_vector (7 downto 0);
       data_out	: out std_logic_vector (7 downto 0)
    );
end keymap_rom;

architecture SYN of keymap_rom is
begin
	
	rom_inst : entity work.sprom
		generic map
		(
			INIT_FILE 	=> "keymap_rom.mif",
			WORD_COUNT	=> 512,
			ADDR_WIDTH	=> 9
		)
		port map
		(
			clk			=> clk,
			addr		=> addr,
			data_in     => data_in,
			data_out	=> data_out
		);

end SYN;

