library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

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

architecture SYN of ram_2k is
signal we	: std_logic;

begin
	
	we <= cs and (not rw) and (not rst);
	
	ram_inst : entity work.spram
		generic map
		(
			INIT_FILE => "char_rom.mif",
			WORD_COUNT => 2048,
			ADDR_WIDTH => 11
		)
		port map
		(
			clk			=> clk,
			addr		=> addr,
			wren		=> we,
			data_in		=> data_in,
			data_out	=> data_out
		);

end SYN;
