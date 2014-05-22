LIBRARY ieee;
USE ieee.std_logic_1164.all;

LIBRARY altera_mf;
USE altera_mf.all;

ENTITY spram IS
	GENERIC
	(
		INIT_FILE		: string	:= "";
		WORD_COUNT		: natural	:= 2048;
		ADDR_WIDTH		: natural	:= 11;
		DATA_WIDTH		: natural	:= 8;
		REG_OUT			: string	:= "UNREGISTERED"
	);
	PORT
	(
		clk				: IN  STD_LOGIC ;
		addr			: IN  STD_LOGIC_VECTOR (ADDR_WIDTH-1 DOWNTO 0);
		wren			: IN  STD_LOGIC ;
		data_in			: IN  STD_LOGIC_VECTOR (DATA_WIDTH-1 DOWNTO 0);
		data_out		: OUT STD_LOGIC_VECTOR (DATA_WIDTH-1 DOWNTO 0)
	);
END spram;

ARCHITECTURE SYN OF spram IS

	COMPONENT altsyncram
	GENERIC (
		clock_enable_input_a	: STRING;
		clock_enable_output_a	: STRING;
		init_file				: STRING;
		intended_device_family	: STRING;
		lpm_hint				: STRING;
		lpm_type				: STRING;
		numwords_a				: NATURAL;
		operation_mode			: STRING;
		outdata_aclr_a			: STRING;
		outdata_reg_a			: STRING;
		power_up_uninitialized	: STRING;
		widthad_a				: NATURAL;
		width_a					: NATURAL;
		width_byteena_a			: NATURAL
	);
	PORT (
		clock0		: IN  STD_LOGIC ;
		wren_a		: IN  STD_LOGIC ;
		address_a	: IN  STD_LOGIC_VECTOR (ADDR_WIDTH-1 DOWNTO 0);
		data_a		: IN  STD_LOGIC_VECTOR (DATA_WIDTH-1 DOWNTO 0);
		q_a			: OUT STD_LOGIC_VECTOR (DATA_WIDTH-1 DOWNTO 0)
	);
	END COMPONENT;

BEGIN

	altsyncram_component : altsyncram
	GENERIC MAP (
		clock_enable_input_a	=> "BYPASS",
		clock_enable_output_a	=> "BYPASS",
		init_file				=> INIT_FILE,
		intended_device_family	=> "Cyclone II",
		lpm_hint				=> "ENABLE_RUNTIME_MOD=NO",
		lpm_type				=> "altsyncram",
		numwords_a				=> WORD_COUNT,
		operation_mode			=> "SINGLE_PORT",
		outdata_aclr_a			=> "NONE",
		outdata_reg_a			=> REG_OUT,
		power_up_uninitialized	=> "FALSE",
		widthad_a				=> ADDR_WIDTH,
		width_a					=> DATA_WIDTH,
		width_byteena_a			=> 1
	)
	PORT MAP (
		wren_a 		=> wren,
		clock0 		=> clk,
		address_a	=> addr,
		data_a 		=> data_in,
		q_a 		=> data_out
	);

END SYN;
