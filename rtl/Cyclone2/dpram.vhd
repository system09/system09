LIBRARY ieee;
USE ieee.std_logic_1164.all;

LIBRARY altera_mf;
USE altera_mf.altera_mf_components.all;

ENTITY dpram IS
	GENERIC
	(
		INIT_FILE		: string	:= "";
		WORD_COUNT		: natural	:= 2048;
		ADDR_WIDTH_A	: natural	:= 11;
		DATA_WIDTH_A	: natural	:= 8;
		REG_OUT_A		: string	:= "UNREGISTERED";
		ADDR_WIDTH_B	: natural	:= 11;
		DATA_WIDTH_B	: natural	:= 8;
		REG_OUT_B		: string	:= "UNREGISTERED"
	);
	PORT
	(
		clk_a           : IN  STD_LOGIC;
		cs_a            : IN  STD_LOGIC;
		rw_a			: IN  STD_LOGIC;
		addr_a			: IN  STD_LOGIC_VECTOR (ADDR_WIDTH_A-1 DOWNTO 0);
		data_in_a		: IN  STD_LOGIC_VECTOR (DATA_WIDTH_A-1 DOWNTO 0);
		data_out_a		: OUT STD_LOGIC_VECTOR (DATA_WIDTH_A-1 DOWNTO 0);
		
		clk_b           : IN  STD_LOGIC;
		cs_b            : IN  STD_LOGIC;
		rw_b			: IN  STD_LOGIC;
		addr_b			: IN  STD_LOGIC_VECTOR (ADDR_WIDTH_B-1 DOWNTO 0);
		data_in_b		: IN  STD_LOGIC_VECTOR (DATA_WIDTH_B-1 DOWNTO 0);
		data_out_b		: OUT STD_LOGIC_VECTOR (DATA_WIDTH_B-1 DOWNTO 0)
	);
END dpram;

ARCHITECTURE SYN OF dpram IS

signal we_a : std_logic;
signal we_b : std_logic;

component altsyncram

    generic (
                address_aclr_a                     :       string := "UNUSED";
                address_aclr_b                     :       string := "NONE";
                address_reg_b                      :       string := "CLOCK1";
                byte_size                          :       natural := 8;
                byteena_aclr_a                     :       string := "UNUSED";
                byteena_aclr_b                     :       string := "NONE";
                byteena_reg_b                      :       string := "CLOCK1";
                clock_enable_core_a                :       string := "USE_INPUT_CLKEN";
                clock_enable_core_b                :       string := "USE_INPUT_CLKEN";
                clock_enable_input_a               :       string := "NORMAL";
                clock_enable_input_b               :       string := "NORMAL";
                clock_enable_output_a              :       string := "NORMAL";
                clock_enable_output_b              :       string := "NORMAL";
                intended_device_family             :       string := "unused";
                enable_ecc                         :       string := "FALSE";
                implement_in_les                   :       string := "OFF";
                indata_aclr_a                      :       string := "UNUSED";
                indata_aclr_b                      :       string := "NONE";
                indata_reg_b                       :       string := "CLOCK1";
                init_file                          :       string := "UNUSED";
                init_file_layout                   :       string := "PORT_A";
                maximum_depth                      :       natural := 0;
                numwords_a                         :       natural := 0;
                numwords_b                         :       natural := 0;
                operation_mode                     :       string := "BIDIR_DUAL_PORT";
                outdata_aclr_a                     :       string := "NONE";
                outdata_aclr_b                     :       string := "NONE";
                outdata_reg_a                      :       string := "UNREGISTERED";
                outdata_reg_b                      :       string := "UNREGISTERED";
                power_up_uninitialized             :       string := "FALSE";
                ram_block_type                     :       string := "AUTO";
                rdcontrol_aclr_b                   :       string := "NONE";
                rdcontrol_reg_b                    :       string := "CLOCK1";
                read_during_write_mode_mixed_ports :       string := "DONT_CARE";
                read_during_write_mode_port_a      :       string := "NEW_DATA_NO_NBE_READ";
                read_during_write_mode_port_b      :       string := "NEW_DATA_NO_NBE_READ";
                width_a                            :       natural := DATA_WIDTH_A;
                width_b                            :       natural := DATA_WIDTH_B;
                width_byteena_a                    :       natural := 1;
                width_byteena_b                    :       natural := 1;
                widthad_a                          :       natural := ADDR_WIDTH_A;
                widthad_b                          :       natural := ADDR_WIDTH_B;
                wrcontrol_aclr_a                   :       string := "UNUSED";
                wrcontrol_aclr_b                   :       string := "NONE";
                wrcontrol_wraddress_reg_b          :       string := "CLOCK1";
                lpm_hint                           :       string := "UNUSED";
                lpm_type                           :       string := "altsyncram"
        );

        port(
                aclr0                              :       in std_logic := '0';
                aclr1                              :       in std_logic := '0';
                address_a                          :       in std_logic_vector(addr_width_a-1 downto 0);
                address_b                          :       in std_logic_vector(addr_width_b-1 downto 0) := (others => '1');
                addressstall_a                     :       in std_logic := '0';
                addressstall_b                     :       in std_logic := '0';
                byteena_a                          :       in std_logic_vector(width_byteena_a-1 downto 0) := (others => '1');
                byteena_b                          :       in std_logic_vector(width_byteena_b-1 downto 0) := (others => '1');
                clock0                             :       in std_logic := '1';
                clock1                             :       in std_logic := '1';
                clocken0                           :       in std_logic := '1';
                clocken1                           :       in std_logic := '1';
                clocken2                           :       in std_logic := '1';
                clocken3                           :       in std_logic := '1';
                data_a                             :       in std_logic_vector(data_width_a-1 downto 0) := (others => '1');
                data_b                             :       in std_logic_vector(data_width_b-1 downto 0) := (others => '1');
                eccstatus                          :       out std_logic_vector(2 downto 0);
                q_a                                :       out std_logic_vector(data_width_a-1 downto 0);
                q_b                                :       out std_logic_vector(data_width_b-1 downto 0);
                rden_a                             :       in std_logic := '1';
                rden_b                             :       in std_logic := '1';
                wren_a                             :       in std_logic := '0';
                wren_b                             :       in std_logic := '0'
        );
end component;

BEGIN

	altsyncram_component : altsyncram
	GENERIC MAP (
		power_up_uninitialized	=> "FALSE",
		init_file				=> INIT_FILE,
		intended_device_family	=> "Cyclone II",
		lpm_hint				=> "ENABLE_RUNTIME_MOD=NO",
		lpm_type				=> "altsyncram",
		operation_mode			=> "DUAL_PORT",

		clock_enable_input_a	=> "BYPASS",
		clock_enable_output_a	=> "BYPASS",
		numwords_a				=> WORD_COUNT,
		outdata_aclr_a			=> "NONE",
		outdata_reg_a			=> REG_OUT_A,
		widthad_a				=> ADDR_WIDTH_A,
		width_a					=> DATA_WIDTH_A,
		width_byteena_a			=> 1,
		
		clock_enable_input_b	=> "BYPASS",
		clock_enable_output_b	=> "BYPASS",
		numwords_b				=> WORD_COUNT,
		outdata_aclr_b			=> "NONE",
		outdata_reg_b			=> REG_OUT_B,
		widthad_b				=> ADDR_WIDTH_B,
		width_b					=> DATA_WIDTH_B,
		width_byteena_b			=> 1
	)
	PORT MAP (
		clock0 		=> clk_a,
		clocken0    => cs_a,
		wren_a 		=> we_a,
		address_a	=> addr_a,
		data_a 		=> data_in_a,
		q_a 		=> data_out_a,

		clock1 		=> clk_b,
		clocken1    => cs_b,
		wren_b 		=> we_b,
		address_b	=> addr_b,
		data_b 		=> data_in_b,
		q_b 		=> data_out_b
	);

process (rw_a, rw_b )
begin
  we_a <= cs_a and (not rw_a);
  we_b <= cs_b and (not rw_b);
end process;

END SYN;
