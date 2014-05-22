--===========================================================================--
--
--  S Y N T H E Z I A B L E    Altera LPM_RAM / WISHBONE interface
--
--  www.OpenCores.Org - August 2003
--  This core adheres to the GNU public license  
--
-- File name      : wb_lpm_ram.vhd
--
-- Purpose        : Implements a WISHBONE compatble interface
--                  for the Altera LPM_ROM
--
-- Dependencies   : ieee.Std_Logic_1164
--                  ieee.std_logic_unsigned
--									work.lpm_components (Altera's 220PACK.vhd)
--
-- Author         : Michael L. Hasenfratz Sr.
--
--===========================================================================----
--
-- Revision History:
--
-- Date:          Revision         Author
--===========================================================================--
-- 4 Aug 2003     0.1              Michael L. Hasenfratz Sr.
--      Created
-- 5 Aug 2003     0.2              Michael L. Hasenfratz Sr.
--      Added Cache check
--

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

library lpm;
use lpm.lpm_components.all;

entity wb_lpm_ram is
	generic (
		LPM_WIDTH	:		positive	range 1 to 64 := 8;				-- data bits WIDE
		LPM_WIDTHAD :	positive	range 1 to 32	:= 8				-- address bits;
	);
	port (	
	  DAT_I :      in  std_logic_vector(LPM_WIDTH-1 downto 0);
	  DAT_O :      out std_logic_vector(LPM_WIDTH-1 downto 0);
		ADR_I :      in  std_logic_vector(LPM_WIDTHAD-1 downto 0);
		SEL_I :      in  std_logic_vector((LPM_WIDTH/8)-1 downto 0);
		WE_I :       in  std_logic;
		STB_I :	     in  std_logic;		-- VMA (Valid Memory Access)
		CYC_I :	     in  std_logic;		-- CYC in progress
		ACK_O :      out std_logic;		-- Data ready
		CLK_I :	     in  std_logic;		-- System Clock
		RST_I :	     in  std_logic		-- Reset
	);
end;

architecture bhv_wb_lpm_ram of wb_lpm_ram is

	signal	iwe :				std_logic_vector(SEL_I'RANGE);	-- Internal Write Enables
	signal	iack :			std_logic;		-- Internal ACK
	signal	sel :				std_logic;		-- device selected

begin

---------------------------------------------------------
--	Instantiate the RAM interface
---------------------------------------------------------
gen : for idx in SEL_I'RANGE generate
ram :	LPM_RAM_DQ
		generic map (
			LPM_WIDTH		=> 8,
			LPM_WIDTHAD	=> LPM_WIDTHAD,
      USE_EAB  => "ON",
	    LPM_OUTDATA => "UNREGISTERED"
		)
		port map (	
		  DATA		=> DAT_I((idx*8)+7 downto (idx*8)),
		  Q				=> DAT_O((idx*8)+7 downto (idx*8)),
		  WE      => iwe(idx),
			ADDRESS	=> ADR_I,
			INCLOCK	=> CLK_I
		);
	end generate;

---------------------------------------------------------
--	Interconnections
---------------------------------------------------------
	ACK_O	<= (iack or WE_I) and sel;
	
-- find SEL_(x)
selx : process(SEL_I, CYC_I, STB_I, WE_I)
	variable	isel :	std_logic ;
	begin
		isel	:= '0';
		for ndx in SEL_I'RANGE loop
			isel			:= isel or SEL_I(ndx);
			iwe(ndx)	<= SEL_I(ndx) and WE_I;
		end loop;
		sel		<= isel and CYC_I and STB_I;
	end process;

--	ACK / HOLD
intc0 : process(CLK_I)
	begin
		if CLK_I'EVENT and CLK_I = '1' then
			if RST_I = '1' then
				iack		<= '0';
			else
				iack		<= sel and not(iack);
			end if;
		end if;
	end process;
	
end bhv_wb_lpm_ram;
	
