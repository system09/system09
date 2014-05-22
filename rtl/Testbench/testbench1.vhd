--===========================================================================--
--                                                                           --
--             TESTBENCH    testbench1 - CPU09 Testbench.                    --
--                                                                           --
--===========================================================================--
--
-- File name      : Testbench1.vhd
--
-- Purpose        : cpu09 Microprocessor Test Bench 1
--                  Contains ROM to print out "Hello World"
--                  on a none existant Uart
--
-- Dependencies   : ieee.Std_Logic_1164
--                  ieee.std_logic_unsigned
--                  ieee.std_logic_arith
--                  ieee.numeric_std
--
-- Uses           : cpu09    (cpu09.vhd)      CPU core
--                   
-- Author         : John E. Kent
--                  dilbert57@opencores.org      
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
--===========================================================================--
--                                                                           --
--                                Revision History                           --
--                                                                           --
--===========================================================================--
--
-- Rev  Date       Author     Changes
-- 0.1  2003-04-12 John Kent  First version
-- 1.0  2003-09-06 John Kent  Initial release to Opencores.org
-- 1.1  2004-01-25 John Kent  removed "test_alu" and "test_cc" from CPU component
-- 1.2  2011-10-09 John Kent  updated for acia6850
--
--===========================================================================--

library ieee;
   use ieee.std_logic_1164.all;
   use IEEE.STD_LOGIC_ARITH.ALL;
   use IEEE.STD_LOGIC_UNSIGNED.ALL;
   use ieee.numeric_std.all;
--library work;
--   use work.UART_Def.all;
--   use work.typedefines.all;
--   use work.memory.all;

entity my_testbench1 is
end my_testbench1;

-------------------------------------------------------------------------------
-- Architecture for memio Controller Unit
-------------------------------------------------------------------------------
architecture behavior of my_testbench1 is
  -----------------------------------------------------------------------------
  -- Signals
  -----------------------------------------------------------------------------
  -- CPU Interface signals
  signal SysClk      : Std_Logic;
  signal cpu_reset   : Std_Logic;
  signal cpu_rw      : Std_Logic;
  signal cpu_vma     : Std_Logic;
  signal cpu_addr    : Std_Logic_Vector(15 downto 0);
  signal cpu_data_in : Std_Logic_Vector(7 downto 0);
  signal cpu_data_out: Std_Logic_Vector(7 downto 0);
  signal cpu_irq     : Std_Logic;
  signal cpu_nmi     : std_logic;
  signal cpu_firq    : Std_Logic;


  constant width   : integer := 8;
  constant memsize : integer := 64;

  type rom_array is array(0 to memsize-1) of std_logic_vector(width-1 downto 0);

  constant rom_data : rom_array :=
  (
    "10001110", "11111000", "00101000", -- F800 - 8E F828  RESET LDX #MSG
	 "10000110", "00010001",             -- F803 - 86 11          LDA #$11
	 "10110111", "11100000", "00000100", -- F805 - B7 E004        STA UARTCR
    "10110110", "11100000", "00000100", -- F808 - B6 E004  POLL1 LDA UARTCR
	 "10000101", "00000010",             -- F80B - 85 02          BITA #TXBE
	 "00100110", "11111001",             -- F80D - 26 F9          BNE POLL1
	 "10100110", "10000000",             -- F80F - A6 80          LDA ,X+
	 "00100111", "00000110",             -- F811 - 27 06          BEQ POLL2
	 "00010010",                         -- F813 - 12             NOP
	 "10110111", "11100000", "00000101", -- F814 - B7 E005        STA UARTDR
    "00100110", "11101111",             -- F817 - 26 EF          BNE POLL1
	 "10110110", "11100000", "00000100", -- F819 - B6 E004  POLL2 LDA UARTCR
	 "10000101", "00000001",             -- F81C - 85 01          BITA #RXBF
	 "00100111", "11111001",             -- F81E - 27 F9          BEQ POLL2
	 "10110110", "11100000", "00000101", -- F820 - B6 E005        LDA UARTDR
	 "01111110", "11111000", "00000000", -- F823 - 7E F800        JMP RESET
	 "00000000", "00000000",             -- F826 - 00 00          fcb $00,$00
    "01001000", "01100101", "01101100", -- F828 - 48 65 6c MSG   FCC "Hel"
	 "01101100", "01101111", "00100000", -- F82B - 6c 6f 20       FCC "lo "
	 "01010111", "01101111", "01110010", -- F82E - 57 6f 72       FCC "Wor"
    "01101100", "01100100",             -- F831 - 6c 64          FCC "ld"
    "00001010", "00001101", "00000000", -- F833 - 0a 0d 00       FCB LF,CR,NULL
    "00000000", "00000000",             -- F836 - 00 00          fcb null,null           
	 "11111000", "00000000",             -- F838 - F8 00          fdb $F800 ; Timer irq
	 "11111000", "00000000",             -- F83A - F8 00          fdb $F800 ; Ext IRQ
	 "11111000", "00000000",             -- F83C - F8 00          fcb $F800 ; SWI
	 "11111000", "00000000"              -- F83E - F8 00          fdb $F800 ; Reset
	 );

component cpu09
  port (    
	 clk:	     in	std_logic;
    rst:	     in	std_logic;
    vma:	     out	std_logic;
    rw:	      out	std_logic;		-- Asynchronous memory interface
    addr:     out	std_logic_vector(15 downto 0);
    data_in:  in	std_logic_vector(7 downto 0);
	 data_out: out std_logic_vector(7 downto 0);
	 halt:     in  std_logic;
	 hold:     in  std_logic;
	 irq:      in  std_logic;
	 nmi:      in  std_logic;
	 firq:     in  std_logic
  );
end component cpu09;


begin
cpu : cpu09  port map (    
	 clk	     => SysClk,
    rst	     => cpu_reset,
    vma       => cpu_vma,
    rw	     => cpu_rw,
    addr      => cpu_addr(15 downto 0),
    data_in   => cpu_data_in,
	 data_out  => cpu_data_out,
	 halt      => '0',
	 hold      => '0',
	 irq       => cpu_irq,
	 nmi       => cpu_nmi,
	 firq      => cpu_firq
  );

  -- *** Test Bench - User Defined Section ***
   tb : PROCESS
	variable count : integer;
   BEGIN

	cpu_reset <= '0';
	SysClk <= '0';
   cpu_irq <= '0';
   cpu_nmi <= '0';
	cpu_firq <= '0';

		for count in 0 to 512 loop
			SysClk <= '0';
			if count = 0 then
				cpu_reset <= '1';
			elsif count = 1 then
				cpu_reset <= '0';
			end if;
			wait for 100 ns;
			SysClk <= '1';
			wait for 100 ns;
		end loop;

      wait; -- will wait forever
   END PROCESS;
-- *** End Test Bench - User Defined Section ***


  rom : PROCESS( cpu_addr )
  begin
    cpu_data_in <= rom_data(conv_integer(cpu_addr(5 downto 0))); 
  end process;

end behavior; --===================== End of architecture =======================--

