--===========================================================================--
--                                                                           --
--             TESTBENCH    testbench3 - CPU09 Testbench.                    --
--                                                                           --
--===========================================================================--
--
-- File name      : Testbench3.vhd
--
-- Purpose        : cpu09 Microprocessor Test Bench 3
--                  Contains ROM to test interrupts
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
-- 1.1  2004-02-26 John kent  removed test_alu and test_cc signals from CPU component.
-- 1.2  2011-10-09 John Kent  renamed address to addr on CPU component, updated header
--
--===========================================================================--

library ieee;
   use ieee.std_logic_1164.all;
   use IEEE.STD_LOGIC_ARITH.ALL;
   use IEEE.STD_LOGIC_UNSIGNED.ALL;
   use ieee.numeric_std.all;

entity my_testbench3 is
end my_testbench3;

-------------------------------------------------------------------------------
-- Architecture for memio Controller Unit
-------------------------------------------------------------------------------
architecture behavior of my_testbench3 is
  -----------------------------------------------------------------------------
  -- Signals
  -----------------------------------------------------------------------------
  signal cpu_irq    : std_Logic;
  signal cpu_firq   : std_logic;
  signal cpu_nmi    : std_logic;

  -- CPU Interface signals
  signal SysClk      : Std_Logic;
  signal cpu_reset   : Std_Logic;
  signal cpu_rw      : Std_Logic;
  signal cpu_vma     : Std_Logic;
  signal cpu_addr    : Std_Logic_Vector(15 downto 0);
  signal cpu_data_in : Std_Logic_Vector(7 downto 0);
  signal cpu_data_out: Std_Logic_Vector(7 downto 0);

  constant width   : integer := 8;
  constant memsize : integer := 64;

  type rom_array is array(0 to memsize-1) of std_logic_vector(width-1 downto 0);

  constant rom_data : rom_array :=
  (
	 "00010000", "11001110", "11111000", "00110000", -- F800 - 10CE F830 RET1    LDS #STACK
	 "00111111",                                     -- F804 -   3F              SWI
	 "00010000", "00111111",                         -- F805 - 103F      SWIVEC  SWI2
	 "00010001", "00111111",                         -- F807 - 113F      SWI2VEC SWI3
	 "00111011",                                     -- F809 -   3B      SWI3VEC RTI
	 "00100000", "11111110",                         -- F80A -   20 FE           BRA *
	 "10110001",                                     -- F80C -   B1      STACK3 FCB $B1 ; CC
	 "00110010",                                     -- F80D -   32             FCB $32 ; ACCA
	 "00110011",                                     -- F8OE -   33             FCB $33 ; ACCB
	 "00110100",                                     -- F8OF -   34             FCB $34 ; DPR
	 "00110101", "00110110",                         -- F810 - 3536             FDB $3536 ; IX
    "00110111", "00111000",                         -- F812 - 3738             FDB $3738 ; IY
    "00111001", "00111010",                         -- F814 - 393A             FDB $393A ; UP
	 "11111000", "00001001",                         -- F816 - F809             FDB SWI3VEC ; PC
	 "10100001",                                     -- F818 -   A1      STACK2 FCB $A1 ; CC
	 "00100010",                                     -- F819 -   22             FCB $22 ; ACCA
	 "00100011",                                     -- F81A -   23             FCB $23 ; ACCB
	 "00100100",                                     -- F81B -   24             FCB $24 ; DPR
	 "00100101", "00100110",                         -- F81C - 2526             FDB $2526 ; IX
    "00100111", "00101000",                         -- F81E - 2728             FDB $2728 ; IY
    "00101001", "00101010",                         -- F820 - 292A             FDB $292A ; UP
	 "11111000", "00001001",                         -- F822 - F809             FDB SWI3VEC ; PC
	 "10010001",                                     -- F824 -   91      STACK1 FCB $91 ; CC
	 "00010010",                                     -- F825 -   12             FCB $12 ; ACCA
	 "00010011",                                     -- F826 -   13             FCB $13 ; ACCB
	 "00010100",                                     -- F827 -   14             FCB $14 ; DPR
	 "00010101", "00010110",                         -- F828 - 1516             FDB $1516 ; IX
    "00010111", "00011000",                         -- F82A - 1718             FDB $1718 ; IY
    "00011001", "00011010",                         -- F82C - 191A             FDB $191A ; UP
	 "11111000", "00000000",                         -- F82E - F800             FDB RESET ; PC
	 																 -- F830             STACK  EQU *
																	 --
																	 -- Interrupt Cectors Start Here
																	 --
	 "11111000", "00000000",                         -- F830 - F800             FDB RESET ; RESV
    "11111000", "00001001",                         -- F832 - F809             FDB SWIVEC3 ; SWI3
	 "11111000", "00000111",                         -- F834 - F807             FDB SWIVEC2 ; SWI2
	 "11111000", "00000000",                         -- F836 - F800             fdb RESET ; FIRQ
	 "11111000", "00000000",                         -- F838 - F800             fdb RESET ; IRQ
	 "11111000", "00000101",                         -- F83A - F805             fdb SWIVEC ; SWI
	 "11111000", "00000000",                         -- F83C - F800             fcb RESET ; NMI
	 "11111000", "00000000"                          -- F83E - F800             fdb RESET ; Reset
	 );

component cpu09
  port (    
	 clk:	     in	std_logic;
    rst:	     in	std_logic;
    rw:	     out	std_logic;		-- Asynchronous memory interface
    vma:	     out	std_logic;
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
    rw	     => cpu_rw,
    vma       => cpu_vma,
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

