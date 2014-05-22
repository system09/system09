--===========================================================================--
--                                                                           --
--             TESTBENCH    testbench2 - CPU09 Testbench.                    --
--                                                                           --
--===========================================================================--
--
-- File name      : Testbench2.vhd
--
-- Purpose        : cpu09 Microprocessor Test Bench 2
--                  Contains ROM to read sector from
--                  a none existant Compact Flash module
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
-- 1.2  2011-10-09 John Kent  renamed address to addr on CPU component, updated header
--
--===========================================================================--

library ieee;
   use ieee.std_logic_1164.all;
   use IEEE.STD_LOGIC_ARITH.ALL;
   use IEEE.STD_LOGIC_UNSIGNED.ALL;
   use ieee.numeric_std.all;

entity my_testbench2 is
end my_testbench2;

-------------------------------------------------------------------------------
-- Architecture for memio Controller Unit
-------------------------------------------------------------------------------
architecture behavior of my_testbench2 is
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
  signal cpu_nmi     : Std_Logic;
  signal cpu_firq    : std_logic;

  constant width   : integer := 8;
  constant memsize : integer := 128;

  type rom_array is array(0 to memsize-1) of std_logic_vector(width-1 downto 0);

  constant rom_data : rom_array :=
  (
"00010000", -- $F800 LDS #$F878 (Point to dummy return to test stack)
"11001110",
"11111000",
"01111000",
"10000110", -- $F804 LDA #$E0 *** START
"11100000",
"00011111", -- $F806 TFR A,DPR
"10001011",
---------------------------
-- "10001101", -- $F80E BSR WAITRDY $F86A
-- "01100000",
"10001101", -- $F808 BSR $F874 -- test sub call
"01101010",
---------------------------
"10000110", -- $F80A LDA #$E0
"11100000",
"10010111", -- $F80C STA <$E016
"00010110",
---------------------------
-- "10001101", -- $F80E BSR WAITRDY $F86A
-- "01011010",
"10001101", -- $F80E BSR $F810
"00000000",
--------------------------
"10000110", -- $F810 LDA #$01
"00000001",
"10010111", -- $F812 STA <$E011
"00010001",
"10000110", -- $F814 LDA #$EF
"11101111",
"10010111", -- $F816 STA <$E017
"00010111",
--------------------------
-- "10001101", -- $F818 BSR WAITRDY $F86A
-- "01010000",
"10001101", -- $F818 BSR $F816
"00000000",
--------------------------
"00010000", -- $F81A LDY #$F800
"10001110",
"11111000",
"00000000",
"11000110", -- $F81E LDB #$7C
"01111100",
"10000110", -- $F820 LDA #$01 *** RDLP1
"00000001",
"10010111", -- $F822 STA <$E012
"00010010",
"11010111", -- $F824 STB <$E013
"00010011",
"10000110", -- $F826 LDA #$F4
"11110100",
"10010111", -- $F828 STA <$E014
"00010100",
"01001111", -- $F82A CLRA
"10010111", -- $F82B STA <$E015
"00010101",
"10001110", -- $F82D LDX #512
"00000010",
"00000000",
"10000110", -- $F830 LDA #$20
"00100000",
"10010111", -- $F832 STA <$E017
"00010111",
--------------------------
-- "10001101", -- $F834 BSR WAITRDY $F86A
-- "00110100",
"10001101", -- $F834 BSR *
"00000000",
--------------------------
"10010110", -- $F836 LDA <$E017 *** WAITDRQ
"00010111",
"10000101", -- $F838 BITA #$08
"00001000",
"00100111", -- $F83A BEQ WAITDRQ
"11111010",
"10010110", -- $F83C LDA <$E010
"00010000",
"10100111", -- $F83E STA ,Y+
"10100000",
"00110000", -- $F840 LEAX -1,X
"00011111",
"10001100", -- $F842 CMPX #$0000
"00000000",
"00000000",
"00100110", -- $F845 BNE RDLP2
"11110011",
--------------------------
-- "10001101", -- $F847 BSR WAITRDY $F86A
-- "00100001",
"10001101", -- $F847 BSR $F841
"00000000",
--------------------------
"01011100", -- $F849 INCB
"11000001", -- $F84A CMPB #$80
"10000000",
"00100110", -- $F84C BNE RDLP1
"11010110",
"10001110", -- $F84E LDX #$FF97
"11111111",
"10010111",
"00010000", -- $F851 LDY #$F000
"10001110",
"11110000",
"00000000",
"11000110", -- $F855 LDB #$61
"01100001",
"10100110", -- $F857 LDA 0,X+ *** MOVELP
"10000000",
"10100111", -- $F859 STA 0,Y+
"10100000",
"01011010", -- $F85B DECB
----------------------------
-- "00100110", -- $F85C BNE MOVELP
-- "11111001",
"00100110", --$F85C BNE $F861
"00000011",
----------------------------
"01111110", -- $F85E JMP $F000
"11110000",
"00000000",
"00001111", -- $F861 CLR <$E030 
"00110000",
"01001111", -- $F863 CLRA
"00011111", -- $F864 TFR A,DPR
"10001011",
"01101110", -- $F866 JMP [$FFFE]
"10011111",
"11111111",
"11111110",
--
-- Wait for Ready
--
"10010110", -- $F86A LDA <$E017 *** WAITRDY
"00010111",
"00101011", -- $F86C BMI WAITRDY
"11111100",
"10010110", -- $F86E LDA <$E017
"00010111",
"10000101", -- $F870 BITA #$40
"01000000",
"00100111", -- $F872 BNE WAITRQY
"11110110",
"00111001", -- $F874 RTS
"00010010", -- $F875 NOP
"11111000", -- $F876 FDB $F80A -- dummy sub return
"00001010",
"11111000", -- $F878 FDB $F800
"00000000",
"11111000", -- $F87A FDB $F800
"00000000",
"11111000", -- $F87C FDB $F800
"00000000",
"11111000", -- $F87E FDB $F800
"00000000"    
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
    cpu_data_in <= rom_data(conv_integer(cpu_addr(6 downto 0))); 
  end process;

end behavior; --===================== End of architecture =======================--

