--===========================================================================--
--                                                                           --
--             TESTBENCH    testbench5 - CPU09 Testbench.                    --
--                                                                           --
--===========================================================================--
--
-- File name      : Testbench5.vhd
--
-- Purpose        : cpu09 Microprocessor Test Bench 5
--                  Contains ROM to test interrupts
--
-- Dependencies   : ieee.Std_Logic_1164
--                  ieee.std_logic_unsigned
--                  ieee.std_logic_arith
--                  ieee.numeric_std
--
-- Uses           : cpu09    (..\VHDL\cpu09.vhd)              CPU core
--                   
-- Author         : John E. Kent
--                  dilbert57@opencores.org      
-- 
--  Copyright (C) 2003 - 2011 John Kent
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
-- 1.1  2004-02-25 John kent  removed test_alu and test_cc signals from CPU component.
-- 1.2  2011-10-09 John Kent  renamed address to addr on CPU component, updated header
--
--===========================================================================--

library ieee;
   use ieee.std_logic_1164.all;
   use IEEE.STD_LOGIC_ARITH.ALL;
   use IEEE.STD_LOGIC_UNSIGNED.ALL;
   use ieee.numeric_std.all;

entity my_testbench5 is
end my_testbench5;

-------------------------------------------------------------------------------
-- Architecture for  test bench for cpu09
-------------------------------------------------------------------------------
architecture behavior of my_testbench5 is
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
  constant memsize : integer := 128;

  type rom_array is array(0 to memsize-1) of std_logic_vector(width-1 downto 0);

  constant rom_data : rom_array :=
  (
	 x"10", x"CE", x"F8", x"30", -- F800 - 10CE F830 RESET   LDS #$F830
	        x"CE", x"20", x"00", -- F804 -   CE 2000         LDU #$2000
	        x"8E", x"F8", x"02", -- F807 -   8E 5000         LDX #$F802
	 x"10", x"8E", x"80", x"00", -- F80A - 108E 8000         LDY #$8000
	        x"86", x"55",        -- F80E -   86 55           LDA #$55
			  x"C6", x"F0",        -- F810 -   C6 F0           LDB #$F0
			  x"97", x"40",        -- F812 -   97 40           STA <$40
			  x"B7", x"90", x"00", -- F814 -   B7 9000         STA $9000
			  x"A7", x"09",        -- F817 -   A7 09           STA 9,X ($F80B)
			  x"A7", x"29",        -- F819 -   A7 29           STA 9,Y ($8009)
			  x"A7", x"49",        -- F81B -   A7 49           STA 9,U ($2009)
			  x"A7", x"69",        -- F81D -   A7 69           STA 9,S ($F839)
			  x"A7", x"80",        -- F81F -   A7 80           STA ,X+ ($F802)
			  x"A7", x"81",        -- F821 -   A7 81           STA ,X++	($F803)
			  x"A7", x"91",        -- F823 -   A7 91           STA [,X++] ($2000)
			  x"A7", x"82",        -- F825 -   A7 82           STA ,-X ($F806)
			  x"A7", x"83",        -- F827 -   A7 83           STA ,--X	($F804)
			  x"A7", x"93",        -- F829 -   A7 93           STA [,--X] ($2000)
			  x"A7", x"84",        -- F82B -   A7 84           STA ,X ($F802)
			  x"A7", x"94",        -- F82D -   A7 94           STA [,X] ($F830)
			  x"A7", x"85",        -- F82F -   A7 85           STA B,X ($F7F2)
			  x"A7", x"95",        -- F831 -   A7 95           STA [B,X] ($01A7)
			  x"A7", x"86",        -- F833 -   A7 86           STA A,X ($F857)
			  x"A7", x"96",        -- F835 -   A7 96           STA [A,X] ($A78C)
			  x"A7", x"88", x"FF", -- F837 -   A7 88 FF        STA -1,X ($F831)
			  x"A7", x"88", x"01", -- F83A -   A7 88 01        STA 1,X ($F833)
			  x"A7", x"98", x"FF", -- F83D -   A7 98 FF        STA [-1,X] ([$F801])
			  x"A7", x"98", x"01", -- F840 -   A7 98 01        STA [1,X] ([$F803])
	 x"A7", x"89", x"FF", x"FF", -- F843 -   A7 89 FFFF      STA -1,X ($F801)
	 x"A7", x"89", x"00", x"01", -- F847 -   A7 89 0001      STA 1,X ($F803)
	 x"A7", x"99", x"FF", x"FF", -- F84B -   A7 99 FFFF      STA [-1,X] ([$F801])
	 x"A7", x"99", x"00", x"01", -- F84F -   A7 99 0001      STA [1,X] ([$F803])
			  x"A7", x"8B",        -- F853 -   A7 8B           STA D,X ($4BF2)
			  x"A7", x"9B",        -- F855 -   A7 9B           STA [D,X] ([$4BF2]))
			  x"A7", x"8C", x"FF", -- F857 -   A7 8C FF        STA -1,X ($F801)
			  x"A7", x"8C", x"01", -- F85A -   A7 8C 01        STA 1,X ($F803)
			  x"A7", x"9C", x"FF", -- F85D -   A7 9C FF        STA [-1,X] ([$F801])
			  x"A7", x"9C", x"01", -- F860 -   A7 9C 01        STA [1,X] ([$F803])
	 x"A7", x"8D", x"FF", x"FF", -- F863 -   A7 8D FFFF      STA -1,X ($F801)
	 x"A7", x"8D", x"00", x"01", -- F867 -   A7 8D 0001      STA 1,X ($F803)
	 x"A7", x"9D", x"FF", x"FF", -- F86B -   A7 9D FFFF      STA [-1,X] ([$F801])
	 x"A7", x"9D", x"00", x"01", -- F86F -   A7 9D 0001      STA [1,X] ([$F803])
	 x"A7", x"8F", x"A0", x"00", -- F873 -   A7 8F A000      STA $A000
	 x"A7", x"9F", x"A0", x"00", -- F877 -   A7 9F A000      STA [$A000]
			  x"7E", x"F8", x"00", -- F87B -   7E F800         JMP RESET
	        x"F8", x"00"         -- F87E -      F800         fdb RESET ; Reset
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

