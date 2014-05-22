--===========================================================================--
--                                                                           --
--             TESTBENCH    testbench4 - CPU09 Testbench.                    --
--                                                                           --
--===========================================================================--
--
-- File name      : Testbench4.vhd
--
-- Purpose        : cpu09 Microprocessor Test Bench 4
--                  Contains SBUG ROM
--
-- Dependencies   : ieee.Std_Logic_1164
--                  ieee.std_logic_unsigned
--                  ieee.std_logic_arith
--                  ieee.numeric_std
--
-- Uses           : cpu09    (..\VHDL\cpu09.vhd)              CPU core
--                  ram_2k   (..\Spartan3\ram2k_b16.vhd)      2KB block RAM
--                  mon_rom  (..\Spartan3\sbug_rom2k_b16.vhd) 2KB SBUG block ROM
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
   use ieee.numeric_std.all;

entity my_testbench4 is
end my_testbench4;

-------------------------------------------------------------------------------
-- Architecture for memio Controller Unit
-------------------------------------------------------------------------------
architecture behavior of my_testbench4 is
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
  signal cpu_halt    : Std_logic;
  signal cpu_hold    : Std_logic;
  signal rom_data_out: Std_Logic_Vector(7 downto 0);
  signal ram_data_out: Std_Logic_Vector(7 downto 0);
  signal ram_cs      : Std_Logic;
 
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
end component;


component mon_rom
    Port (
       clk      : in  std_logic;
		 rst      : in  std_logic;
		 cs       : in  std_logic;
		 rw       : in  std_logic;
       addr     : in  std_logic_vector (10 downto 0);
       data_in  : in  std_logic_vector (7 downto 0);
       data_out : out std_logic_vector (7 downto 0)
    );
end component;

component ram_2k
    Port (
       clk      : in  std_logic;
       rst      : in  std_logic;
       cs       : in  std_logic;
       addr     : in  std_logic_vector (10 downto 0);
       rw       : in  std_logic;
       data_in  : in  std_logic_vector (7 downto 0);
       data_out : out std_logic_vector (7 downto 0)
    );
end component;

begin
my_cpu : cpu09  port map (    
	 clk	     => SysClk,
    rst	     => cpu_reset,
    rw	     => cpu_rw,
    vma       => cpu_vma,
    addr      => cpu_addr(15 downto 0),
    data_in   => cpu_data_in,
	 data_out  => cpu_data_out,
	 halt      => cpu_halt,
	 hold      => cpu_hold,
	 irq       => cpu_irq,
	 nmi       => cpu_nmi,
	 firq      => cpu_firq
  );


my_rom : mon_rom port map (
       clk      => SysClk,
       rst      => cpu_reset,
		 cs       => ram_cs,
		 rw       => cpu_rw,
       addr     => cpu_addr(10 downto 0),
       data_in  => cpu_data_out,
       data_out => rom_data_out
    );


my_ram : ram_2k port map (
       clk      => SysClk,
       rst      => cpu_reset,
		 cs       => ram_cs,
		 rw       => cpu_rw,
       addr     => cpu_addr(10 downto 0),
       data_in  => cpu_data_out,
       data_out => ram_data_out
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
   cpu_halt <= '0';
	cpu_hold <= '0';

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


  rom : PROCESS( cpu_addr, rom_data_out, ram_data_out )
  begin
    if( cpu_addr(15 downto 11) = "11111" ) then
      cpu_data_in <= rom_data_out;
		ram_cs <= '0';
 	 else
      cpu_data_in <= ram_data_out;
		ram_cs <= '1';
 	 end if;
  end process;

end behavior; --===================== End of architecture =======================--

