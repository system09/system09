--===========================================================================--
--                                                                           --
--             TESTBENCH  Priority Encoder testbench                         --
--                                                                           --
--===========================================================================--
--
-- File name      : Pri_Enc_TB.vhd
--
-- Purpose        : Priority Encoder test bench
--
-- Dependencies   : ieee.Std_Logic_1164
--                  ieee.std_logic_unsigned
--                  ieee.std_logic_arith
--                  ieee.numeric_std
--
-- Uses           : priority_enc (..\VHDL\priority_enc.vhd) generic priority encoder
--                   
-- Author         : John E. Kent
--                  dilbert57@opencores.org      
-- 
--  Copyright (C) 2011 John Kent
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
-- 0.1  2011-10-23 John Kent  First version
--
--===========================================================================--

library ieee;
   use ieee.std_logic_1164.all;
   use IEEE.STD_LOGIC_ARITH.ALL;
   use IEEE.STD_LOGIC_UNSIGNED.ALL;
   use ieee.numeric_std.all;
   use work.bit_funcs.all;
library unisim;
  use unisim.vcomponents.all;

entity my_pri_enc_tb is
end my_pri_enc_tb;

-------------------------------------------------------------------------------
-- Architecture for memio Controller Unit
-------------------------------------------------------------------------------
architecture behavior of my_pri_enc_tb is
  -----------------------------------------------------------------------------
  -- constants
  -----------------------------------------------------------------------------
  constant IN_WIDTH   : integer := 128;
  constant OUT_WIDTH  : integer := log2(IN_WIDTH);

  -----------------------------------------------------------------------------
  -- Signals
  -----------------------------------------------------------------------------
  signal tb_d   : std_logic_vector(IN_WIDTH-1 downto 0);
  signal tb_q   : std_logic_vector(OUT_WIDTH-1 downto 0);
  signal tb_v   : std_logic;


component priority_enc is
  port (    
	 d:	  in	std_logic_vector(IN_WIDTH-1 downto 0);
    q:	  out	std_logic_vector(log2(IN_WIDTH)-1 downto 0);
    v:	  out	std_logic
  );
end component;


begin

uut : priority_enc
  port map (    
	 d     => tb_d,
    q     => tb_q,
    v     => tb_v
  );

  -- *** Test Bench - User Defined Section ***
   tb : PROCESS
   BEGIN
     tb_d <= (others=>'0');

     tb_d(7 downto 0) <= "00000000";
	  wait for 100 ns;
     tb_d(7 downto 0) <= "00000001";
	  wait for 100 ns;
     tb_d(7 downto 0) <= "00000010";
	  wait for 100 ns;
     tb_d(7 downto 0) <= "00000100";
	  wait for 100 ns;
     tb_d(7 downto 0) <= "00001000";
	  wait for 100 ns;
     tb_d(7 downto 0) <= "00010000";
	  wait for 100 ns;
     tb_d(7 downto 0) <= "00100000";
	  wait for 100 ns;
     tb_d(7 downto 0) <= "01000000";
	  wait for 100 ns;
     tb_d(7 downto 0) <= "10000000";
	  wait for 100 ns;
     tb_d(7 downto 0) <= "00000011";
	  wait for 100 ns;
     tb_d(7 downto 0) <= "00000110";
	  wait for 100 ns;
     tb_d(7 downto 0) <= "00001100";
	  wait for 100 ns;
     tb_d(7 downto 0) <= "00011000";
	  wait for 100 ns;
     tb_d(7 downto 0) <= "00110000";
	  wait for 100 ns;
     tb_d(7 downto 0) <= "01100000";
	  wait for 100 ns;
     tb_d(7 downto 0) <= "11000000";
	  wait for 100 ns;
     tb_d(7 downto 0) <= "00000111";
	  wait for 100 ns;
     tb_d(7 downto 0) <= "00001110";
	  wait for 100 ns;
     tb_d(7 downto 0) <= "00011100";
	  wait for 100 ns;
     tb_d(7 downto 0) <= "00111000";
	  wait for 100 ns;
     tb_d(7 downto 0) <= "01110000";
	  wait for 100 ns;
     tb_d(7 downto 0) <= "11100000";
	  wait for 100 ns;
     tb_d(7 downto 0) <= "00001111";
	  wait for 100 ns;
     tb_d(7 downto 0) <= "00011110";
	  wait for 100 ns;
     tb_d(7 downto 0) <= "00111100";
	  wait for 100 ns;
     tb_d(7 downto 0) <= "01111000";
	  wait for 100 ns;
     tb_d(7 downto 0) <= "11110000";
	  wait for 100 ns;
     tb_d(7 downto 0) <= "00011111";
	  wait for 100 ns;
     tb_d(7 downto 0) <= "00111110";
	  wait for 100 ns;
     tb_d(7 downto 0) <= "01111100";
	  wait for 100 ns;
     tb_d(7 downto 0) <= "11111000";
	  wait for 100 ns;
     tb_d(7 downto 0) <= "00111111";
	  wait for 100 ns;
     tb_d(7 downto 0) <= "01111110";
	  wait for 100 ns;
     tb_d(7 downto 0) <= "11111100";
	  wait for 100 ns;
     tb_d(7 downto 0) <= "01111111";
	  wait for 100 ns;
     tb_d(7 downto 0) <= "11111110";
	  wait for 100 ns;
     tb_d(7 downto 0) <= "11111111";
	  wait for 100 ns;

      wait; -- will wait forever
   END PROCESS;
-- *** End Test Bench - User Defined Section ***

end behavior; --===================== End of architecture =======================--

