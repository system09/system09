--===========================================================================--
--                                                                           --
--  umul32.vhd - Synthesizable 32 bit unsigned integer multiplier            --
--                                                                           --
--===========================================================================--
--
--  File name      : umul32.vhd
--
--  Entity name    : umul32
--
--  Purpose        : Implements a 32 bit x 32 bit unsigned integer hardware multiplier
--                   with 64 bit result. Consists of 16 x 8 bit registers.
--                   Designed for Spartan 3/3E with 18 x 18 bit multiplier blocks.
--                   Uses 4 x 18 bit multipliers.
--                   Performs 32 x 32 unsigned multiply in 2 clock cycles. 
--                  
--  Dependencies   : ieee.std_logic_1164
--                   ieee.std_logic_unsigned
--                   unisim.vcomponents
--
--  Author         : John E. Kent
--
--  Email          : dilbert57@opencores.org      
--
--  Web            : http://opencores.org/project,system09
--
--  Registers      :
-- 
--   0 R/W left   input  Most Significant Byte
--   1 R/W left   input
--   2 R/W left   input
--   3 R/W left   input  Least Significant Byte
--   4 R/W right  input  Most Significant Byte
--   5 R/W right  input
--   6 R/W right  input
--   7 R/W right  input  Least Significant Byte
--   8 R/O result output Most Significant Byte
--   9 R/O result output 
--  10 R/O result output 
--  11 R/O result output 
--  12 R/O result output 
--  13 R/O result output 
--  14 R/O result output 
--  15 R/O result output Least Significant Byte
--
--  Copyright (C) 2010 - 2012 John Kent
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
--                              Revision  History                            --
--                                                                           --
--===========================================================================--
--
-- Version  Author        Date         Description
--
-- 0.1      John Kent     2008-09-07   Initial version
-- 0.2      John Kent     2010-06-17   Header & GPL added
-- 0.3      John Kent     2012-04-06   converted into umul32
--

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_unsigned.all;
library unisim;
  use unisim.vcomponents.all;

entity umul32 is
	port (	
	 clk       : in  std_logic;
    rst       : in  std_logic;
    cs        : in  std_logic;
    rw        : in  std_logic;
    addr      : in  std_logic_vector(3 downto 0);
    data_in   : in  std_logic_vector(7 downto 0);
	 data_out  : out std_logic_vector(7 downto 0));
end entity;

architecture rtl of umul32 is

--
-- 16 bit zero
--
constant zero16     : std_logic_vector(15 downto 0) := (others =>'0');

--
-- registers
--
signal multiplicand : std_logic_vector(31 downto 0);
signal multiplier   : std_logic_vector(31 downto 0);
signal result       : std_logic_vector(63 downto 0);

--
-- intermediate 36 bit multiplier outputs
--
signal mul_out_0    : std_logic_vector(35 downto 0);
signal mul_out_1    : std_logic_vector(35 downto 0);
signal mul_out_2    : std_logic_vector(35 downto 0);
signal mul_out_3    : std_logic_vector(35 downto 0);

begin

---------------------------------
--
-- Write Multiplier Registers
--
---------------------------------

umul32_write : process( clk, rst, cs, rw, data_in )
begin
  if falling_edge( clk ) then
    if rst = '1' then
      multiplicand <= (others => '0');
      multiplier   <= (others => '0');
    elsif (cs = '1') and (rw = '0') then
      case addr is
	   when "0000" =>
		  multiplicand(31 downto 24) <= data_in;
	   when "0001" =>
		  multiplicand(23 downto 16) <= data_in;
	   when "0010" =>
		  multiplicand(15 downto  8) <= data_in;
	   when "0011" =>
		  multiplicand( 7 downto  0) <= data_in;
	   when "0100" =>
		  multiplier(31 downto 24) <= data_in;
	   when "0101" =>
		  multiplier(23 downto 16) <= data_in;
	   when "0110" =>
		  multiplier(15 downto  8) <= data_in;
	   when "0111" =>
        multiplier( 7 downto  0) <= data_in;
      when others =>
        null;
		end case;
	 end if; -- rst
  end if; -- clk
end process;

---------------------------------
--
-- Read Multiplier Registers
--
---------------------------------

umul32_read : process( addr, multiplicand, multiplier, result )
begin
  case addr is
  when "0000" =>
	 data_out <= multiplicand(31 downto 24);
  when "0001" =>
    data_out <= multiplicand(23 downto 16);
  when "0010" =>
    data_out <= multiplicand(15 downto  8);
  when "0011" =>
    data_out <= multiplicand( 7 downto  0);
  when "0100" =>
    data_out <= multiplier(31 downto 24);
  when "0101" =>
    data_out <= multiplier(23 downto 16);
  when "0110" =>
    data_out <= multiplier(15 downto  8);
  when "0111" =>
    data_out <= multiplier( 7 downto  0);
  when "1000" =>
    data_out <= result(63 downto 56);
  when "1001" =>
    data_out <= result(55 downto 48);
  when "1010" =>
    data_out <= result(47 downto 40);
  when "1011" =>
    data_out <= result(39 downto 32);
  when "1100" =>
    data_out <= result(31 downto 24);
  when "1101" =>
    data_out <= result(23 downto 16);
  when "1110" =>
    data_out <= result(15 downto  8);
  when "1111" =>
    data_out <= result( 7 downto  0);
  when others =>
    null;
  end case;
		
end process;

---------------------------------
--
-- Perform 32 x 32 multiply
--
---------------------------------

umul32_multiply : process( rst, clk, multiplicand, multiplier )
begin
  if falling_edge( clk ) then
    if (rst = '1') then
      mul_out_0 <= (others => '0');
      mul_out_1 <= (others => '0');
      mul_out_2 <= (others => '0');
      mul_out_3 <= (others => '0');
      result    <= (others => '0');
    else

      --
      -- cycle 1
      -- Multiply 18 bit unsigned segments together to generate a 36 bit unsigned result
      --
      mul_out_0 <= ("00" & multiplicand(31 downto 16)) * ("00" & multiplier(31 downto 16));
      mul_out_1 <= ("00" & multiplicand(31 downto 16)) * ("00" & multiplier(15 downto  0));
      mul_out_2 <= ("00" & multiplicand(15 downto  0)) * ("00" & multiplier(31 downto 16));
      mul_out_3 <= ("00" & multiplicand(15 downto  0)) * ("00" & multiplier(15 downto  0));

      --
      -- Cycle 2
      -- Add multiplied 32 bit segment of results together
      --
      result <= (mul_out_0(31 downto 0) & mul_out_3(31 downto 0)) +
                (zero16 & mul_out_1(31 downto 0) & zero16) +
                (zero16 & mul_out_2(31 downto 0) & zero16);

    end if; -- rst
  end if; -- clk
end process;

end rtl;
	
