--===========================================================================--
--                                                                           --
--  mul32.vhd - Synthesizable 32 bit Multiplier Register for Spartan 3/3E    --
--                                                                           --
--===========================================================================--
--
--  File name      : mul32.vhd
--
--  Entity name    : mul32
--
--  Purpose        : Implements a 32 bit x 32 bit hardware multiplier register
--                   with 64 bit result. Consists of 16 x 8 bit registers.
--                   Designed for Spartan 3/3E with 18 x 18 bit multiplier blocks. 
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
--  Copyright (C) 2008 - 2010 John Kent
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
--
-- 0.2      John Kent     2010-06-17   Header & GPL added
--

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_unsigned.all;
library unisim;
  use unisim.vcomponents.all;

entity mul32 is
	port (	
	 clk       : in  std_logic;
    rst       : in  std_logic;
    cs        : in  std_logic;
    rw        : in  std_logic;
    addr      : in  std_logic_vector(3 downto 0);
    dati      : in  std_logic_vector(7 downto 0);
	 dato      : out std_logic_vector(7 downto 0));
end entity;

architecture rtl of mul32 is
--
-- registers
--
signal mul_reg0 : std_logic_vector(7 downto 0);
signal mul_reg1 : std_logic_vector(7 downto 0);
signal mul_reg2 : std_logic_vector(7 downto 0);
signal mul_reg3 : std_logic_vector(7 downto 0);
signal mul_reg4 : std_logic_vector(7 downto 0);
signal mul_reg5 : std_logic_vector(7 downto 0);
signal mul_reg6 : std_logic_vector(7 downto 0);
signal mul_reg7 : std_logic_vector(7 downto 0);
signal mul_reg8 : std_logic_vector(7 downto 0);
signal mul_reg9 : std_logic_vector(7 downto 0);
signal mul_reg10 : std_logic_vector(7 downto 0);
signal mul_reg11 : std_logic_vector(7 downto 0);
signal mul_reg12 : std_logic_vector(7 downto 0);
signal mul_reg13 : std_logic_vector(7 downto 0);
signal mul_reg14 : std_logic_vector(7 downto 0);
signal mul_reg15 : std_logic_vector(7 downto 0);

begin

---------------------------------
--
-- Write Multiplier Registers
--
---------------------------------

mul_write : process( clk )
begin
  if clk'event and clk = '0' then
    if rst = '1' then
      mul_reg0  <= "00000000";
      mul_reg1  <= "00000000";
      mul_reg2  <= "00000000";
      mul_reg3  <= "00000000";
      mul_reg4  <= "00000000";
      mul_reg5  <= "00000000";
      mul_reg6  <= "00000000";
      mul_reg7  <= "00000000";
    else
	   if cs = '1' and rw = '0' then
        case addr is
	     when "0000" =>
		    mul_reg0 <= dati;
	     when "0001" =>
		    mul_reg1 <= dati;
	     when "0010" =>
		    mul_reg2 <= dati;
	     when "0011" =>
		    mul_reg3 <= dati;
	     when "0100" =>
		    mul_reg4 <= dati;
	     when "0101" =>
		    mul_reg5 <= dati;
	     when "0110" =>
		    mul_reg6 <= dati;
	     when "0111" =>
		    mul_reg7 <= dati;
        when others =>
		    null;
		  end case;
	   end if;
	 end if;
  end if;
end process;

---------------------------------
--
-- Read Multiplier Registers
--
---------------------------------

mul_read : process(  addr,
                     mul_reg0, mul_reg1, mul_reg2, mul_reg3,
                     mul_reg4, mul_reg5, mul_reg6, mul_reg7,
                     mul_reg8, mul_reg9, mul_reg10, mul_reg11,
                     mul_reg12, mul_reg13, mul_reg14, mul_reg15 )
begin
      case addr is
	     when "0000" =>
		    dato <= mul_reg0;
	     when "0001" =>
		    dato <= mul_reg1;
	     when "0010" =>
		    dato <= mul_reg2;
	     when "0011" =>
		    dato <= mul_reg3;
	     when "0100" =>
		    dato <= mul_reg4;
	     when "0101" =>
		    dato <= mul_reg5;
	     when "0110" =>
		    dato <= mul_reg6;
	     when "0111" =>
		    dato <= mul_reg7;
	     when "1000" =>
		    dato <= mul_reg8;
	     when "1001" =>
		    dato <= mul_reg9;
	     when "1010" =>
		    dato <= mul_reg10;
	     when "1011" =>
		    dato <= mul_reg11;
	     when "1100" =>
		    dato <= mul_reg12;
	     when "1101" =>
		    dato <= mul_reg13;
	     when "1110" =>
		    dato <= mul_reg14;
	     when "1111" =>
		    dato <= mul_reg15;
        when others =>
		    null;
		end case;
		
end process;

---------------------------------
--
-- Perform 32 x 32 multiply
--
---------------------------------

my_mul32 : process(
                     mul_reg0, mul_reg1, mul_reg2, mul_reg3,
                     mul_reg4, mul_reg5, mul_reg6, mul_reg7
							 )
variable mul_left_hi  : std_logic_vector(17 downto 0);
variable mul_left_lo  : std_logic_vector(17 downto 0);
variable mul_right_hi : std_logic_vector(17 downto 0);
variable mul_right_lo : std_logic_vector(17 downto 0);
variable mul_out_0    : std_logic_vector(35 downto 0);
variable mul_out_1    : std_logic_vector(35 downto 0);
variable mul_out_2    : std_logic_vector(35 downto 0);
variable mul_out_3    : std_logic_vector(35 downto 0);
variable mul_out      : std_logic_vector(63 downto 0);
begin
  mul_left_hi  := "00" & mul_reg0 & mul_reg1;
  mul_left_lo  := "00" & mul_reg2 & mul_reg3; 
  mul_right_hi := "00" & mul_reg4 & mul_reg5;
  mul_right_lo := "00" &mul_reg6 & mul_reg7;
  mul_out_0    := mul_left_hi * mul_right_hi;
  mul_out_1    := mul_left_hi * mul_right_lo;
  mul_out_2    := mul_left_lo * mul_right_hi;
  mul_out_3    := mul_left_lo * mul_right_lo;
  mul_out      := (mul_out_0( 31 downto 0) & "0000000000000000" & "0000000000000000") +
                  ("0000000000000000" & mul_out_1( 31 downto 0) & "0000000000000000") +
                  ("0000000000000000" & mul_out_2( 31 downto 0) & "0000000000000000") +
                  ("0000000000000000" & "0000000000000000" & mul_out_3( 31 downto 0));
  mul_reg8  <= mul_out(63 downto 56);
  mul_reg9  <= mul_out(55 downto 48);
  mul_reg10 <= mul_out(47 downto 40);
  mul_reg11 <= mul_out(39 downto 32);
  mul_reg12 <= mul_out(31 downto 24);
  mul_reg13 <= mul_out(23 downto 16);
  mul_reg14 <= mul_out(15 downto  8);
  mul_reg15 <= mul_out( 7 downto  0);
end process;

end rtl;
	
