--===========================================================================--
--                                                                           --
--                  Synthesizable unsigned 32 bit integer divider            --
--                                                                           --
--===========================================================================--
--
--  File name      : udiv32.vhd
--
--  Entity name    : udiv32
--
--  Purpose        : Implements a 32 bit unsigned integer divider 
--                  
--  Dependencies   : ieee.std_logic_1164
--                   ieee.numeric_std
--                   ieee.std_logic_unsigned
--
--  Author         : John E. Kent
--
--  Email          : dilbert57@opencores.org      
--
--  Web            : http://opencores.org/project,system09
--
--  Registers      :
--  0 Dividend 1st Byte MSB
--  1          2nd Byte
--  2          3rd Byte 
--  3          4th Byte LSB
--  4 Divisor  1st Byte MSB
--  5          2nd Byte
--  6          3rd Byte
--  7          4th Byte LSB
--  8 Result   1st Byte MSB
--  9          2nd Byte
-- 10          3rd Byte
-- 11          4th byte LSB
-- 12 Status Register
--
--
--  Copyright (C) 2012 John Kent
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
-- Version Author        Date         Changes
--
-- 0.1     John Kent     2012-04-06    Initial version
--

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;
  use ieee.std_logic_unsigned.all;
--library unisim;
--  use unisim.vcomponents.all;

entity udiv32 is
  port (
    --
    -- CPU Interface signals
    --
    clk      : in  std_logic;                     -- CPU Clock
    rst      : in  std_logic;                     -- Reset input (active high)
    cs       : in  std_logic;                     -- Chip Select
    addr     : in  std_logic_vector(3 downto 0);  -- Register Select
    rw       : in  std_logic;                     -- Read / Not Write
    data_in  : in  std_logic_vector(7 downto 0);  -- Data Bus In 
    data_out : out std_logic_vector(7 downto 0)  -- Data Bus Out
    );
end udiv32;  
--================== End of entity ==============================--

-------------------------------------------------------------------
-- Architecture for unsigned 32 bit integer divider interface
-------------------------------------------------------------------

architecture rtl of udiv32 is

--
-- Status register bits
--
constant START       : integer := 6;
constant FINISH      : integer := 7;

signal dividend      : std_logic_vector(31 downto 0) := (others => '0');
signal divisor       : std_logic_vector(31 downto 0) := (others => '0');
signal result        : std_logic_vector(31 downto 0) := (others => '0');
signal stat_reg      : std_logic_vector( 7 downto 0) := (others => '0');
signal count         : std_logic_vector( 4 downto 0) := (others => '0');

signal dividend_temp : std_logic_vector(64 downto 0);
signal divisor_temp  : std_logic_vector(64 downto 0);

begin

--
-- Write registers
--
udiv32_write : process( clk, rst )
begin
  if falling_edge( clk ) then
    if rst = '1' then
      dividend <= (others=> '0');
      divisor  <= (others=> '0');
      stat_reg(5 downto 0) <= (others=> '0');
      stat_reg(START)      <= '0';
      stat_reg(FINISH)     <= '1';
    else
      --
      -- start bit is normally reset
      --
      stat_reg(START) <= '0';
      --
      -- write to registers
      --
      if (cs = '1') and (rw = '0') then
        case addr is
        when "0000" =>
          dividend(31 downto 24) <= data_in;
        when "0001" =>
          dividend(23 downto 16) <= data_in;
        when "0010" =>
          dividend(15 downto  8) <= data_in;
        when "0011" =>
          dividend( 7 downto  0) <= data_in;
        when "0100" =>
          divisor(31 downto 24)  <= data_in;
        when "0101" =>
          divisor(23 downto 16)  <= data_in;
        when "0110" =>
          divisor(15 downto  8)  <= data_in;
        when "0111" =>
          divisor( 7 downto  0)  <= data_in;
          --
          -- writing to the last byte of the divisor should start the division
          -- by pulsing the start status bit high for one cycle
          --
          stat_reg(START) <= '1';
        when others =>
           null;
        end case;
      end if;

      --
      -- status register finish control
      -- if the last division was complete and the start flag pulsed
      -- clear the finish status bit indicating a division is in progress
      --
      if (stat_reg(FINISH) = '1') and (stat_reg(START) = '1') then
        stat_reg(FINISH) <= '0';
      --
      -- when division is in progress and the count rolls over to zero 
      -- the division should be complete
      --
      elsif (stat_reg(FINISH) = '0') and count = "11111" then
        stat_reg(FINISH) <= '1';  -- flag division complete
      end if; -- stat_reg

    end if; -- rst
  end if; -- clk
end process;

--
-- Read registers
--
udiv32_read : process( addr, dividend, divisor, result, stat_reg )
begin
  case addr is
  when "0000" =>
    data_out <= dividend(31 downto 24);
  when "0001" =>
    data_out <= dividend(23 downto 16);
  when "0010" =>
    data_out <= dividend(15 downto  8);
  when "0011" =>
    data_out <= dividend( 7 downto  0);
  when "0100" =>
      data_out <= divisor(31 downto 24);
  when "0101" =>
    data_out <= divisor(23 downto 16);
  when "0110" =>
    data_out <= divisor(15 downto  8);
  when "0111" =>
    data_out <= divisor( 7 downto  0);
  when "1000" =>
    data_out <= result(31 downto 24);
  when "1001" =>
    data_out <= result(23 downto 16);
  when "1010" =>
    data_out <= result(15 downto  8);
  when "1011" =>
    data_out <= result( 7 downto  0);
  when "1100" =>
    data_out <= stat_reg;
  when others =>
    data_out <= (others => '0');
  end case;
end process;

--
-- When finish is high and start goes high, initiate division
--
udiv32_divide : process( rst, clk )
variable result_temp   : std_logic_vector(64 downto 0);
begin
  if falling_edge( clk ) then
    if (rst = '1') then
      dividend_temp <= (others=>'0');
      divisor_temp  <= (others=>'0');
      result_temp   := (others=>'0');
      result        <= (others=>'0');
      count         <= (others=>'0');
    else
      --
      -- start the division if the last division was complete
      -- and the last byte of the divisor was just written
      --      
      if (stat_reg(FINISH) = '1') and (stat_reg(START) = '1') then

        dividend_temp(64)           <= '0';                    -- dividend carry bit
        dividend_temp(63 downto 32) <= (others => '0');        -- zero MSW
        dividend_temp(31 downto  0) <= dividend(31 downto 0);  -- Mantissa in the bottom

        divisor_temp(64)            <= '0';                    -- divisor carry bit
        divisor_temp(63)            <= '0';                    -- 
        divisor_temp(62 downto 31)  <= divisor(31 downto 0);   -- divisor starts off one bit down in MSW
        divisor_temp(30 downto  0)  <= (others => '0');        -- bottom of divisor is zero

        result_temp                 := (others => '0');        -- clear the result variable

        count                       <= (others => '0');        -- zero the bit counter

      elsif ( stat_reg(FINISH)= '0' ) then                     -- if finish status is zero division must be active

        result_temp   := dividend_temp - divisor_temp;         -- subtract the divisor from the dividend

        if result_temp(64) = '0' then                          -- if the result carry is clear
          dividend_temp <= result_temp;                        -- update the dividend variable with the result variable
        end if;
        --
        -- shift divisor down one bit
        --
        divisor_temp(62 downto 0) <= divisor_temp(63 downto 1);
        --
        -- shift the result up one bit
        -- The LSBit is the inverted result carry
        --
        result(31 downto 1) <= result(30 downto 0);
        result(0)           <= not result_temp(64);
        --
        -- 32 bit division should take 32 clock cycles
        --
        count               <= count + "00001";

      end if; -- start/finish
    end if; -- rst
  end if; -- clk
end process;

end rtl; -- end of architecture