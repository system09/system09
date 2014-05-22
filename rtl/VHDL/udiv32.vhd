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
--  0 Dividend  1st Byte MSB
--  1           2nd Byte
--  2           3rd Byte 
--  3           4th Byte LSB
--  4 Divisor   1st Byte MSB
--  5           2nd Byte
--  6           3rd Byte
--  7           4th Byte LSB
--  8 Result    1st Byte MSB
--  9           2nd Byte
-- 10           3rd Byte
-- 11           4th byte LSB
-- 12 Remainder 1st Byte MSB
-- 13           2nd Byte
-- 14           3rd Byte
-- 15           4th byte LSB
--
--  32 bit unsigned binary division.
--
--  Write the most significant byte of the dividend at the 0th register first 
--  down to the least significant byte of the divisor in the 7th register.
--  Writing the least significant byte of the divisor will start the division.
--
--  The 32 bit division will take 32 clock cycles.
--  There is no status register so the CPU must execute a software delay
--  to wait 32 clock cycles after the least significant byte of the divisor 
--  is written before reading the result of the division or the remainder.
--
--  The dividend and divisor input registers are read/writable
--  The result and remainder output registers are read only.
--  The result register holds the integer part of the result of the division.
--  The remainder register holds the dividend modulo the divisor.
--
--  Copyright (C) 2012 - 2014 John Kent
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
-- 0.2     John Kent     2014-05-07    Replaced Status register with 4 byte remainder
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

signal dividend      : std_logic_vector(31 downto 0) := (others => '0');
signal divisor       : std_logic_vector(31 downto 0) := (others => '0');
signal result        : std_logic_vector(31 downto 0) := (others => '0');
signal count         : std_logic_vector( 4 downto 0) := (others => '0');
signal start_flag    : std_logic := '0';
signal finish_flag   : std_logic := '0';

signal dividend_temp : std_logic_vector(64 downto 0);
signal divisor_temp  : std_logic_vector(64 downto 0);

begin

--
-- Write registers
--
udiv32_write : process( clk, rst, data_in, finish_flag )
begin
  if falling_edge( clk ) then
    if rst = '1' then
      dividend    <= (others=> '0');   -- reset the dividend to zero
      divisor     <= (others=> '0');   -- reset the divisor to zero
      start_flag  <= '0';					-- the default state is stopped
    else
      --
      -- start bit is normally reset
      --
      start_flag <= '0';
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
          -- writing the last byte of the divisor 
			 -- should pulse the start flag high for one cycle 
			 -- starting the division,
          -- provided the previous division has finished
          --
			 if (finish_flag = '1') then
				start_flag <= '1';
			 end if;
        when others =>
           null;
        end case;
      end if;
    end if; -- rst
  end if; -- clk
end process;

--
-- Read registers
--
udiv32_read : process( addr, dividend, divisor, result, dividend_temp )
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
    data_out <= dividend_temp(31 downto 24);
  when "1101" =>
    data_out <= dividend_temp(23 downto 16);
  when "1110" =>
    data_out <= dividend_temp(15 downto  8);
  when "1111" =>
    data_out <= dividend_temp( 7 downto  0);
  when others =>
    null;
  end case;
end process;

--
-- When the finish flag is high and the start flag goes high, 
-- start the division by clearing the finish flag
-- When the finish flag is low and the count reaches 31 
udiv32_divide : process( rst, clk, start_flag, finish_flag )
variable result_temp    : std_logic_vector(64 downto 0);
begin
  if falling_edge( clk ) then
    if (rst = '1') then
      dividend_temp  <= (others=>'0');
      divisor_temp   <= (others=>'0');
      result_temp    := (others=>'0');
      result         <= (others=>'0');
      count          <= (others=>'0');
      finish_flag    <= '1';             -- default state is finished
    else
      --
      -- start the division if the last division was complete
		-- i.e. the finish flag was set
      -- and the last byte of the divisor was just written
      -- i.e. the start flag was pulsed high for one clock cycle
		--
      if (start_flag = '1') and (finish_flag = '1') then
        dividend_temp(64)           <= '0';                    -- dividend carry bit
        dividend_temp(63 downto 32) <= (others => '0');        -- zero MSW
        dividend_temp(31 downto  0) <= dividend(31 downto 0);  -- Mantissa in the bottom

        divisor_temp(64)            <= '0';                    -- divisor carry bit
        divisor_temp(63)            <= '0';                    -- 
        divisor_temp(62 downto 31)  <= divisor(31 downto 0);   -- divisor starts off one bit down in MSW
        divisor_temp(30 downto  0)  <= (others => '0');        -- bottom of divisor is zero

        result_temp                 := (others => '0');        -- clear the result variable

        count                       <= (others => '0');        -- zero the bit counter
        finish_flag                 <= '0';                    -- flag that the division is in progress

      elsif ( finish_flag = '0' ) then                         -- if finish flag is clear the division must be in progress

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
        result(0)           <= not result_temp(64);
        result(31 downto 1) <= result(30 downto 0);
        --
        -- 32 bit division should take 32 clock cycles
        --
        count               <= count + "00001";
        --
		  -- When the count reaches the 31st cycle of the division 
        -- flag that the division is complete by setting the finish flag.
        -- 
        if count = "11111" then
          finish_flag <= '1';  -- flag division complete
        end if;

      end if; -- start/finish
    end if; -- rst
  end if; -- clk
end process;

end rtl; -- end of architecture