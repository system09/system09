--
-- ram_controller.vhd
--

library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

entity ram_controller is
  port(
    reset      : in std_logic;
	 clk        : in std_logic;
    cs_ram     : in std_logic;
    rw         : in std_logic;
    din        : in std_logic_vector(7 downto 0);
    dout       : out std_logic_vector(7 downto 0);
    addr       : in std_logic_vector(19 downto 0);

    -- External interface
    ram_oen    : out   std_logic;
    ram_cen    : out   std_logic;
    ram_wen    : out   std_logic;
    ram_io     : inout std_logic_vector(15 downto 0);
    ram_a      : out   std_logic_vector(20 downto 1);
    ram_bhen   : out   std_logic;
    ram_blen   : out   std_logic
  );
end;

architecture external_ram of ram_controller is

signal we : std_logic;

begin

--
-- 1M byte SRAM Control
-- Processes to read and write memory based on bus signals
-- Uses bhe/ble controlled write 
-- so that clock stretching can be performed on the CF
--
ram_process: process( clk, addr, rw,
					       cs_ram, ram_io, din )
begin
	 ram_wen  <=     rw;
	 ram_oen  <= not rw;
    ram_cen  <= not cs_ram;
    ram_bhen <= not( (not addr(0)) and clk );
    ram_blen <= not(      addr(0)  and clk );

	 ram_a(20) <= '0';
	 ram_a(19 downto 1) <= addr(19 downto 1);

    if (rw = '0') and (addr(0) = '0') then
		ram_io(15 downto 8) <= din;
	 else
      ram_io(15 downto 8)  <= "ZZZZZZZZ";
	 end if;

    if (rw = '0') and (addr(0) = '1') then
		ram_io(7 downto 0) <= din;
	 else
      ram_io(7 downto 0)  <= "ZZZZZZZZ";
	 end if;

	 if addr(0) = '0' then
      dout <= ram_io(15 downto 8);
    else
      dout <= ram_io(7 downto 0);
    end if;
end process;

end;
