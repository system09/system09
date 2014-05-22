--===========================================================================--
--                                                                           --
--             TESTBENCH    vdu8_tb - VDU8 Testbench.                    --
--                                                                           --
--===========================================================================--
--
-- File name      : vdu8_tb.vhd
--
-- Purpose        : Test system09 VDU8 component
--
-- Dependencies   : ieee.Std_Logic_1164
--                  ieee.std_logic_unsigned
--                  ieee.std_logic_arith
--                  ieee.numeric_std
--
-- Uses           : vdu8     (..\VHDL\vdu8.vhd)              CPU core
--                  ram_2k   (..\Spartan3\ram2k_b16.vhd)      2KB block RAM
--                  char_rom (..\Spartan3\char_rom2k_b16.vhd) 2KB chracter block ROM
--                   
-- Author         : John E. Kent
--                  dilbert57@opencores.org      
-- 
--  Copyright (C) 2008 - 2011 John Kent
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
-- 0.1  2008-07-30 John Kent  First version
-- 0.2  2011-10-09 John Kent  updated header & vdu component
--
--===========================================================================--

library ieee;
   use ieee.std_logic_1164.all;
   use IEEE.STD_LOGIC_ARITH.ALL;
   use IEEE.STD_LOGIC_UNSIGNED.ALL;
   use ieee.numeric_std.all;
library unisim;
   use unisim.vcomponents.all;

entity my_vdu8_tb is
end my_vdu8_tb;

architecture behavior of my_vdu8_tb is

constant CPU_FREQ             : natural := 25_000_000;  -- CPU Clock (Hz)
constant PIX_FREQ             : natural := 25_000_000;  -- VGA Pixel Clock

	 -- CRTC output signals
signal    vga_vsync_n  : Std_Logic;
signal    vga_hsync_n  : Std_Logic;
signal    vga_blue     : std_logic;
signal    vga_green    : std_logic;
signal    vga_red      : std_logic;

-- CPU Debug Interface signals
signal    cpu_reset     : Std_Logic;
signal    cpu_clk       : Std_Logic;
signal    cpu_rw        : std_logic;
signal    cpu_addr      : std_logic_vector(2 downto 0);
signal    vdu_data_out  : std_logic_vector(7 downto 0);
signal    cpu_data_out  : std_logic_vector(7 downto 0);
signal    pix_clk       : std_logic;
signal    vdu_cs        : std_logic;

----------------------------------------
--
-- Video Display Unit.
--
----------------------------------------
component vdu8
  generic(
        VGA_CLK_FREQ           : integer := PIX_FREQ; -- HZ
	     VGA_HOR_CHARS          : integer := 80; -- CHARACTERS 25.6us
	     VGA_HOR_CHAR_PIXELS    : integer := 8;  -- PIXELS 0.32us
	     VGA_HOR_FRONT_PORCH    : integer := 16; -- PIXELS 0.64us
	     VGA_HOR_SYNC           : integer := 96; -- PIXELS 3.84us
	     VGA_HOR_BACK_PORCH     : integer := 48; -- PIXELS 1.92us
	     VGA_VER_CHARS          : integer := 25; -- CHARACTERS 12.8ms
	     VGA_VER_CHAR_LINES     : integer := 16; -- LINES 0.512ms
	     VGA_VER_FRONT_PORCH    : integer := 10; -- LINES 0.320ms
	     VGA_VER_SYNC           : integer := 2;  -- LINES 0.064ms
	     VGA_VER_BACK_PORCH     : integer := 34  -- LINES 1.088ms
  );
  port(
    -- control register interface
    vdu_clk      : in  std_logic;       -- 12.5/25 MHz CPU Clock
    vdu_rst      : in  std_logic;
    vdu_cs       : in  std_logic;
    vdu_rw       : in  std_logic;
    vdu_addr     : in  std_logic_vector(2 downto 0);
    vdu_data_in  : in  std_logic_vector(7 downto 0);
    vdu_data_out : out std_logic_vector(7 downto 0);

    -- vga port connections
    vga_clk      : in  std_logic;       -- 25MHz clock
    vga_red_o    : out std_logic;
    vga_green_o  : out std_logic;
    vga_blue_o   : out std_logic;
    vga_hsync_o  : out std_logic;
    vga_vsync_o  : out std_logic
    );
end component;

begin
----------------------------------------
--
-- Video Display Unit instantiation
--
----------------------------------------
my_vdu : vdu8 
  generic map(
      VGA_CLK_FREQ           => PIX_FREQ, -- HZ
	   VGA_HOR_CHARS          => 80, -- CHARACTERS
	   VGA_HOR_CHAR_PIXELS    => 8,  -- PIXELS
	   VGA_HOR_FRONT_PORCH    => 16, -- PIXELS
	   VGA_HOR_SYNC           => 96, -- PIXELS
	   VGA_HOR_BACK_PORCH     => 48, -- PIXELS
	   VGA_VER_CHARS          => 25, -- CHARACTERS
	   VGA_VER_CHAR_LINES     => 16, -- LINES
	   VGA_VER_FRONT_PORCH    => 10, -- LINES
	   VGA_VER_SYNC           => 2,  -- LINES
	   VGA_VER_BACK_PORCH     => 34  -- LINES
  )
  port map(

		-- Control Registers
		vdu_clk       => cpu_clk,					 -- 12.5 MHz System Clock in
      vdu_rst       => cpu_reset,
		vdu_cs        => vdu_cs,
		vdu_rw        => cpu_rw,
		vdu_addr      => cpu_addr(2 downto 0),
		vdu_data_in   => cpu_data_out,
		vdu_data_out  => vdu_data_out,

      -- vga port connections
      vga_clk       => pix_clk,					 -- 25 MHz VDU pixel clock
      vga_red_o     => vga_red,
      vga_green_o   => vga_green,
      vga_blue_o    => vga_blue,
      vga_hsync_o   => vga_hsync_n,
      vga_vsync_o   => vga_vsync_n
   );

events : PROCESS(pix_clk)
variable count : integer := 0;
BEGIN
		if falling_edge(cpu_clk) then
		   case count is
			--
			-- reset VDU registers
			--
		   when 0 =>
			  cpu_reset    <= '1';
			  vdu_cs       <= '0';
			  cpu_rw       <= '1';
			  cpu_addr     <= "000";
			  cpu_data_out <= "00000000";
         when  8 =>
			  cpu_reset <= '0';
         --
			-- write data register
			--
         when 10 =>
			  vdu_cs       <= '1';
			  cpu_rw       <= '0';
			  cpu_addr     <= "000";
			  cpu_data_out <= "01101001";
         when 11 =>
			  vdu_cs       <= '0';
			  cpu_rw       <= '1';
         --
			-- write attribute register
			--
         when 12 =>
			  vdu_cs       <= '1';
			  cpu_rw       <= '0';
			  cpu_addr     <= "001";
			  cpu_data_out <= "00000111";
         when 13 =>
			  vdu_cs       <= '0';
			  cpu_rw       <= '1';
         --
			-- write cursor column ?
         when 14 =>
			  vdu_cs       <= '1';
			  cpu_rw       <= '0';
			  cpu_addr     <= "010";
			  cpu_data_out <= "00000001";
         when 15 =>
			  vdu_cs       <= '0';
			  cpu_rw       <= '1';
         --
			-- write cursor row ?
			--
         when 16 =>
			  vdu_cs       <= '1';
			  cpu_rw       <= '0';
			  cpu_addr     <= "011";
			  cpu_data_out <= "00000011";
         when 17 =>
			  vdu_cs       <= '0';
			  cpu_rw       <= '1';
         --
			-- write vertical offset
			--
         when 18 =>
			  vdu_cs       <= '1';
			  cpu_rw       <= '0';
			  cpu_addr     <= "100";
			  cpu_data_out <= "00001001";
         when 19 =>
			  vdu_cs       <= '0';
			  cpu_rw       <= '1';
         when others =>
			  null;
         end case;
			count := count + 1;
       end if;
end process;

--
-- Generate a master clock for the SDRAM controller
--
   tb : PROCESS
	variable i : integer;
   BEGIN
		for i in 0 to 360000 loop
			pix_clk <= '0';
			cpu_clk <= '0';
			wait for 20 ns;
			pix_clk <= '1';
			cpu_clk <= '1';
			wait for 20 ns;
      end loop;
      wait; -- will wait forever
   end process;

-- *** End Test Bench - User Defined Section ***

end architecture;
