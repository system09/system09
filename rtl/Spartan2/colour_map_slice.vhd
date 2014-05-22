--===========================================================================--
--                                                                           --
--  colour_map.vhd - Synthesizable Colour Map Look Up Table for the vdg6847  --
--                                                                           --
--===========================================================================--
--
-- File name      : colour_map.vhd
--
-- Purpose        : colour look up table for the vdg6847.vhd
--                  
-- Dependencies   : ieee.std_logic_1164
--                  ieee.numeric_std
--
-- Uses           : none
--
-- Author         : John E. Kent
--
-- Email          : dilbert57@opencores.org      
--
-- Web            : http://opencores.org/project,system01
--
-- Description    : Colour look up table for the vdg6847
--                  Dithers Red & Yellow to produce orange
--              
-- A3 A2 A1 A0                    D5 D4 D3 D2 D1 D0
-- Lx C2 C1 C0                    R1 G1 B1 R0 G0 B0
-- 0  X   X  X  Black             0  0  0  0  0  0
-- 1  0   0  0  Green   (G)       0  1  0  0  1  0
-- 1  0   0  1  Yellow  (R+G)     1  1  0  1  1  0
-- 1  0   1  0  Blue    (B)       0  0  1  0  0  1
-- 1  0   1  1  Red     (R)       1  0  0  1  0  0
-- 1  1   0  0  Buff    (R+G+B)   1  1  1  1  1  1
-- 1  1   0  1  Cyan    (G+B)     0  1  1  0  1  1
-- 1  1   1  0  Magenta (R+B)     1  0  1  1  0  1
-- 1  1   1  1  Orange  (R+G, R)  1  1  0  1  0  0
--

library ieee;
	use ieee.std_logic_1164.all;
	use ieee.std_logic_arith.all;
	use ieee.std_logic_unsigned.all;

entity colour_map is
  port (
    addr   : in   std_logic_vector(3 downto 0);
    data   : out  std_logic_vector(6 downto 0)
  );
end entity;

architecture rtl of colour_map is
  constant width   : integer := 6;
  constant memsize : integer := 8;

  type rom_array is array(0 to memsize-1) of std_logic_vector(width-1 downto 0);

  constant rom_data : rom_array :=
(
"000000",  -- black
"000000",  -- black
"000000",  -- black
"000000",  -- black
"000000",  -- black
"000000",  -- black
"000000",  -- black
"000000",  -- black
"010010",  -- green   (G, G)
"110110",  -- yellow  (R+G,R+G)
"001001",  -- blue    (B, B)
"100100",  -- red     (R, R)
"111111",  -- buff    (R+G+B, R+G+B))
"011011",  -- cyan    (G+B, G+B)
"101101",  -- magenta (R+B, R+B)
"110100"   -- orange  (R+G,R
);
begin

   data <= rom_data(conv_integer(addr)); 

end architecture;

