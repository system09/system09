--===========================================================================--
--                                                                           --
--  vdg6847.vhd - Synthesizable Video Display Generator                      --
--                                                                           --
--===========================================================================--
--
--  File name      : vdg6847.vhd
--
--  Purpose        : 
--                  
--  Dependencies   : ieee.std_logic_1164
--                   ieee.numeric_std
--
--  Uses           : ram_2k (ram2k_b16.vhd)             32 byte row buffer
--                   char_rom (char_rom2k_b16.vhd)      2KByte Character Generator ROM 
--
--  Author         : John E. Kent
--
--  Email          : dilbert57@opencores.org      
--
--  Web            : http://opencores.org/project,system01
--
--  Description    : Display Timing:
--                       800 pixels / line
--                       525 lines / frame
--                       None interlaced
--                       25MHz pixel clock implies 
--                       31.25 KHz line rate
--                       59.52 Hz frame rate   
--                       Timing settable by generics.
--
--                   Display Size:
--                       2 x 256 horizontal display pixels
--                       2 x 192 vertical display lines
--                       32 characters across
--                       16 characters down.
--
--                   Character Size:
--                       2 x 8 horizontal pixels across
--                       2 x 12 vertical scan lines down
--
--
-- Video Timing :
--
-- http://tinyvga.com/vga-timing/640x480@60Hz
--
-- Horizontal 800 Pixels/ 25MHz Pixel Clock = 32usec Line period = 31.25 KHz Line Frequency
-- /--------------------------\_____________/---------------\______________/
--     640 Pixels Display       16 Pixel FP    96 Pixel HS     48 Pixel BP
--    
--      VDG_CLK_FREQ           : integer := 25000000; -- HZ
--      VDG_HOR_LEFT_BORDER    : integer := 64; -- PIXELS 2.56us
--	     VDG_HOR_CHAR_PIX       : integer := 16; -- PIXELS 0.64us
--	     VDG_HOR_CHARS          : integer := 32; -- CHARACTERS 20.48us
--      VDG_HOR_RIGHT_BORDER   : integer := 64; -- PIXELS 2.56us
--	     VDG_HOR_FRONT_PORCH    : integer := 16; -- PIXELS 0.64us
--	     VDG_HOR_SYNC           : integer := 96; -- PIXELS 3.84us
--	     VDG_HOR_BACK_PORCH     : integer := 48; -- PIXELS 1.92us
--
-- Vertical 525 Lines * 32 usec Line rate = 16.64ms Frame Period = 60 Frame frequency  
-- /---------------------------\____________/---------------\______________/
--     480 Line Display          10 Line FP     2 Line VS      33 Line BP
--
--      VDG_VER_TOP_BORDER     : integer := 48; -- LINES 1.536ms
--	     VDG_VER_CHAR_LINES     : integer := 24; -- LINES 0.768ms
--	     VDG_VER_CHARS          : integer := 16; -- CHARACTERS 12.288ms
--      VDG_VER_BOT_BORDER     : integer := 48; -- LINES 0.768ms
--	     VDG_VER_FRONT_PORCH    : integer := 10; -- LINES 0.320ms
--	     VDG_VER_SYNC           : integer := 2;  -- LINES 0.064ms
--	     VDG_VER_BACK_PORCH     : integer := 33; -- LINES 1.056ms
--
-- 800 pixel VGA => 640 pixels active
-- 512 horizontal pixels = 256 VDG pixels
-- 128 boarder pixels = 64 VDG border pixels = 32 pixels left + 32 pixels right
-- 
-- 520 vertical lines for 60 Hz Frame rate
-- 400 display lines 16 scan lines * 25 character rows
-- 384 VGA lines = 192 VDG line
-- 
-- G/A_N . CSS_N Sampled during Left Border 
-- 
-- G/A_N S/A_N EXT/INT_N INV GM2 GM1 GM0 Alpha/Graphic Mode Select        # of Colors Memory
-- -----------------------------------------------------------------------------------------------
--   0     0       0      0   x   x   x   Internal Alphanumerics                  2    512  32x16
--   0     0       0      1   x   x   x   Internal Alphanumerics Inverted         2    512  32x16
--   0     0       1      0   x   x   x   External Alphanumerics                  2    512  32x16
--   0     0       1      1   x   x   x   External Alphanumerics Inverted         2    512  32x16
-- -----------------------------------------------------------------------------------------------
--   0     1       0      x   x   x   x   Semigraphics 4 (SG4)                    8    512  32x16
--   0     1       1      x   x   x   x   Semigraphics 6 (SG6)                    4    512  32x16
-- -----------------------------------------------------------------------------------------------
--   1     x       x      x   0   0   0    64x 64 Color Graphics One (CG1)        4   1024  64x64
--   1     x       x      x   0   0   1   128x 64 Resolution Graphics One (RG1)   2   1024 128x64
--   1     x       x      x   0   1   0   128x 64 Color Graphics Two (CG2)        4   2048 128x64
--   1     x       x      x   0   1   1   128x 96 Resolution Graphics Two (RG2)   2   1536 128x96
--   1     x       x      x   1   0   0   128x 96 Color Graphics Three (CG3)      4   3072 128x96
--   1     x       x      x   1   0   1   128x192 Resolution Graphics Three (RG3) 2   3072 128x192
--   1     x       x      x   1   1   0   128x192 Color Graphics Six (CG6)        4   6144 128x192
--   1     x       x      x   1   1   1   256x192 Resolution Graphics Six (RG6)   2   6144 256x192
-- -----------------------------------------------------------------------------------------------
-- 
-- MS_N=0 disables address bus
--
-- 3.58MHz clock divided by 3.5 tp produce 1.02 MHz clock
--
-- Trp = 1/1.02MHz
-- RP occurs after 1st 12 active lines resest external character generator row counter
-- FS presets external character generator counter
-- 
-- 1.02MHZ -> DA[0..3]  4 bit Hor counter
--            DA[4]     5th bit Hor counter + New Row (0th bit of ver counter)
--            DA[5..12] 8 bit Ver counter
-- 
-- G/A_N enables 1/12 row counter to address generator & semi-grapghics S4&S6 Character Generator address bus
-- Row pulse (RP_N) generated by row counter
--
-- DD[7..0] -> 3way 8bit multiplexer
-- DD[5..0] -> Alpha Character generator -> 3way 8bit multiplexer
-- DD[5..0] -> S4 & S6 semigraphics generator -> 3way 8bit multiplexer
-- 
-- Alphanumeric
-- ============
-- 
-- Internal 
-- 
-- hor - 2 blank 5 active 1 blank
-- ver - 3 blank 7 active 2 blank
-- colours 2
-- memory 512 x 8
-- 
-- External
-- hor -  8 active
-- ver - 12 active
-- colours 2
-- memory 512 x 8
--
-- CSS INV FG     BG     Border
-- 0   0   Green  Black  Black
-- 0   1   Black  Green  Black
-- 1   0   Orange Black  Black
-- 1   1   Black  Orange Black
--
-- Semigraphics
-- ============
--
-- SG4
-- 
-- memory 512 x 8
--
-- L3  L2                    XT C2 C1 C0 L3 L2 L1 L0
-- L1  L0
--
-- Lx C2 C1 C0
-- 0  X  X  X  Black
-- 1  0  0  0  Green   (G)
-- 1  0  0  1  Yellow  (R+G)
-- 1  0  1  0  Blue    (B)
-- 1  0  1  1  Red     (R)
-- 1  1  0  0  Buff    (R+G+B)
-- 1  1  0  1  Cyan    (G+B)
-- 1  1  1  0  Magenta (R+B)
-- 1  1  1  1  Orange  (?)
-- 
-- SG4 pixels 4x6
--
-- SG6
--
-- memory 512 x 8
-- 
-- L5 L4                     C1 C0 L5 L4 L3 L2 L1 L0
-- L3 L2
-- L1 L0
--
-- SG6 pixels 4x4
--
-- Lx CSS c1 c0
-- 0  X   X  X  Black
-- 1  0   0  0  Green   (G)
-- 1  0   0  1  Yellow  (R+G)
-- 1  0   1  0  Blue    (B)
-- 1  0   1  1  Red     (R)
-- 1  1   0  0  Buff    (R+G+B)
-- 1  1   0  1  Cyan    (G+B)
-- 1  1   1  0  Magenta (R+B)
-- 1  1   1  1  Orange  (?)
--
-- Colour graphics
-- ===============
-- 
--                           E3    E2    E1    E0
-- E3 E2 E1 E0               C1 C0 C1 C0 C1 C0 C1 C0
--
-- CG1 pixels 4x3  64x64
-- CG2 pixels 2x3  128x64
-- CG3 pixels 2x2  128x96
-- CG6 pixels 2x1  128x192
--
-- CSS c1 c0
-- 0   0  0  Green   (G)
-- 0   0  1  Yellow  (R+G)
-- 0   1  0  Blue    (B)
-- 0   1  1  Red     (R)
-- 1   0  0  Buff    (R+G+B)
-- 1   0  1  Cyan    (G+B)
-- 1   1  0  Magenta (R+B)
-- 1   1  1  Orange  (?)
--
-- Resolution Graphics
-- ===================
--
-- L7 L6 L5 L4 L3 L2 L1 L0   L7 L6 L5 L4 L3 L2 L1 L0
-- 
-- RG1 pixels 2x3  128x64
-- RG2 pixels 2x2  128x96
-- RG3 pixesl 2x1  128x192
-- RG6 pixels 1x1  256x192
-- 
-- CSS Lx
-- 0   0 Black
-- 0   1 Green
-- 1   0 Black
-- 1   1 Buff
--
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
--                              Revision  History                            --
--                                                                           --
--===========================================================================--
--
-- Version Author      Date        Changes
--
-- 0.1     John Kent   2011-01-06  Initial release derived from vdu8
--
--

Library IEEE;
  use IEEE.std_logic_1164.all;
  use IEEE.numeric_std.all;
--Library unisim;
--  use unisim.vcomponents.all;

Entity vdg6847 is
  generic(
        VDG_CLK_FREQ           : integer := 25000000; -- HZ
        VDG_HOR_LEFT_BORDER    : integer := 64; -- Pixels 0.64us
	     VDG_HOR_CHARS          : integer := 32; -- CHARACTERS
	     VDG_HOR_CHAR_PIXELS    : integer := 16; -- PIXELS 0.64us
        VHG_HOR_RIGHT_BORDER   : integer := 64; -- PIXELS 2.56us
	     VDG_HOR_FRONT_PORCH    : integer := 16; -- PIXELS 0.64us
	     VDG_HOR_SYNC           : integer := 96; -- PIXELS 3.84us
	     VDG_HOR_BACK_PORCH     : integer := 48; -- PIXELS 1.536us
        VDG_VER_TOP_BORDER     : integer := 48; -- LINES  1.536ms
	     VDG_VER_CHARS          : integer := 16; -- CHARACTERS 8.53ms
	     VDG_VER_CHAR_LINES     : integer := 24; -- LINES 0.768ms
        VDG_VER_BOT_BORDER     : integer := 48; -- LINES 1.536ms
	     VDG_VER_FRONT_PORCH    : integer := 10; -- LINES 0.320ms
	     VDG_VER_SYNC           : integer := 2;  -- LINES 0.064ms
	     VDG_VER_BACK_PORCH     : integer := 33  -- LINES 1.056ms
  );
  port(
    -- control register interface
    vdg_clk      : in  std_logic;                     -- 25 MHz Pixel Clock
    vdg_rst      : in  std_logic;                     -- Reset in
    vdg_gm       : in  std_logic_vector(2 downto 0);  -- Graphics Mode
    vdg_css      : in  std_logic;                     -- Colour Set Select
    vdg_inv      : in  std_logic;                     -- Inverse video
    vdg_ga_n     : in  std_logic;                     -- Graphics/Alphanumerics_n
    vdg_sa_n     : in  std_logic;                     -- Semigraphics/Alphanumeric_n
    vdg_extint_n : in  std_logic;                     -- External/Internal_n character generator
    vdg_data_in  : in  std_logic_vector(15 downto 0); -- Data In (access memory as 16 bits wide)
    vdg_rp       : out std_logic;                     -- Character Row Pulse
    vdg_vma      : out std_logic;                     -- Memory request
    vdg_rw       : out std_logic;                     -- Memory read
    vdg_addr     : out std_logic_vector(12 downto 0); -- Address Out

    -- video outputs
    vdg_red_o    : out std_logic;
    vdg_green_o  : out std_logic;
    vdg_blue_o   : out std_logic;
    vdg_hsync_o  : out std_logic;
    vdg_vsync_o  : out std_logic
    );
end vdg6847;

Architecture RTL of vdg6847 is
  --
  -- Synchronisation constants
  --
  -- Horizontal Display length
  constant HOR_DISP_LEN    : integer := VDG_HOR_CHARS * VDG_HOR_CHAR_PIXELS;
  -- Horizontal Display starts
  constant HOR_DISP_BEG    : integer := VDG_HOR_LEFT_BORDER;
  -- Horizontal Display ends
  constant HOR_DISP_END    : integer := HOR_DISP_BEG + HOR_DISP_LEN;
  -- Horizontal Border ends
  constant HOR_BORD_END    : integer := HOR_DISP_END + VDG_HOR_RIGHT_BORDER;
  -- Horizontal synch starts
  constant HOR_SYNC_BEG    : integer := HOR_BORD_END + VDG_HOR_FRONT_PORCH;
  -- Horizontal Synch pulse
  constant HOR_SYNC_END    : integer := HOR_SYNC_BEG + VDG_HOR_SYNC;
  -- Last pixel in scan line
  constant HOR_SCAN_END    : integer := HOR_SYNC_END + VDG_HOR_BACK_PORCH;

  -- Vertical Display length
  constant VER_DISP_LEN    : integer := VDG_VER_CHARS * VDG_VER_CHAR_LINES;
  --  Vertical Display Starts
  constant VER_DISP_BEG    : integer := VDG_VER_TOP_BORDER;
  -- Vertical Display Ends
  constant VER_DISP_END    : integer := VER_BORD_BEG + VER_DISP_LEN;
  -- Vertical Border Ends
  constant VER_BORD_END    : integer := VER_DISP_END + VDG_VER_BOT_BORDER;
  -- start of vertical synch pulse
  constant VER_SYNC_BEG    : integer := VER_BORD_END + VDG_VER_FRONT_PORCH;
  -- end of vertical synch pulse
  constant VER_SYNC_END    : integer := VER_SYNC_BEG + VDG_VER_SYNC;
  -- Last scan row in the frame
  constant VER_SCAN_END    : integer := VER_SYNC_END + VDG_VER_BACK_PORCH;

  --
  -- Scan lines per row of alphanumeric or graphics characters
  -- for various graphics modes
  --
  constant VER_ALPHA_ROWS  : integer := 24; -- 24 x  16 lines
  constant VER_SEMI_ROWS   : integer := 24; -- 24 x  16 lines
  constant VER_CG1_ROWS    : integer :=  6; --  6 x  64 lines
  constant VER_RG1_ROWS    : integer :=  6; --  6 x  64 lines
  constant VER_CG2_ROWS    : integer :=  6; --  6 x  64 lines
  constant VER_RG2_ROWS    : integer :=  4; --  4 x  96 lines
  constant VER_CG3_ROWS    : integer :=  4; --  4 x  96 lines
  constant VER_RG3_ROWS    : integer :=  2; --  2 x 192 lines
  constant VER_CG6_ROWS    : integer :=  2; --  2 x 192 lines
  constant VER_RG6_ROWS    : integer :=  2; --  2 x 192 lines

  --
  -- Scan pixels per line of alphanumeric or graphics characters
  -- for various graphics modes
  --
  --                                          +-------------- Clocks per pixel
  --                                          |   +---------- Pixels per block
  --                                          |   |   +------ Blocks per access
  --                                          |   |   |   +-- Access per line
  --                                          |   |   |   |
  constant HOR_ALPHA_COLS  : integer := 16; -- 2 x 8 x 1 x 32 ( 32 x  16 characters)
  constant HOR_SEMI_COLS   : integer := 16; -- 2 x 8 x 1 x 32 ( 32 x  16 semi graphics)
  constant HOR_CG1_COLS    : integer := 32; -- 2 x 4 x 4 x 16 ( 64 x  64 x 4) blocks x lines x colours 
  constant HOR_RG1_COLS    : integer := 32; -- 2 x 2 x 8 x 16 (128 x  64 x 2)
  constant HOR_CG2_COLS    : integer := 16; -- 2 x 2 x 4 x 32 (128 x  64 x 4)
  constant HOR_RG2_COLS    : integer := 32; -- 2 x 2 x 8 x 16 (128 x  96 x 2)
  constant HOR_CG3_COLS    : integer := 16; -- 2 x 2 x 4 x 32 (128 x  96 x 4)
  constant HOR_RG3_COLS    : integer := 32; -- 2 x 2 x 8 x 16 (128 x 192 x 2)
  constant HOR_CG6_COLS    : integer := 16; -- 2 x 2 x 4 x 32 (128 x 192 x 4)
  constant HOR_RG6_COLS    : integer := 16; -- 2 x 1 x 8 x 32 (256 x 192 x 2)

  signal hor_count         : std_logic_vector(9 downto 0) := (others=>'0'); -- 0 to 799
  signal hor_addr          : std_logic_vector(4 downto 0) := (others=>'0'); -- 0 to 31
  signal hor_border        : std_logic := '0';
  signal hor_border_dly1   : std_logic := '0';
  signal hor_border_dly2   : std_logic := '0';
  signal hor_display       : std_logic := '0';
  signal hor_display_dly1  : std_logic := '0';
  signal hor_display_dly2  : std_logic := '0';
  signal hor_sync          : std_logic := '1';
  signal hor_sync_dly1     : std_logic := '1';
  signal hor_sync_dly2     : std_logic := '1';
  signal col_count         : std_logic_vector(4 downto 0) := (others=>'0'); -- 0 to 31
  signal ver_count         : std_logic_vector(9 downto 0) := (others=>'0'); -- 0 to 524
  signal ver_addr          : std_logic_vector(7 downto 0) := (others=>'0'); -- 0 to 191
  signal ver_border        : std_logic := '0';
  signal ver_border_dly1   : std_logic := '0';
  signal ver_border_dly2   : std_logic := '0';
  signal ver_display       : std_logic := '0';
  signal ver_display_dly1  : std_logic := '0';
  signal ver_display_dly2  : std_logic := '0';
  signal ver_sync          : std_logic := '1';
  signal ver_sync_dly1     : std_logic := '1';
  signal ver_sync_dly2     : std_logic := '1';
  signal row_count         : std_logic_vector(4 downto 0) := (other=>'0');  -- 0 to 23

  --
  -- Delayed input signals
  --
  signal vdg_ga_n_dly1     : std_logic := '0';
  signal vdg_ga_n_dly2     : std_logic := '0';
  signal vdg_sa_n_dly1     : std_logic := '0';
  signal vdg_sa_n_dly2     : std_logic := '0';
  signal vdg_inv_dly1      : std_logic := '0';
  signal vdg_inv_dly2      : std_logic := '0';
  signal vdg_extint_n_dly1 : std_logic := '0';
  signal vdg_extint_n_dly2 : std_logic := '0';
  signal vdg_gm_dly1       : std_logic_vector(2 downto 0) := "000";
  signal vdg_gm_dly2       : std_logic_vector(2 downto 0) := "000";
  signal vdg_css_dly1      : std_logic := '0';
  signal vdg_css_dly2      : std_logic := '0';

  signal vdg_mem_req       : std_logic := '0';                              -- video memory request (internal vdg_vma)
  signal vdg_data_reg      : std_logic_vector(7 downto 0) := (others=>'0'); -- video input data register
  --
  -- Character Generator ROM
  --
  signal char_addr         : std_logic_vector(10 downto 0);
  signal char_data_out     : std_logic_vector(7 downto 0);
  --
  -- Colour Lookup ROM
  --
  signal colour_addr       : std_logic_vector(3 downto 0);
  signal colour_data       : std_logic_vector(5 downto 0);
  --
  -- Video Shift register
  --
  signal shift_reg_c0      : std_logic_vector(7 downto 0);
  signal shift_reg_c1      : std_logic_vector(7 downto 0);
  signal shift_reg_c2      : std_logic_vector(7 downto 0);
  signal shift_reg_lx      : std_logic_vector(7 downto 0);

  --
  -- Block Ram Character gen
  --
  component char_rom
    port (
      clk      : in  std_logic;
      rst      : in  std_logic;
      cs       : in  std_logic;
      rw       : in  std_logic;
      addr     : in  std_logic_vector (10 downto 0);
      data_in  : in  std_logic_vector (7 downto 0);
      data_out : out std_logic_vector (7 downto 0)
      );
  end component;

  --
  -- Colour Look-up table
  -- Maps VDG colour codes to R1G1B1R0G0B0
  --
  component colour_map
    port (
      addr   : in   std_logic_vector(3 downto 0);
      data   : out  std_logic_vector(6 downto 0)
    );
  end component;


begin

--
-- Instantiate Character generator ROM
--
vdg_char_rom : char_rom port map(
	clk      => vdg_clk,
	rst      => vdg_rst,
	cs       => '1',
	rw       => '1',
	addr     => char_addr,
	data_in  => "00000000",
	data_out => char_data_out
    );

--
-- Instantiate Colour Look-up ROM
--
vdg_colour_rom : char_rom port map(
	addr     => colour_addr,
	data     => colour_data
    );


--
-- Display Timing & Sync generator
-- Generate Horizontal and Vertical Timing Signals for Video Signal
--
vdg_timing : process(vdg_clk)
begin
  if vdg_clk'event and vdg_clk = '0' then
    --
    -- hor_count counts pixels (640 + extra time for sync signals)
    --
    if unsigned(hor_count) = (HOR_SCAN_END-1) then
      hor_count <= (others=>'0');
    else
      hor_count <= std_logic_vector(unsigned(hor_count) + 1);
    end if;
    --
    -- Generate Border On signal
    --
    --  hor_count   0                    640                  800
    --  hor_border  /--------------------\____________________/--
    --
    if unsigned(hor_count) = (HOR_SCAN_END-1) then
      hor_border <= '1';
    elsif unsigned(hor_count) = (HOR_BORD_END-1) then
      hor_border <= '0';
    end if;
    -- 
    -- Generate Display On signal
    --
    --  hor_count   0  64             576                      800
    --  hor_border  ___/--------------\___________________________
    --                                                          
    if unsigned(hor_count) = (HOR_DISP_BEG-1) then
      hor_display <= '1';
    elsif unsigned(hor_count) = (HOR_DISP_END-1) then
      hor_display <= '0';
    end if;
    --
    -- Generate Horizontal Sync Signal using hor_count
    --
    --  hor_count  0                           656       752   800
    --  hor_sync  -----------------------------__________---------
    --
    if unsigned(hor_count) = (HOR_SYNC_BEG-1) then
      hor_sync <= '0';
    elsif unsigned(hor_count) = (HOR_SYNC_END-1) then
      hor_sync <= '1';
    end if;
    --
    -- ver_count counts rows of pixels
    -- 480 lines + extra time for sync signals
    -- active display 16 rows * 24 scan lines
    --
    if unsigned(hor_count) = (HOR_SYNC_END-1) then
      if unsigned(ver_count) = (VER_SCAN_END-1) then
        ver_count <= "0000000000";
      else
        ver_count <= std_logic_vector(unsigned(ver_count) + 1);
      end if;
    end if;
    --
    -- Generate vertical border
    --
    --  ver_count      0                      480                   525
    --  ver_display   /-----------------------\________________________
    --
    if unsigned(ver_count) = (VER_SCAN_END-1) then
      ver_border <= '1';
    elsif unsigned(ver_count) = (VER_BORD_END-1) then
      ver_border <= '0';
    end if;
    --
    -- Generate vertical display
    --
    --  ver_count     0   48              432   480                  525
    --  ver_display   ___/----------------\_____________________________
    --
    if unsigned(ver_count) = (VER_DISP_BEG-1) then
      ver_display <= '1';
    elsif unsigned(ver_count) = (VER_DISP_END-1) then
      ver_display <= '0';
    end if;

    --
    -- Generate Vertical Sync timing using ver_count
    --
    --  ver_count     0                       480      490   492      525
    --  ver_sync      ---------------------------------______------------
    --
    if unsigned(ver_count) = (VER_SYNC_BEG-1) then
      ver_sync <= '0';
    elsif unsigned(ver_count) = (VER_SYNC_END-1) then
      ver_sync <= '1';
    end if;
  end if;

end process;

--
-- VDG Memory address access
--
-- G/A_N S/A_N EXT/INT_N INV GM2 GM1 GM0 Alpha/Graphic Mode Select        # of Colors Memory
-- -----------------------------------------------------------------------------------------------
--   0     0       0      0   x   x   x   Internal Alphanumerics                  2    512  32x16
--   0     0       0      1   x   x   x   Internal Alphanumerics Inverted         2    512  32x16
--   0     0       1      0   x   x   x   External Alphanumerics                  2    512  32x16
--   0     0       1      1   x   x   x   External Alphanumerics Inverted         2    512  32x16
-- -----------------------------------------------------------------------------------------------
--   0     1       0      x   x   x   x   Semigraphics 4 (SG4)                    8    512  32x16
--   0     1       1      x   x   x   x   Semigraphics 6 (SG6)                    4    512  32x16
-- -----------------------------------------------------------------------------------------------
--   1     x       x      x   0   0   0    64x 64 Color Graphics One (CG1)        4   1024  64x64
--   1     x       x      x   0   0   1   128x 64 Resolution Graphics One (RG1)   2   1024 128x64
--   1     x       x      x   0   1   0   128x 64 Color Graphics Two (CG2)        4   2048 128x64
--   1     x       x      x   0   1   1   128x 96 Resolution Graphics Two (RG2)   2   1536 128x96
--   1     x       x      x   1   0   0   128x 96 Color Graphics Three (CG3)      4   3072 128x96
--   1     x       x      x   1   0   1   128x192 Resolution Graphics Three (RG3) 2   3072 128x192
--   1     x       x      x   1   1   0   128x192 Color Graphics Six (CG6)        4   6144 128x192
--   1     x       x      x   1   1   1   256x192 Resolution Graphics Six (RG6)   2   6144 256x192
-- -----------------------------------------------------------------------------------------------
--
-- Horizontal memory access is every 2 x 8 clock cycles.
-- Vertical memory access is every 2, 4, 6 or 24 lines.
--  
vdg_memory_address : process( vdg_clk, hor_display, ver_display, col_count, row_count, vdg_ga_n, vdg_gm, hor_addr, ver_addr )
variable hor_addr_inc : std_logic;
variable ver_row_inc  : std_logic;
variable ver_addr_inc : std_logic;
begin
  if vdg_clk'event and vdg_clk='0' then
    vdg_mem_req  <= '0';
    hor_addr_inc := '0';
    ver_row_inc  := '0';
    ver_addr_inc := '0';

    if hor_display = '1' and ver_display = '1' then
      --
      -- Request the display byte on a column count of zero
      --
      if col_count = "00000" then
        vdg_mem_req <= '1';
      end if;
      --
      -- Increment the column count for each active display clock cycle
      --
      col_count <= col_count + "00001";
      --
      -- Test the graphic mode to determine the column counter terminal column
      -- 
      if vdg_ga_n = 0 then
        --
        -- Alphanumeric of semi graphic mode
        --
        if unsigned(col_count) = (HOR_ALPHA_COLS-1) then
          hor_addr_inc := '1';
        end if;               
      elsif
        --
        -- Colour Graphic or Resolution Graphic mode
        --
        case vdg_gm is
        when "000" =>
          if unsigned(col_count) = (HOR_CG1_COLS-1) then
            hor_addr_inc := '1';
          end if;               
        when "001" =>
          if unsigned(col_count) = (HOR_RG1_COLS-1) then
            hor_addr_inc := '1';
          end if;               
        when "010" =>
          if unsigned(col_count) = (HOR_CG2_COLS-1) then
            hor_addr_inc := '1';
          end if;               
        when "011" =>
          if unsigned(col_count) = (HOR_RG2_COLS-1) then
            hor_addr_inc := '1';
          end if;               
        when "100" =>
          if unsigned(col_count) = (HOR_CG3_COLS-1) then
            hor_addr_inc := '1';
          end if;               
        when "101" =>
          if unsigned(col_count) = (HOR_RG3_COLS-1) then
            hor_addr_inc := '1';
          end if;               
        when "110" =>
          if unsigned(col_count) = (HOR_CG6_COLS-1) then
            hor_addr_inc := '1';
          end if;               
        when "111" =>
          if unsigned(col_count) = (HOR_RG6_COLS-1) then
            hor_addr_inc := '1';
          end if;               
        end case;
      end if;
      
      --
      -- Increment the Horizontal Address while the display is active
      --        
      if hor_addr_inc = '1' then
        --
        -- Reset the coulmn counter
        --
        col_count <= "00000";
        --
        -- Incrument the horizontal address
        -- 
        hor_addr(4 downto 0) <= hor_addr(4 downto 0) + "00001";
        --
        -- Test the graphic mode to determine the horizontal address terminal count
        -- 
        if vdg_ga_n = 0 then
          --
          -- Alphanumeric of semi graphic mode
          --
          if unsigned(hor_addr) = ((HOR_DISP_LEN/HOR_ALPHA_COLS)-1) then
            ver_row_inc := '1';
          end if;               
        elsif
          --
          -- Colour Graphic or Resolution Graphic mode
          --
          case vdg_gm is
          when "000" =>
            if unsigned(hor_addr) = ((HOR_DISP_LEN/HOR_CG1_COLS)-1) then
              ver_row_inc := '1';
            end if;               
          when "001" =>
            if unsigned(hor_addr) = ((HOR_DISP_LEN/HOR_RG1_COLS)-1) then
              ver_row_inc := '1';
            end if;               
          when "010" =>
            if unsigned(hor_addr) = ((HOR_DISP_LEN/HOR_CG2_COLS)-1) then
              ver_row_inc := '1';
            end if;               
          when "011" =>
            if unsigned(hor_addr) = ((HOR_DISP_LEN/HOR_RG2_COLS)-1) then
              ver_row_inc := '1';
            end if;               
          when "100" =>
            if unsigned(hor_addr) = ((HOR_DISP_LEN/HOR_CG3_COLS)-1) then
              ver_row_inc := '1';
            end if;               
          when "101" =>
            if unsigned(hor_addr) = ((HOR_DISP_LEN/HOR_RG3_COLS)-1) then
              ver_row_inc := '1';
            end if;               
          when "110" =>
            if unsigned(hor_addr) = ((HOR_DISP_LEN/HOR_CG6_COLS)-1) then
              ver_row_inc := '1';
            end if;               
          when "111" =>
            if unsigned(hor_addr) = ((HOR_DISP_LEN/HOR_RG6_COLS)-1) then
              ver_row_inc := '1';
            end if;               
          end case;
        end if;
      end if;
      --
      -- Vertical row address
      -- Increment the row counter at the end of each horizontal display line
      --
      if ver_row_inc = '1' then
        --
        -- reset the horizontal address
        --
        hor_addr <= (others=>'0');
        --
        -- increment the row counter
        --
        row_count <= row_count + "00001";
        --
        -- Test the graphic mode to determine the row counter terminal count
        --
        if vdg_ga_n = 0 then
          if unsigned(row_count) = (VER_ALPHA_ROWS-1) then
            ver_addr_inc := '1';
          end if;               
        elsif
          case vdg_gm is
          when "000" =>
            if unsigned(row_count) = (VER_CG1_ROWS-1) then
              ver_addr_inc := '1';
            end if;               
          when "001" =>
            if unsigned(row_count) = (VER_RG1_ROWS-1) then
              ver_addr_inc := '1';
            end if;               
          when "010" =>
            if unsigned(row_count) = (VER_CG2_ROWS-1) then
              ver_addr_inc := '1';
            end if;               
          when "011" =>
            if unsigned(row_count) = (VER_RG2_ROWS-1) then
              ver_addr_inc := '1';
            end if;               
          when "100" =>
            if unsigned(row_count) = (VER_CG3_ROWS-1) then
              ver_addr_inc := '1';
            end if;               
          when "101" =>
            if unsigned(row_count) = (VER_RG3_ROWS-1) then
              ver_addr_inc := '1';
            end if;               
          when "110" =>
            if unsigned(row_count) = (VER_CG6_ROWS-1) then
              ver_addr_inc := '1';
            end if;               
          when "111" =>
            if unsigned(row_count) = (VER_RG6_ROWS-1) then
              ver_addr_inc := '1';
            end if;               
          end case;
        end if;
        --
        -- increment the vertical address
        --
        if ver_addr_inc = '1' then
          --
          -- reset the row counter
          --
          row_count <= "00000";
          --
          -- Increment the vertical address
          --
          ver_addr(7 downto 0) <= ver_addr(7 downto 0) + "00000001";
        end if;
      end if;          
    else
      --
      -- Display not active
      -- reset the counters and address
      --
      if hor_display = '0' then
        col_count <= (others=>'0');
        hor_addr  <= (others=>'0');
      end if;

      if ver_display = '0' then
        ver_addr  <= (others=>'0');
        row_count <= (others=>'0');
      end if;

      --
      -- No memory accesses if the display is not active
      --
      vdg_mem_req <= '0';

    end if;
    --
    -- Delay horizontal and vertical display enables by one clock cycle
    --
    hor_display_dly1  <= hor_display;
    ver_display_dly1  <= ver_display;
    hor_border_dly1   <= hor_border;
    ver_border_dly1   <= ver_border;
    hor_sync_dly1     <= hor_sync;
    ver_sync_dly1     <= ver_sync;
    vdg_ga_n_dly1     <= vdg_ga_n;
    vdg_sa_n_dly1     <= vdg_sa_n;
    vdg_inv_dly1      <= vdg_inv;
    vdg_extint_n_dly1 <= vdg_extint_n;
    vdg_gm_dly1       <= vdg_gm;
    vdg_css_dly1      <= vdg_css;
  end if;
end process;


--
-- VDG Address multiplexer
--
vdg_addr_mux : process( vdg_ga_n, vdg_gm, ver_addr, hor_addr, vga_mem_req )
begin 
  if vdg_ga_n = 0 then -- Alphanumeric / Semi-graphics 32 x 16 = 512 bytes
      vdg_addr(12 downto 0) <=       ver_addr(7 downto 0) & hor_addr(4 downto 0);
  elsif
    case vdg_gm is
    when "000" || "001" || "011" || "101" =>
       -- 000 CG1 16 x  64 = 1KB,
       -- 001 RG1 16 x  64 = 1KB,
       -- 011 RG2 16 x  96 = 1.5KB
       -- 101 RG3 16 x 192 = 3KB
      vdg_addr(12 downto 0) <= "0" & ver_addr(7 downto 0) & hor_addr(3 downto 0);
    when others =>
      -- 010 CG2 32 x  64 = 2KB
      -- 100 CG3 32 x  96 = 3KB
      -- 110 CG6 32 x 192 = 6KB
      -- 111 RG6 32 x 192 = 6KB
      vdg_addr(12 downto 0) <=       ver_addr(7 downto 0) & hor_addr(4 downto 0);
    end case;
  end if;        
  vdg_vma <= vdg_mem_req; -- valid memory adddress
end process;

--
-- VDG Data read
--
vdg_data_read : process( vdg_clk, vdg_rst, vdg_mem_req, vdg_data_in )
begin
  if vdg_clk'event and vdg_clk='0' then
    if vdg_rst = '1' the
      vdg_data_reg <= (others=>'0');
    elsif vdg_mem_req = '1' then  -- this will occur when col_count = "00001"
      vdg_data_reg = vdg_data_in; -- vdg_data_reg valid when col_count = "00010"
    end if;
    --
    -- Delay horizontal and vertical display enables by one clock cycle
    --
    hor_display_dly2  <= hor_display_dly1;
    ver_display_dly2  <= ver_display_dly1;
    hor_border_dly2   <= hor_border_dly1;
    ver_border_dly2   <= ver_border_dly1;
    hor_sync_dly2     <= hor_sync_dly1;
    ver_sync_dly2     <= ver_sync_dly1;
    vdg_ga_n_dly2     <= vdg_ga_n_dly1;
    vdg_sa_n_dly2     <= vdg_sa_n_dly1;
    vdg_inv_dly2      <= vdg_inv_dly1;
    vdg_extint_n_dly2 <= vdg_extint_n_dly1;
    vdg_gm_dly2       <= vdg_gm_dly1;
    vdg_css_dly2      <= vdg_css_dly1;
  end if;

end process;

--
-- Decode Graphics Mode
--
-- gm2 gm1 gm0 mode                                colours mem
-- 0   0   0    64x 64 Color Graphics One (CG1)        4   1024  64x64
-- 0   0   1   128x 64 Resolution Graphics One (RG1)   2   1024 128x64
-- 0   1   0   128x 64 Color Graphics Two (CG2)        4   2048 128x64
-- 0   1   1   128x 96 Resolution Graphics Two (RG2)   2   1536 128x96
-- 1   0   0   128x 96 Color Graphics Three (cG3)      4   3072 128x96
-- 1   0   1   128x192 Resolution Graphics Three (RG3) 2   3072 128x192
-- 1   1   0   128x192 Color Graphics Six (CG6)        4   6144 128x192
-- 1   1   1   256x192 Resolution Graphics Six (RG6)   2   6144 256x192
--
-- Colour graphics
-- ===============
-- gm0 == 0
--                           E3    E2    E1    E0
-- E3 E2 E1 E0               C1 C0 C1 C0 C1 C0 C1 C0
--
-- CSS c1 c0
-- 0   0  0  Green   (G)
-- 0   0  1  Yellow  (R+G)
-- 0   1  0  Blue    (B)
-- 0   1  1  Red     (R)
-- 1   0  0  Buff    (R+G+B)
-- 1   0  1  Cyan    (G+B)
-- 1   1  0  Magenta (R+B)
-- 1   1  1  Orange  (?)
--
-- Resolution Graphics
-- ===================
-- gm0 == 1
--
-- L7 L6 L5 L4 L3 L2 L1 L0   L7 L6 L5 L4 L3 L2 L1 L0
-- 
-- CSS Lx
-- 0   0 Black
-- 0   1 Green
-- 1   0 Black
-- 1   1 Buff
--
vdg_data_decode : process( vdg_clk, vdg_ga_n_dly2, vdg_sa_n_dly2, vdg_gm_dly2, vdg_data_reg, col_count, row_count )
variable vdg_bit : std_logic;
begin
  --
  -- Here to look up character ROM
  -- This will take one clock cycle
  -- and should be performed on rising clock when col_count = "00010"
  --
  char_addr(9 downto 4) <= vdg_data_reg(5 downto 0);
  char_addr(3 downto 0) <= row_count(3 downto 0);

  if vdg_clk'event and vdg_clk='0' then
    if col_count = "00010" then
      if hor_display_dly2 = '1' and ver_display_dly2 = '1' then
        if vdg_ga_n_dly2 = '0' then
          if vdg_sa_n_dly2 = '0' then
            --
            -- Alphanumeric mode
            --
            -- CSS INV FG     BG     Border
            -- 0   0   Green  Black  Black
            -- 0   1   Black  Green  Black
            -- 1   0   Orange Black  Black
            -- 1   1   Black  Orange Black
            --
            shift_reg_c0 <= vdg_css_dly2 & vdg_css_dly2 & vdg_css_dly2 & vdg_css_dly2 & 
                            vdg_css_dly2 & vdg_css_dly2 & vdg_css_dly2 & vdg_css_dly2; 
            shift_reg_c1 <= vdg_css_dly2 & vdg_css_dly2 & vdg_css_dly2 & vdg_css_dly2 & 
                            vdg_css_dly2 & vdg_css_dly2 & vdg_css_dly2 & vdg_css_dly2; 
            shift_reg_c2 <= vdg_css_dly2 & vdg_css_dly2 & vdg_css_dly2 & vdg_css_dly2 & 
                            vdg_css_dly2 & vdg_css_dly2 & vdg_css_dly2 & vdg_css_dly2; 
            if vdg_extint_n_dly2 = '0' then
              --
              -- Alpha numerics, internal character generator
              --
              shift_reg_lx <= char_data_out(7) & char_data_out(6) & char_data_out(5) & char_data_out(4) & 
                              char_data_out(3) & char_data_out(2) & char_data_out(1) & char_data_out(0);
            else
              --
              -- Alpha numerics, external character generator
              --
              shift_reg_lx <= vdg_data_reg(7) & vdg_data_reg(6) & vdg_data_reg(5) & vdg_data_reg(4) & 
                              vdg_data_reg(3) & vdg_data_reg(2) & vdg_data_reg(1) & vdg_data_reg(0);
            end if;
          else
            if vdg_extint_n_dly2 = '0' then
              --
              -- Semi graphics 4
              --
              -- L3  L2                    XT C2 C1 C0 L3 L2 L1 L0
              -- L1  L0
              --
              -- SG4 pixels 4x6
              -- 
              shift_reg_c0 <= vdg_data_reg(4) & vdg_data_reg(4) & vdg_data_reg(4) & vdg_data_reg(4) & 
                              vdg_data_reg(4) & vdg_data_reg(4) & vdg_data_reg(4) & vdg_data_reg(4); 
              shift_reg_c1 <= vdg_data_reg(5) & vdg_data_reg(5) & vdg_data_reg(5) & vdg_data_reg(5) & 
                              vdg_data_reg(5) & vdg_data_reg(5) & vdg_data_reg(5) & vdg_data_reg(5); 
              shift_reg_c2 <= vdg_data_reg(6) & vdg_data_reg(6) & vdg_data_reg(6) & vdg_data_reg(6) & 
                              vdg_data_reg(6) & vdg_data_reg(6) & vdg_data_reg(6) & vdg_data_reg(6); 
              if row_count(4 downto 2) < "011" then
                shift_reg_lx <= vdg_data_reg(3) & vdg_data_reg(3) & vdg_data_reg(3) & vdg_data_reg(3) & 
                                vdg_data_reg(2) & vdg_data_reg(2) & vdg_data_reg(2) & vdg_data_reg(2);
              else
                shift_reg_lx <= vdg_data_reg(1) & vdg_data_reg(1) & vdg_data_reg(1) & vdg_data_reg(1) & 
                                vdg_data_reg(0) & vdg_data_reg(0) & vdg_data_reg(0) & vdg_data_reg(0);
              end if;
            else
              -- Semi graphics 6
              -- 
              -- L5 L4                     C1 C0 L5 L4 L3 L2 L1 L0
              -- L3 L2
              -- L1 L0
              --
              -- SG6 pixels 4x4
              --
              -- Lx CSS C1 C0
              --
              shift_reg_c0 <= vdg_data_reg(6) & vdg_data_reg(6) & vdg_data_reg(6) & vdg_data_reg(6) & 
                              vdg_data_reg(6) & vdg_data_reg(6) & vdg_data_reg(6) & vdg_data_reg(6); 
              shift_reg_c1 <= vdg_data_reg(7) & vdg_data_reg(7) & vdg_data_reg(7) & vdg_data_reg(7) & 
                              vdg_data_reg(7) & vdg_data_reg(7) & vdg_data_reg(7) & vdg_data_reg(7); 
              shift_reg_c2 <= vdg_css_dly2 & vdg_css_dly2 & vdg_css_dly2 & vdg_css_dly2 & 
                              vdg_css_dly2 & vdg_css_dly2 & vdg_css_dly2 & vdg_css_dly2; 
              case row_count(4 downto 3) is
              when "00" =>
                shift_reg_lx <= vdg_data_reg(5) & vdg_data_reg(5) & vdg_data_reg(5) & vdg_data_reg(5) & 
                                vdg_data_reg(4) & vdg_data_reg(4) & vdg_data_reg(4) & vdg_data_reg(4); 
              when "01" =>
                shift_reg_lx <= vdg_data_reg(3) & vdg_data_reg(3) & vdg_data_reg(3) & vdg_data_reg(3) & 
                                vdg_data_reg(2) & vdg_data_reg(2) & vdg_data_reg(2) & vdg_data_reg(2); 
              when "10" =>
                shift_reg_lx <= vdg_data_reg(1) & vdg_data_reg(1) & vdg_data_reg(1) & vdg_data_reg(1) & 
                                vdg_data_reg(0) & vdg_data_reg(0) & vdg_data_reg(0) & vdg_data_reg(0); 
              when others =>
                shift_reg_lx <= "00000000";
              end case;
            end if; -- ext/int_n
          end if; -- alpha / semi graphics
        else
          --
          -- Colour Graphics / Resolution Graphics
          --
          if vdg_gm_dly2(0) = '0' then
            --
            -- Colour Graphics
            -- C1 C0 C1 C0 C1 C0 C1 C0
            --
            shift_reg_c0 <= vdg_data_reg(6) & vdg_data_reg(6) & vdg_data_reg(4) & vdg_data_reg(4) & 
                            vdg_data_reg(2) & vdg_data_reg(2) & vdg_data_reg(0) & vdg_data_reg(0); 
            shift_reg_c1 <= vdg_data_reg(7) & vdg_data_reg(7) & vdg_data_reg(5) & vdg_data_reg(5) & 
                            vdg_data_reg(3) & vdg_data_reg(3) & vdg_data_reg(1) & vdg_data_reg(1); 
            shift_reg_c2 <= vdg_css_dly2 & vdg_css_dly2 & vdg_css_dly2 & vdg_css_dly2 &
                            vdg_css_dly2 & vdg_css_dly2 & vdg_css_dly2 & vdg_css_dly2; 
            shift_reg_lx <= "11111111"; 
          else
            -- Resolution Graphics 1
            -- L7 L6 L5 L4 L3 L2 L1 L0
            -- CSS Lx
            -- 0   0 Black
            -- 0   1 Green
            -- 1   0 Black
            -- 1   1 Buff
            --
            -- C2=CSS C1 C0
            -- 0      0  0  Green   (G)
            -- 1      0  0  Buff    (R+G+B)
            --
            shift_reg_c0 <= "00000000"; 
            shift_reg_c1 <= "00000000"; 
            shift_reg_c2 <= vdg_css_dly2 & vdg_css_dly2 & vdg_css_dly2 & vdg_css_dly2 & 
                            vdg_css_dly2 & vdg_css_dly2 & vdg_css_dly2 & vdg_css_dly2; 
            shift_reg_lx <= vdg_data_reg(7) & vdg_data_reg(6) & vdg_data_reg(5) & vdg_data_reg(4) & 
                            vdg_data_reg(3) & vdg_data_reg(2) & vdg_data_reg(1) & vdg_data_reg(0); 
          end case;
        end if; -- ga_n
      elsif hor_border_dly2 = '1' and ver_border_dly2 = '1' then
        --
        -- Border
        -- Lx C2=CSS C1 C0 Colour
        -- 1  0      0  0  Green
        -- 1  1      0  0  Buff
        --
        shift_reg_c0 <= "00000000"; 
        shift_reg_c1 <= "00000000"; 
        shift_reg_c2 <= vdg_css_dly2 & vdg_css_dly2 & vdg_css_dly2 & vdg_css_dly2 & 
                        vdg_css_dly2 & vdg_css_dly2 & vdg_css_dly2 & vdg_css_dly2; 
        shift_reg_lx <= "11111111"; 
      else
        --
        -- Retrace
        -- Lx C2 C1 C0 Colour
        -- 0  X  X  X  Black
        --
        shift_reg_c0 <= "00000000"; 
        shift_reg_c1 <= "00000000"; 
        shift_reg_c2 <= "00000000"; 
        shift_reg_lx <= "00000000"; 
      end if;
    else -- col_count != "00010"
      if (vdg_ga_n_dly2 = 0) or (vdg_gm_dly2 = "010") or (vdg_gm_dly2 = "100") or (vdg_gm_dly2 = "110") or (vdg_gm_dly2 = "111") then 
        -- Alphanumeric  32 x 16 = 512 bytes
        -- Semi-graphics 32 x 16 = 512 bytes
        -- 010 CG2 32 x  64 = 2KB
        -- 100 CG3 32 x  96 = 3KB
        -- 110 CG6 32 x 192 = 6KB
        -- 111 RG6 32 x 192 = 6KB
        if col_count(0) = '0' then
          shift_reg_c0 <= shift_reg_c0(6 downto 0) & "0";
          shift_reg_c1 <= shift_reg_c1(6 downto 0) & "0";
          shift_reg_c2 <= shift_reg_c2(6 downto 0) & "0";
          shift_reg_lx <= shift_reg_lx(6 downto 0) & "0";
        end if;
      else
        -- 000 CG1 16 x  64 = 1KB,
        -- 001 RG1 16 x  64 = 1KB,
        -- 011 RG2 16 x  96 = 1.5KB
        -- 101 RG3 16 x 192 = 3KB
        if col_count(1 downto 0) = "10" then
          shift_reg_c0 <= shift_reg_c0(6 downto 0) & "0";
          shift_reg_c1 <= shift_reg_c1(6 downto 0) & "0";
          shift_reg_c2 <= shift_reg_c2(6 downto 0) & "0";
          shift_reg_lx <= shift_reg_lx(6 downto 0) & "0";
        end if;
      end if;        
    end if; -- col_count
  end if; -- clk
end process;

--
-- Convert shift register output into RGB
--
vdg_colour_map : process( vdg_clk, shift_reg_c0, shift_reg_c1, shift_reg_c2, shift_reg_lx,
                          colour_data, vdg_inv, vdg_ga_n, vdg_sa_n,
                          vdg_red, vdg_green, vdg_blue, hor_sync_dly2, ver_sync_dly2 )
variable vdg_div2 : std_logic;
variable vdg_tog : std_logic;
begin
  vdg_toggle := vdg_inv_dly2 and (not vdg_ga_n_dly2) and (not vdg_sa_n_dly2);
  colour_addr(0) <= shift_reg_c0(7) xor vdg_tog;
  colour_addr(1) <= shift_reg_c1(7) xor vdg_tog;
  colour_addr(2) <= shift_reg_c2(7) xor vdg_tog;
  colour_addr(3) <= shift_reg_lx(7) xor vdg_tog;
  if vdg_clk'event and vdg_clk='0' then
    if vdg_div2 = '0' then
      vdg_red   <= colour_data(2);
      vdg_green <= colour_data(1);
      vdg_blue  <= colour_data(0);
    else
      vdg_red   <= colour_data(5);
      vdg_green <= colour_data(4);
      vdg_blue  <= colour_data(3);
    end if;
    vdg_div2 := not vdg_div2;
    vdg_red_o   <= vdg_red;
    vdg_green_o <= vdg_green;
    vdg_blue_o  <= vdg_blue;
    vdg_hsync_o <= hor_sync_dly2;
    vdg_vsync_o <= ver_sync_dly2;
  end if;

end process;

end RTL;
