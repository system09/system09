--===========================================================================--
--                                                                           --
--  S Y N T H E S I Z A B L E    CRTC6845   C O R E                          --
--                                                                           --
--  www.opencores.org - January 2000                                         --
--  This IP core adheres to the GNU public license.                          --
--                                                                           --
--  VHDL model of MC6845 compatible CRTC                                     --
--                                                                           --
--  This model doesn't implement interlace mode. Everything else is          --
--  (probably) according to original MC6845 data sheet (except VTOTADJ).     --
--                                                                           --
--  Implementation in Xilinx Virtex XCV50-6 runs at 50 MHz (character clock).--
--  With external pixel	generator this CRTC could handle 450MHz pixel rate   --
--  (see MC6845 datasheet for typical application).	                       --
--                                                                           --
--  Author:   Damjan Lampret, lampret@opencores.org                          --
--  Reworked: John Kent,      dilbert57@opencores.org                        --
--                                                                           --
--  TO DO:                                                                   --
--                                                                           --
--   - testbench                                                             --
--                                                                           --
--   - interlace mode support, extend VSYNC for V.Total Adjust value (R5)    --
--                                                                           --
--   - verification in a real application                                    --
--                                                                           --
--===========================================================================--
--
-- Revision History
--
-- Version  Date        Author          Modification
-- 1.0      2000-01-??  Damjan Lampret  Original Version
-- 2.0      2012-04-07  John Kent       Substantial rework for System09
--                                      Added vertical synch width to hsw_reg
--                                      Made light pen strobe positive going
-- 
library IEEE;
   use ieee.std_logic_1164.all;
   use IEEE.STD_LOGIC_ARITH.ALL;
   use IEEE.STD_LOGIC_UNSIGNED.ALL;
   use ieee.numeric_std.all;

entity crtc6845 is
  generic (
    DB_WIDTH : integer :=  8;
    MA_WIDTH : integer := 14;
    RA_WIDTH : integer :=  5
    );                      
  port (
    clk       : in  STD_LOGIC;                             -- cpu clock (falling edge)
    rst       : in  STD_LOGIC;                             -- reset (active high)
    cs        : in  STD_LOGIC;                             -- register chip select
    addr      : in  STD_LOGIC;                             -- register select
    rw        : in  STD_LOGIC;                             -- register read write
    data_in   : in  STD_LOGIC_VECTOR(DB_WIDTH-1 downto 0); -- register data bus in
    data_out  : out STD_LOGIC_VECTOR(DB_WIDTH-1 downto 0); -- register data bus out

    chr_clk   : in  STD_LOGIC;                             -- character clock input (rising edge)
    MA        : out STD_LOGIC_VECTOR(MA_WIDTH-1 downto 0); -- memory address (characters)
    RA        : out STD_LOGIC_VECTOR(RA_WIDTH-1 downto 0); -- row address (character generator lines)
    HSYNC     : out STD_LOGIC;                             -- Horizontal synch
    VSYNC     : out STD_LOGIC;                             -- Vertical synch
    dsp_ena   : out STD_LOGIC;                             -- Display enable
    cur_ena   : out STD_LOGIC;                             -- Cursor enable
    lpn_stb   : in  STD_LOGIC                              -- light pen strobe input (active high)
    );
end crtc6845;

architecture rtl of crtc6845 is

constant AR_WIDTH  : integer :=  5;

--
-- 6845 Register Index Numbers
--
constant hto_ind   : STD_LOGIC_VECTOR(AR_WIDTH-1 downto 0) := "00000"; -- Horizontal Total          (Characters) WO
constant hds_ind   : STD_LOGIC_VECTOR(AR_WIDTH-1 downto 0) := "00001"; -- Horizontal Displayed      (Characters) WO
constant hsp_ind   : STD_LOGIC_VECTOR(AR_WIDTH-1 downto 0) := "00010"; -- Horizontal Synch position (Characters) WO
constant hsw_ind   : STD_LOGIC_VECTOR(AR_WIDTH-1 downto 0) := "00011"; -- Sync Width (Ver & Hor)    (SL / Chars) WO
constant vto_ind   : STD_LOGIC_VECTOR(AR_WIDTH-1 downto 0) := "00100"; -- Vertical Total            (Char Rows)  WO
constant adj_ind   : STD_LOGIC_VECTOR(AR_WIDTH-1 downto 0) := "00101"; -- Vertical Total Adjust     (Scan Lines) WO
constant vds_ind   : STD_LOGIC_VECTOR(AR_WIDTH-1 downto 0) := "00110"; -- Vertical Displayed        (Char Rows)  WO
constant vsp_ind   : STD_LOGIC_VECTOR(AR_WIDTH-1 downto 0) := "00111"; -- Vertical Synch position   (Char Rows)  WO
constant imd_ind   : STD_LOGIC_VECTOR(AR_WIDTH-1 downto 0) := "01000"; -- Interlace Mode & Skew                  WO
constant sln_ind   : STD_LOGIC_VECTOR(AR_WIDTH-1 downto 0) := "01001"; -- Maximum Scan Line Address (Scan Lines) WO
constant cur_s_ind : STD_LOGIC_VECTOR(AR_WIDTH-1 downto 0) := "01010"; -- Cursor Start              (Scan Lines) WO Bit 5 BP, Bit 6 BE
constant cur_e_ind : STD_LOGIC_VECTOR(AR_WIDTH-1 downto 0) := "01011"; -- Cursor End                (Scan Lines) WO
constant sta_h_ind : STD_LOGIC_VECTOR(AR_WIDTH-1 downto 0) := "01100"; -- Start Address High                     RW
constant sta_l_ind : STD_LOGIC_VECTOR(AR_WIDTH-1 downto 0) := "01101"; -- Start Address Low                      RW
constant cur_h_ind : STD_LOGIC_VECTOR(AR_WIDTH-1 downto 0) := "01110"; -- Cursor Position High                   RW
constant cur_l_ind : STD_LOGIC_VECTOR(AR_WIDTH-1 downto 0) := "01111"; -- Cusror Position Low                    RW
constant lpn_h_ind : STD_LOGIC_VECTOR(AR_WIDTH-1 downto 0) := "10000"; -- Light Pen Position High                RO
constant lpn_l_ind : STD_LOGIC_VECTOR(AR_WIDTH-1 downto 0) := "10001"; -- Light Pen Position Low                 RO

--
-- I/O address register
--
signal ind_reg     : STD_LOGIC_VECTOR(AR_WIDTH-1 downto 0);

--
-- 6845 Registers R0-R17
--
signal hto_reg	   : STD_LOGIC_VECTOR(7 downto 0); -- Horizontal Total          (Chars) WO
signal hds_reg    : STD_LOGIC_VECTOR(7 downto 0); -- Horizontal Display        (Chars) WO
signal hsp_reg    : STD_LOGIC_VECTOR(7 downto 0); -- Horizontal Synch Position (Chars) WO
signal hsw_reg    : STD_LOGIC_VECTOR(7 downto 0); -- Horizontal Synch Width    (Chars) WO
signal vto_reg    : STD_LOGIC_VECTOR(6 downto 0); -- Vertical Total            (Rows)  WO
signal adj_reg    : STD_LOGIC_VECTOR(4 downto 0); -- Vertical Total Adjust     (Lines) WO
signal vds_reg    : STD_LOGIC_VECTOR(6 downto 0); -- Vertcal Display           (Rows)  WO
signal vsp_reg    : STD_LOGIC_VECTOR(6 downto 0); -- Vertical Synch Position   (Rows)  WO
signal imd_reg    : STD_LOGIC_VECTOR(1 downto 0); -- Interlace Mode & Skew             WO
signal sln_reg    : STD_LOGIC_VECTOR(4 downto 0); -- Maximum Scan Line Address (Lines) WO
signal cur_s_reg  : STD_LOGIC_VECTOR(6 downto 0); -- Cursor Start              (Lines) WO
signal cur_e_reg  : STD_LOGIC_VECTOR(4 downto 0); -- Cursor End                (Lines) WO
signal sta_h_reg  : STD_LOGIC_VECTOR(5 downto 0); -- Start Address High                RW
signal sta_l_reg  : STD_LOGIC_VECTOR(7 downto 0); -- Start Address Low                 RW
signal cur_h_reg  : STD_LOGIC_VECTOR(5 downto 0); -- Cursor Position High              RW
signal cur_l_reg  : STD_LOGIC_VECTOR(7 downto 0); -- Cursor Position Low               RW
signal lpn_h_reg  : STD_LOGIC_VECTOR(5 downto 0); -- Light Pen Address High            RO
signal lpn_l_reg  : STD_LOGIC_VECTOR(7 downto 0); -- Light Pen Address Low             RO

--
-- Counters
--
signal hor_ctr    : STD_LOGIC_VECTOR( 7 downto 0); -- Horizontal Counter             (Chars)
signal hsw_ctr		: STD_LOGIC_VECTOR( 3 downto 0); -- Horizontal Synch Width Counter (Chars)
signal sln_ctr		: STD_LOGIC_VECTOR( 4 downto 0); -- Scan Line Counter              (Lines)
signal ver_ctr	   : STD_LOGIC_VECTOR( 6 downto 0); -- Vertical Counter               (Rows)
signal vsw_ctr		: STD_LOGIC_VECTOR( 3 downto 0); -- Vertical Synch widtrh Counter  (Lines)
signal row_ctr    : STD_LOGIC_VECTOR(13 downto 0); -- Vertical Row counter
signal lag_ctr		: STD_LOGIC_VECTOR(13 downto 0); -- Linear Address Generator Counter
signal bnk_ctr    : STD_LOGIC_VECTOR( 4 downto 0); -- Blink Counter

--
-- Interconnect signals
--
signal hor_end    : STD_LOGIC; -- Horizontal display end
signal ver_end    : STD_LOGIC; -- Vertical display end
signal hor_syn    : STD_LOGIC; -- Horizontal Synch
signal ver_syn    : STD_LOGIC; -- Vertical Synch
signal hor_dsp    : STD_LOGIC; -- Horizontal Display Enable
signal ver_dsp    : STD_LOGIC; -- Vertical Display Enable
signal hor_rst    : STD_LOGIC; -- Horizontal Reset (End of Line)
signal ver_rst    : STD_LOGIC; -- Vertical Reset   (End of Frame)
signal sln_rst    : STD_LOGIC; -- Scan Line Reset  (End of Row)
signal sln_adj    : STD_LOGIC; -- Scan Line Adjust (End of Frame)
signal cur_act    : STD_LOGIC; -- Cursor active

begin

--
-- Read CRTC6845 registers
--
crtc_read: process(addr, ind_reg, sta_h_reg, sta_l_reg, cur_h_reg, cur_l_reg, lpn_h_reg, lpn_l_reg )
begin
  if addr = '0' then
    --
    -- Read register address
    --
    data_out(AR_WIDTH-1 downto 0) <= ind_reg;
    data_out(7 downto AR_WIDTH)   <= (others=>'0');
  else
    --
    -- Read register value
    --
	 case ind_reg is
    when sta_h_ind =>
      data_out <= "00" & sta_h_reg;
    when sta_l_ind =>
      data_out <= sta_l_reg;
    when cur_h_ind =>
      data_out <= "00" & cur_h_reg;
    when cur_l_ind =>
		data_out <= cur_l_reg;
    when lpn_h_ind =>
		data_out <= "00" & lpn_h_reg;
    when lpn_l_ind =>
      data_out <= lpn_l_reg;
    when others =>
      data_out <= (others => '0');
	 end case;
  end if;
end process;

--
-- Write CRTC registers
--
crtc_write: process(clk, rst, cs, rw, addr, data_in)
begin
  if falling_edge(clk) then
    if rst = '1' then
      ind_reg    <= b"0" & x"0";
      hto_reg    <= x"65";
		hds_reg    <= x"50";
		hsp_reg    <= x"56";
		hsw_reg    <= x"F9";
		sln_reg    <= '0' & x"b";
		vto_reg    <= b"001" & x"8"; --18
		adj_reg    <= b"0" & x"a";
		vds_reg    <= b"001" & x"8"; --18
		vsp_reg    <= b"001" & x"8"; --18
      imd_reg    <= b"00";
		cur_s_reg  <= b"000" & x"0";
		cur_e_reg  <= b"0" & x"B";
		sta_h_reg  <= b"00" & x"0";
		sta_l_reg  <= x"80";
		cur_h_reg  <= b"00" & x"0";
		cur_l_reg  <= x"80";

    elsif cs = '1' and rw = '0' then
      if addr = '0' then
        ind_reg <= data_in(AR_WIDTH-1 downto 0);
      else
        case ind_reg is
		  when hto_ind =>
		    hto_reg <= data_in;
		  when hds_ind =>
			 hds_reg <= data_in;
		  when hsp_ind =>
			 hsp_reg <= data_in;
		  when hsw_ind =>
			 hsw_reg <= data_in;
		  when sln_ind =>
			 sln_reg <= data_in(4 downto 0);
		  when vto_ind =>
			 vto_reg <= data_in(6 downto 0);
		  when adj_ind =>
			 adj_reg <= data_in(4 downto 0);
		  when vds_ind =>
			 vds_reg <= data_in(6 downto 0);
		  when vsp_ind =>
			 vsp_reg <= data_in(6 downto 0);
        when imd_ind =>
          imd_reg <= data_in(1 downto 0);
		  when cur_s_ind =>
			 cur_s_reg <= data_in(6 downto 0);
		  when cur_e_ind =>
			 cur_e_reg <= data_in(4 downto 0);
		  when sta_h_ind =>
			 sta_h_reg <= data_in(5 downto 0);
		  when sta_l_ind =>
			 sta_l_reg <= data_in;
		  when cur_h_ind =>
			 cur_h_reg <= data_in(5 downto 0);
		  when cur_l_ind =>
			 cur_l_reg <= data_in;
		  when others =>
			 null;
		  end case;
      end if; -- addr
    end if; -- cs
  end if; -- E

end process;

--------------------------------------------
-- Horizontal Counter                     --
--------------------------------------------
--
-- hor_ctr (horizontal counter) increments
-- until it reaches the horizontal total 
-- then resets to zero
--
crtc_hor_ctr_p : process(chr_clk, rst, hor_rst, hor_ctr)
begin
  if rising_edge(chr_clk) then
    if rst = '1' then
		hor_ctr <= (others => '0');
    else
		if hor_rst = '1' then
		  hor_ctr <= (others => '0');
      else
        hor_ctr <= hor_ctr + 1;
      end if;
    end if;
  end if;
end process;

--------------------------------------------
-- Horizontal Reset                     --
--------------------------------------------
--
-- hor_rst (horizontal reset) goes high
-- for one horizontal character cycle
-- when the horizontal counte reaches 
-- the horizontal total, then it goes low
--
crtc_hor_rst_p : process(hor_ctr, hto_reg)
begin
  if hor_ctr = hto_reg then
    hor_rst <= '1';
  else
    hor_rst <= '0';
  end if;
end process;

--------------------------------------------
-- Horizontal Display End                 --
--------------------------------------------
--
-- hor_end (horizontal end) goes high
-- for one horizontal character clock cycles
-- when the horizontal counter reaches
-- the horizontal display count
--
crtc_hor_end_p: process(hor_ctr, hds_reg)
begin
	if hor_ctr = hds_reg then
		hor_end <= '1';
	else
		hor_end <= '0';
	end if;
end process;

--------------------------------------------
-- Horizontal Display Enable              --
--------------------------------------------
--
-- Horizontal display goes high on a
-- horizontal reset and goes low at
-- horizontal end.
--
crtc_hor_dsp_p: process(chr_clk, rst, hor_rst, hor_end )
begin

  if rising_edge( chr_clk ) then
    if rst = '1' then
      hor_dsp <= '0';
    elsif hor_rst = '1' and hor_end = '0' then
      hor_dsp <= '1';
    elsif hor_rst = '0' and hor_end = '1' then
      hor_dsp <= '0';
    end if;
  end if;  
end process;

--------------------------------------------
-- Horizontal Horizontal Synch            --
--------------------------------------------
--
-- hor_syn (horizontal synch) goes high
-- when the horizontal counter reaches 
-- the the value in the horizontal synch position
-- register. It is reset when the horizontal
-- synch width counter reaches the value
-- in the horizontal synch width register
--
crtc_hor_syn_p: process(chr_clk, rst, hor_ctr, hsp_reg, hsw_reg, hor_syn)
begin
  if rising_edge(chr_clk) then
    if rst = '1' then
      hor_syn <= '0';
	 elsif hor_ctr = hsp_reg then
		hor_syn <= '1';
	 elsif hsw_ctr = hsw_reg(3 downto 0) then
      hor_syn <= '0';
	 end if;
  end if;
  HSYNC <= hor_syn;

end process;

--------------------------------------------
-- Horizontal Synch Width Counter         --
--------------------------------------------
--
-- The horizaontal synch width counter
-- increments each character clock cycle
-- while the horizontal synch pulse is high
-- It is reset when horizontal synch goes low
--
crtc_hsw_ctr_p: process(chr_clk, rst, hor_syn, hsw_ctr)
begin
  if rising_edge(chr_clk) then
    if rst = '1' then
		hsw_ctr <= (others => '0');
	 else
		if hor_syn = '1' then
			hsw_ctr <= hsw_ctr + 1;
		else
			hsw_ctr <= (others => '0');
		end if;
    end if;
  end if;
end process;

--------------------------------------------
-- Scan Line Counter                      --
--------------------------------------------
--
-- The Scan line counter increments
-- when horizontal reset goes high.
-- It is reset when the scan line counter
-- reaches the value in the scan line register
-- The Scan line counter is used to generate
-- the row address of the character generator
--
crtc_sln_ctr_p: process(chr_clk, rst, hor_rst, sln_rst, sln_ctr )
begin
  if rising_edge(chr_clk) then
    if rst = '1' then
      sln_ctr <= (others => '0');
    elsif sln_rst = '1' then
      sln_ctr <= (others => '0');
	 elsif hor_rst = '1' then
      sln_ctr <= sln_ctr + 1;
    end if;
  end if;
  RA <= sln_ctr;
end process;

--------------------------------------------
-- Scan Line Reset                        --
--------------------------------------------
--
-- Scan line reset is goes high when the
-- scan line counter reaches the value in
-- the scan line register and is zero otherwise
--
crtc_sln_rst_p: process(sln_reg, sln_ctr)
begin
  if sln_ctr = sln_reg then
    sln_rst <= '1';
  else
    sln_rst <= '0';
  end if;
end process;

--------------------------------------------
-- Scan Line Adjust                       --
--------------------------------------------
--
-- Scan line Adjust goes high when the
-- scan line counter reaches the value
-- in the scan line adjust register.
-- It is use to reset the vertical counter
-- when it reaches the vertical total
--
crtc_sln_adj_p: process(sln_ctr, adj_reg)
begin
	if sln_ctr = adj_reg then
		sln_adj <= '1';
	else
		sln_adj <= '0';
	end if;
end process;

--------------------------------------------
-- Vertical Row Counter                   --
--------------------------------------------
--
-- The Vertical Row counter is incremenented
-- when there is a scan line reset and there
-- is a horizontal reset.
-- The vertical row counter is reset when
-- there is a vertical reset due to the
-- vertical counter reaching the value in
-- the vertical total register and a
-- scan line adjust signal is generated
--
ctrc_ver_ctr_p: process(chr_clk, rst, sln_rst)
begin
  if rising_edge(chr_clk) then
    if rst = '1' then
		ver_ctr <= (others => '0');
	 elsif ver_rst = '1' then
      ver_ctr <= (others => '0');
	 elsif sln_rst = '1' and hor_rst = '1' then
      ver_ctr <= ver_ctr + 1;
    end if;
  end if;
end process;

--------------------------------------------
-- Vertical Reset                         --
--------------------------------------------
--
-- Vertical reset is generated when the
-- vertical counter reaches the value in
-- the vertical total register and the
-- scan line counter reaches the scan
-- line adjust register value
--
ctrc_ver_rst_p: process(ver_ctr, vto_reg, sln_adj )
begin

  if ver_ctr = vto_reg and sln_adj = '1' then
	 ver_rst <= '1';
  else
	 ver_rst <= '0';
  end if;

end process;

--------------------------------------------
-- Vertical Display End Process           --
--------------------------------------------
--
-- Vertical end is generated when the
-- vertical counter reaches the value in
-- the display end register
--
crtc_ver_end_p: process(ver_ctr, vds_reg)
begin
	if ver_ctr = vds_reg then
		ver_end <= '1';
	else
		ver_end <= '0';
	end if;
end process;

--------------------------------------------
-- Vertical Display Enable                --
--------------------------------------------

crtc_ver_dsp_p: process(chr_clk, rst, ver_rst, ver_end )
begin

  if rising_edge( chr_clk ) then
    if rst = '1' then
      ver_dsp <= '0';
    elsif ver_rst = '1' and ver_end = '0' then
      ver_dsp <= '1';
    elsif ver_rst = '0' and ver_end = '1' then
      ver_dsp <= '0';
    end if;
  end if;

end process;

--------------------------------------------
-- Vertical Synch Width Counter           --
--------------------------------------------
--
-- The Vertical Synch Width Counter
-- is incremented when vertical synch goes high
-- as a result of the Vertical Counter reaching
-- the Vertical Synch Width Position and horizontal
-- reset goes high.
-- The Vertical synch width counter is reset
-- when Vertical synch goes low.
--
crtc_vsw_ctr_p: process(chr_clk, rst, hor_rst, ver_ctr, vsp_reg, vsw_ctr)
begin
  if rising_edge(chr_clk) then
    if rst = '1' then
      vsw_ctr <= (others => '0');
	 elsif ver_syn = '0' then
      vsw_ctr <= (others => '0');
	 elsif ver_syn = '1' and  hor_rst = '1' then
      vsw_ctr <= vsw_ctr + 1;
    end if;
  end if;
end process;

--------------------------------------------
-- Vertical Synch                         --
--------------------------------------------
--
-- The Vertical Synch goes high when the
-- vertical counter reaches the value in the
-- vertical synch position register.
-- It is reset when the vertical synch width
-- counter reaches 16.
--
crtc_ver_syn_p: process(chr_clk, rst, ver_ctr, vsp_reg, vsw_ctr, hsw_reg, ver_syn)
begin
  if rising_edge(chr_clk) then
    if rst = '1' then
      ver_syn <= '0';
	 elsif ver_ctr = vsp_reg then
      ver_syn <= '1';
	 elsif vsw_ctr = hsw_reg(7 downto 4) then
      ver_syn <= '0';
    end if;
  end if;
  VSYNC <= ver_syn;
end process;

--------------------------------------------
-- Vertical Row Counter                   --
--------------------------------------------
--
-- The character row counter is incremented 
-- by the horizontal display count 
-- on a scan line reset and a horizontal reset.
-- It is reset to the start address on a vertical reset
--
crtc_row_ctr_p: process(chr_clk, rst, sln_rst, ver_rst, hor_rst, sta_h_reg, sta_l_reg)
begin
  if rising_edge(chr_clk) then
    if rst = '1' then
		row_ctr <= sta_h_reg & sta_l_reg;
	 elsif sln_rst = '1' and hor_rst = '1' then
		row_ctr <= row_ctr + hds_reg;
		if ver_rst = '1' then
			row_ctr <= sta_h_reg & sta_l_reg;
		end if;
    end if;
  end if;
end process;

--------------------------------------------
-- Display Enable                         --
--------------------------------------------
--
-- Display enable is active when both
-- horizantal display and vertical displays 
-- are active
--
crtc_dsp_ena_p: process(hor_dsp, ver_dsp)
begin

  dsp_ena <= hor_dsp and ver_dsp;

end process;


--------------------------------------------
-- Linear Address Generator               --
--------------------------------------------

crtc_lag_p: process(chr_clk, rst, hor_rst, sta_h_reg, sta_l_reg, lag_ctr)
begin
  if rising_edge(chr_clk) then
    if rst = '1' then
		lag_ctr <= sta_h_reg & sta_l_reg;
	 else
		if hor_rst = '1' then
		  lag_ctr <= row_ctr;
		end if;
		lag_ctr <= lag_ctr + 1;
    end if;
  end if;
  MA <= lag_ctr;
end process;

--------------------------------------------
-- Cursor Control Unit Instantiation      --
--------------------------------------------
--
-- Cursor active when the Linear Address Generator
-- reaches the value in the Cursor position register
--
crtc_cur_act_p: process(lag_ctr, cur_h_reg, cur_l_reg)
begin
	if lag_ctr = (cur_h_reg & cur_l_reg) then
		cur_act <= '1';
	else
		cur_act <= '0';
	end if;
end process;

--------------------------------------------
-- Cursor Blink Counter                   --
--------------------------------------------
--
-- The Cursor Blink Counter increments
-- every frame
--
crtc_blink_ctr_p: process (chr_clk, rst, hor_rst, ver_rst)
begin
  if rising_edge(chr_clk) then
	 if rst = '1' then
		bnk_ctr <= (others => '0');
	 elsif hor_rst = '1' and ver_rst = '1' then
		bnk_ctr <= bnk_ctr + 1;
    end if;
  end if;
end process;

--------------------------------------------
-- Cursor Enable                          --
--------------------------------------------
--
-- The Cursor is enabled when the Scan line
-- counter is great or equal to the Cursor Start Line
-- and the scan line counter is less than or equal
-- to the Cursor End line and the Cusor is active 
--
crtc_cur_ena_p: process (sln_ctr, cur_s_reg, cur_e_reg, cur_act, bnk_ctr)
begin
	if sln_ctr >= cur_s_reg(4 downto 0) and sln_ctr <= cur_e_reg and cur_act = '1' then
		case cur_s_reg(6 downto 5) is
		when "00" =>
			cur_ena <= '1';
		when "10" =>
			cur_ena <= bnk_ctr(3);
		when "11" =>
			cur_ena <= bnk_ctr(4);
		when others =>
			cur_ena <= '0';
		end case;		
	else
		cur_ena <= '0';
	end if;
end process;

--------------------------------------------
-- Light Pen Capture                      --
--------------------------------------------
--
-- The light pen resister is loaded
-- when ther is a high on the light
-- pen strobe input
--
ctrc_lpn_stb_p: process(chr_clk, rst, lpn_stb)
begin
  if rising_edge(chr_clk) then
    if rst = '1' then   
      lpn_h_reg <= (others => '0');
      lpn_l_reg <= (others => '0');
    elsif lpn_stb = '1' then
      lpn_h_reg <= lag_ctr(13 downto 8);
      lpn_l_reg <= lag_ctr(7 downto 0);
	 end if;
  end if;
end process;

end rtl;

