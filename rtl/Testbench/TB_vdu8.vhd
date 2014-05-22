-- ------------------------------------------------------------
--  TB_vdu8 Test Bench
-- ------------------------------------------------------------
-- (c) Bertrand Cuzeau
--

Library IEEE;
  use IEEE.std_logic_1164.all;
  use IEEE.numeric_std.all;
  use STD.textio.all;
  use IEEE.std_logic_textio.all;

Entity TB_vdu8 is end;

Architecture TEST of tb_vdu8 is

  subtype Byte is std_logic_vector (7 downto 0);

  signal vdu_clk_in   : std_logic := '0';
  signal cpu_clk_out  : std_logic;
  signal vdu_rst      : std_logic;
  signal vdu_cs       : std_logic := '0';
  signal vdu_rw       : std_logic := '1';
  signal vdu_addr     : std_logic_vector(2 downto 0) := "000";
  signal vdu_data_in  : std_logic_vector(7 downto 0);
  signal vdu_data_out : std_logic_vector(7 downto 0);
  signal vga_red_o   : std_logic;
  signal vga_green_o : std_logic;
  signal vga_blue_o  : std_logic;
  signal vga_hsync_o : std_logic;
  signal vga_vsync_o : std_logic;

  constant Msg       : string := "Hello! X";
  signal done        : boolean;
  constant Period : time := 10 ns;

begin

vdu_clk_in <= '0' when Done else not vdu_clk_in after Period / 2;
vdu_rst <= '1', '0' after 16 * Period;

vdu: Entity work.vdu8
  port map (
    vdu_clk_in    => vdu_clk_in   ,
    cpu_clk_out   => cpu_clk_out  ,
    vdu_rst       => vdu_rst      ,
    vdu_cs        => vdu_cs       ,
    vdu_rw        => vdu_rw       ,
    vdu_addr      => vdu_addr     ,
    vdu_data_in   => vdu_data_in  ,
    vdu_data_out  => vdu_data_out ,
    vga_red_o     => vga_red_o    ,
    vga_green_o   => vga_green_o  ,
    vga_blue_o    => vga_blue_o   ,
    vga_hsync_o   => vga_hsync_o  ,
    vga_vsync_o   => vga_vsync_o
    );

process
  procedure Writebyte (b : Byte; Addr : integer) is
  begin
    wait for 500 ns;
    vdu_cs <= '1';
    vdu_data_in <= b;
    vdu_addr <= std_logic_vector(to_unsigned(Addr,3));
    wait for 100 ns;
    vdu_rw <= '0';
    wait for 200 ns;
    vdu_rw <= '1';
    vdu_cs <= '0';
  end procedure;

  procedure WriteChar (c : character; color:byte; x:integer; y:integer; offs:integer) is
  begin
    WriteByte(std_logic_vector(to_unsigned(character'pos(c),8)),0);
    WriteByte(color,1);
    WriteByte(std_logic_vector(to_unsigned(x,8)),2);
    WriteByte(std_logic_vector(to_unsigned(y,8)),3);
    WriteByte(std_logic_vector(to_unsigned(offs,8)),4);
  end procedure;

begin
  vdu_data_in <= x"00";
  wait until vga_hsync_o='0';
  for i in Msg'range loop
    WriteChar(Msg(i), x"17",i,i,1);  -- ?? bgB bgG bgR Blink?? fgB fgG fgR  <<< TBV
  end loop;
  wait until vga_vsync_o='0';
  wait for 5 ms;
  report "End of Simulation";
  done <= true;
  wait;
end process;

end architecture TEST;
