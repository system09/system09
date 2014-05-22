--===========================================================================--
--                                                                           --
--                Synthesizable Hardware Trace Capture                       --
--                                                                           --
--===========================================================================--
--
--  File name      : trace.vhd
--
--  Entity name    : trace
--
--  Purpose        : Implements a hardware real-time trace buffer for system09.
--                   Captures all the CPU bus cycles to a block RAM trace buffer.
--                   A trigger condition may be used to start or stop a trace 
--                   capture. The trigger condition is determined by the address,
--                   data and control, comparator and qualifier registers.
--                   The comparator registers determine the level of the signals 
--                   to trigger on. The qualifier registers determine if the 
--                   comparison is to be made for that bit (qualifier bit set) 
--                   or not (qualifier bit cleared). The hardware trace capture 
--                   module has 9 x 8 bit registers and 5 trace buffers. Separate 
--                   chip selects are provided for the register bank and trace 
--                   buffer bank. The individual trace buffers are selected via 
--                   the bank select register. Trace buffers may be read by the 
--                   CPU but are only written to when tracing CPU bus cycles.
--                   The lowest trace buffer address always points to the start
--                   of the trace capture. This is achieved by adding an offset
--                   to the buffer address which points to the last cycle captured. 
--
--  Dependencies   : ieee.Std_Logic_1164
--                   ieee.std_logic_unsigned
--
--  Author         : John E. Kent      
--
--  Email          : dilbert57@opencores.org      
--
--  Web            : http://opencores.org/project,system09
--
--  Description    : Register Memory Map
--
--                   Register Bank(CS_R)
--                   Base + $00 - Address Comparitor High Byte
--                   Base + $01 - Address Comparitor Low byte
--                   Base + $02 - Data    Comparitor
--                   Base + $03 - Control Comparitor
--                   Base + $04 - Address Qualifier High Byte
--                   Base + $05 - Address Qualifier Low byte
--                   Base + $06 - Data    Qualifier
--                   Base + $07 - Control Qualifier
--                   Base + $08 - Buffer  Bank Select
--                                Bits[2..0] Select Buffer Bank 0 to 4
--                  
--                   Buffer Bank (CS_B)
--                   Bank 0 - CPU Address high trace buffer
--                   Bank 1 - CPU Address low trace buffer
--                   Bank 2 - CPU Data output (write) trace buffer
--                   Bank 3 - CPU Data input (read) trace buffer
--                   Bank 4 - CPU Control signal trace buffer
-- 
--                   Address, Data and Control signals 
--                   must match in the Comparitor registers 
--                   Matches are qualified by setting a bit 
--                   in the Qualifier registers
--
--                   Control Comparitor / Control Qualify Write
--                   b0 - r/w        1=read   0=write
--                   b1 - vma        1=valid  0=invalid
--                   b5 - trace      1=enable 0=disable
--                   b6 - pre/post   1=before 0=after
--                   b7 - irq output 1=match  0=mismatch
-- 
--                   Control Qualifier Read
--                   b7 - Interrupt Flag (unmasked)
-- 
--  Copyright (C) 2004 - 2010 John Kent
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
--                            Revision History                               --
--                                                                           --
--===========================================================================--
--
-- Version  Date        Author        Description
--
-- 0.1      2004-06-19  John E. Kent  Initial version
-- 0.2      2010-08-09  John E. Kent  Updated header and added GPL
--                                    Added trigger on read or write data
--                                    (not just write data).
--                                    Rearranged register and buffer bank decoding
--                                    Added Generics for data width and buffer size
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity trace is
  generic (
    BUFF_SIZE     : integer := 9;          -- Buffer Address Bits (B4=9, B16=11)
    DATA_WIDTH    : integer := 8           -- Data Width
  )
  port (	
    clk           : in  std_logic;         -- CPU bus clock
    rst           : in  std_logic;         -- Reset
    cs_r          : in  std_logic;	       -- Register bank chip select
    cs_b          : in  std_logic;	       -- Buffer bank chip select
    rw            : in  std_logic;         -- Read not Write
    addr          : in  std_logic_vector((2*DATA_WIDTH)-1 downto 0); -- CPU address / Trace buffer address in
    data_in       : in  std_logic_vector(DATA_WIDTH-1 downto 0);     -- CPU data out (write)/ Trace data in
    data_out      : out std_logic_vector(DATA_WIDTH-1 downto 0);     -- Trace data out (read)
    irq           : out std_logic;
    cpu_vma       : in  std_logic;                                   -- CPU VMA / Trace buffer VMA in
    cpu_data_in   : in  std_logic_vector(DATA_WIDTH-1 downto 0)      -- CPU read data in
  );
end;

architecture trace_arch of trace is

--
-- Registers
--
signal comp_addr_hi : std_logic_vector(DATA_WIDTH-1 downto 0); -- Address High Comparator  Reg
signal comp_addr_lo : std_logic_vector(DATA_WIDTH-1 downto 0); -- Address Low  Comparator  Reg
signal comp_data    : std_logic_vector(DATA_WIDTH-1 downto 0); -- Data         Comparator  Reg
signal comp_ctrl    : std_logic_vector(DATA_WIDTH-1 downto 0); -- Control Sig  Comparator  Reg
signal qual_addr_hi : std_logic_vector(DATA_WIDTH-1 downto 0); -- Address High Qualifier   Reg
signal qual_addr_lo : std_logic_vector(DATA_WIDTH-1 downto 0); -- Address Low  Qualifier   Reg
signal qual_data    : std_logic_vector(DATA_WIDTH-1 downto 0); -- Data         Qualifier   Reg
signal qual_ctrl    : std_logic_vector(DATA_WIDTH-1 downto 0); -- Control Sig  Qualifier   Reg
signal bank_reg     : std_logic_vector(DATA_WIDTH-1 downto 0); -- Trace Buffer Bank Select Reg
signal reg_data_out : std_logic_vector(DATA_WIDTH-1 downto 0); -- Register Data Output

--
-- Trace enable, counter and offset registers
--
signal trace_en     : std_logic;
signal trace_count  : std_logic_vector(BUFF_SIZE-1 downto 0);
signal trace_offset : std_logic_vector(BUFF_SIZE-1 downto 0);

--
-- Buffer address
--
signal buf_addr     : std_logic_vector(BUFF_SIZE-1 downto 0);

--
-- Block RAM buffer Mux signals
--
signal mux_stb      : std_logic;
signal mux_addr     : std_logic_vector(BUFF_SIZE-1 downto 0);
signal mux_we       : std_logic;

--
-- Block RAM trace buffer data outputs
--
signal buf_data_out_0 : std_logic_vector(DATA_WIDTH-1 downto 0);
signal buf_data_out_1 : std_logic_vector(DATA_WIDTH-1 downto 0);
signal buf_data_out_2 : std_logic_vector(DATA_WIDTH-1 downto 0);
signal buf_data_out_3 : std_logic_vector(DATA_WIDTH-1 downto 0);
signal buf_data_out_4 : std_logic_vector(DATA_WIDTH-1 downto 0);
signal buf_data_out   : std_logic_vector(DATA_WIDTH-1 downto 0);

--
-- Various other signals
--
signal irq_out      : std_logic;  -- Interrupt Request signal (unmasked)
signal qual_write   : std_logic;  -- Qualifier Control Register Write Strobe
signal qual_read    : std_logic;  -- Qualifier Control Register Read Strobe
signal trigger      : std_logic;  -- Event Trigger derived from bus comparator 
signal ctrl_in      : std_logic_vector(7 downto 0);

--
-- Block RAM Trace buffer
-- For Spartan 2 these will be B4  RAMs ( 512 Bytes)
-- For Spartan 3 these will be B16 RAMs (2048 Bytes)
--
component trace_ram
    Port (
       WB_CLK_I  : in  std_logic;
       WB_RST_I  : in  std_logic;
       WB_ADR_I  : in  std_logic_vector (BUFF_SIZE-1 downto 0);
       WB_DAT_O  : out std_logic_vector (DATA_WIDTH-1 downto 0);
       WB_DAT_I  : in  std_logic_vector (DATA_WIDTH-1 downto 0);
       WB_WE_I   : in  std_logic;
       WB_STB_I  : in  std_logic
    );
end component;

--
-- Start of Architecture
--
begin

--
-- Instantiate (Port Map) Block RAM trace buffers
--

--
-- Bank 0 - Trace buffer for CPU address high bits
--
trace_buffer_0 : trace_ram port map (
       WB_CLK_I  => clk,
       WB_RST_I  => rst,
       WB_ADR_I  => mux_addr,
       WB_DAT_O  => buf_data_out_0,
       WB_DAT_I  => addr((2*DATA_WIDTH)-1 downto DATA_WIDTH),
       WB_WE_I   => mux_we,
       WB_STB_I  => mux_stb
    );

--
-- Bank 1 - Trace buffer for CPU address low bits
--
trace_buffer_1 : trace_ram port map (
       WB_CLK_I  => clk,
       WB_RST_I  => rst,
       WB_ADR_I  => mux_addr,
       WB_DAT_O  => buf_data_out_1,
       WB_DAT_I  => addr(DATA_WIDTH-1 downto 0),
       WB_WE_I   => mux_we,
       WB_STB_I  => mux_stb
    );

--
-- Bank 2 - Trace buffer for CPU data out (write)
--
trace_buffer_2 : trace_ram port map (
       WB_CLK_I  => clk,
       WB_RST_I  => rst,
       WB_ADR_I  => mux_addr,
       WB_DAT_O  => buf_data_out_2,
       WB_DAT_I  => data_in(DATA_WIDTH-1 downto 0),
       WB_WE_I   => mux_we,
       WB_STB_I  => mux_stb
    );

--
-- Bank 3 - Trace buffer for CPU data in (read)
--
trace_buffer_3 : trace_ram port map (
       WB_CLK_I  => clk,
       WB_RST_I  => rst,
       WB_ADR_I  => mux_addr,
       WB_DAT_O  => buf_data_out_3,
       WB_DAT_I  => cpu_data_in(DATA_WIDTH-1 downto 0),
       WB_WE_I   => mux_we,
       WB_STB_I  => mux_stb
    );

--
-- Bank 4 - Trace buffer for CPU control bits
--
trace_buffer_4 : trace_ram port map (
       WB_CLK_I  => clk,
       WB_RST_I  => rst,
       WB_ADR_I  => mux_addr,
       WB_DAT_O  => buf_data_out_4,
       WB_DAT_I  => ctrl_in(DATA_WIDTH-1 downto 0),
       WB_WE_I   => mux_we,
       WB_STB_I  => mux_stb
    );

--------------------------------
--
-- Assign control signal input
--
--------------------------------

trace_ctrl_assign : process( irq_out, qual_ctrl, rw, cpu_vma )
begin
  ctrl_in(0) <= rw;
  ctrl_in(1) <= cpu_vma;
  ctrl_in(7 downto 2) <= (others=>'0');
end process;

--------------------------------
--
-- Write Trace Registers
--
--------------------------------

trace_reg_write : process( clk, rst, cs_r, rw, addr, data_in )
begin
  if clk'event and clk = '0' then
    qual_write  <= '0';
    if rst = '1' then
      comp_addr_hi <= (others=>'0');
      comp_addr_lo <= (others=>'0');
      comp_data    <= (others=>'0');
      comp_ctrl    <= (others=>'0');
      qual_addr_hi <= (others=>'0');
      qual_addr_lo <= (others=>'0');
      qual_data    <= (others=>'0');
      qual_ctrl    <= (others=>'0');
      bank_reg     <= (others=>'0');
    elsif cs_r = '1' and rw = '0' then
      case addr(3 downto 0) is
      when "0000" =>
        comp_addr_hi <= data_in;
      when "0001" =>
        comp_addr_lo <= data_in;
      when "0010" =>
        comp_data    <= data_in;
      when "0011" =>
        comp_ctrl    <= data_in;
      when "0100" =>
        qual_addr_hi <= data_in;
      when "0101" =>
        qual_addr_lo <= data_in;
      when "0110" =>
        qual_data    <= data_in;
      when "0111" =>      
        qual_ctrl    <= data_in;
        qual_write   <= '1';
      when others =>
        bank_reg     <= data_in;
      end case;
    end if;
  end if;
end process;

------------------------------------------
--
-- Read Trace Register (output mux)
--
------------------------------------------
trace_reg_read : process( addr, rw, cs_r,
                          comp_addr_hi, comp_addr_lo, comp_data, comp_ctrl,
                          qual_addr_hi, qual_addr_lo, qual_data, qual_ctrl, 
                          bank_reg, irq_out )
begin
  qual_read <= '0';
  case addr(3 downto 0) is
  when "0000" =>
    reg_data_out <= comp_addr_hi;
  when "0001" =>
    reg_data_out <= comp_addr_lo;
  when "0010" =>
    reg_data_out <= comp_data;
  when "0011" =>
    reg_data_out <= comp_ctrl;
  when "0100" =>
    reg_data_out <= qual_addr_hi;
  when "0101" =>
    reg_data_out <= qual_addr_lo;
  when "0110" =>
    reg_data_out <= qual_data;
  when "0111" =>
    qual_read    <= cs_r and rw;
    reg_data_out(6 downto 0) <= qual_ctrl(6 downto 0);
    reg_data_out(7) <= irq_out;
  when others =>
    reg_data_out <= bank_reg;
  end case;

end process;


--------------------------------
--
-- Read Trace Buffers
--
--------------------------------

trace_buf_read : process( bank_reg, buf_data_out_0, buf_data_out_1, 
                          buf_data_out_2, buf_data_out_3, buf_data_out_4 )
begin
  case bank_reg(2 downto 0) is
  when "000" =>
    buf_data_out <= buf_data_out_0;
  when "001" =>
    buf_data_out <= buf_data_out_1;
  when "010" =>
    buf_data_out <= buf_data_out_2;
  when "011" =>
    buf_data_out <= buf_data_out_3;
  when "100" =>
    buf_data_out <= buf_data_out_4;
  when others =>
    buf_data_out <= (others=>'0');
  end case;	 	
end process;

--------------------------------
--
-- Read Registers or Buffers
--
--------------------------------

trace_read : process( cs_r, reg_data_out, buf_data_out )
begin
  if cs_r = '1' then
    data_out = reg_data_out;
  else
    data_out = buf_data_out;
  else
end process;


------------------------------------------------------------------------
--
-- Multiplex the trace buffer between the trace capture and the CPU read
--
------------------------------------------------------------------------

trace_buf_mux : process( cs_b, trace_count, trace_en, buf_addr )
begin
  if cs_b = '0' then
    mux_addr <= trace_count;
    mux_we   <= '1';
    mux_stb  <= trace_en;
  else
    mux_addr <= buf_addr;
    mux_we   <= '0';
    mux_stb  <= '1';
  end if;

end process;

------------------------------
--
-- Trigger comparator process
--
------------------------------

trace_trigger : process( clk, rst, cs_r, addr, rw, cpu_vma, data_in, cpu_data_in,
                         comp_addr_hi, comp_addr_lo, comp_data, comp_ctrl,
                         qual_addr_hi, qual_addr_lo, qual_data, qual_ctrl)
variable hit          : std_logic;
variable miss_addr_hi : std_logic;
variable miss_addr_lo : std_logic;
variable miss_data_rd : std_logic;
variable miss_data_wr : std_logic;
variable miss_ctrl    : std_logic;

begin
  miss_addr_hi := 
     ((comp_addr_hi(7) xor addr(15)  ) and qual_addr_hi(7) ) or
     ((comp_addr_hi(6) xor addr(14)  ) and qual_addr_hi(6) ) or
     ((comp_addr_hi(5) xor addr(13)  ) and qual_addr_hi(5) ) or
     ((comp_addr_hi(4) xor addr(12)  ) and qual_addr_hi(4) ) or
     ((comp_addr_hi(3) xor addr(11)  ) and qual_addr_hi(3) ) or
     ((comp_addr_hi(2) xor addr(10)  ) and qual_addr_hi(2) ) or
     ((comp_addr_hi(1) xor addr( 9)  ) and qual_addr_hi(1) ) or
     ((comp_addr_hi(0) xor addr( 8)  ) and qual_addr_hi(0) );
  miss_addr_lo :=
     ((comp_addr_lo(7) xor addr( 7)  ) and qual_addr_lo(7) ) or
     ((comp_addr_lo(6) xor addr( 6)  ) and qual_addr_lo(6) ) or
     ((comp_addr_lo(5) xor addr( 5)  ) and qual_addr_lo(5) ) or
     ((comp_addr_lo(4) xor addr( 4)  ) and qual_addr_lo(4) ) or
     ((comp_addr_lo(3) xor addr( 3)  ) and qual_addr_lo(3) ) or
     ((comp_addr_lo(2) xor addr( 2)  ) and qual_addr_lo(2) ) or
     ((comp_addr_lo(1) xor addr( 1)  ) and qual_addr_lo(1) ) or
     ((comp_addr_lo(0) xor addr( 0)  ) and qual_addr_lo(0) );
  miss_data_wr :=
     ((comp_data(7)    xor data_in(7)) and qual_data(7)    ) or
     ((comp_data(6)    xor data_in(6)) and qual_data(6)    ) or
     ((comp_data(5)    xor data_in(5)) and qual_data(5)    ) or
     ((comp_data(4)    xor data_in(4)) and qual_data(4)    ) or
     ((comp_data(3)    xor data_in(3)) and qual_data(3)    ) or
     ((comp_data(2)    xor data_in(2)) and qual_data(2)    ) or
     ((comp_data(1)    xor data_in(1)) and qual_data(1)    ) or
     ((comp_data(0)    xor data_in(0)) and qual_data(0)    );
  miss_data_rd :=
     ((comp_data(7)    xor cpu_data_in(7)) and qual_data(7)    ) or
     ((comp_data(6)    xor cpu_data_in(6)) and qual_data(6)    ) or
     ((comp_data(5)    xor cpu_data_in(5)) and qual_data(5)    ) or
     ((comp_data(4)    xor cpu_data_in(4)) and qual_data(4)    ) or
     ((comp_data(3)    xor cpu_data_in(3)) and qual_data(3)    ) or
     ((comp_data(2)    xor cpu_data_in(2)) and qual_data(2)    ) or
     ((comp_data(1)    xor cpu_data_in(1)) and qual_data(1)    ) or
     ((comp_data(0)    xor cpu_data_in(0)) and qual_data(0)    );
  miss_ctrl :=
     ((comp_ctrl(0)    xor rw        ) and qual_ctrl(0)    ) or
     ((comp_ctrl(1)    xor cpu_vma   ) and qual_ctrl(1)    );

  hit := not( miss_addr_hi or miss_addr_lo or (miss_data_rd and not(miss_ctrl) and rw) or (miss_data_wr and not(miss_ctrl) and not(rw)) or miss_ctrl);
  trigger <= not(hit xor comp_ctrl(7));

end process;

-----------------------------------------
--
-- Trace buffer capture on event trigger
--
-----------------------------------------

trace_capture : process( clk, rst, addr, qual_write, 
                         trigger, trace_en, qual_ctrl, irq_out, 
                         trace_count, trace_offset )
begin
  if clk'event and clk = '1' then
    if rst = '1' then
      trace_count  <= (others=>'0');
      trace_offset <= (others=>'0');
      trace_en     <= '0';
      irq_out      <= '0';
    else
      --
      -- qualifier control register bit 6 = zero
      -- means start capture after trigger point.
      --
      if qual_ctrl(6) = '0' then

        --
        -- Activate trace on a trigger condition
        -- Deactive trace when the buffer is full
        --
        if trace_en = '0' then
          trace_en     <= trigger;
        elsif trace_count = trace_offset then
          trace_offset <= trace_count;
          trace_en     <= '0';
        end if;

        --
        -- Set IRQ on trace buffer full
        -- Reset IRQ on qualifier control register write
        --
        if qual_write = '1' then
          irq_out   <= '0';
        elsif trace_count = trace_offset then
          irq_out   <= '1';
        end if;

	--
      -- qualifier control register bit 6 = one
      -- means finish capture at trigger point.
      --
      else
        --
        -- Activate trace on qualifier control write
        -- Deactivate trace on a trigger condition
        --
        if trace_en = '0' then
          trace_en     <= qual_write;
        elsif trigger = '1' then
          trace_offset <= trace_count;
          trace_en     <= '0';
        end if;

        --
        -- Set IRQ on a trigger condition
        -- Reset IRQ on qualifier control register write
        --
        if qual_write = '1' then
          irq_out <= '0';
        elsif trigger = '1' then
          irq_out <= '1';
        end if;

      end if;

      trace_count <= trace_count + 1;

    end if;
  end if;

  --
  -- The IRQ output is qualified by the interrupt enable bit
  -- (bit 7 of the qualifier control register)
  --
  irq <= irq_out and qual_ctrl(7);

  --
  -- Trace buffer is offset by start address
  --
  buf_addr  <= addr(BUFF_SIZE-1 downto 0) + trace_offset;

end process;

end trace_arch;
	
