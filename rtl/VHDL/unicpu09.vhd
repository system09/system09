--===========================================================================--
--                                                                           --
--       Synthesizable Single 6809 Instruction Compatible CPU Module         --
--                                                                           --
--===========================================================================--
--
--  File name      : unicpu09.vhd
--
--  Purpose        : Implements a single 6809 CPU module with
--                   Memory Management Unit (Extended Dynamic Address Translation)
--                   CPU Module ID register and
--                   32 bit Hardware Multiplier register
--                  
--  Dependencies   : ieee.std_logic_1164
--                   ieee.std_logic_unsigned
--                   ieee.std_logic_arith
--                   ieee.numeric_std
--                   unisim.vcomponents
--
--  Uses           : cpu09   (cpu09.vhd)     6809 CPU core
--                   mmu     (mmu.vhd)       Memory Management Unit
--                   mon_rom (sys09bug_s3s_rom4k_b16.vhd) Monitor ROM
--                   mul32   (mul32.vhd)     32 bit hardware multiplier
--                   dpr_2k  (dpr2k_b16.vhd) 2KByte Dual Port RAM using B16 Block RAM
--
--  Author         : John E. Kent
--
--  Email          : dilbert57@opencores.org      
--
--  Web            : http://opencores.org/project,system09
--
--
--  Copyright (C) 2003 - 2010 John Kent
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
-- 0.1     John Kent     2003-03-20   Started work on design (?)
-- 0.2     John Kent     2010-06-16   Updated header with GPL
--
--===========================================================================--
library ieee;
   use ieee.std_logic_1164.all;
   use ieee.std_logic_arith.all;
   use ieee.std_logic_unsigned.all;
   use ieee.numeric_std.all;
library unisim;
	use unisim.vcomponents.all;

entity unicpu09 is
  port
  (
	 clk      :	in  std_logic;
    rst      : in  std_logic;
	 --
	 -- cpu side signals
	 --
    id       : in  std_logic_vector( 7 downto 0);
    vma      : out std_logic;
    rw       : out std_logic;
    addr     : out std_logic_vector(19 downto 0);
	 --
	 -- memory side signals
	 --
    uni_vma      : in  std_logic;
    uni_rw       : in  std_logic;
    uni_addr     : in  std_logic_vector(19 downto 0);
    uni_data_in  : in  std_logic_vector(7 downto 0);
	 uni_data_out : out std_logic_vector(7 downto 0);
	 --
	 -- controls
	 --
	 halt     : in  std_logic;
	 hold     : in  std_logic;
	 irq      : in  std_logic;
	 nmi      : in  std_logic;
	 firq     : in  std_logic
  );
end entity;

-------------------------------------------------------------------------------
-- Architecture for unicpu09
-------------------------------------------------------------------------------

architecture RTL of unicpu09 is

  -- CPU Interface signals
  signal cpu_rw       : std_logic;
  signal cpu_vma      : std_logic;
  signal cpu_addr     : std_logic_vector(15 downto 0);
  signal cpu_data_in  : std_logic_vector(7 downto 0);
  signal cpu_data_out : std_logic_vector(7 downto 0);

  -- BOOT ROM
  signal rom_cs       : std_logic;
  signal rom_data_out : std_Logic_Vector(7 downto 0);

  -- Memory Management Unit
  signal mmu_cs       : std_logic;
  signal mmu_addr     : std_logic_vector(27 downto 0);

  -- 32 bit harware multiplier
  signal mul_cs       : std_logic;
  signal mul_data_out : std_logic_vector(7 downto 0);

  -- external access
  signal ext_cs       : std_logic;
  signal ext_data_out : std_logic_vector(7 downto 0);

  -- cache host signals
  signal cache_hcs      : std_logic;
  signal cache_haddr     : std_logic_vector(31 downto 0);
  signal cache_hdata_in  : std_logic_vector(15 downto 0);
  signal cache_hdata_out : std_logic_vector(15 downto 0);
  signal cache_hen       : std_logic;

  -- cache memory signals
  signal cache_scs      : std_logic;
  signal cache_saddr     : std_logic_vector(31 downto 0);
  signal cache_sdata_in  : std_logic_vector(15 downto 0);
  signal cache_sdata_out : std_logic_vector(15 downto 0);

component cpu09
  port (    
	 clk      :	in  std_logic;
    rst      : in  std_logic;
    rw       :	out std_logic;
    vma      :	out std_logic;
    address  : out std_logic_vector(15 downto 0);
    data_in  : in	 std_logic_vector(7 downto 0);
	 data_out : out std_logic_vector(7 downto 0);
	 halt     : in  std_logic;
	 hold     : in  std_logic;
	 irq      : in  std_logic;
	 nmi      : in  std_logic;
	 firq     : in  std_logic
  );
end component;

----------------------------------------
--
-- Memory Management Unit (32 x 16)
--
----------------------------------------
component mmu
  port (
    clk      : in  std_logic;
	 rst      : in  std_logic;
	 cs       : in  std_logic;
	 rw       : in  std_logic;
	 addr     : in  std_logic_vector(15 downto 0);
    data_in  : in  std_logic_vector( 7 downto 0);
	 addr_out : out std_logic_vector(27 downto 0)
  );
end component;


----------------------------------------
--
-- 4KByte Block RAM Monitor ROM
--
----------------------------------------
component mon_rom
  Port (
    clk      : in  std_logic;
    rst      : in  std_logic;
    cs       : in  std_logic;
    addr     : in  std_logic_vector (11 downto 0);
    rw       : in  std_logic;
    data_in  : in  std_logic_vector (7 downto 0);
    data_out : out std_logic_vector (7 downto 0)
    );
end component;

----------------------------------------
--
-- Dual Port Cache memory 2K x 8 Bit
--
----------------------------------------
component dpr_2k
  port (
    --
    -- Port A (Host)
    --
    clk_a      : in  std_logic;
    rst_a      : in  std_logic;
    cs_a       : in  std_logic;
    rw_a       : in  std_logic;
    addr_a     : in  std_logic_vector (10 downto 0);
    data_in_a  : in  std_logic_vector ( 7 downto 0);
    data_out_a : out std_logic_vector ( 7 downto 0);
    --
    -- Port B (Slave)
    --
    clk_b      : in  std_logic;
    rst_b      : in  std_logic;
    cs_b       : in  std_logic;
    rw_b       : in  std_logic;
    addr_b     : in  std_logic_vector (10 downto 0);
    data_in_b  : in  std_logic_vector ( 7 downto 0);
    data_out_b : out std_logic_vector ( 7 downto 0)
  );
end component;

----------------------------------------
--
-- 32 bit hardware multiplier
--
----------------------------------------
component mul32
  port (
    clk      : in  std_logic;
	 rst      : in  std_logic;
	 cs       : in  std_logic;
	 rw       : in  std_logic;
	 addr     : in  std_logic_vector(3 downto 0);
    data_in  : in  std_logic_vector(7 downto 0);
	 data_out : out std_logic_vector(7 downto 0)
  );
end component;

begin
  -----------------------------------------------------------------------------
  -- Instantiation of internal components
  -----------------------------------------------------------------------------

my_cpu : cpu09  port map (    
	 clk	     => clk,
    rst       => rst,
    rw	     => cpu_rw,
    vma       => cpu_vma,
    address   => cpu_addr(15 downto 0),
    data_in   => cpu_data_in,
	 data_out  => cpu_data_out,
	 halt      => halt,
	 hold      => hold,
	 irq       => irq,
	 nmi       => nmi,
	 firq      => firq
    );

my_mmu : mmu port map (
    clk       => clk,
	 rst       => rst,
	 cs        => mmu_cs,
	 rw        => cpu_rw,
	 addr      => cpu_addr,
    data_in   => cpu_data_out,
	 addr_out  => mmu_addr
	 );

my_rom : mon_rom port map (
    clk       => clk,
    rst       => rst,
	 cs        => rom_cs,
	 rw        => '1',
    addr      => cpu_addr(11 downto 0),
    data_in   => cpu_data_out,
    data_out  => rom_data_out
    );

--
-- High Address Cache
--
my_dpr_0 : dpr_2k port map (
    --
    -- Port A (Host / CPU interface)
    --
    clk_a      => clk,
    rst_a      => rst,
    cs_a       => cpu_vma,
    rw_a       => '0',
    addr_a     => cpu_addr(10 downto 0),
    data_in_a  => mmu_addr(26 downto 19),
    data_out_a => cache_haddr(26 downto 19),
    --
    -- Port B (Slave / Memory Interface)
    --
    clk_b      => clk,
    rst_b      => rst,
    cs_b       => uni_vma,
    rw_b       => '1',
    addr_b     => uni_addr(10 downto 0),
    data_in_b  => (others => '0'),
    data_out_b => cache_saddr(26 downto 19)
  );

--
-- Low Address cache
--
my_dpr_1 : dpr_2k port map (
    --
    -- Port A (Host / CPU interface)
    --
    clk_a      => clk,
    rst_a      => rst,
    cs_a       => cpu_vma,
    rw_a       => '0',
    addr_a     => cpu_addr(10 downto  0),
    data_in_a  => mmu_addr(18 downto 11),
    data_out_a => cache_haddr(18 downto 11),
    --
    -- Port B (Slave / Memory Interface)
    --
    clk_b      => clk,
    rst_b      => rst,
    cs_b       => uni_vma,
    rw_b       => '1',
    addr_b     => uni_addr(10 downto 0),
    data_in_b  => (others => '0'),
    data_out_b => cache_saddr(18 downto 11)
  );


--
-- data cache
--
my_dpr_2 : dpr_2k port map (
    --
    -- Port A (Host / CPU Interface)
    --
    clk_a      => clk,
    rst_a      => rst,
    cs_a       => cache_hcs,
    rw_a       => cpu_rw,
    addr_a     => cpu_addr(10 downto 0),
    data_in_a  => cache_hdata_in(7 downto 0),
    data_out_a => cache_hdata_out(7 downto 0),
    --
    -- Port B (Slave / Memory Interface)
    --
    clk_b      => clk,
    rst_b      => rst,
    cs_b       => cache_scs,
    rw_b       => uni_rw,
    addr_b     => uni_addr(10 downto 0),
    data_in_b  => cache_sdata_in(7 downto 0),
    data_out_b => cache_sdata_out(7 downto 0)
  );

my_mul32 : mul32 port map (
    clk       => clk,
	 rst       => rst,
	 cs        => mul_cs,
	 rw        => cpu_rw,
	 addr      => cpu_addr(3 downto 0),
    data_in   => cpu_data_out,
	 data_out  => mul_data_out
	 );

----------------------------------------------------------------------
--
-- Process to decode internal registers
--
----------------------------------------------------------------------

uni_decode: process( cpu_addr, cpu_rw, cpu_vma,
                     cache_hdata_out,
					      mmu_cs, mmu_addr,
							cache_hdata_out,
							mul_data_out,
							rom_data_out
							)
begin
  --
  -- By default CPU accesses memory cache
  --
  cpu_data_in  <= cache_hdata_out( 7 downto 0);
  cache_hen    <= '1';     -- Cache enabled by default
  ext_cs       <= cpu_vma; -- Assume external access
  mmu_cs       <= '0';     -- Memory Management Unit (Extended DAT)
  rom_cs       <= '0';     -- Monitor ROM
  mul_cs       <= '0';     -- Hardware Multiplier

  if cpu_addr( 15 downto 8 ) = "11111111" then
    --
    -- MMU write registers at $FF00 to $FFFF
	 --
    cpu_data_in  <= rom_data_out;
    rom_cs       <= cpu_vma;
    mmu_cs       <= cpu_vma;
	 ext_cs       <= '0';
    cache_hen    <= '0';
  --
  -- ROM  $F000 - $FFFF
  --
  elsif mmu_addr(3 downto 0) = "1111" then -- $XF000 - $XFFFF
    cpu_data_in  <= rom_data_out;
    rom_cs       <= cpu_vma;
    cache_hen    <= '0';
  --
  -- IO Devices $E000 - $EFFF
  --
  elsif mmu_addr(3 downto 0) = "1110" then -- $XE000 - $XEFFF
    --
	 -- disable cache for I/O
	 --
    cache_hen <= '0';

	 case cpu_addr(11 downto 8) is
    --
    -- CPU specific registers from $E200 to $E2FF
    --
    when "0010" =>
	   ext_cs      <= '0';              -- assume this segment is internal
      cpu_data_in <= (others=>'0');		-- default to null data

      --
      -- Module ID number
      --
      case cpu_addr(7 downto 4) is
      when "0000" =>
        cpu_data_in <= id;             -- unicpu module ID

      --
      -- hardware 32 bit multiplier
      --
      when "0001" =>
        cpu_data_in <= mul_data_out;   -- Hardware Multiplier register
        mul_cs      <= cpu_vma;

      when others =>
		  null;
      end case;

    --
    -- Everything else is external
    --
    when others =>
	   null;
 
    end case;
  end if;
end process;

--
-- cpu side cache controller
--
my_cpu_cache : process( cpu_vma, cpu_rw, cpu_data_out, cpu_addr, mmu_addr, 
                        cache_haddr, cache_hen, ext_cs )
begin
  addr(19 downto 12) <= mmu_addr(19 downto 12) xor id;
  addr(11 downto  0) <= cpu_addr(11 downto  0);
  rw   <= cpu_rw;
  vma  <= '0';
  --
  -- external access if cache miss or write through or if i/o space
  --
  if (cache_haddr(23 downto 16) /= mmu_addr( 7 downto 0) ) or
	  (cache_haddr(11 downto  0) /= cpu_addr(11 downto 0) ) or
     (cpu_rw = '0') or (cache_hen = '0') then
    vma <= ext_cs;
  end if;
  cache_hdata_in( 7 downto 0) <= cpu_data_out;    
  cache_hdata_in(15 downto 8) <= (others=>'0');    
end process;

--
-- memory side cache controller
--
my_uni_cache : process( uni_vma, uni_addr, uni_data_in, uni_rw, 
                        cache_saddr, cache_sdata_out )
begin
  --
  -- write through from another CPU will update cache entry
  -- if there is a cache hit
  --
  cache_scs <= '0';
  if (cache_saddr(23 downto 16) = uni_addr(19 downto 12)) and
     (cache_saddr(11 downto  0) = uni_addr(11 downto  0)) then
    cache_scs <= uni_vma;
  end if;
  uni_data_out <= cache_sdata_out( 7 downto 0);
  cache_sdata_in( 7 downto 0) <= uni_data_in;    
  cache_sdata_in(15 downto 8) <= (others=>'0');      
end process;

end architecture;

-----------------------------------------------------------------------------
