--===========================================================================--
--                                                                           --
--                       Peripheral Bus Interface                            --
--                                                                           --
--===========================================================================--
--
--  File name      : peripheral_bus.vhd
--
--  Entity name    : peripheral_bus
--
--  Purpose        : Implements a 16 bit peripheral bus interface 
--                   On the XESS XST-3.0 carrier board it is shared 
--                   by an IDE interface, and Ethernet MAC
--                   and two 16 bit expansion slots.
--                   The same bus structure is used on the 
--                   BurchED B3 and B5-X300 Spartan 2 boards
--                   to implement an IDE Compact Flash interface.
--
--                   The 16 bit data bus is accessed by two
--                   consecutive byte wide read or write cycles.
--
--                   On an even byte read a read strobe is generated
--                   on the peripheral bus and the high bits of the
--                   peripheral data bus are output to the CPU
--                   data bus and the lower 8 bits latched.
--                   A bus hold cycle is generated to allow time
--                   for the peripheral data bus to settle.
--                   On the odd byte read, the latched lower data 
--                   bits of the peripheral bus are output on the
--                   CPU data bus.
--
--                   Conversely, on an even byte write the CPU data
--                   bus value is latched. On the odd byte write, the
--                   latched value is output to the high 8 bits of
--                   the peripheral bus, and the CPU data is output
--                   to the lower 8 bits of the peripheral bus and
--                   a peripheral write strobe is generated.
--                   A hold signal is geneated back to the CPU to
--                   allow the peripheral bus to settle.
--                  
--  Dependencies   : ieee.std_logic_1164
--                   ieee.numeric_std
--                   unisim.vcomponents
--
--  Author         : John E. Kent
--
--  Email          : dilbert57@opencores.org      
--
--  Web            : http://opencores.org/project,system09
--
--  Memory Map     :
--
--  IO address + $00 IDE Compact Flash interface
--  IO address + $40 Ethernet MAC interface (XESS XST-3.0)
--  IO address + $80 Expansion Slot 0       (XESS XST-3.0)
--  IO address + $C0 Expansion Slot 1       (XESS XST-3.0)
--
--
--  Copyright (C) 2010 John Kent
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
-- Version Author       Date         Changes
--
-- 0.1     John Kent    2010-08-28   New model
--

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;
  use ieee.std_logic_unsigned.all;
--library unisim;
--  use unisim.vcomponents.all;

-----------------------------------------------------------------------
--                 Entity for peripheral bus                         --
-----------------------------------------------------------------------

entity peripheral_bus is
  port (
    --
    -- CPU Interface signals
    --
    clk      : in  std_logic;                     -- System Clock
    rst      : in  std_logic;                     -- Reset input (active high)
    cs       : in  std_logic;                     -- Peripheral Bus Chip Select
    addr     : in  std_logic_vector(7 downto 0);  -- Register Select
    rw       : in  std_logic;                     -- Read / Not Write
    data_in  : in  std_logic_vector(7 downto 0);  -- Data Bus In 
    data_out : out std_logic_vector(7 downto 0);  -- Data Bus Out
    hold     : out std_logic;                     -- Hold bus cycle output
    --
    -- Peripheral Bus Interface Signals
    -- IO + ($00 - $FF) 
    -- (for compatibility with XSA-3S1000 / XST 3.0)
    --
    pb_rd_n  : out   std_logic; -- ide pin 25
    pb_wr_n  : out   std_logic; -- ide pin 23
    pb_addr  : out   std_logic_vector( 4 downto 0);
    pb_data  : inout std_logic_vector(15 downto 0);

    -- Peripheral chip selects on Peripheral Bus 
    ide_cs   : out  std_logic;  -- IDE / CF interface ($00 - $3F)
    eth_cs   : out  std_logic;  -- Ethernet interface ($40 - $7F)
    sl1_cs   : out  std_logic;  -- Expansion slot 1   ($80 - $BF)
    sl2_cs   : out  std_logic   -- Expansion slot 2   ($C0 - $FF)
    );
end peripheral_bus;  
--================== End of entity ==============================--

-------------------------------------------------------------------------------
-- Architecture for peripheral bus interface
-------------------------------------------------------------------------------

architecture rtl of peripheral_bus is


  type hold_state_type is ( hold_release_state, hold_request_state );
  signal pb_hold_state : hold_state_type := hold_release_state;
  signal pb_wru        : std_logic;	  -- upper byte write strobe
  signal pb_wrl        : std_logic;	  -- lower byte write strobe
  signal pb_rdu        : std_logic;	  -- upper byte read strobe
  signal pb_rdl        : std_logic;	  -- lower byte read strobe
  signal pb_hold       : std_logic := '0';	  -- hold peripheral bus access
  signal pb_count      : std_logic_vector(3 downto 0) := (others=>'0'); -- hold counter
  signal pb_wreg       : std_logic_vector(7 downto 0) := (others=>'0'); -- lower byte write register
  signal pb_rreg       : std_logic_vector(7 downto 0) := (others=>'0'); -- lower byte read register

begin

peripheral_bus_decode : process( addr, cs )
begin

  ide_cs <= '0';
  eth_cs <= '0';
  sl1_cs <= '0';
  sl2_cs <= '0';
   
  case addr(7 downto 6) is
  --
  -- IDE Interface $E100 to $E13F
  --
  when "00" =>
    ide_cs <= cs;
  --
  -- Ethernet Interface $E140 to $E17F
  --
  when "01" =>
    eth_cs <= cs;
  --
  -- Slot 1 Interface $E180 to $E1BF
  --
  when "10" =>
    sl1_cs <= cs;
  --
  -- Slot 2 Interface $E1C0 to $E1FF
  --
  when "11" =>
    sl2_cs <= cs;
  --
  -- Nothing else
  --
  when others =>
    null;
  end case;

end process;

--
-- 16-bit Peripheral Bus
-- 6809 Big endian
-- ISA bus little endian
-- Not sure about IDE interface
--
peripheral_bus_control: process( clk, rst, cs, addr, rw, data_in,
                                 pb_hold, pb_wreg, pb_rreg,
                                 pb_wru, pb_wrl, pb_rdu, pb_rdl, pb_data )
begin
   pb_addr <= addr(5 downto 1);
   --
   -- internal read/write strobes
   --
   pb_wru  <= cs and (not rw) and (not addr(0));
   pb_wrl  <= cs and (not rw) and      addr(0) ;
   pb_rdu  <= cs and      rw  and (not addr(0));
   pb_rdl  <= cs and      rw  and      addr(0) ;

   pb_wr_n  <= not pb_wrl;
   pb_rd_n  <= not pb_rdu;

   --
   -- The peripheral bus will be an output 
   -- the registered even byte on data(15 downto 8)
   -- and the CPU odd bytes on data(7 downto 0)
   -- on odd byte writes
   --
   if pb_wrl = '1' then
     pb_data <= pb_wreg & data_in;
   else
     pb_data <= (others => 'Z');
   end if;

   --
   -- On even byte reads,
   -- the CPU reads the low (even) byte of the peripheral bus
   -- On odd byte reads,
   -- the CPU reads the registered (odd byte) input from the peripheral bus
   --
   if pb_rdu = '1' then
      data_out <= pb_data(15 downto 8);
   elsif pb_rdl = '1' then
      data_out <= pb_rreg;
   else
      data_out <= (others => '0');
   end if;
  
   --
   -- Register upper byte from CPU on first CPU write
   -- and lower byte from the peripheral bus on first CPU read
   --
   if falling_edge(clk) then
     if rst = '1' then
       pb_wreg   <= (others => '0');
       pb_rreg   <= (others => '0');
     else

       if pb_wru = '1' then
         pb_wreg <= data_in;
       end if;

       if pb_rdu = '1' then
         pb_rreg <= pb_data(7 downto 0);
       end if;

     end if;
   end if;

end process;

--
-- Hold Peripheral bus accesses for a few cycles
--
peripheral_bus_hold: process( clk, rst, cs,
                              pb_hold_state, pb_hold,
                              pb_rdu, pb_wrl )
begin
  if rising_edge( clk ) then
    if rst = '1' then
		 pb_hold       <= '0';
		 pb_count      <= "0000";
	    pb_hold_state <= hold_release_state;
	 else
      --
      -- The perpheral bus hold signal should be generated on 
      -- 16 bit bus even upper byte read or 
      -- 16 bit bus odd lower byte write.
      -- 
      case pb_hold_state is

		when hold_release_state =>
		  if (pb_rdu = '1') or (pb_wrl = '1') then
          pb_count      <= "0011";
          pb_hold       <= '1';
          pb_hold_state <= hold_request_state;
        else
          pb_hold       <= '0';
          pb_hold_state <= hold_release_state;
        end if;

		 when hold_request_state =>
         if pb_count = "0000" then
           pb_hold       <= '0';
           pb_hold_state <= hold_release_state;
         else
		     pb_count <= pb_count - "0001";
			end if;

       when others =>
		    null;

       end case;
	 end if;
  end if;
  hold <= cs and pb_hold;
end process;

end rtl;
