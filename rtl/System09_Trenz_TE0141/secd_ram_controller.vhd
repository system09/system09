-- secd_ram_controller.vhd
--
-- Multiplex the external 16 bit SRAM to the 32 bit interface required
-- by the CPU and provide for an 8 bit backside port for the 6809 to
-- read and write SECD memory

library ieee;

use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_unsigned.all;

entity secd_ram_controller is
  port(
    clk              : in std_logic;
    reset            : in std_logic;

    secd_stopped     : in std_logic;

    -- Internal interface to SECD (16k x 32)
    din32            : in std_logic_vector(31 downto 0);
    dout32           : out std_logic_vector(31 downto 0);
    addr32           : in std_logic_vector(13 downto 0);
    read32_enable    : in std_logic;
    write32_enable   : in std_logic;
    busy32           : out std_logic;

    -- Internal interface to 6809 (64k x 8)
	 clk8             : in std_logic;
    din8             : in std_logic_vector(7 downto 0);
    dout8            : out std_logic_vector(7 downto 0);
    addr8            : in std_logic_vector(15 downto 0);
    rw8              : in std_logic;
    cs8_ram          : in std_logic;
    hold8            : out std_logic;
	 cs8_cf           : in std_logic;

    -- External interface
    ram_oen          : out std_logic;
    ram_cen          : out std_logic;
    ram_wen          : out std_logic;
    ram_io           : inout std_logic_vector(15 downto 0);
    ram_a            : out std_logic_vector(20 downto 1);
    ram_bhen         : out std_logic;
    ram_blen         : out std_logic
  );
end;

architecture external_ram of secd_ram_controller is

  type hold_state_type is ( hold_release_state, hold_request_state );

  signal cf_hold_state : hold_state_type;

  signal cf_release  : std_logic;
  signal cf_count    : std_logic_vector(3 downto 0);

  type state_type is (idle, 
                      read32_high, read32_high_deselect, read32_low,	read32_low_deselect,
                      write32_high, write32_high_deselect, write32_low, write32_low_deselect,
							 read8_ram, write8_ram, read8_cf, write8_cf );

  signal state, next_state : state_type;

  signal read32_buff : std_logic_vector(31 downto 0);
  signal read32_hen, read32_len : std_logic;

begin


secd_ram_process : process( state, 
                            read32_enable, write32_enable, addr32, din32, 
                            cs8_ram, rw8, addr8, din8 )
begin
    case state is
	 when idle =>
		 ram_a(20 downto 1) <= (others => '0');
		 ram_cen  <= '1';
		 ram_oen  <= '1';
		 ram_wen  <= '1';
		 ram_bhen <= '1';
		 ram_blen <= '1'; 
		 ram_io   <= (others => 'Z');
		 read32_hen <= '0';
		 read32_len <= '0';
		 dout8      <= (others => '0');
       if read32_enable = '1' then
           hold8  <= '0';
		     busy32 <= '1';
		     next_state <= read32_high;
       elsif write32_enable = '1' then
           hold8  <= '0';
		     busy32 <= '1';
		     next_state <= write32_high;
       elsif (cs8_ram = '1') and (rw8 = '1') then
           hold8  <= '1';
		     busy32 <= '1';
		     next_state <= read8_ram;
       elsif (cs8_ram = '1') and (rw8 = '0') then
           hold8  <= '1';
		     busy32 <= '1';
		     next_state <= write8_ram;
       elsif (cs8_cf = '1') and (rw8 = '1') then
           hold8  <= '1';
		     busy32 <= '1';
		     next_state <= read8_cf;
       elsif (cs8_cf = '1') and (rw8 = '0') then
           hold8  <= '1';
		     busy32 <= '1';
		     next_state <= write8_cf;
       else
           hold8  <= '0';
		     busy32 <= '0';
		     next_state <= idle;
       end if;

    when read32_high =>
	    ram_a(1) <= '0';
		 ram_a(20 downto 2) <= "00000" & addr32(13 downto 0);
		 ram_cen  <= '0';
		 ram_oen  <= '0';
		 ram_wen  <= '1';
		 ram_bhen <= '0';
		 ram_blen <= '0'; 
		 ram_io <= (others => 'Z');
		 read32_hen <= '1';
		 read32_len <= '0';
		 busy32 <= '1';
		 dout8  <= (others => '0');
		 hold8  <= cs8_ram;
		 next_state <= read32_high_deselect;

    when read32_high_deselect =>
	    ram_a(1) <= '1';
		 ram_a(20 downto 2) <= "00000" & addr32(13 downto 0);
		 ram_cen  <= '1';
		 ram_oen  <= '1';
		 ram_wen  <= '1';
		 ram_bhen <= '1';
		 ram_blen <= '1'; 
		 ram_io <= (others => 'Z');
		 read32_hen <= '0';
		 read32_len <= '0';
		 busy32 <= '1';
		 dout8  <= (others => '0');
		 hold8  <= cs8_ram;
		 next_state <= read32_low;

    when read32_low =>
	    ram_a(1) <= '1';
		 ram_a(20 downto 2) <= "00000" & addr32(13 downto 0);
		 ram_cen  <= '0';
		 ram_oen  <= '0';
		 ram_wen  <= '1';
		 ram_bhen <= '0';
		 ram_blen <= '0'; 
		 ram_io <= (others => 'Z');
		 read32_hen <= '0';
		 read32_len <= '1';
		 busy32 <= '1';
		 dout8  <= (others => '0');
		 hold8  <= cs8_ram;
		 next_state <= read32_low_deselect;

    when read32_low_deselect =>
	    ram_a(1) <= '1';
		 ram_a(20 downto 2) <= "00000" & addr32(13 downto 0);
		 ram_cen  <= '1';
		 ram_oen  <= '1';
		 ram_wen  <= '1';
		 ram_bhen <= '1';
		 ram_blen <= '1'; 
		 ram_io <= (others => 'Z');
		 read32_hen <= '0';
		 read32_len <= '0';
		 busy32 <= '0';
		 dout8  <= (others => '0');
		 hold8  <= cs8_ram;
		 next_state <= idle;

    when write32_high =>
	    ram_a(1) <= '0';
		 ram_a(20 downto 2) <= "00000" & addr32(13 downto 0);
		 ram_cen  <= '0';
		 ram_oen  <= '1';
		 ram_wen  <= '0';
		 ram_bhen <= '0';
		 ram_blen <= '0'; 
		 ram_io <= din32(31 downto 16);
		 read32_hen <= '0';
		 read32_len <= '0';
		 busy32 <= '1';
		 dout8  <= (others => '0');
		 hold8  <= cs8_ram;
		 next_state <= write32_high_deselect;

    when write32_high_deselect =>
	    ram_a(1) <= '1';
		 ram_a(20 downto 2) <= "00000" & addr32(13 downto 0);
		 ram_cen  <= '1';
		 ram_oen  <= '1';
		 ram_wen  <= '1';
		 ram_bhen <= '1';
		 ram_blen <= '1'; 
		 ram_io <= (others => 'Z');
		 read32_hen <= '0';
		 read32_len <= '0';
		 busy32 <= '1';
		 dout8  <= (others => '0');
		 hold8  <= cs8_ram;
		 next_state <= write32_low;

    when write32_low =>
	    ram_a(1) <= '1';
		 ram_a(20 downto 2) <= "00000" & addr32(13 downto 0);
		 ram_cen  <= '0';
		 ram_oen  <= '0';
		 ram_wen  <= '1';
		 ram_bhen <= '0';
		 ram_blen <= '0'; 
		 ram_io <= din32(15 downto 0);
		 read32_hen <= '0';
		 read32_len <= '0';
		 busy32 <= '0';
		 dout8  <= (others => '0');
		 hold8  <= cs8_ram;
		 next_state <= write32_low_deselect;

    when write32_low_deselect =>
	    ram_a(1) <= '1';
		 ram_a(20 downto 2) <= "00000" & addr32(13 downto 0);
		 ram_cen  <= '1';
		 ram_oen  <= '1';
		 ram_wen  <= '1';
		 ram_bhen <= '1';
		 ram_blen <= '1'; 
		 ram_io <= (others => 'Z');
		 read32_hen <= '0';
		 read32_len <= '0';
		 busy32 <= '0';
		 dout8  <= (others => '0');
		 hold8  <= cs8_ram;
		 next_state <= idle;

    when read8_ram =>
		 ram_a(20 downto 1) <= "00000" & addr8(15 downto 1);
		 ram_cen  <= '0';
		 ram_oen  <= '0';
		 ram_wen  <= '1';
		 ram_bhen <= addr8(0);
		 ram_blen <= not addr8(0); 
		 ram_io <= (others => 'Z');
		 read32_hen <= '0';
		 read32_len <= '0';
		 busy32 <= '1';
		 if addr8(0) = '0' then
	       dout8 <= ram_io(15 downto 8);
       else
		    dout8 <= ram_io(7 downto 0);
       end if;
		 hold8 <= '0';
		 -- Synchronize on the CPU clock
		 if clk8 = '1' then
		     next_state <= idle;
       else
		     next_state <= read8_ram;
       end if;

    when write8_ram =>
	    ram_a(1) <= '1';
		 ram_a(20 downto 1) <= "00000" & addr8(15 downto 1);
		 ram_cen  <= '0';
		 ram_oen  <= '0';
		 ram_wen  <= '1';
		 ram_bhen <= addr8(0);
		 ram_blen <= not addr8(0); 
		 if addr8(0) = '0' then
	       ram_io(15 downto 8) <= din8;
			 ram_io( 7 downto 0) <= (others => 'Z');
       else
			 ram_io(15 downto 8) <= (others => 'Z');
		    ram_io( 7 downto 0) <= din8;
       end if;
		 read32_hen <= '0';
		 read32_len <= '0';
		 busy32 <= '1';
		 dout8  <= (others => '0');
		 hold8  <= '0';
		 -- Synchronize on the CPU clock
		 if clk8 = '1' then 
		     next_state <= idle;
       else
		     next_state <= write8_ram;
       end if;


    when read8_cf =>
		 ram_a(20 downto 1) <= "00000" & addr8(15 downto 1);
		 ram_cen  <= '1';
		 ram_oen  <= '1';
		 ram_wen  <= '1';
		 ram_bhen <= '1';
		 ram_blen <= '1'; 
		 ram_io <= (others => 'Z');
		 read32_hen <= '0';
		 read32_len <= '0';
		 busy32 <= '1';
		 dout8 <= ram_io(7 downto 0);
		 if cf_release = '1' then
           hold8 <= '0';
		     next_state <= idle;
       else
           hold8 <= '1';
		     next_state <= read8_cf;
       end if;

    when write8_cf =>
	    ram_a(1) <= '1';
		 ram_a(20 downto 1) <= "00000" & addr8(15 downto 1);
		 ram_cen  <= '1';
		 ram_oen  <= '1';
		 ram_wen  <= '1';
		 ram_bhen <= '1';
		 ram_blen <= '1'; 
	    ram_io(15 downto 8) <= (others => '0');
		 ram_io( 7 downto 0) <= din8;
		 read32_hen <= '0';
		 read32_len <= '0';
		 busy32 <= '1';
		 dout8  <= (others => '0');
		 if cf_release = '1' then
           hold8 <= '0';
		     next_state <= idle;
       else
           hold8 <= '1';
		     next_state <= write8_cf;
       end if;

   when others =>
	    null;

	end case;
end process;

  --
  -- RAM state machine
  -- clock state transitions
  -- and register 32 bit reads.
  --
  -- Try experimenting with the clock edge
  -- The Clock edge should be the same
  -- as the transition edge of the
  -- 12.5 MHz 6809 clock.
  -- 
  ram_state_machine : process( clk, reset, read32_buff )
  begin
     if reset = '1' then
	      state <= idle;
		   read32_buff <= (others => '0');
     elsif falling_edge( clk ) then
	      state <= next_state;
		   if read32_hen = '1' then
		       read32_buff(31 downto 16) <= ram_io;
         end if;
		   if read32_len = '1' then
		       read32_buff(15 downto 0) <= ram_io;
         end if;
	  end if;
	  dout32 <= read32_buff; 
  end process;

--
-- Hold CF access	for a few cycles
-- synchronize with the CPU clock
-- hold release is set on the rising edge
-- of the CPU clock so that you have one
-- VGA clock cycle to return to the idle state
-- of the secd_ram_process state machine.
--
cf_hold_proc: process( clk8, reset )
begin
    if reset = '1' then
		 cf_release    <= '0';
		 cf_count      <= "0000";
	    cf_hold_state <= hold_release_state;
	 elsif rising_edge( clk8 ) then
	    case cf_hold_state is
		 when hold_release_state =>
          cf_release <= '0';
		    if cs8_cf = '1' then
			    cf_count      <= "0011";
				 cf_hold_state <= hold_request_state;
			 end if;

		 when hold_request_state =>
		    cf_count <= cf_count - "0001";
			 if cf_count = "0000" then
             cf_release    <= '1';
				 cf_hold_state <= hold_release_state;
			 end if;
       when others =>
		    null;
       end case;
	 end if;

end process;

end;
