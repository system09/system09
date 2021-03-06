--*********************************************************************
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
-- of the License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.
--
-- �2001-2012 - X Engineering Software Systems Corp. (www.xess.com)
--*********************************************************************

--*********************************************************************
-- SDRAM controller and dual-port interface.
--*********************************************************************



library IEEE;
use IEEE.std_logic_1164.all;
use work.CommonPckg.all;

package DualPortSdramCntlPckg is

  component DualPort is
    generic(
      PIPE_EN_G : boolean := false; -- enable pipelined read operations.
      PORT_TIME_SLOTS_G : std_logic_vector(15 downto 0) := "1111000011110000";
      DATA_WIDTH_G : natural := 16; -- host & SDRAM data width.
      HADDR_WIDTH_G : natural := 23 -- host-side address width.
      );
    port(
      clk_i : in std_logic; -- master clock.

      -- Host-side port 0.
      rst0_i : in std_logic := NO; -- reset.
      rd0_i : in std_logic := NO; -- initiate read operation.
      wr0_i : in std_logic := NO; -- initiate write operation.
      earlyOpBegun0_o : out std_logic; -- read/write op has begun (async).
      opBegun0_o : out std_logic := NO; -- read/write op has begun (clocked).
      rdPending0_o : out std_logic; -- true if read operation(s) are still in the pipeline.
      done0_o : out std_logic; -- read or write operation is done_i.
      rdDone0_o : out std_logic; -- read operation is done_i and data is available.
      addr0_i : in std_logic_vector(HADDR_WIDTH_G-1 downto 0) := (others => ZERO); -- address from host to SDRAM.
      data0_i : in std_logic_vector(DATA_WIDTH_G-1 downto 0) := (others => ZERO); -- data from host to SDRAM.
      data0_o : out std_logic_vector(DATA_WIDTH_G-1 downto 0) := (others => ZERO); -- data from SDRAM to host.
      status0_o : out std_logic_vector(3 downto 0); -- diagnostic status of the SDRAM controller FSM .

      -- Host-side port 1.
      rst1_i : in std_logic := NO;
      rd1_i : in std_logic := NO;
      wr1_i : in std_logic := NO;
      earlyOpBegun1_o : out std_logic;
      opBegun1_o : out std_logic := NO;
      rdPending1_o : out std_logic;
      done1_o : out std_logic;
      rdDone1_o : out std_logic;
      addr1_i : in std_logic_vector(HADDR_WIDTH_G-1 downto 0) := (others => ZERO);
      data1_i : in std_logic_vector(DATA_WIDTH_G-1 downto 0) := (others => ZERO);
      data1_o : out std_logic_vector(DATA_WIDTH_G-1 downto 0) := (others => ZERO);
      status1_o : out std_logic_vector(3 downto 0);

      -- SDRAM controller host-side port.
      rst_o : out std_logic;
      rd_o : out std_logic;
      wr_o : out std_logic;
      earlyOpBegun_i : in std_logic;
      opBegun_i : in std_logic;
      rdPending_i : in std_logic;
      done_i : in std_logic;
      rdDone_i : in std_logic;
      addr_o : out std_logic_vector(HADDR_WIDTH_G-1 downto 0);
      data_o : out std_logic_vector(DATA_WIDTH_G-1 downto 0);
      data_i : in std_logic_vector(DATA_WIDTH_G-1 downto 0);
      status_i : in std_logic_vector(3 downto 0)
      );
  end component;

  component DualPortSdram is
    generic(
      PORT_TIME_SLOTS_G : std_logic_vector(15 downto 0) := "1111000011110000";
      FREQ_G : real := 100.0; -- Operating frequency in MHz.
      IN_PHASE_G : boolean := true; -- SDRAM and controller work on same or opposite clock edge.
      PIPE_EN_G : boolean := false; -- If true, enable pipelined read operations.
      MAX_NOP_G : natural := 10000; -- Number of NOPs before entering self-refresh.
      ENABLE_REFRESH_G : boolean := true; -- If true, row refreshes are automatically inserted.
      MULTIPLE_ACTIVE_ROWS_G : boolean := false; -- If true, allow an active row in each bank.
      DATA_WIDTH_G : natural := 16; -- Host & SDRAM data width.
      -- Parameters for Winbond W9812G6JH-75 (all times are in nanoseconds).
      NROWS_G : natural := 4096; -- Number of rows in SDRAM array.
      NCOLS_G : natural := 512; -- Number of columns in SDRAM array.
      HADDR_WIDTH_G : natural := 23; -- Host-side address width.
      SADDR_WIDTH_G : natural := 12; -- SDRAM-side address width.
      T_INIT_G : real := 200_000.0; -- min initialization interval (ns).
      T_RAS_G : real := 45.0; -- min interval between active to precharge commands (ns).
      T_RCD_G : real := 20.0; -- min interval between active and R/W commands (ns).
      T_REF_G : real := 64_000_000.0; -- maximum refresh interval (ns).
      T_RFC_G : real := 65.0; -- duration of refresh operation (ns).
      T_RP_G : real := 20.0; -- min precharge command duration (ns).
      T_XSR_G : real := 75.0 -- exit self-refresh time (ns).
      );
    port(
      clk_i : in std_logic; -- master clock.

      -- Host-side port 0.
      rst0_i : in std_logic := NO; -- reset.
      rd0_i : in std_logic := NO; -- initiate read operation.
      wr0_i : in std_logic := NO; -- initiate write operation.
      earlyOpBegun0_o : out std_logic; -- read/write op has begun (async).
      opBegun0_o : out std_logic := NO; -- read/write op has begun (clocked).
      rdPending0_o : out std_logic; -- true if read operation(s) are still in the pipeline.
      done0_o : out std_logic; -- read or write operation is done_i.
      rdDone0_o : out std_logic; -- read operation is done_i and data is available.
      addr0_i : in std_logic_vector(HADDR_WIDTH_G-1 downto 0) := (others => ZERO); -- address from host to SDRAM.
      data0_i : in std_logic_vector(DATA_WIDTH_G-1 downto 0) := (others => ZERO); -- data from host to SDRAM.
      data0_o : out std_logic_vector(DATA_WIDTH_G-1 downto 0) := (others => ZERO); -- data from SDRAM to host.
      status0_o : out std_logic_vector(3 downto 0); -- diagnostic status of the SDRAM controller FSM .

      -- Host-side port 1.
      rst1_i : in std_logic := NO;
      rd1_i : in std_logic := NO;
      wr1_i : in std_logic := NO;
      earlyOpBegun1_o : out std_logic;
      opBegun1_o : out std_logic := NO;
      rdPending1_o : out std_logic;
      done1_o : out std_logic;
      rdDone1_o : out std_logic;
      addr1_i : in std_logic_vector(HADDR_WIDTH_G-1 downto 0) := (others => ZERO);
      data1_i : in std_logic_vector(DATA_WIDTH_G-1 downto 0) := (others => ZERO);
      data1_o : out std_logic_vector(DATA_WIDTH_G-1 downto 0) := (others => ZERO);
      status1_o : out std_logic_vector(3 downto 0);

      -- SDRAM side.
      sdCke_o : out std_logic; -- Clock-enable to SDRAM.
      sdCe_bo : out std_logic; -- Chip-select to SDRAM.
      sdRas_bo : out std_logic; -- SDRAM row address strobe.
      sdCas_bo : out std_logic; -- SDRAM column address strobe.
      sdWe_bo : out std_logic; -- SDRAM write enable.
      sdBs_o : out std_logic_vector(1 downto 0); -- SDRAM bank address.
      sdAddr_o : out std_logic_vector(SADDR_WIDTH_G-1 downto 0); -- SDRAM row/column address.
      sdData_io : inout std_logic_vector(DATA_WIDTH_G-1 downto 0); -- Data to/from SDRAM.
      sdDqmh_o : out std_logic; -- Enable upper-byte of SDRAM databus if true.
      sdDqml_o : out std_logic -- Enable lower-byte of SDRAM databus if true.
      );
  end component;

end package;





--*********************************************************************
-- Dual-port interface to SDRAM controller.
--*********************************************************************

library IEEE, UNISIM;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
use WORK.CommonPckg.all;

entity DualPort is
  generic(
    PIPE_EN_G : boolean := false; -- enable pipelined read operations.
    PORT_TIME_SLOTS_G : std_logic_vector(15 downto 0) := "1111000011110000";
    DATA_WIDTH_G : natural := 16; -- host & SDRAM data width.
    HADDR_WIDTH_G : natural := 23 -- host-side address width.
    );
  port(
    clk_i : in std_logic; -- master clock.

    -- Host-side port 0.
    rst0_i : in std_logic := NO; -- reset.
    rd0_i : in std_logic := NO; -- initiate read operation.
    wr0_i : in std_logic := NO; -- initiate write operation.
    earlyOpBegun0_o : out std_logic; -- read/write op has begun (async).
    opBegun0_o : out std_logic := NO; -- read/write op has begun (clocked).
    rdPending0_o : out std_logic; -- true if read operation(s) are still in the pipeline.
    done0_o : out std_logic; -- read or write operation is done_i.
    rdDone0_o : out std_logic; -- read operation is done_i and data is available.
    addr0_i : in std_logic_vector(HADDR_WIDTH_G-1 downto 0) := (others => ZERO); -- address from host to SDRAM.
    data0_i : in std_logic_vector(DATA_WIDTH_G-1 downto 0) := (others => ZERO); -- data from host to SDRAM.
    data0_o : out std_logic_vector(DATA_WIDTH_G-1 downto 0) := (others => ZERO); -- data from SDRAM to host.
    status0_o : out std_logic_vector(3 downto 0); -- diagnostic status of the SDRAM controller FSM .

    -- Host-side port 1.
    rst1_i : in std_logic := NO;
    rd1_i : in std_logic := NO;
    wr1_i : in std_logic := NO;
    earlyOpBegun1_o : out std_logic;
    opBegun1_o : out std_logic := NO;
    rdPending1_o : out std_logic;
    done1_o : out std_logic;
    rdDone1_o : out std_logic;
    addr1_i : in std_logic_vector(HADDR_WIDTH_G-1 downto 0) := (others => ZERO);
    data1_i : in std_logic_vector(DATA_WIDTH_G-1 downto 0) := (others => ZERO);
    data1_o : out std_logic_vector(DATA_WIDTH_G-1 downto 0) := (others => ZERO);
    status1_o : out std_logic_vector(3 downto 0);

    -- SDRAM controller host-side port.
    rst_o : out std_logic;
    rd_o : out std_logic;
    wr_o : out std_logic;
    earlyOpBegun_i : in std_logic;
    opBegun_i : in std_logic;
    rdPending_i : in std_logic;
    done_i : in std_logic;
    rdDone_i : in std_logic;
    addr_o : out std_logic_vector(HADDR_WIDTH_G-1 downto 0);
    data_o : out std_logic_vector(DATA_WIDTH_G-1 downto 0);
    data_i : in std_logic_vector(DATA_WIDTH_G-1 downto 0);
    status_i : in std_logic_vector(3 downto 0)
    );
end entity;



architecture arch of DualPort is
  -- The door signal controls whether the read/write signal from the active port
  -- is allowed through to the read/write inputs of the SDRAM controller.
  type DoorStateType is (OPENED_C, CLOSED_C);
  signal door_r, door_x : DoorStateType := CLOSED_C;

  -- The port signal indicates which port is connected to the SDRAM controller.
  type PortStateType is (PORT0_C, PORT1_C);
  signal port_r, port_x : PortStateType := PORT0_C;

  signal switch_s : std_logic; -- indicates that the active port should be switched.
  signal inProgress_s : std_logic; -- the active port has a read/write op in-progress.
  signal rd_s : std_logic; -- read signal to the SDRAM controller (internal copy).
  signal wr_s : std_logic; -- write signal to the SDRAM controller (internal copy).
  signal earlyOpBegun0_s, earlyOpBegun1_s : std_logic; -- (internal copies).
  signal slot_r, slot_x : std_logic_vector(PORT_TIME_SLOTS_G'range) := PORT_TIME_SLOTS_G; -- time-slot allocation shift-register.
begin

  --*********************************************************************
  -- multiplex the SDRAM controller port signals to/from the dual host-side ports
  --*********************************************************************

  -- send the SDRAM controller the address and data from the currently active port.
  addr_o <= addr0_i when port_r = PORT0_C else addr1_i;
  data_o <= data0_i when port_r = PORT0_C else data1_i;

  -- both ports get the data from the SDRAM but only the active port will use it.
  data0_o <= data_i;
  data1_o <= data_i;

  -- send the SDRAM controller status to the active port and give the inactive port an inactive status code.
  status0_o <= status_i when port_r = PORT0_C else "1111";
  status1_o <= status_i when port_r = PORT1_C else "1111";

  -- either port can reset the SDRAM controller.
  rst_o <= rst0_i or rst1_i;

  -- apply the read signal from the active port to the SDRAM controller only if the door is open..
  rd_s <= rd0_i when (port_r = PORT0_C) and (door_r = OPENED_C) else
          rd1_i when (port_r = PORT1_C) and (door_r = OPENED_C) else
          NO;
  rd_o <= rd_s;

  -- apply the write signal from the active port to the SDRAM controller only if the door is open..
  wr_s <= wr0_i when (port_r = PORT0_C) and (door_r = OPENED_C) else
          wr1_i when (port_r = PORT1_C) and (door_r = OPENED_C) else
          NO;
  wr_o <= wr_s;

  -- send the status signals for various SDRAM controller operations back to the active port.
  earlyOpBegun0_s <= earlyOpBegun_i when port_r = PORT0_C else NO;
  earlyOpBegun0_o <= earlyOpBegun0_s;
  earlyOpBegun1_s <= earlyOpBegun_i when port_r = PORT1_C else NO;
  earlyOpBegun1_o <= earlyOpBegun1_s;
  rdPending0_o <= rdPending_i when port_r = PORT0_C else NO;
  rdPending1_o <= rdPending_i when port_r = PORT1_C else NO;
  done0_o <= done_i when port_r = PORT0_C else NO;
  done1_o <= done_i when port_r = PORT1_C else NO;
  rdDone0_o <= rdDone_i when port_r = PORT0_C else NO;
  rdDone1_o <= rdDone_i when port_r = PORT1_C else NO;

  --*********************************************************************
  -- Indicate when the active port needs to be switched. A switch occurs if
  -- a read or write operation is requested on the port that is not currently active and:
  -- 1) no R/W operation is being performed on the active port or
  -- 2) a R/W operation is in progress on the active port, but the time-slot allocation
  -- register is giving precedence to the inactive port. (The R/W operation on the
  -- active port will be completed before the switch is made.)
  -- This rule keeps the active port from hogging all the bandwidth.
  --*********************************************************************
  switch_s <= (rd0_i or wr0_i) when (port_r = PORT1_C) and (((rd1_i = NO) and (wr1_i = NO)) or (slot_r(0) = '0')) else
              (rd1_i or wr1_i) when (port_r = PORT0_C) and (((rd0_i = NO) and (wr0_i = NO)) or (slot_r(0) = '1')) else
              NO;

  --*********************************************************************
  -- Indicate when an operation on the active port is in-progress and
  -- can't be interrupted by a switch to the other port. (Only read operations
  -- are looked at since write operations always complete in one cycle once they
  -- are initiated.)
  --*********************************************************************
  inProgress_s <= rdPending_i or (rd_s and earlyOpBegun_i);

  --*********************************************************************
  -- Update the time-slot allocation shift-register. The port with priority is indicated by the
  -- least-significant bit of the register. The register is rotated right if:
  -- 1) the current R/W operation has started, and
  -- 2) both ports are requesting R/W operations (indicating contention), and
  -- 3) the currently active port matches the port that currently has priority.
  -- Under these conditions, the current time slot port allocation has been used so
  -- the shift register is rotated right to bring the next port time-slot allocation
  -- bit into play.
  --*********************************************************************
  slot_x <= slot_r(0) & slot_r(slot_r'high downto 1) when (earlyOpBegun_i = YES) and
            (((rd0_i = YES) or (wr0_i = YES)) and ((rd1_i = YES) or (wr1_i = YES))) and
            (((port_r = PORT0_C) and (slot_r(0) = '0')) or ((port_r = PORT1_C) and (slot_r(0) = '1')))
            else slot_r;

  --*********************************************************************
  -- Determine which port will be active on the next cycle. The active port is switched if:
  -- 1) there are no pending operations in progress, and
  -- 2) the port switch indicator is active.
  --*********************************************************************
  port_process : process(port_r, inProgress_s, switch_s, done_i)
  begin
    port_x <= port_r; -- by default, the active port is not changed
    case port_r is
      when PORT0_C =>
        if (inProgress_s = NO) and (switch_s = YES) then
          port_x <= PORT1_C;
        end if;
      when PORT1_C =>
        if (inProgress_s = NO) and (switch_s = YES) then
          port_x <= PORT0_C;
        end if;
      when others =>
        port_x <= port_r;
    end case;
  end process port_process;

  --*********************************************************************
  -- Determine if the door is open for the active port to initiate new R/W operations to
  -- the SDRAM controller. If the door is open and R/W operations are in progress but
  -- a switch to the other port is indicated, then the door is closed to prevent any
  -- further R/W operations from the active port. The door is re-opened once all
  -- in-progress operations are completed, at which time the switch to the other port
  -- is also completed so it can issue its own R/W commands.
  --*********************************************************************
  door_process : process(door_r, inProgress_s, switch_s)
  begin
    door_x <= door_r; -- by default, the door remains as it is.
    case door_r is
      when OPENED_C =>
        if (inProgress_s = YES) and (switch_s = YES) then
          door_x <= CLOSED_C;
        end if;
      when CLOSED_C =>
        if inProgress_s = NO then
          door_x <= OPENED_C;
        end if;
      when others =>
        door_x <= door_r;
    end case;
  end process door_process;

  --*********************************************************************
  -- update registers on the appropriate clock edge.
  --*********************************************************************
  update : process(rst0_i, rst1_i, clk_i)
  begin
    if (rst0_i = YES) or (rst1_i = YES) then
      -- asynchronous reset.
      door_r <= CLOSED_C;
      port_r <= PORT0_C;
      slot_r <= PORT_TIME_SLOTS_G;
      opBegun0_o <= NO;
      opBegun1_o <= NO;
    elsif rising_edge(clk_i) then
      door_r <= door_x;
      port_r <= port_x;
      slot_r <= slot_x;
      --*********************************************************************
      -- opBegun signals are cycle-delayed versions of earlyOpBegun signals.
      -- We can't use the actual opBegun signal from the SDRAM controller
      -- because it would be turned off if the active port was switched on the
      -- cycle immediately after earlyOpBegun went active.
      --*********************************************************************
      opBegun0_o <= earlyOpBegun0_s;
      opBegun1_o <= earlyOpBegun1_s;
    end if;
  end process update;

end architecture;




--*********************************************************************
-- Dual-port interface + SDRAM controller.
--*********************************************************************

library IEEE, UNISIM;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
use WORK.CommonPckg.all;
use work.SdramCntlPckg.all;

entity DualPortSdram is
  generic(
    PORT_TIME_SLOTS_G : std_logic_vector(15 downto 0) := "1111000011110000";
    FREQ_G : real := 100.0; -- Operating frequency in MHz.
    IN_PHASE_G : boolean := true; -- SDRAM and controller work on same or opposite clock edge.
    PIPE_EN_G : boolean := false; -- If true, enable pipelined read operations.
    MAX_NOP_G : natural := 10000; -- Number of NOPs before entering self-refresh.
    ENABLE_REFRESH_G : boolean := true; -- If true, row refreshes are automatically inserted.
    MULTIPLE_ACTIVE_ROWS_G : boolean := false; -- If true, allow an active row in each bank.
    DATA_WIDTH_G : natural := 16; -- Host & SDRAM data width.
    -- Parameters for Winbond W9812G6JH-75 (all times are in nanoseconds).
    NROWS_G : natural := 4096; -- Number of rows in SDRAM array.
    NCOLS_G : natural := 512; -- Number of columns in SDRAM array.
    HADDR_WIDTH_G : natural := 23; -- Host-side address width.
    SADDR_WIDTH_G : natural := 12; -- SDRAM-side address width.
    T_INIT_G : real := 200_000.0; -- min initialization interval (ns).
    T_RAS_G : real := 45.0; -- min interval between active to precharge commands (ns).
    T_RCD_G : real := 20.0; -- min interval between active and R/W commands (ns).
    T_REF_G : real := 64_000_000.0; -- maximum refresh interval (ns).
    T_RFC_G : real := 65.0; -- duration of refresh operation (ns).
    T_RP_G : real := 20.0; -- min precharge command duration (ns).
    T_XSR_G : real := 75.0 -- exit self-refresh time (ns).
    );
  port(
    clk_i : in std_logic; -- master clock.

    -- Host-side port 0.
    rst0_i : in std_logic := NO; -- reset.
    rd0_i : in std_logic := NO; -- initiate read operation.
    wr0_i : in std_logic := NO; -- initiate write operation.
    earlyOpBegun0_o : out std_logic; -- read/write op has begun (async).
    opBegun0_o : out std_logic := NO; -- read/write op has begun (clocked).
    rdPending0_o : out std_logic; -- true if read operation(s) are still in the pipeline.
    done0_o : out std_logic; -- read or write operation is done_i.
    rdDone0_o : out std_logic; -- read operation is done_i and data is available.
    addr0_i : in std_logic_vector(HADDR_WIDTH_G-1 downto 0) := (others => ZERO); -- address from host to SDRAM.
    data0_i : in std_logic_vector(DATA_WIDTH_G-1 downto 0) := (others => ZERO); -- data from host to SDRAM.
    data0_o : out std_logic_vector(DATA_WIDTH_G-1 downto 0) := (others => ZERO); -- data from SDRAM to host.
    status0_o : out std_logic_vector(3 downto 0); -- diagnostic status of the SDRAM controller FSM .

    -- Host-side port 1.
    rst1_i : in std_logic := NO;
    rd1_i : in std_logic := NO;
    wr1_i : in std_logic := NO;
    earlyOpBegun1_o : out std_logic;
    opBegun1_o : out std_logic := NO;
    rdPending1_o : out std_logic;
    done1_o : out std_logic;
    rdDone1_o : out std_logic;
    addr1_i : in std_logic_vector(HADDR_WIDTH_G-1 downto 0) := (others => ZERO);
    data1_i : in std_logic_vector(DATA_WIDTH_G-1 downto 0) := (others => ZERO);
    data1_o : out std_logic_vector(DATA_WIDTH_G-1 downto 0) := (others => ZERO);
    status1_o : out std_logic_vector(3 downto 0);

    -- SDRAM side.
    sdCke_o : out std_logic; -- Clock-enable to SDRAM.
    sdCe_bo : out std_logic; -- Chip-select to SDRAM.
    sdRas_bo : out std_logic; -- SDRAM row address strobe.
    sdCas_bo : out std_logic; -- SDRAM column address strobe.
    sdWe_bo : out std_logic; -- SDRAM write enable.
    sdBs_o : out std_logic_vector(1 downto 0); -- SDRAM bank address.
    sdAddr_o : out std_logic_vector(SADDR_WIDTH_G-1 downto 0); -- SDRAM row/column address.
    sdData_io : inout std_logic_vector(DATA_WIDTH_G-1 downto 0); -- Data to/from SDRAM.
    sdDqmh_o : out std_logic; -- Enable upper-byte of SDRAM databus if true.
    sdDqml_o : out std_logic -- Enable lower-byte of SDRAM databus if true.
    );
end entity;


architecture arch of DualPortSdram is
  signal rst_s : std_logic;
  signal rd_s : std_logic;
  signal wr_s : std_logic;
  signal earlyOpBegun_s : std_logic;
  signal opBegun_s : std_logic;
  signal rdPending_s : std_logic;
  signal done_s : std_logic;
  signal rdDone_s : std_logic;
  signal addr_s : std_logic_vector(addr0_i'range);
  signal dataFromHost_s : std_logic_vector(sdData_io'range);
  signal dataToHost_s : std_logic_vector(sdData_io'range);
  signal status_s : std_logic_vector(status0_o'range);
begin

  u0 : DualPort
    generic map(
      PIPE_EN_G => PIPE_EN_G,
      PORT_TIME_SLOTS_G => PORT_TIME_SLOTS_G,
      DATA_WIDTH_G => DATA_WIDTH_G,
      HADDR_WIDTH_G => HADDR_WIDTH_G
      )
    port map(
      clk_i => clk_i,

      -- Host-side port 0.
      rst0_i => rst0_i,
      rd0_i => rd0_i,
      wr0_i => wr0_i,
      earlyOpBegun0_o => earlyOpBegun0_o,
      opBegun0_o => opBegun0_o,
      rdPending0_o => rdPending0_o,
      done0_o => done0_o,
      rdDone0_o => rdDone0_o,
      addr0_i => addr0_i,
      data0_i => data0_i,
      data0_o => data0_o,
      status0_o => status0_o,

      -- Host-side port 1.
      rst1_i => rst1_i,
      rd1_i => rd1_i,
      wr1_i => wr1_i,
      earlyOpBegun1_o => earlyOpBegun1_o,
      opBegun1_o => opBegun1_o,
      rdPending1_o => rdPending1_o,
      done1_o => done1_o,
      rdDone1_o => rdDone1_o,
      addr1_i => addr1_i,
      data1_i => data1_i,
      data1_o => data1_o,
      status1_o => status1_o,

      -- SDRAM controller host-side port.
      rst_o => rst_s,
      rd_o => rd_s,
      wr_o => wr_s,
      earlyOpBegun_i => earlyOpBegun_s,
      opBegun_i => opBegun_s,
      rdPending_i => rdPending_s,
      done_i => done_s,
      rdDone_i => rdDone_s,
      addr_o => addr_s,
      data_o => dataFromHost_s,
      data_i => dataToHost_s,
      status_i => status_s
      );

  u1 : SdramCntl
    generic map(
      FREQ_G => FREQ_G,
      IN_PHASE_G => IN_PHASE_G,
      PIPE_EN_G => PIPE_EN_G,
      MAX_NOP_G => MAX_NOP_G,
      DATA_WIDTH_G => DATA_WIDTH_G,
      NROWS_G => NROWS_G,
      NCOLS_G => NCOLS_G,
      HADDR_WIDTH_G => HADDR_WIDTH_G,
      SADDR_WIDTH_G => SADDR_WIDTH_G
      )
    port map(
      clk_i => clk_i, -- master clock from external clock source (unbuffered)
      lock_i => YES, -- no DLLs, so frequency is always locked
      rst_i => rst_s, -- reset
      rd_i => rd_s, -- host-side SDRAM read control from memory tester
      wr_i => wr_s, -- host-side SDRAM write control from memory tester
      earlyOpBegun_o => earlyOpBegun_s, -- early indicator that memory operation has begun
      opBegun_o => opBegun_s, -- indicates memory read/write has begun
      rdPending_o => rdPending_s, -- read operation to SDRAM is in progress_o
      done_o => done_s, -- SDRAM memory read/write done indicator
      rdDone_o => rdDone_s, -- indicates SDRAM memory read operation is done
      addr_i => addr_s, -- host-side address from memory tester to SDRAM
      data_i => dataFromHost_s, -- test data pattern from memory tester to SDRAM
      data_o => dataToHost_s, -- SDRAM data output to memory tester
      status_o => status_s, -- SDRAM controller state (for diagnostics)
      sdCke_o => sdCke_o, -- Clock-enable to SDRAM.
      sdCe_bo => sdCe_bo, -- Chip-select to SDRAM.
      sdRas_bo => sdRas_bo, -- SDRAM RAS
      sdCas_bo => sdCas_bo, -- SDRAM CAS
      sdWe_bo => sdWe_bo, -- SDRAM write-enable
      sdBs_o => sdBs_o, -- SDRAM bank address
      sdAddr_o => sdAddr_o, -- SDRAM address
      sdData_io => sdData_io, -- data to/from SDRAM
      sdDqmh_o => sdDqmh_o, -- Enable upper-byte of SDRAM databus if true.
      sdDqml_o => sdDqml_o -- Enable lower-byte of SDRAM databus if true.
      );

end architecture;
