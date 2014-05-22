--===========================================================================--
--                                                                           --
--      ptm6840.vhd - Synthesizable Programmable Timer Module                --
--                                                                           --
--===========================================================================--
--
--  File name      : ptm6840.vhd
--
--  Purpose        : Programmable Timer Module for SystemXX
-- 
--  Dependencies   : ieee.std_logic_1164
--                   ieee.std_logic_unsigned
--                   unisim.vcomponents
--
--  Author         : John E. Kent
--
--  Email          : dilbert57@opencores.org      
--
--  Web            : http://opencores.org/project,system09
--
--
--  RS2 RS1 RS0 RW=0                          RW=1
--  === === === ============================= ========================
--   0   0   0  CR20 = 0 Write Control Reg #3 No Operation
--              CR20 = 1 Write Control Reg #1
--   0   0   1  Write Control Reg 2           Read Status Register
--   0   1   0  Write MSB Buffer Register     Read Timer #1 Counter
--   0   1   1  Write Timer #1 Latches        Read LSB Buffer Register
--   1   0   0  Write MSB Buffer Register     Read Timer #2 Counter
--   1   0   1  Write Timer #2 Latches        Read LSB Buffer Register
--   1   1   0  Write MSB Buffer Register     Read Timer #3 Counter
--   1   1   1  Write Timer #3 Latches        Read LSB Buffer Register
--
--  Control Register CRXX
--  Bit  =0                             =1
--  ==== ============================== ===============================
--  CRX7 TX output masked on output Ox  TX Output enabled on output OX
--  CRX6 Interrupt Flag masked on IRQ*  Interrupt Flag enabled to IRQ*
--  CRX5
--  CRX4
--  CRX3
--  CRX2 TX normal (16 bit) counting    TX dual 8 bit counting mode
--  CRX1 TX uses external clock on CX   TX uses enable clock
--  CR10 All Timers allowed to operate  All timers held in preset state
--  CR20 CR3X May be written            CRX1 may be written
--  CR30 T3 Clock is not prescaled      T3 clock is prescaled by 8
--
--  CRX5 CRX4 CRX3 Operating Mode
--  ==== ==== ==== ====================================================
--   0    0    0   Continuous: Gate -\_ or Write Latches or Reset initializes
--   0    0    1   Freq Compare: Interrupt if Gate \_/-\ is < Counter Timeout
--   0    1    0   Continuous: Gate -\_ or Reset causes counter initialization
--   0    1    1   Pulse Width Compare: Interrupt if Gate \_/ is < Counter Timeout
--   1    0    0   Single Shot: Gate -\_ or Write latches or reset initializes counter
--   1    0    1   Freq Compare: Interrupt if Gate \_/-\ is > Counter Timeout
--   1    1    0   Single Shot: Gate -\_ or or reset initializes counter
--   1    1    1   Pulse Width Compare: Interrupt if Gate \_/ is > Counter Timeout
--
--   0    X    0   Continuous
--   1    X    0   Single Shot
--   X    0    0   Gate_n -\_   or Write latches
--   X    0    1   Gate_n \_/-\ Frequency Comparison
--   X    1    0   Gate_n  -\_  or Reset initializes counter
--   X    1    1   Gate_n \_/   Pulse Width Comparison
--
--  G_N-\_ negative transition of gate input
--  G_N_/- positive transition of gate input
--  W      write timer latch command
--  R      reset timer (CR10=1 or rst=1)
--  CE     Counter enable flip flop
--  CI     Counter Initialization
--  TO     time out (all zero condition)
--  I      interrupt for a given timer
--  assume G_N and C_N are synchonized to the cpu clock
--
--  Continuous mode
--  ===============
--  (CRX7=1, CRX5=0, CRX3=0)
--  IX=1 when (TO=1) when M = L = 0 or N = 0
--  CRX4=0 
--  CI=1 when G_N-\_ or (R=1) or (W=1)
--  CRX4=1 
--  CI=1 when G_N-\_ or (R=1)
--
--  16 bit mode (CRX2=0)
--  OX = low  for (N+1)*(T)
--  TO at (N+1)*(T)
--  OX = high for (N+1)*(T)
--  TO at (N+1)*(T)
--  
--  Dual 8 Bit Mode (CRX2=1)
--  OX = low  for ((L)*(M+1)+1)*(T)
--  OX = high for (L)*(T)
--  TO at (L+1)*(M+1)*(T) => OX = low
--  OX = low  for ((L)*(M+1)+1)*(T)
--  OX = high for (L)*(T)
--  TO at (L+1)*(M+1)*(T) => OX = low
-- 
--  eg. If M = 3 and L = 4 and T=enables
--  OX = low  for 3*(4+1)+1 = 16 enables
--  OX = high for 4 enables
--
--  Single shot mode
--  ================
--  (CRX7=1, CRX5=1, CRX3=0)
--  CRX4=0 
--  CI=1 when G_N-\_ or (R=1) or (W=1)
--  CRX4=1 
--  CI=1 when G_N-\_ or (R=1)
--
--  16 bit mode (CRX2=0)
--  OX = low  for     (T)
--  OX = high for (N)*(T)
--  TO at (N+1)*(T) => OX = low
--  TO at (N+1)*(T) => OX = low
--
--  Dual 8 bit mode (CRX2=1)
--  OX = low  for ((L)*(M+1)+1)*(T)
--  OX = high for  (L)         *(T)
--  TO at (L+1)*(M+1)*(T) => OX = low
--  TO at (L+1)*(M+1)*(T) => OX = low
--
--  Frequency Comparison mode
--  =========================
--  (CRX4=0, CRX3=1)
--  CE=1 when G_N-\_ and (I=0) and (W=0) and (R=0)
--  CE=0 when            (I=1) or  (W=1) or  (R=1)
--
--  CRX5=0 
--  CI=1 when G_N-\_ and (I=0) and ((CE=0) or (TO=1)) or (R=0)
--  IX=1 when G_N-\_ before TO
--
--  CRX5=1 
--  CI=1 when G_N-\_ and (I=0) or  (R=0)
--  IX=1 when G_N-\_ after  TO
--
--  Pulse Width Comparison Mode
--  ===========================
--  (CRX4=1, CRX3=1)
--  CI=1 when G_N-\_ and (I=0) or  (R=0)
--  CE=1 when G_N-\_ and (I=0) and (W=0) and (R=0)
--  CE=0 when (G_N=1)  or  (I=1) or  (W=1) or  (R=1)
--           
--  CRX5=0
--  IX=1 when G_N_/- before TO
--
--  CRX5=1   
--  IX=1 when G_N_/- after TO
--
--  Status Register
--  ===============
--  SR7 INT = (I1.CR16) + (I2.CR26) + (I3.CR36)
--  SR6 0
--  SR5 0
--  SR4 0
--  SR3 0
--  SR2 I3
--  SR1 I2
--  SR0 I1
--
--  Interrupts are reset by a reset condition (R) RST = 1, CR10 = 1 or
--  Read Status Register (RS) followed by a Read Timer (RTX) Command
--  provided the interrupt (IX) is set when the Status Register is read
--  and the timer (TX) corresponding to the particular interrupt is read
--  or a write timer register (W) or a counter initialization (CI).
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
-- Version  Author        Date           Description
-- 0.1      John E. Kent  1 May 2011     Initial version
-- 
--===========================================================================
--

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_unsigned.all;
--library unisim;
--  use unisim.vcomponents.all;

entity ptm6840 is
  port (	
    clk       : in    std_logic;
    rst       : in    std_logic;
    cs        : in    std_logic;
    rw        : in    std_logic;
    addr      : in    std_logic_vector(1 downto 0);
    data_in   : in    std_logic_vector(7 downto 0);
    data_out  : out   std_logic_vector(7 downto 0);
    irq       : out   std_logic;
    tclk_n    : in    std_logic_vector(2 downto 0);  -- Timer Clock Inputs
    tgate_n   : in    std_logic_vector(2 downto 0);  -- Timer Gate inputs
    tout      : out   std_logic_vector(2 downto 0)   -- Timer Outputs
  );
end;

architecture rtl of ptm6840 is

signal porta_ddr : std_logic_vector(7 downto 0);
signal portb_ddr : std_logic_vector(7 downto 0);
signal porta_data : std_logic_vector(7 downto 0);
signal portb_data : std_logic_vector(7 downto 0);

begin

end rtl;
