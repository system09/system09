--===========================================================================--
--                                                                           --
--                Synthesizable Hand-Shake buffer                            --
--                                                                           --
--===========================================================================--
--
--  File name      : handshake_buff.vhd
--
--  Entity name    : handshake_buff
--
--  Purpose        : Implements an input output buffer
--                   with 2 wire handskake
--
--  Dependencies   : ieee.std_logic_1164
--                   ieee.numeric_std
--                   ieee.std_logic_unsigned
--                   unisim.vcomponents
--
--  Author         : John E. Kent
--
--  Email          : dilbert57@opencores.org      
--
--  Web            : http://opencores.org/project,system09
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
-- Version  Author        Date               Changes
--
-- 0.1      John Kent     3 July     2010    Initial version
-------------------------------------------------------------------------------
library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;
  use ieee.std_logic_unsigned.all;
library unisim;
  use unisim.vcomponents.all;

-------------------------------------------------------------------------------
-- Entity for the handshake buffer
-------------------------------------------------------------------------------
entity handshake_buff is
  generic (
      ADDR_WIDTH : integer := 16;
      DATA_WIDTH : integer := 8
  );
  port (
      --
      -- System inputs
      --
		CLK_I   :	 in  std_logic;					-- System Clock
      RST_I   :	 in  std_logic;					-- Reset input

      --
      -- Slave Input port
      --
      STB_I   :    in  std_logic;               -- Inpute strobe
      CYC_I   :    in  std_logic;               -- Cycle in progress
      SEL_I   :    in  std_logic_vector( 0 downto 0);
      ADR_I   :    in  std_logic_vector( ADDR_WIDTH-1 downto 0);
      WE_I    :    in  std_logic;               -- Write input
      DAT_I   :    in  std_logic_vector( DATA_WIDTH-1 downto 0);
      ACK_O   :    out std_logic;               -- Handshake acknowledge

      --
      -- Master Output port
      --
		STB_O   :	 out std_logic;					-- Stobe output
		CYC_O   :	 out std_logic;					-- CYC in progress
		SEL_O   :    out std_logic_vector( 0 downto 0);
		ADR_O   :    out std_logic_vector( ADDR_WIDTH-1 downto 0);
		WE_O    :    out std_logic;					-- Memory WRITE in progress
      DAT_O   :    out std_logic_vector( DATA_WIDTH-1 downto 0);
		ACK_I   :    in  std_logic;               -- Not HOLD

  );
end handshake_buff;  
--================== End of entity ==============================--

    req_in   : in  std_Logic;                     -- receive request
    ack_out  : out std_logic;                     -- receive acknowledge
    req_out  : out std_logic;                     -- transmit request
    ack_in   : in  std_logic;                     -- transmit Acknowledge

-------------------------------------------------------------------------------
-- Architecture for  ACIA_TX
-------------------------------------------------------------------------------

architecture rtl of ACIA_TX is

