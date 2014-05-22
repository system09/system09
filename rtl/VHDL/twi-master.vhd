--===========================================================================--
--                                                                           --
--                  Synthesizable VHDL Two Wire Interface                    --
--                                                                           --
--===========================================================================--
--
--  File name      : twi_master.vhd
--
--  Entity name    : twi_master
--
--  Purpose        : Implements a two wire interface
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
--  Registers      :
--
--  IO address + 0 Read - Status Register
--    Bit[7] - TXIRQ - Receive Interrupt Request
--    Bit[6] - TXIRQ - Transmit Interrupt Request
--    Bit[5] - ACKE - Acknowledge Error
--    Bit[1] - TXRDY - Transmit Ready (byte transmitted)
--    Bit[0] - RXRDY - Receive Ready (byte received)
--
--  IO address + 0 Write - Control Register
--
--    Bit[7] - RXIE
--     0       - Rx Interrupt disabled
--     1       - Rx Interrupt enabled
--    Bit[6] - TXIE
--     0       - Tx Interrupt disabled
--     1       - Tx Interrupt enabled
--
--    SCL frequency = CPU Clock Frequency / ( 16 + 2(TWBR) . 4^TWPS)
--
--    Bits[5..4] - TWPS
--     0 0     - Prescale by  1
--     0 1     - Prescale by  4
--     1 0     - Prescale by 16
--     1 1     - Prescale by 64
--
--    Bits[3..0] - TWBR
--     0 0 0 0   - Baud Clk divide by  1
--     0 0 0 1   - Baud Clk divide by  2
--     0 0 1 0   - Baud Clk divide by  3
--     0 0 1 1   - Baud Clk divide by  4
--     0 1 0 0   - Baud Clk divide by  5
--     0 1 0 1   - Baud Clk divide by  6
--     0 1 1 0   - Baud Clk divide by  7
--     0 1 1 1   - Baud Clk divide by  8
--     1 0 0 0   - Baud Clk divide by  9
--     1 0 0 1   - Baud Clk divide by 10
--     1 0 1 0   - Baud Clk divide by 11
--     1 0 1 1   - Baud Clk divide by 12
--     1 1 0 0   - Baud Clk divide by 13
--     1 1 0 1   - Baud Clk divide by 14
--     1 1 1 0   - Baud Clk divide by 15
--     1 1 1 1   - Baud Clk divide by 16
--
--  IO address + 1 Read - Receive Data Register
--
--     Read when Receive Data Ready bit set
--     Read resets Receive Data Ready bit 
--
--  IO address + 1 Write - Transmit Data Register
--
--     Write when Transmit Buffer Empty bit set
--     Write resets Transmit Buffer Empty Bit
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
-- Version Author        Date         Changes
--
-- 0.1     John Kent     2010-05-04   New model
--
--         dilbert57@opencores.org
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library unisim;
      use unisim.vcomponents.all;

-----------------------------------------------------------------------
-- Entity for TWI                                                    --
-----------------------------------------------------------------------

entity twi_master is
  generic (
    CLK_FREQ : integer := 25_000_000;
  );

  port (
    --
    -- CPU signals
    --
    clk      : in  std_logic;                     -- System Clock
    rst      : in  std_logic;                     -- Reset input (active high)
    cs       : in  std_logic;                     -- Chip Select
    rw       : in  std_logic;                     -- Read / Not Write
    irq      : out std_logic;                     -- Interrupt
    addr     : in  std_logic;                     -- Register Select
    data_in  : in  std_logic_vector(7 downto 0);  -- Data Bus In 
    data_out : out std_logic_vector(7 downto 0);  -- Data Bus Out

    -- I2C Signals
    --
    scl     : inout std_logic;                    -- serial clock
    sda     : inout std_logic                     -- serial data
    );
end twi_master;  

-------------------------------------------------------------------------------
-- Architecture for Two Wire Interface registers
-------------------------------------------------------------------------------

architecture rtl of twi_master is

  -----------------------------------------------------------------------------
  -- Signals
  -----------------------------------------------------------------------------

  ----------------------------------------------------------------------
  --  Status Register: twi_status_reg 
  ----------------------------------------------------------------------
  --
  -- IO address + 0 Read
  --
  --+-------+-------+-------+-------+-------+-------+-------+-------+
  --| RXIRQ | TXIRQ | ACKE  |       |       |       | TXRDY | RXRDY |
  --+-------+-------+-------+-------+-------+-------+-------+-------+
  -- RXIRQ - Bit[7] - Receive Interrupt Request
  -- TXIRQ - Bit[6] - Transmit Interrupt Request
  -- ACKE  - Bit[5] - Acknowledge Error
  -- TXRDY - Bit[1] - Transmit Ready (byte transmitted)
  -- RXRDY - Bit[0] - Receive Ready (byte received)
  -- 
  signal twi_stat_reg : std_logic_vector(7 downto 0) := (others => '0');

  ----------------------------------------------------------------------
  --  Control Register: twi_control_reg
  ----------------------------------------------------------------------
  --
  -- IO address + 0 Write
  --
  --+--------+-------+--------+--------+--------+--------+--------+--------+
  --|  RXIE  | TXIE  | TWPS(1)| TWPS(0)| TWBR(3)| TWBR(2)| TWBR(1)| TWBR(0)|
  --+--------+-------+--------+--------+--------+--------+--------+--------+
  -- RXIE - Bit[7]
  -- 0       - Rx Interrupt disabled
  -- 1       - Rx Interrupt enabled
  -- TXIE - Bit[6]
  -- 0       - Tx Interrupt disabled
  -- 1       - Tx Interrupt enabled
  --
  -- SCL frequency = CPU Clock Frequency / ( 16 + 2(TWBR) . 4^TWPS)
  --
  -- TWPS - Bits[5..4]
  -- 0 0     - Prescale by  1
  -- 0 1     - Prescale by  4
  -- 1 0     - Prescale by 16
  -- 1 1     - Prescale by 64
  --
  -- TWBR - Bits[3..0]
  -- 0 0 0 0   - Baud Clk divide by  1
  -- 0 0 0 1   - Baud Clk divide by  2
  -- 0 0 1 0   - Baud Clk divide by  3
  -- 0 0 1 1   - Baud Clk divide by  4
  -- 0 1 0 0   - Baud Clk divide by  5
  -- 0 1 0 1   - Baud Clk divide by  6
  -- 0 1 1 0   - Baud Clk divide by  7
  -- 0 1 1 1   - Baud Clk divide by  8
  -- 1 0 0 0   - Baud Clk divide by  9
  -- 1 0 0 1   - Baud Clk divide by 10
  -- 1 0 1 0   - Baud Clk divide by 11
  -- 1 0 1 1   - Baud Clk divide by 12
  -- 1 1 0 0   - Baud Clk divide by 13
  -- 1 1 0 1   - Baud Clk divide by 14
  -- 1 1 1 0   - Baud Clk divide by 15
  -- 1 1 1 1   - Baud Clk divide by 16
  

  signal twi_ctrl_reg : std_logic_vector(7 downto 0) := (others => '0'); -- control register

  ----------------------------------------------------------------------
  -- Receive Register
  ----------------------------------------------------------------------
  --
  -- IO address + 1     Read
  --
  signal twi_rx_reg : std_logic_vector(7 downto 0) := (others => '0');

  ----------------------------------------------------------------------
  -- Transmit Register
  ----------------------------------------------------------------------
  --
  -- IO address + 1     Write
  --
  signal twi_tx_reg : std_logic_vector(7 downto 0) := (others => '0');
  
  
  signal TxDat    : std_logic := '1';   -- Transmit data bit
  signal TxRdy    : std_logic := '0';   -- Transmit buffer empty
  signal RxRdy    : std_logic := '0';   -- Receive Data ready
   --
  signal TxIE     : std_logic := '0';   -- Transmit interrupt enable
  signal RxIE     : std_logic := '0';   -- Receive interrupt enable
  --
  signal RxRd     : std_logic := '0';   -- Read receive buffer
  signal TxWr     : std_logic := '0';   -- Write Transmit buffer
  signal StRd     : std_logic := '0';   -- Read status register

  -----------------------------------------------------------------------------
  -- RX Signals
  -----------------------------------------------------------------------------

  type RxStateType is ( RxState_Wait, RxState_Data, RxState_Parity, RxState_Stop );

  signal RxState    : RxStateType;                  -- receive bit state
  -----------------------------------------------------------------------------
  -- TX Signals
  -----------------------------------------------------------------------------
  type TxStateType is ( TxState_Idle, TxState_Start, TxState_Data, TxState_Parity, TxState_Stop );

  signal TxState     : TxStateType;                   -- Transmitter state
        
-----------------------------------------------------------------------------
-- Generate Read / Write strobes.
-----------------------------------------------------------------------------

  TWI_read_write : process(clk, ac_rst)
  begin
    if falling_edge(clk) then
      if rst = '1' then
        twi_ctrl_reg(1 downto 0) <= "11";
        twi_ctrl_reg(7 downto 2) <= (others => '0');
        TxReg   <= (others => '0');
        RxRd  <= '0';
        TxWr  <= '0';
        StRd  <= '0';
      else
        RxRd  <= '0';
        TxWr  <= '0';
        StRd  <= '0';
        if cs = '1' then
          if Addr = '0' then              -- Control / Status register
            if rw = '0' then              -- write control register
              twi_ctrl_reg <= data_in;
            else                          -- read status register
              StRd <= '1';
            end if;
          else                            -- Data Register
            if rw = '0' then              -- write transmiter register
              TxReg <= data_in;
              TxWr  <= '1';
            else                          -- read receiver register
              RxRd <= '1';
            end if;
          end if;
        end if;
      end if;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- TWI Status Register
  -----------------------------------------------------------------------------

  TWI_status : process( clk )
  begin
    if falling_edge( clk ) then
      twi_stat_reg(0) <= RxRdy;                 -- Receive Data Ready
      twi_stat_reg(1) <= TxRdy and (not CTS_n); -- Transmit Buffer Empty
      twi_stat_reg(2) <= DCDInt;                -- Data Carrier Detect
      twi_stat_reg(3) <= CTS_n;                 -- Clear To Send
      twi_stat_reg(4) <= FErr;                  -- Framing error
      twi_stat_reg(5) <= OErr;                  -- Overrun error
      twi_stat_reg(6) <= PErr;                  -- Parity error
      twi_stat_reg(7) <= (RxIE and RxRdy) or
                    (RxIE and DCDInt) or
                    (TxIE and TxRdy);
    end if;
  end process;


-----------------------------------------------------------------------------
-- TWI Transmit Control
-----------------------------------------------------------------------------

  TWI_control : process(twi_ctrl_reg, TxDat)
  begin
    case twi_ctrl_reg(6 downto 5) is
      when "00" =>                      -- Disable TX Interrupts, Assert RTS
        TxD   <= TxDat;
        TxIE  <= '0';
        RTS_n <= '0';
      when "01" =>                      -- Enable TX interrupts, Assert RTS
        TxD   <= TxDat;
        TxIE  <= '1';
        RTS_n <= '0';
      when "10" =>                      -- Disable Tx Interrupts, Clear RTS
        TxD   <= TxDat;
        TxIE  <= '0';
        RTS_n <= '1';
      when "11" =>                      -- Disable Tx interrupts, Assert RTS, send break
        TxD   <= '0';
        TxIE  <= '0';
        RTS_n <= '0';
      when others =>
        null;
    end case;

    RxIE  <= twi_ctrl_reg(7);
    WdFmt <= twi_ctrl_reg(4 downto 2);
    BdFmt <= twi_ctrl_reg(1 downto 0);
  end process;

---------------------------------------------------------------
-- Set Data Output Multiplexer
--------------------------------------------------------------

  TWI_data_mux : process(Addr, RxReg, twi_stat_reg)
  begin
    if Addr = '1' then
      data_out <= RxReg;               -- read receiver register
    else
      data_out <= twi_stat_reg;               -- read status register
    end if;
  end process;

  irq <= twi_stat_reg(7);

---------------------------------------------------------------
-- Data Carrier Detect Edge rising edge detect
---------------------------------------------------------------
  TWI_dcd_edge : process( clk, ac_rst )
  begin
    if falling_edge(clk) then
      if ac_rst = '1' then
        DCDDel  <= '0';
        DCDEdge <= '0';
      else
        DCDDel  <= DCD_n;
        DCDEdge <= DCD_n and (not DCDDel);
      end if;
    end if;
  end process;


---------------------------------------------------------------
-- Data Carrier Detect Interrupt
---------------------------------------------------------------
-- If Data Carrier is lost, an interrupt is generated
-- To clear the interrupt, first read the status register
--      then read the data receive register

  TWI_dcd_int : process( clk, ac_rst )
  begin
    if falling_edge(clk) then
      if ac_rst = '1' then
        DCDInt   <= '0';
        DCDState <= DCD_State_Idle;
      else
        case DCDState is
        when DCD_State_Idle =>
          -- DCD Edge activates interrupt
          if DCDEdge = '1' then
            DCDInt   <= '1';
            DCDState <= DCD_State_Int;
          end if;
        when DCD_State_Int =>
          -- To reset DCD interrupt, 
          -- First read status
          if StRd = '1' then
            DCDState <= DCD_State_Reset;
          end if;
        when DCD_State_Reset =>
          -- Then read receive register
          if RxRd = '1' then
            DCDInt   <= '0';
            DCDState <= DCD_State_Idle;
          end if;
        when others =>
          null;
        end case;
      end if;
    end if;
  end process;


  ---------------------------------------------------------------------
  -- Receiver Clock Edge Detection
  ---------------------------------------------------------------------
  -- A rising edge will produce a one clock cycle pulse
  --
  TWI_rx_clock_edge : process( clk, rx_rst )
  begin
    if falling_edge(clk) then
      if rx_rst = '1' then
        RxClkDel  <= '0';
        RxClkEdge <= '0';
      else
        RxClkDel  <= RxC;
        RxClkEdge <= (not RxClkDel) and RxC;
      end if;
    end if;
  end process;

  ---------------------------------------------------------------------
  -- Receiver Data Edge Detection
  ---------------------------------------------------------------------
  -- A falling edge will produce a pulse on RxClk wide
  --
  TWI_rx_data_edge : process( clk, rx_rst )
  begin
    if falling_edge(clk) then
      if rx_rst = '1' then
        RxDatDel0 <= '0';
        RxDatDel1 <= '0';
        RxDatDel2 <= '0';
        RxDatEdge <= '0';
      else
        RxDatDel0 <= RxD;
        RxDatDel1 <= RxDatDel0;
        RxDatDel2 <= RxDatDel1;
        RxDatEdge <= RxDatDel0 and (not RxD);
      end if;
    end if;
  end process;

  ---------------------------------------------------------------------
  -- Receiver Start / Stop
  ---------------------------------------------------------------------
  -- Enable the receive clock on detection of a start bit
  -- Disable the receive clock after a byte is received.
  -- 
  TWI_rx_start_stop : process( clk, rx_rst )
  begin
    if falling_edge(clk) then
      if rx_rst = '1' then
        RxEnable <= '0';
        RxStart  <= '0';
      elsif (RxEnable = '0') and (RxDatEdge = '1') then
        -- Data Edge detected 
        RxStart  <= '1';                    -- Request Start and
        RxEnable <= '1';                    -- Enable Receive Clock
      elsif (RxStart = '1') and (RxAck = '1') then
        -- Data is being received
        RxStart <= '0';                     -- Reset Start Request
      elsif (RxStart = '0') and (RxAck = '0') then
        -- Data has now been received
        RxEnable <= '0';                    -- Disable Receiver until next Start Bit
      end if;
    end if;
  end process;

  ---------------------------------------------------------------------
  -- Receiver Clock Divider
  ---------------------------------------------------------------------
  -- Hold the Rx Clock divider in reset when the receiver is disabled
  -- Advance the count only on a rising Rx clock edge
  --
  TWI_rx_clock_divide : process( clk, rx_rst )
  begin
    if falling_edge(clk) then
      if rx_rst = '1' then
        RxClkCnt  <= (others => '0');
      elsif RxDatEdge = '1' then
        -- reset on falling data edge
        RxClkCnt <= (others => '0');   
      elsif RxClkEdge = '1' then          
        -- increment count on Clock edge
        RxClkCnt <= RxClkCnt + "000001";
      end if;
    end if;
  end process;

  ---------------------------------------------------------------------
  -- Receiver Baud Clock Selector
  ---------------------------------------------------------------------
  -- BdFmt
  -- 0 0     - Baud Clk divide by 1
  -- 0 1     - Baud Clk divide by 16
  -- 1 0     - Baud Clk divide by 64
  -- 1 1     - Reset
  --
  TWI_rx_baud_clock_select : process( BdFmt, RxC, RxClkCnt )
  begin
    case BdFmt is
      when "00" =>	                        -- Div by 1
        RxBdClk <= RxC;
      when "01" =>	                        -- Div by 16
        RxBdClk <= RxClkCnt(3);
      when "10" =>	                        -- Div by 64
        RxBdClk <= RxClkCnt(5);
      when others =>                         -- Software Reset
        RxBdClk <= '0';
    end case;
  end process;

  ---------------------------------------------------------------------
  -- Receiver process
  ---------------------------------------------------------------------
  -- WdFmt - Bits[4..2]
  -- 0 0 0   - 7 data, even parity, 2 stop
  -- 0 0 1   - 7 data, odd  parity, 2 stop
  -- 0 1 0   - 7 data, even parity, 1 stop
  -- 0 1 1   - 7 data, odd  parity, 1 stop
  -- 1 0 0   - 8 data, no   parity, 2 stop
  -- 1 0 1   - 8 data, no   parity, 1 stop
  -- 1 1 0   - 8 data, even parity, 1 stop
  -- 1 1 1   - 8 data, odd  parity, 1 stop
  TWI_rx_receive : process( clk, rst )
  begin
    if falling_edge( clk ) then
      if rx_rst = '1' then
        FErr       <= '0';
        OErr       <= '0';
        PErr       <= '0';
        RxShiftReg <= (others => '0');       -- Reset Shift register
        RxReg      <= (others => '0');
        RxParity   <= '0';                   -- reset Parity bit
        RxAck      <= '0';                   -- Receiving data
        RxBitCount <= (others => '0');
        RxState    <= RxState_Wait;
      else
        RxBdDel      <= RxBdClk;
        if RxBdDel = '0' and RxBdClk = '1' then
          case RxState is
          when RxState_Wait =>
            RxShiftReg <= (others => '0');   -- Reset Shift register
            RxParity   <= '0';               -- Reset Parity bit
            if WdFmt(2) = '0' then           -- WdFmt(2) = '0' => 7 data bits
              RxBitCount <= "110";
            else                             -- WdFmt(2) = '1' => 8 data bits
              RxBitCount <= "111";
            end if;
            if RxDatDel2 = '0' then          -- look for start bit
              RxState <= RxState_Data;       -- if low, start reading data
            end if;
            
          when RxState_Data =>               -- Receiving data bits 
            RxShiftReg <= RxDatDel2 & RxShiftReg(7 downto 1);
            RxParity   <= RxParity xor RxDatDel2;
            RxAck      <= '1';               -- Flag receive in progress
            RxBitCount <= RxBitCount - "001";
            if RxBitCount = "000" then
              if WdFmt(2) = '0' then         -- WdFmt(2) = '0' => 7 data
                RxState <= RxState_Parity;   -- 7 bits always has parity
              elsif WdFmt(1) = '0' then      -- WdFmt(2) = '1' => 8 data			         
                RxState <= RxState_Stop;     -- WdFmt(1) = '0' => no parity
                PErr <= '0';               -- Reset Parity Error
              else
                RxState <= RxState_Parity;   -- WdFmt(1) = '1' => 8 data + parity
              end if;
            end if;

          when RxState_Parity =>             -- Receive Parity bit
            if WdFmt(2) = '0' then           -- if 7 data bits, shift parity into MSB
              RxShiftReg <= RxDatDel2 & RxShiftReg(7 downto 1); -- 7 data + parity
            end if;
            if RxParity = (RxDatDel2 xor WdFmt(0)) then
              PErr <= '1';                 -- If parity not the same flag error
            else
              PErr <= '0';
            end if;
            RxState <= RxState_Stop;

          when RxState_Stop =>             -- stop bit (Only one required for RX)
            RxAck  <= '0';                 -- Flag Receive Complete
            RxReg <= RxShiftReg;
            if RxDatDel2 = '1' then        -- stop bit expected
              FErr <= '0';                 -- yes, no framing error
            else
              FErr <= '1';                 -- no, framing error
            end if;
            if RxRdy = '1' then            -- Has previous data been read ? 
              OErr <= '1';                 -- no, overrun error
            else
              OErr <= '0';                 -- yes, no over run error
            end if;
            RxState <= RxState_Wait;

          when others =>
            RxAck   <= '0';                -- Flag Receive Complete
            RxState <= RxState_Wait;
          end case;
        end if;
      end if;
    end if;

  end process;

  ---------------------------------------------------------------------
  -- Receiver Read process
  ---------------------------------------------------------------------
  TWI_rx_read : process( clk, rst, RxRdy )
  begin
    if falling_edge(clk) then
      if rx_rst = '1' then
        RxRdy <= '0';
        RxReq <= '0';
      elsif RxRd = '1' then
        -- Data was read,        
        RxRdy <= '0';                        -- Reset receive full
        RxReq <= '1';                        -- Request more data
      elsif RxReq = '1' and RxAck = '1' then
        -- Data is being received
        RxReq <= '0';                        -- reset receive request
      elsif RxReq = '0' and RxAck = '0' then
        -- Data now received
        RxRdy <= '1';                        -- Flag RxRdy and read Shift Register
      end if;
    end if;
  end process;


  ---------------------------------------------------------------------
  -- Transmit Clock Edge Detection
  -- A falling edge will produce a one clock cycle pulse
  ---------------------------------------------------------------------

  TWI_tx_clock_edge : process( Clk, tx_rst )
  begin
    if falling_edge(clk) then
      if tx_rst = '1' then
        TxClkDel  <= '0';
        TxClkEdge <= '0';
      else
        TxClkDel  <= TxC;
        TxClkEdge <= TxClkDel and (not TxC);
      end if;
    end if;
  end process;

  ---------------------------------------------------------------------
  -- Transmit Clock Divider
  -- Advance the count only on an input clock pulse
  ---------------------------------------------------------------------

  TWI_tx_clock_divide : process( clk, tx_rst )
  begin
    if falling_edge(clk) then
      if tx_rst = '1' then
        TxClkCnt <= (others=>'0');
      elsif TxClkEdge = '1' then 
        TxClkCnt <= TxClkCnt + "000001";
      end if;
    end if;
  end process;

  ---------------------------------------------------------------------
  -- Transmit Baud Clock Selector
  ---------------------------------------------------------------------
  TWI_tx_baud_clock_select : process( BdFmt, TxClkCnt, TxC )
  begin
    -- BdFmt
    -- 0 0     - Baud Clk divide by 1
    -- 0 1     - Baud Clk divide by 16
    -- 1 0     - Baud Clk divide by 64
    -- 1 1     - reset
    case BdFmt is
      when "00" =>	                         -- Div by 1
        TxBdClk <= TxC;
      when "01" =>	                         -- Div by 16
        TxBdClk <= TxClkCnt(3);
      when "10" =>	                         -- Div by 64
        TxBdClk <= TxClkCnt(5);
      when others =>                          -- Software reset
        TxBdClk <= '0';
    end case;
  end process;

  -----------------------------------------------------------------------------
  -- Implements the Tx unit
  -----------------------------------------------------------------------------
  -- WdFmt - Bits[4..2]
  -- 0 0 0   - 7 data, even parity, 2 stop
  -- 0 0 1   - 7 data, odd  parity, 2 stop
  -- 0 1 0   - 7 data, even parity, 1 stop
  -- 0 1 1   - 7 data, odd  parity, 1 stop
  -- 1 0 0   - 8 data, no   parity, 2 stop
  -- 1 0 1   - 8 data, no   parity, 1 stop
  -- 1 1 0   - 8 data, even parity, 1 stop
  -- 1 1 1   - 8 data, odd  parity, 1 stop
  TWI_tx_transmit : process( clk, tx_rst)
  begin
    if falling_edge(clk) then
      if tx_rst = '1' then
        TxDat      <= '1';
        TxShiftReg <= (others=>'0');
        TxParity   <= '0';
        TxBitCount <= (others=>'0');
        TxAck      <= '0';
        TxState    <= TxState_Idle;
      else

        TxBdDel <= TxBdClk;
        -- On rising edge of baud clock, run the state machine
        if TxBdDel = '0' and TxBdClk = '1' then

          case TxState is
          when TxState_Idle =>
            TxDat <= '1';
            if TxReq = '1' then
              TxShiftReg <= TxReg;              -- Load Shift reg with Tx Data
              TxAck      <= '1';
              TxState    <= TxState_Start;
            end if;

          when TxState_Start =>
            TxDat    <= '0';                    -- Start bit
            TxParity <= '0';
            if WdFmt(2) = '0' then
              TxBitCount <= "110";              -- 7 data + parity
            else
              TxBitCount <= "111";              -- 8 data
            end if;
            TxState <= TxState_Data;

          when TxState_Data =>
            TxDat      <= TxShiftReg(0);
            TxShiftReg <= '1' & TxShiftReg(7 downto 1);
            TxParity   <= TxParity xor TxShiftReg(0);
            TxBitCount <= TxBitCount - "001";  
            if TxBitCount = "000" then
              if (WdFmt(2) = '1') and (WdFmt(1) = '0') then
                if WdFmt(0) = '0' then        -- 8 data bits
                  TxState <= TxState_Stop;    -- 2 stops
                else
                  TxAck   <= '0';
                  TxState <= TxState_Idle;    -- 1 stop
                end if;
              else
                TxState <= TxState_Parity;    -- parity
              end if;
            end if;

          when TxState_Parity =>              -- 7/8 data + parity bit
            if WdFmt(0) = '0' then
              TxDat <= not(TxParity);         -- even parity
            else
              TxDat <= TxParity;              -- odd parity
            end if;
            if WdFmt(1) = '0' then
              TxState <= TxState_Stop;        -- 2 stops
            else
              TxAck   <= '0';
              TxState <= TxState_Idle;        -- 1 stop
            end if;

          when TxState_Stop =>                -- first of two stop bits
            TxDat   <= '1';
            TxAck   <= '0';
            TxState <= TxState_Idle;

          end case;
        end if;
      end if;
    end if;
  end process;

  ---------------------------------------------------------------------
  -- Transmitter Write process
  ---------------------------------------------------------------------

  TWI_tx_write : process( clk, tx_rst, TxWr, TxReq, TxAck )
  begin
    if falling_edge(clk) then
      if tx_rst = '1' then
        TxRdy <= '0';
        TxReq <= '0';
      elsif TxWr = '1' then
        -- Data was read,        
        TxRdy <= '0';                        -- Reset transmit empty
        TxReq <= '1';                        -- Request data transmit
      elsif TxReq = '1' and TxAck = '1' then -- Data is being transmitted
        TxReq <= '0';                        -- reset transmit request
      elsif TxReq = '0' and TxAck = '0' then -- Data transmitted
        TxRdy <= '1';                        -- Flag TxRdy
      end if;
    end if;
  end process;

end rtl;

