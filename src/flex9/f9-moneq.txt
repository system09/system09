 PAGE 
* 
***************************************************
*   MEMORY MAP EQUATES                            *
***************************************************
MONIO   EQU  $E000  I/O SPACE
MONRAM  EQU  $DFC0  STACK SPACE
MONROM  EQU  $F800  START OF ROM
MONEXT  EQU  $F000  EXTENDED COMMANDS
*MONEXT  EQU  $1000  EXTENDED COMMANDS
*
***************************************************
*   SYS09BUG VARIABLE SPACE
***************************************************
* 
STACK   EQU  MONRAM+0  TOP OF INTERNAL STACK / USER VECTOR 
SWI3    EQU  MONRAM+2  SOFTWARE INTERRUPT VECTOR #3 
SWI2    EQU  MONRAM+4  SOFTWARE INTERRUPT VECTOR #2 
FIRQ    EQU  MONRAM+6  FAST INTERRUPT VECTOR 
IRQ     EQU  MONRAM+8  INTERRUPT VECTOR 
SWI     EQU  MONRAM+10 SOFTWARE INTERRUPT VECTOR 
SVCVO   EQU  MONRAM+12 SUPERVISOR CALL VECTOR ORGIN 
SVCVL   EQU  MONRAM+14 SUPERVISOR CALL VECTOR LIMIT 
LRARAM  EQU  MONRAM+16 LRA ADDRESSES 
CPORT   EQU  MONRAM+32 RE-VECTORABLE CONTROL PORT 
ECHO    EQU  MONRAM+34 ECHO FLAG 
BPTBL   EQU  MONRAM+35 BREAKPOINT TABLE BASE ADDR 
**************************************************
*   VDU BYTES                                    *
**************************************************
*
**** ALWAYS KEEP COLADX AND ROWADX TOGETHER ******
COLADX  EQU  MONRAM+59 CURSOR COLUMN
ROWADX  EQU  MONRAM+60 CURSOR ROW
**************************************************
*
NEWROW  EQU  MONRAM+61 NEW ROW TEMP FOR ESCAPE
ESCFLG  EQU  MONRAM+62 ESCAPE SEQUENCE ACTIVE
* 
***************************************************
*   SERIAL PORT                                   *
***************************************************
*
ACIAS   EQU  MONIO+$00   CONTROL PORT 
* 
***************************************************
*   PS/2 KEYBOARD PORT                            *
***************************************************
*
PS2KBD  EQU  MONIO+$20   PS/2 KEYBOARD PORT 
* 
***************************************************
*   ADM3A DISPLAY DRIVER VARIABLES                *
***************************************************
*
** VIDEO DISPLAY DEFINITIONS
*
VDU     EQU  MONIO+$30
VDUCHR  EQU  0        CHARACTER REGISTER
VDUATT  EQU  1        ATTRIBUTE REGISTER
VDUCOL  EQU  2        CURSOR COLUMN
VDUROW  EQU  3        CURSOR ROW
VDUOFF  EQU  4        ROW OFFSET
*
LINLEN  EQU  80       LENGTH OF A LINE
NUMLIN  EQU  25       NUMBER OF LINES
* 
***************************************************
* Serial PROM register
***************************************************
*
** CONFIGURATION PROM DEFINITIONS
*
PROMREG EQU MONIO+$C0
PCLKHI  EQU $01     Toggle PROM Clock High
PCLKLO  EQU $00     Toggle PROM Clock Low
PRSTHI  EQU $02     Toggle PROM Reset High
PRSTLO  EQU $00     Toggle PROM Reset Low
SYNCHI  EQU $AA55   Synch Pattern High Word
SYNCLO  EQU $FF00   Synch Pattern Low Word
*
* 
***************************************************
* Dynamic Address Translation Registers
***************************************************
*
** DAT Table
*
IC11    EQU  $FFF0  DAT RAM CHIP 
*
***************************************************
*   START OF ROM                                  *
***************************************************
*
MONITV EQU MONROM+0  FDB MONITOR 
NXTCMV EQU MONROM+2  FDB NEXTCMD 
INCHV  EQU MONROM+4  FDB INCH 
INCHEV EQU MONROM+6  FDB INCHE 
INCHKV EQU MONROM+8  FDB INCHEK 
OUTCHV EQU MONROM+10 FDB OUTCH 
PDATAV EQU MONROM+12 FDB PDATA 
PCRLFV EQU MONROM+14 FDB PCRLF 
PSTRGV EQU MONROM+16 FDB PSTRNG 
LRAV   EQU MONROM+18 FDB LRA 
       END

