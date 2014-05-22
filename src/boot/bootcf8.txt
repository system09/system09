*******************************************************
*
* Bootstrap FLEX Loader
*
* SBUG1.8 loads the bootstap loader at $C000
* however the Flex adaption manual has the
* bootstrap loader residing at $C100
*
******************************************************
*
* Equates
*
STACK   EQU $C0FF
SCTBUF  EQU $C300
*
* Start of Utility
*
        ORG $C000
BOOT    BRA LOAD0
        FCB 0,0,0
TRK     FCB 0        File start track
SCT     FCB 0        File start sector
DNS     FCB 0        Density Flag (not used)
TADR    FDB $C000    Transfer address
LADR    FDB 0        Load Address
DRVNUM  FCB 0        Drive number 0
*
LOAD0   LDS  #STACK   Set up stack
        LDD  TRK      Set up start track and sector
        STD  SCTBUF
        LDY  #SCTBUF+256
*
* Perform actual file load
*
LOAD1   BSR GETCH    Get acharcater
        CMPA #$02    Data record hearder ?
        BEQ  LOAD2   Skip, is so
        CMPA #$16    Xfr address hearder ?
        BNE LOAD1    Loop if neither
*
* Get transfer address
*
        BSR  GETCH
        STA  TADR
        BSR  GETCH
        STA  TADR+1
        BRA  LOAD1
*
* Load data record
*
LOAD2  BSR  GETCH  Get load address
       STA  LADR
       BSR  GETCH
       STA  LADR+1
       BSR  GETCH  Get Bytes count
       TFR  A,B
       TSTB
       BEQ  LOAD1 Loop if count = 0
       LDX  LADR  Get load address
LOAD3  PSHS B,X
       BSR  GETCH  Get data character
       PULS B,X
       STA  ,X+    Store at load address
       DECB
       BNE  LOAD3  Loop until count = 0
       BRA  LOAD1
*
* Get Character routine
* Reads a sector if needed
*
GETCH  CMPY #SCTBUF+256 out of data ?
       BNE  GETCH4      Go read Character if not
GETCH2 LDX  #SCTBUF     Point to buffer
       LDD  0,X         Get forward Link
       BEQ  GO          if zero, file is loaded
       BSR  READ        Read next sector
       BNE  BOOT        start over if error
       LDY  #SCTBUF+4   Point past link
GETCH4 LDA  ,Y+         Else, get a character
       RTS
*
* File is loaded, Jump to it
*
GO     JMP  [TADR]      Jump to transfer address

*
** FLEX 9 COMPACT FLASH DISK DRIVERS
*
* FOR SYS09BUG 1.2 ON THE BURCHED B5-X300
* WITH I/O MAPPED AT $XE000
* AND ROM MAPPED AT $XF000
* THE BURCHED B5-X300 HAS 256KBYTES OF SRAM
* THE FIRST 64K IS USED BY FLEX,
* THE SECOND 192K MAY BE USED AS A RAM DISK
*
*
IMASK  EQU $10     IRQ MASK CC
FMASK  EQU $40     FIRQ MASK CC
DATREG EQU $FFF0   DAT REGISTERS
*
CF_BASE    EQU $E040
CF_DATA    EQU CF_BASE+0
CF_ERROR   EQU CF_BASE+1 ; read error
CF_FEATURE EQU CF_BASE+1 ; write feature
CF_SECCNT  EQU CF_BASE+2
CF_SECNUM  EQU CF_BASE+3
CF_CYLLO   EQU CF_BASE+4
CF_CYLHI   EQU CF_BASE+5
CF_HEAD    EQU CF_BASE+6
CF_STATUS  EQU CF_BASE+7 ; read status
CF_COMAND  EQU CF_BASE+7 ; write command
*
* Command Equates
*
CMDREAD    EQU $20 ; Read Single sector
CMDWRITE   EQU $30 ; Write Single sector
CMDFEATURE EQU $EF
FEAT8BIT   EQU $01 ; enable 8 bit transfers
HEADLBA    EQU $E0
*
* Status bit equates
*
BSY        EQU $80
DRDY       EQU $40
DRQ        EQU $08
ERR        EQU $01
*  
* RESTORE DISK DRIVER (SEEK TRACK 00)
*  
RESTR1 CLR  DRVNUM
       CLRA           ; Track 0
       LDB  #$01     ; Sector 1
*
* Seek track and sector
* A holds track number (0 - ??)
* B holds sector number (1 - ??)
* Sector numbers starts from 1
* subtract 1 to start from sector 0 on CF
*
SEEKTS DECB
       STB  CF_SECNUM
       STA  CF_CYLLO
       LDB  DRVNUM
       STB  CF_CYLHI
       LDB  #$01
       STB  CF_SECCNT
       CLRB
WARMD1 RTS
*
* READ SECTORS FROM CF
*
*
READ   BSR  SEEKTS
       LDA  #CMDREAD ; IDE READ MULTIPLE
       STA  CF_COMAND
       BSR  WAITRDY
*
* READ LOOP
*
       CLRB
RDLP1  BSR  WAITDRQ
       LDA  CF_DATA
       STA  ,X+
       DECB
       BNE  RDLP1
*
       CLRB
RDLP2  BSR  WAITDRQ
       LDA  CF_DATA
       DECB
       BNE  RDLP2
*
       BSR  WAITRDY
       CLRB
       RTS
*
* WAIT UNTIL READY
*
WAITRDY LDA  CF_STATUS
        BITA #BSY
        BNE  WAITRDY
        LDA  CF_STATUS
        BITA #DRDY
        BEQ  WAITRDY
        RTS
*
* WAIT FOR DATA REQUEST
*
WAITDRQ LDA  CF_STATUS
        BITA #DRQ
        BEQ  WAITDRQ
        RTS
*
        END
