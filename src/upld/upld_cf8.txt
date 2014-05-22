*
** FLEX 9 DISK DRIVERS
*
* FOR SYS09BUG ON THE DIGILENT SPARTAN 3 STARTER BOARD
* WITH I/O MAPPED AT $XE000
* AND ROM MAPPED AT $XF000
* THE DIGILENT SPARTAN 3 STARTER BOARD HAS 1MBYTE OF SRAM
* THE FIRST 64K IS USED BY FLEX,
* THE SECOND 128K IS USED AS A ROM DISK
* THE REMAINING RAM IS USED FOR A RAM DISK
*
*
CFLAG   EQU $01     CARRY FLAG
VFLAG   EQU $02     OVERFLOW FLAG
ZFLAG   EQU $04     ZERO FLAG
NFLAG   EQU $08     NEGATIVE FLAG
IFLAG   EQU $10     IRQ MASK CC
HFLAG   EQU $20     HALF CARRY
FFLAG   EQU $40     FIRQ MASK CC
EFLAG   EQU $80     ENTIRE FLAG
*
MAPPAG  EQU $00     PAGE $0000 DAT ADDRESS
*
* Serial Port
*
ACIAS   EQU $E000
ACIAC1  EQU ACIAS
ACIAD1  EQU ACIAS+1
DELCON  EQU 1250    Delay (Processor clock in MHz * 50)
*
* XMODEM Control characters
*
SOH     EQU $01
EOT     EQU $04
ACK     EQU $06
NAK     EQU $15
CAN     EQU $18
*
* Some dummy Constants
*
RMAXTRK EQU 48
RMAXSEC EQU 14
RTOTSEC EQU RMAXTRK*RMAXSEC-RMAXSEC
*
* Start
*
        ORG $0100
START   LBSR  UXSUB
        JMP  [$F800]  Jump to monitor on Completion.
*
*
* RAM SPACE
*
DRVNUM  FCB  0
TRACK   FCB  0
SECTOR  FCB  0
CHKSUM  FCB  0
BLKNUM  FCB  0 Xmodem block number
BYTCNT  FCB  0 Xmodem byte count
XSTATE  FDB  0 Xmodem State Vector
DELCNT  FCB  $00,$00,$00 Xmodem Poll timer
MAXTRK  FCB  0
MAXSEC  FCB  0
        ORG  $0200
*
* SECTOR BUFFER
*
BUFFER  RMB  256
*
* ACIA INPUT TEST
*
INTEST  LDA ACIAC1
        BITA #$01
        RTS
*
* RESET ACIA
*
ACIRST  LDA #$03 master reset
        STA  ACIAC1
        LDA #$11
        STA ACIAC1
        RTS
*
* ACIA INPUT
*
INTER   LDA  #16
        STA  DELCNT+0
        CLR  DELCNT+1
        CLR  DELCNT+2
INTER0  LDA  ACIAC1
        BITA #$01
        BNE  INTER1
        BITA #$78
        BEQ  INTER2
        BSR  ACIRST
        BRA  INTER
*
INTER1  LDA  ACIAD1
        ANDCC #$FF-VFLAG
        RTS
*
INTER2  DEC  DELCNT+2
        BNE  INTER0
        DEC  DELCNT+1
        BNE  INTER0
        DEC  DELCNT+0
        BNE  INTER0
        CLRA
        ORCC #VFLAG
        RTS
*
* ACIA OUTPUT
*
OUTTER  PSHS A
*
OUTTE1  LDA ACIAC1
        BITA #$02
        BNE  OUTTE2
        BITA #$78
        BEQ  OUTTE1
        BSR  ACIRST
        BRA  OUTTE1
*
OUTTE2  PULS A
        STA ACIAD1
        RTS
*
* Print Data
*
PDATA0  BSR  OUTTER
PDATA1  LDA  ,X+
        CMPA #$04
        BNE  PDATA0
        RTS 
*
** 'UX' Xmodem ROM Disk upload
*
UXMES0  FCB $0D,$0A
        FCC 'Xmodem ROM Disk Upload'
        FCB 4
UXMES1  FCB $0D,$0A
        FCC 'Upload Complete'
        FCB 4
UXMES2  FCB $0D,$0A
        FCC 'Upload Error'
        FCB 4
UXMSG3  FCB $0D,$0A
        FCC 'Drive Number :'
        FCB 4
UXMSG4  FCB $0D,$0A
        FCC 'Are You Sure ? (Y/N)'
        FCB 4
*
* Print Banner
*
UXSUB   LDX #UXMES0
        LBSR PDATA1
*
* Prompt for Disk drive number (0 to 3)
*
        LDX #UXMSG3
        LBSR PDATA1
UXSUB1  LBSR INTER
        BVS  UXSUB1
        LBSR OUTTER
        CMPA #'0
        LBLO UXEXIT
        CMPA #'3
        LBHI UXEXIT
        SUBA #'0
        STA  DRVNUM
*
* Report selected drive
*
        LDX #UXMSG3
        LBSR PDATA1
        LDA  DRVNUM
        ADDA #'0
        LBSR OUTTER
*
* Ask for confirmation (Y/N)
*
        LDX #UXMSG4
        LBSR PDATA1
UXSUB2  LBSR INTER
        BVS  UXSUB2
        LBSR OUTTER
        ANDA #$5F
        CMPA #'N
        LBEQ UXEXIT
        CMPA #'Y
        BNE  UXSUB 
*
* We have confirmation ... now load the disk image
*
        LBSR INITDR
        LDU  #XSTST
        STU  XSTATE
        LDA  #1
        STA  BLKNUM
*
* Sector1
*
        LDX  #BUFFER
*
        CLRA         TRACK 0
        LDB  #$01    SECTOR 1
        STA  TRACK
        STB  SECTOR
*
        LBSR XREAD
        LBCS UXERR
        LBSR XACK
        LBSR XREAD
        LBCS UXERR
*
        LDX  #BUFFER
        LDA  TRACK
        LDB  SECTOR
        LBSR WRITSC
        LBSR XACK
*
* Sector 2
*
        LDX  #BUFFER
*
        LDA  TRACK
        LDB  SECTOR
        INCB
        STA  TRACK
        STB  SECTOR
*
        LBSR XREAD
        LBCS UXERR
        LBSR XACK
        LBSR XREAD
        LBCS UXERR
*
        LDX  #BUFFER
        LDA  TRACK
        LDB  SECTOR
        LBSR WRITSC
*
        LBSR XACK
*
* Sector 3 - SIR
*
        LDX  #BUFFER
*
        LDA  TRACK
        LDB  SECTOR
        INCB
        STA  TRACK
        STB  SECTOR
*
        LBSR XREAD
        LBCS UXERR
        LBSR XACK
        LBSR XREAD
        LBCS UXERR
*
        LDX  #BUFFER
        LDA  38,X
        INCA
        STA  MAXTRK
        LDB  39,X
        INCB
        STB  MAXSEC
        LDA  TRACK
        LDB  SECTOR
        LBSR WRITSC
*
        LBSR XACK
*
* Sector 4 to Last Track & Sector
*
*
        LDA  TRACK
        LDB  SECTOR
        INCB
*
UXLOOP  LDX  #BUFFER
        STA  TRACK
        STB  SECTOR
*
        LBSR XREAD
        LBCS UXERR
        LBSR XACK
        LBSR XREAD
        LBCS UXERR
*
        LDX  #BUFFER
        LDA  TRACK
        LDB  SECTOR
        LBSR WRITSC
        LBSR XACK
*
        LDA  TRACK
        LDB  SECTOR
        INCB
        CMPB MAXSEC
        BNE  UXLOOP
        LDB  #1
        INCA
        CMPA MAXTRK
        BNE  UXLOOP
*
*
*  Write Boot sector
*
        LDX  #$C000
        CLRA         TRACK 0
        LDB  #$01    SECTOR 1
        STA  TRACK
        STB  SECTOR
        LBSR WRITSC
*
UXEXIT  LDX  #UXMES1
        JMP  PDATA1
*
UXERR   LDX  #UXMES2
        LBRA PDATA1
*
* Get a Byte using XModem protocol
* Carry clear => no errors
* Carry set   => errors
*
XREAD   PSHS U
        LDU  XSTATE
*
XBYTE0  LBSR INTER
        BVC  XBYTE1
        LDA  #NAK
        LBSR OUTTER
        LDU  #XSTST
        BRA  XBYTE0
*
XBYTE1  JSR  ,U
        BNE  XBYTE0
        STU  XSTATE
        PULS U,PC
*
* START - LOOK FOR SOH (START OF HEADER) = $01
*
XSTST   CMPA #SOH
        BNE  XSTST1
        LDU  #XSTBL
        ANDCC #$FF-CFLAG-ZFLAG No abort, no valid data (no exit)
        RTS
*
XSTST1  CMPA #EOT
        BNE  XSTST2
        LDA  #ACK
        LBSR OUTTER
        ORCC  #CFLAG+ZFLAG  Set (c)=1 abort & exit
        RTS
*
XSTST2  CMPA #CAN
        BNE  XSTST3 
        ORCC  #CFLAG+ZFLAG  Set (c)=1 abort & exit
        RTS
*
XSTST3  ANDCC #$FF-CFLAG-ZFLAG
        RTS
*
* Got SOH
* Now get block number
*
XSTBL   CMPA BLKNUM
        BNE  XSTBLE
        LDU  #XSTCOM
        ANDCC #$FF-CFLAG-ZFLAG No abort, No valid data (no exit)
        RTS
*
* Error in block number
*
XSTBLE  LDA  #NAK
        LBSR OUTTER
        LDU  #XSTST
        ANDCC #$FF-CFLAG-ZFLAG No abort, No valid data (no exit)
        RTS
*
* Get complement of block number
*
XSTCOM  COMA
        CMPA BLKNUM
        BNE  XSTBLE
        CLR  CHKSUM
        LDA  #128
        STA  BYTCNT
        LDU  #XSTDA
        ANDCC #$FF-CFLAG-ZFLAG No abort, No valid data (no exit)
        RTS
*
* Get data bytes
*
XSTDA   PSHS A
        ADDA CHKSUM
        STA  CHKSUM
        PULS A
        DEC  BYTCNT
        BNE  XSTDA1
        LDU  #XSTCK
XSTDA1  STA  ,X+
        ANDCC #$FF-CFLAG-ZFLAG No abort, no valid data (no exit)
        RTS
*
* Byte count reached zero
* Check checksum byte
*
XSTCK   CMPA CHKSUM
        BNE  XSTCK1 retry if wrong checksum
*
* Checksum OK ... 
* increment block number
* Don't send ACK until data written to CF
*
        INC  BLKNUM
        LDU  #XSTST
        ANDCC #$FF-CFLAG No abort
        ORCC #ZFLAG      Valid data (exit)
        RTS
*
* Checksum Error detected ...
* Reset Sector counter in ACCB to last 128 byte boundary
* and send NAK
*
XSTCK1  PSHS B
        TFR  X,D
        DECB
        ANDB #128 
        TFR  D,X
        PULS B
        LDA  #NAK
XSTCK2  LBSR OUTTER
        LDU  #XSTST
        ANDCC #$FF-CFLAG-ZFLAG No abort, no valid data (no exit)
        RTS
*
* Acknowledge Data Received
*
XACK    PSHS A
        LDA  #ACK
        LBSR OUTTER
        PULS A,PC
*
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
CF_SCNT    EQU CF_BASE+2
CF_SNUM    EQU CF_BASE+3
CF_CLO     EQU CF_BASE+4
CF_CHI     EQU CF_BASE+5
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
*
* INITIALIZE CF CARD FOR 8 BIT LBA MODE
*
INITDR BSR WAITRDY
       LDA  #HEADLBA
       STA  CF_HEAD
       LDA #FEAT8BIT
       STA CF_FEATURE
       LDA #CMDFEATURE
       STA CF_COMAND
       BRA WAITRDY
*
* Seek track and sector
* A holds track number (0 - ??)
* B holds sector number (1 - ??)
* Sector numbers starts from 1
* subtract 1 to start from sector 0 on CF
*
SEEKTS DECB
       STB  CF_SNUM
       STA  CF_CLO
       LDB  DRVNUM
       STB  CF_CHI
       LDB  #$01
       STB  CF_SCNT
       CLRB
       RTS
*
* READ SECTORS FROM CF
*
*
READSC BSR  SEEKTS
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
* WRITE SECTOR TO CF
*  
WRITSC BSR  SEEKTS   ; SEEK TRACK & SECTOR
       LDA  #CMDWRITE ; IDE WRITE MULTIPLE
       STA  CF_COMAND
       BSR  WAITRDY
*
* WRITE LOOP
*
       CLRB
WRTLP1 BSR  WAITDRQ
       LDA  ,X+
       STA  CF_DATA
       DECB
       BNE  WRTLP1
*
       CLRB
WRTLP2 BSR  WAITDRQ
       CLRA
       STA  CF_DATA
       DECB
       BNE WRTLP2
*
       BSR  WAITRDY
       CLRB
       RTS
*  
* CHECK FOR BUSY  
* Doubles as VERIFY
*  
BUSY   CLRB            Never busy
       RTS
*  
* DRIVE SELECT DISK DRIVER
*  
DRVSEL LDA   3,X       GET DRIVE # FROM FCB
       CMPA  #3  
       BLS   DRVS2     IF > 3, SET IT TO 0  
       CLRA  
DRVS2  STA   DRVNUM
       CLRB            ; SET Z, CLEAR C
       RTS
*  
* CHECK DRIVE READY DISK DRIVER
*  
CHKDRV LDA  3,X
       CLRB             ; CLEAR C, SET Z
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
DRNUM   FCB 0        Drive number 0
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
*IMASK  EQU $10     IRQ MASK CC
*FMASK  EQU $40     FIRQ MASK CC
*DATREG EQU $FFF0   DAT REGISTERS
*
*CF_BASE    EQU $E040
*CF_DATA    EQU CF_BASE+0
*CF_ERROR   EQU CF_BASE+1 ; read error
*CF_FEATURE EQU CF_BASE+1 ; write feature
*CF_SCNT  EQU CF_BASE+2
*CF_SNUM  EQU CF_BASE+3
*CF_CLO   EQU CF_BASE+4
*CF_CHI   EQU CF_BASE+5
*CF_HEAD    EQU CF_BASE+6
*CF_STATUS  EQU CF_BASE+7 ; read status
*CF_COMAND  EQU CF_BASE+7 ; write command
*
* Command Equates
*
*CMDREAD    EQU $20 ; Read Single sector
*CMDWRITE   EQU $30 ; Write Single sector
*CMDFEATURE EQU $EF
*FEAT8BIT   EQU $01 ; enable 8 bit transfers
*HEADLBA    EQU $E0
*
* Status bit equates
*
*BSY        EQU $80
*DRDY       EQU $40
*DRQ        EQU $08
*ERR        EQU $01
*
* Seek track and sector
* A holds track number (0 - ??)
* B holds sector number (1 - ??)
* Sector numbers starts from 1
* subtract 1 to start from sector 0 on CF
*
SEEK   DECB
       STB  CF_SNUM
       STA  CF_CLO
       LDB  DRNUM
       STB  CF_CHI
       LDB  #$01
       STB  CF_SCNT
       CLRB
       RTS
*
* READ SECTORS FROM CF
*
*
READ   BSR  SEEK
       LDA  #CMDREAD ; IDE READ MULTIPLE
       STA  CF_COMAND
       BSR  WTRDY
*
* READ LOOP
*
       CLRB
READ1  BSR  WTDRQ
       LDA  CF_DATA
       STA  ,X+
       DECB
       BNE  READ1
*
       CLRB
READ2  BSR  WTDRQ
       LDA  CF_DATA
       DECB
       BNE  READ2
*
       BSR  WTRDY
       CLRB
       RTS
*
* WAIT UNTIL READY
*
WTRDY   LDA  CF_STATUS
        BITA #BSY
        BNE  WTRDY
        LDA  CF_STATUS
        BITA #DRDY
        BEQ  WTRDY
        RTS
*
* WAIT FOR DATA REQUEST
*
WTDRQ   LDA  CF_STATUS
        BITA #DRQ
        BEQ  WTDRQ
        RTS
*
       END START
