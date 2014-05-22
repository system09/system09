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
RMAXTRK EQU 64
RMAXSEC EQU 255
RTOTSEC EQU RMAXTRK*RMAXSEC-RMAXSEC
*
* Start
*
        ORG $0100
START   LBSR  UFSUB
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
*
* recieve char from remote drive.
* timeout if no response for approx 1s.
* Entry: no parameters
* Exit:  (A) = recieved char, (C)=1 if valid char, (C)=0 if timeout.
*
RCHAR   PSHS    X,Y
*
        LDX     #1000         1000x inner loop
RCHAR1  LDY     #DELCON       delay constant for inner loop (approx 1ms).
RCHAR2  LDA     ACIAC1        test for recieved char
        ASRA
        BCS     RCHAR3        get character
        LEAY    -1,Y          else, continue to count delay
        BNE     RCHAR2
        LEAX    -1,X
        BNE     RCHAR1
        PULS    X,Y,PC        return with error if timed out
*
RCHAR3  LDA     ACIAD1        return data (carry bit still set)
        PULS    X,Y,PC
*
*
* transmit char to remote drive.
* timeout if no response for approx 1s. (allows for use of hardware flow control)
* Entry: (A) = char to transmit
* Exit:  (A) = recieved char, (C)=1 if valid char, (C)=0 if timeout.
*
SCHAR   PSHS    X,Y
        PSHS    A
*
        LDX     #1000         1000x inner loop
SCHAR1  LDY     #DELCON       delay constant for inner loop (approx 1ms).
SCHAR2  LDA     ACIAC1        test for space in transmit FIFO
        ASRA
        ASRA
        BCS     SCHAR3        send character
        LEAY    -1,Y          else, continue to count delay
        BNE     SCHAR2
        LEAX    -1,X
        BNE     SCHAR1
        PULS    A
        PULS    X,Y,PC        return with error if timed out
*
SCHAR3  PULS    A
        STA     ACIAD1        send data (carry bit still set)
        PULS    X,Y,PC
*
** 'UF' Format IDE Drive to FLEX standard.
*
DISFOS  FCB $0A,$0D 
        FCC 'Formating IDE disk... '
        FCB $0A,$0D
        FCC 'Drive Number ?'
        FCB 4
MESS6   FCB $0A,$0D,4
        FCC 'IDE drive not allocated! '
	FCB 4
UFMSG1  FCB $0A,$0D
        FCC 'Format Complete'
        FCB 4
*
UFSUB   JSR  INITDR
        LDX #DISFOS
        JSR PDATA1
UFSUB1  LBSR RCHAR
        BCC  UFSUB1
        LBSR SCHAR
        CMPA #'0'
        LBLO UFEXIT
        CMPA #'3'
        LBHI  UFEXIT
        SUBA #'0'
        TFR  A,B
        STB DRVNUM
        LDX #DRVNUM-3
        JSR DRVSEL
*
* set up free chain
*
        LDX #BUFFER clear out buffer
        CLRA
        CLRB
DFL1    STA 0,X+
        DECB
        BNE DFL1
*
        CLR TRACK
        LDA #1
        STA SECTOR
DFL2    LDX #BUFFER
        LDA TRACK
        STA 0,X
        LDA SECTOR
        INCA
        CMPA #RMAXSEC+1 last sector on track?
        BNE DFL3
        INC 0,X
        LDA #1
DFL3    STA 1,X
        LDA TRACK
        LDB SECTOR
        JSR WRITSC
        INC SECTOR
        LDA SECTOR
        CMPA #RMAXSEC+1
        BNE DFL2
        LDA #1
        STA  SECTOR
        INC TRACK
        LDA TRACK
        CMPA #RMAXTRK
        BNE DFL2
* break free chain at last track/sector
        LDX  #BUFFER
        LDA  #RMAXTRK-1
        LDB  #RMAXSEC
        JSR  READSC
        LDX  #BUFFER
        CLR  0,X
        CLR  1,X
        LDA  #RMAXTRK-1
        LDB  #RMAXSEC
        JSR  WRITSC 
* set up sector structure, SIR, directory etc
        LDX  #BUFFER
        CLRA
        LDB  #RMAXSEC
        JSR  READSC
        LDX  #BUFFER
        CLR  0,X break end of directory chain
        CLR  1,X
        CLRA
        LDB  #RMAXSEC
        JSR  WRITSC
*
        LDX  #BUFFER
        CLRA
        LDB  #3 set up SIR
        JSR  READSC
        LDX  #BUFFER
        CLR  0,X break forward link
        CLR  1,X
        LDD  #$5241 set volume name (RAMDISK )
        STD  16,X
        LDD  #$4D44
        STD  18,X
        LDD  #$4953
        STD  20,X
        LDD  #$4B20
        STD  22,X
        LDD  #1 volume number
        STD  27,X
        LDD  #$0101 first trk/sec  01-01
        STD  29,X
        LDA  #RMAXTRK-1
        LDB  #RMAXSEC
        STD  31,X
        STD  38,X
        LDD  #RTOTSEC total DATA sectors (2912-14)
        STD  33,X
*
        LDA #01 month   set default creation date (SYS09's birthday!)
        STA 35,X
        LDA #07 day
        STA 36,X
        LDA #07 year
        STA 37,X
*
RF3     CLRA
        LDB  #3
        JSR  WRITSC
*
* Not sure what this is about
* put bootstrap on track 0 sector 1
*
*        LDX #BUFFER
*        CLRA
*        LDB #1
*        JSR READSC
*        LDX #BUFFER
*        LDA #$AA set the init flag
*        STA 0,X
*        LDA  #$55
*        STA 1,X
*        CLRA
*        LDB #1
*        JSR WRITSC
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
UFEXIT  LDX #UFMSG1
        JMP PDATA1
*
* Read a byte from the serial port
*
LRBYTE  PSHS B
        BSR  LRHEX                  Get hex digit.
        ASLA
        ASLA                           Shift to msb.
        ASLA
        ASLA
        TFR  A,B                    Save in B.
        BSR  LRHEX                  Get next digit.
        PSHS B
        ADDA 0,S+                    Add together bytes.
        PULS B,PC
*
LRHEX   JSR  INTER
        BVS  LRHEX
        SUBA #$30                   Remove ascii bias.
        BMI  LRHEX
        CMPA #$09                   Number?
        BLE  LRHEX1                 Yes.
        CMPA #$11                   Keep testing.
        BMI  LRHEX
        CMPA #$16
        BGT  LRHEX
        SUBA #$07
LRHEX1  RTS
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
        ANDCC #VFLAG
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
** FLEX 9 IDE DISK DRIVERS
*
* FOR SYS09BUG 1.2 ON THE XSA-3S1000
* WITH I/O MAPPED AT $XE000
* AND ROM MAPPED AT $XF000
*
*
IMASK  EQU $10     IRQ MASK CC
FMASK  EQU $40     FIRQ MASK CC
*
CF_BASE    EQU $E100
CF_DATA    EQU CF_BASE+0
CF_ERROR   EQU CF_BASE+2 ; read error
CF_FEATURE EQU CF_BASE+2 ; write feature
CF_SCNT    EQU CF_BASE+4
CF_SNUM    EQU CF_BASE+6
CF_CLO     EQU CF_BASE+8
CF_CHI     EQU CF_BASE+10
CF_HEAD    EQU CF_BASE+12
CF_STATUS  EQU CF_BASE+14 ; read status
CF_COMAND  EQU CF_BASE+14 ; write command
CF_AUX     EQU CF_BASE+30
*
* Command Equates
*
CMDREAD    EQU $20 ; Read Single sector
CMDWRITE   EQU $30 ; Write Single sector
AUXRESET   EQU $06
AUXRSTREL  EQU $02
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
INITDR LDD #AUXRESET
       STD CF_AUX
       LDD #AUXRSTREL
       STD CF_AUX
       LDD  #HEADLBA
       STD  CF_HEAD
       BRA WAITRDY
*  
* RESTORE DISK DRIVER (SEEK TRACK 00)
*  
RESTR1 BSR   DRVSEL
       CLRA           ; Track 0
       LDB   #$01     ; Sector 1
*
* Seek track and sector
* A holds track number (0 - ??)
* B holds sector number (1 - ??)
* Sector numbers starts from 1
* subtract 1 to start from sector 0 on CF
*
SEEKTS PSHS A
       CLRA
       DECB
       STD  CF_SNUM
       LDB ,S
       STD  CF_CLO
       LDB  DRVNUM
       STD  CF_CHI
       LDB  #$01
       STD  CF_SCNT
       CLRB
       PULS A,PC
*
* READ SECTORS FROM CF
*
*
READSC BSR  SEEKTS
       LDD  #CMDREAD ; IDE READ MULTIPLE
       STD  CF_COMAND
       BSR  WAITRDY
*
* READ LOOP
*
       PSHS Y
       LDY #256
RDLP1  BSR  WAITDRQ
       LDD  CF_DATA
       STB  ,X+
       LEAY -1,Y
       BNE  RDLP1
       PULS Y
*
       BSR  WAITRDY
       CLRB
       RTS
*  
* WRITE SECTOR TO CF
*  
WRITSC BSR  SEEKTS   ; SEEK TRACK & SECTOR
       LDD  #CMDWRITE ; IDE WRITE MULTIPLE
       STD  CF_COMAND
       BSR  WAITRDY
*
* WRITE LOOP
*
       PSHS Y
       LDY #256
       CLRA
WRTLP1 BSR  WAITDRQ
       LDB  ,X+
       STD  CF_DATA
       LEAY -1,Y
       BNE  WRTLP1
       PULS Y
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
WAITRDY LDD  CF_STATUS
        BITB #BSY
        BNE  WAITRDY
        LDD  CF_STATUS
        BITB #DRDY
        BEQ  WAITRDY
        RTS
*
* WAIT FOR DATA REQUEST
*
WAITDRQ LDD  CF_STATUS
        BITB #DRQ
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
** FLEX 9 IDE DISK DRIVERS
*
* FOR SYS09BUG 1.2 ON THE XSA-3S1000
* WITH I/O MAPPED AT $XE000
* AND ROM MAPPED AT $XF000
*
*IMASK  EQU $10     IRQ MASK CC
*FMASK  EQU $40     FIRQ MASK CC
*
*CF_BASE    EQU $E100
*CF_DATA    EQU CF_BASE+0
*CF_ERROR   EQU CF_BASE+2 ; read error
*CF_FEATURE EQU CF_BASE+2 ; write feature
*CF_SCNT  EQU CF_BASE+4
*CF_SNUM  EQU CF_BASE+6
*CF_CLO   EQU CF_BASE+8
*CF_CHI   EQU CF_BASE+10
*CF_HEAD    EQU CF_BASE+12
*CF_STATUS  EQU CF_BASE+14 ; read status
*CF_COMAND  EQU CF_BASE+14 ; write command
*CF_AUX     EQU CF_BASE+30
*
* Command Equates
*
*CMDREAD    EQU $20 ; Read Single sector
*CMDWRITE   EQU $30 ; Write Single sector
*HEADLBA    EQU $E0
*AUXRESET   EQU $06
*AUXRSTREL  EQU $02
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
SEEK   PSHS A
       CLRA
       DECB
       STD  CF_SNUM
       LDB  ,S
       STD  CF_CLO
       LDB  DRNUM
       STD  CF_CHI
       LDB  #$01
       STD  CF_SCNT
       CLRB
       PULS A,PC 
*
* READ SECTORS FROM CF
*
*
READ   BSR  SEEK
       LDD  #CMDREAD ; IDE READ MULTIPLE
       STD  CF_COMAND
       BSR  WTRDY
*
* READ LOOP
*
       PSHS Y
       LDY #256
READ1  BSR  WTDRQ
       LDD  CF_DATA
       STB  ,X+
       LEAY -1,Y
       BNE  READ1
       PULS Y
*
       BSR  WTRDY
       CLRB
       RTS
*
* WAIT UNTIL READY
*
WTRDY   LDD  CF_STATUS
        BITB #BSY
        BNE  WTRDY
        LDD  CF_STATUS
        BITB #DRDY
        BEQ  WTRDY
        RTS
*
* WAIT FOR DATA REQUEST
*
WTDRQ   LDD  CF_STATUS
        BITB #DRQ
        BEQ  WTDRQ
        RTS
*
       END START
