*
** SYS09BUG MONITOR EXTENSIONS
*
* FOR SYS09BUG ON THE XESS XSA-3S100 / XST-3.0
* WITH I/O MAPPED AT $XE000
* 16 BIT IDE MAPPED AT $E100
* AND ROM MAPPED AT $XF000
*
*
 PAGE 
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
*
* Condition code flags
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
* Serial Port
*
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
* Some Disk Constants
*
RMAXTRK EQU 256
RMAXSEC EQU 255
RTOTSEC EQU RMAXTRK*RMAXSEC-RMAXSEC
*
* RAM SPACE
*
* PUT THIS DOWN THE BOTTOM OF MEMORY
*
        ORG  $0100
DRVNUM  RMB 1
TRACK   RMB 1 
SECTOR  RMB 1
CHKSUM  RMB 1
BLKNUM  RMB 1 Xmodem block number
BYTCNT  RMB 1 Xmodem byte count
XSTATE  RMB 2 Xmodem State Vector
DELCNT  RMB 3  $00,$00,$00 Xmodem Poll timer
MAXTRK  RMB 1
MAXSEC  RMB 1
        ORG  $0200
*
* SECTOR BUFFER
*
BUFFER  RMB  256
*
****************************************
*
* START OF EXTENSION COMMANDS
*
****************************************
*
        ORG MONEXT
        FDB NEXTEXT   Jump to next extended command
* 
***** NEXTCMD ***** 
* 
NEXTEXT JSR [INCHEV]  GET ONE CHAR. FROM TERMINAL 
        ANDA #$7F STRIP PARITY FROM CHAR. 
        TFR  A,B
        LDA  #$20 
        JSR [OUTCHV] PRNT SPACE 
        CMPB #$60 
        BLE NXTEX0 
        SUBB #$20 
* 
***** DO TABLE LOOKUP ***** 
*   FOR COMMAND FUNCTIONS 
* 
NXTEX0  LDX #EXTTAB    POINT TO JUMP TABLE 
NXTEX1  CMPB ,X+       DOES COMMAND MATCH TABLE ENTRY ? 
        BEQ  JMPEXT    BRANCH IF MATCH FOUND 
        LEAX 2,X       POINT TO NEXT ENTRY IN TABLE 
        CMPX #EXTEND   REACHED END OF TABLE YET ? 
        BNE  NXTEX1    IF NOT END, CHECK NEXT ENTRY 
        LDX  #MSGWHAT  POINT TO MSG "WHAT?" 
        LBRA PDATA1    PRINT MSG AND RETURN
JMPEXT  JMP  [,X]      JUMP TO COMMAND ROUTINE 
*
* EXTENDED COMMAND JUMP TABLE 
* 
EXTTAB EQU * 
       FCC 'B'   BOOT FLEX
       FDB UBSUB
       FCC 'F'   FORMAT IDE DISK
       FDB UFSUB 
       FCC 'X'   XMODEM ROM DISK UPLOAD
       FDB UXSUB
* 
EXTEND EQU * 
*
MSGWHAT FCC "WHAT ?"
        FCB $0A,$0D,$04
*
* GO TO FLEX RESIDENT IN MEMORY
*
UBSUB   LDX #$CD00
        STX 10,U
        TFR  U,S 
        RTI 
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
VOLMSG  FCC 'IDEDISK '
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
*
        PSHS Y
        LDY  #VOLMSG
        LDB  #16
DFL4    LDA  ,Y+
        STA  B,X
        INCB
        CMPB #24
        BNE  DFL4
        PULS Y
*
        CLRA
        LDB  DRVNUM volume number
        STD  27,X
*
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
        LDX  #BOOT
        CLRA         TRACK 0
        LDB  #$01    SECTOR 1
        STA  TRACK
        STB  SECTOR
        LBSR WRITSC
*
UFEXIT  LDX #UFMSG1
        JMP PDATA1
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
*
** 'UX' Xmodem IDE Disk upload
*
UXMES0  FCB $0D,$0A
        FCC 'Xmodem IDE Disk Upload'
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
        LDX  #BOOT
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
** FLEX 9 IDE DISK DRIVERS
*
* FOR SYS09BUG 1.2 ON THE XSA-3S1000
* WITH I/O MAPPED AT $XE000
* AND ROM MAPPED AT $XF000
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
       LBRA WTRDY
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
       STD  CF_SECNUM
       LDB ,S
       STD  CF_CYLLO
       LDB  DRVNUM
       STD  CF_CYLHI
       LDB  #$01
       STD  CF_SECCNT
       CLRB
       PULS A,PC
*
* READ SECTORS FROM CF
*
*
READSC BSR  SEEKTS
       LDD  #CMDREAD ; IDE READ MULTIPLE
       STD  CF_COMAND
       LBSR  WTRDY
*
* READ LOOP
*
       PSHS Y
       LDY #256
RDLP1  LBSR  WTDRQ
       LDD  CF_DATA
       STB  ,X+
       LEAY -1,Y
       BNE  RDLP1
       PULS Y
*
       LBSR  WTRDY
       CLRB
       RTS
*  
* WRITE SECTOR TO CF
*  
WRITSC BSR  SEEKTS   ; SEEK TRACK & SECTOR
       LDD  #CMDWRITE ; IDE WRITE MULTIPLE
       STD  CF_COMAND
       LBSR  WTRDY
*
* WRITE LOOP
*
       PSHS Y
       LDY #256
       CLRA
WRTLP1 LBSR  WTDRQ
       LDB  ,X+
       STD  CF_DATA
       LEAY -1,Y
       BNE  WRTLP1
       PULS Y
*
       LBSR  WTRDY
       CLRB
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
*******************************************************
*
* Bootstrap FLEX Loader
*
* SBUG1.8 loads the bootstap loader at $C000
* however the Flex adaption manual has the
* bootstrap loader residing at $C100
* Bootstrap Loader is position independent code
*
******************************************************
*
* Equates
*
BSSTACK EQU $C0FF
SCTBUF  EQU $C300
*
* Start of Utility
*
        ORG MONEXT+$0700
BOOT    BRA BLOAD0
        FCB 0,0,0
TRK     FCB 0        File start track
SCT     FCB 0        File start sector
DNS     FCB 0        Density Flag (not used)
TADR    FDB $C000    Transfer address
LADR    FDB 0        Load Address
DRNUM   FCB 0        Drive number 0
*
BLOAD0  LDS  #BSSTACK   Set up Bootstrap stack
        LDD  TRK,PCR    Set up start track and sector
        STD  SCTBUF
        LDY  #SCTBUF+256
*
* Perform actual file load
*
BLOAD1  BSR GETCH    Get acharcater
        CMPA #$02    Data record hearder ?
        BEQ  BLOAD2   Skip, is so
        CMPA #$16    Xfr address hearder ?
        BNE BLOAD1    Loop if neither
*
* Get transfer address
*
        BSR  GETCH
        STA  TADR,PCR
        BSR  GETCH
        STA  TADR+1,PCR
        BRA  BLOAD1
*
* Load data record
*
BLOAD2 BSR  GETCH  Get load address
       STA  LADR,PCR
       BSR  GETCH
       STA  LADR+1,PCR
       BSR  GETCH  Get Bytes count
       TFR  A,B
       TSTB
       BEQ  BLOAD1 Loop if count = 0
       LDX  LADR,PCR  Get load address
BLOAD3 PSHS B,X
       BSR  GETCH  Get data character
       PULS B,X
       STA  ,X+    Store at load address
       DECB
       BNE  BLOAD3  Loop until count = 0
       BRA  BLOAD1
*
* Get Character routine
* Reads a sector if needed
*
GETCH  CMPY #SCTBUF+256 out of data ?
       BNE  GETCH4      Go read Character if not
GETCH2 LDX  #SCTBUF     Point to buffer
       LDD  0,X         Get forward Link
       BEQ  GOFLEX      if zero, file is loaded
       BSR  READ        Read next sector
       BNE  BOOT        start over if error
       LDY  #SCTBUF+4   Point past link
GETCH4 LDA  ,Y+         Else, get a character
       RTS
*
* File is loaded, Jump to it
*
GOFLEX JMP  [TADR,PCR]      Jump to transfer address

*
** FLEX 9 IDE DISK DRIVERS
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
       STD  CF_SECNUM
       LDB  ,S
       STD  CF_CYLLO
       LDB  DRNUM,PCR
       STD  CF_CYLHI
       LDB  #$01
       STD  CF_SECCNT
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
        BITB #BUSY
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
       END
