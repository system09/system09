*
** FLEX 9 COMPACT FLASH / IDE DISK DRIVERS
*
* FOR SYS09BUG ON THE XESS XSA-3S1000 & XST3.0 
* WITH I/O MAPPED AT $XE000
* AND ROM MAPPED AT $XF000
* 
* These drivers should work with either
* an IDE disk drive or a IDE CF adapter card
* It uses 16 bit transfer mode, 
* as some IDE drives do not support
* the set feature command found on CF cards
* that allows for 8 bit operation
*
*
IMASK  EQU $10     IRQ MASK CC
FMASK  EQU $40     FIRQ MASK CC
*
CF_BASE    EQU $E100
CF_DATA    EQU CF_BASE+0
CF_ERROR   EQU CF_BASE+2 ; read error
CF_FEATURE EQU CF_BASE+2 ; write feature
CF_SECCNT  EQU CF_BASE+4
CF_SECNUM  EQU CF_BASE+6
CF_CYLLO   EQU CF_BASE+8
CF_CYLHI   EQU CF_BASE+10
CF_HEAD    EQU CF_BASE+12
CF_STATUS  EQU CF_BASE+14 ; read status
CF_COMAND  EQU CF_BASE+14 ; write command
CF_AUX     EQU CF_BASE+30 ; Reset register
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
       ORG   $DE00
*  
* DISK DRIVER JUMP TABLE
*
READ   JMP   READSC
WRITE  JMP   WRITSC
VERIFY JMP   BUSY
RESTOR JMP   RESTR1
DRIVE  JMP   DRVSEL
DRVRDY JMP   CHKDRV
QUICK  JMP   CHKDRV
COLDDR JMP   INITDR
WARMDR JMP   WARMD1
SEEK   JMP   SEEKTS
*
* RAM SPACE
*
DRVNUM FCB   0  
*
*
* INITIALIZE CF CARD
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
SEEKTS DECB
       PSHS A
       CLRA
       STD  CF_SECNUM
       LDB ,S
       STD  CF_CYLLO
       LDB  DRVNUM
       STD  CF_CYLHI
       LDB  #$01
       STD  CF_SECCNT
       PULS A
       CLRB
WARMD1 RTS
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
       LDY  #256
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
       LDD  #CMDWRITE; IDE WRITE MULTIPLE
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
        END

