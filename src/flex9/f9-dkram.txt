*
** FLEX 9 DISK DRIVERS
*
* FOR SYS09BUG ON THE DIGILENT SPARTAN 3 STARTER BOARD
* WITH I/O MAPPED AT $XE000
* AND ROM MAPPED AT $XF000
* THE DIGILENT SPARTAN 3 STARTER BOARD HAS 1MBYTE OF SRAM
* THE FIRST 64K IS USED BY FLEX,
* THE SECOND 192K IS USED AS A ROM DISK
* THE REMAINING RAM IS USED FOR A RAM DISK
*
* These drivers should also work on the B5-X300 board
* although there is only enough room for the ROM Disk.
*
IMASK  EQU $10     IRQ MASK CC
FMASK  EQU $40     FIRQ MASK CC
TRPAGE EQU $0E     PAGE $E000 DAT ADDRESS
DATREG EQU $FFF0   DAT REGISTERS
       ORG   $DE00
*  
* DISK DRIVER JUMP TABLE LAST UPDATE: 22/12/2006
* Disk driver for RAM Disk.
*
* 14 SECTORS PER TRACK
* 16 * N TRACKS PER DISK
*
* ROM DISK OCCUPIES $10000 - $1E000, $20000 - $2E000, $30000 - $3E000
* RAM DISK OCCUPIES $40000 - $4E000 ... $F0000 - $FE000
* Track Buffer page mapped at $E000 - $EFFF
* TRPAGE = $0E = 14 x $1000 (4 K pages)
* LEAST SIGNIFICANT NYBBLE OF THE DAT IS INVERTED
* ON SWTPC ROM AT $XF000 AND IO AT $XE000
* APPEARS THROUGHOUT THE MEMORY SO MUST BE SKIPPED OVER
* WHEN USING RAM AS A RAMDISK.
* THE MSN OF THE TRACK MAPS INTO THE MSN OF THE DAT
* THE LSN OF THE TRACK NUMBER INDEXES INTO THE 4K RAM PAGE
* THE SECTOR MAPS INTO THE LSN OF THE DAT WHICH IS INVERTED
* 
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
WRKDR  FDB   0  
WRKTR  FDB   0,0,0,0
SECPTR FDB   0
TRKCNT FCB   0
SECCNT FCB   0
VERERR FCB   0
CCSAVE FCB   0
*
DRVBAS FCB   $10  DRIVE 0
       FCB   $40  DRIVE 1 
       FCB   $40  DRIVE 2 
       FCB   $40  DRIVE 3 
*
INITDR RTS
WARMD1 RTS
*
* Seek track and sector
* A holds track number (0-32)
* B holds sector number (1-14)
*
SEEKTS STA  TRKCNT
       STB  SECCNT
       ANDCC  #$FE   ; CLEAR CARRY
       ORCC   #$40   ; SET Z
       RTS
*
* MAP RAM DISK INTO I/O SPACE
*
MAPIN  TFR   CC,A     ; Save state of interrupt masks
       STA   CCSAVE
       ORCC  #FMASK+IMASK ; Mask interrupts while IO mapped out
       LDA   TRKCNT
       LDU   #DRVBAS  ; Point to Drive base offset
       LDB   WRKDR    ; Get working drive number
       ADDA  B,U      ; Add Base offset into RAM
       ANDA  #$F0     ; Mask MSN
       STA   ,-S      ; Save A on stack
*
       LDA   SECCNT
       SUBA  #1       ; Sectors 1 to 14 => 0 to 13
       EORA  #$0F     ; Complement LSNybble
       ANDA  #$0F
*
       ADDA  ,S+       ; Add sector to LSN of Track and pop
       STA   DATREG+TRPAGE
*
       LDA   TRKCNT   ; LSN of Track indexes into 4K page
       ANDA  #$0F
       ADDA  #TRPAGE*16
       STA   SECPTR
       CLR   SECPTR+1
       LDU   SECPTR
       RTS
*
* MAP RAM DISK OUT OF MEMORY
*
MAPOUT LDA   #TRPAGE  ; Point to the I/O page
       EORA  #$0F     ; Complement LSNybble
       STA   DATREG+TRPAGE ; map in I/O page
       LDA   CCSAVE   ; restore interrupt masks
       TFR   A,CC
       RTS
*  
* READ DISK DRIVER
*  
READSC LBSR  SEEKTS   ; SEEK TRACK & SECTOR
       PSHS  U,X
       BSR   MAPIN    ; MAP RAM DISK INTO I/O SPACE
*
       CLRB
READ3  LDA  ,U+
       STA  ,X+       ; Move Sector to FCB
       INCB
       BNE   READ3
*
       BSR  MAPOUT    ; MAP RAM DISK OUT OF I/O SPACE
       CLRB           ; Z SET C CLEAR
       PULS U,X,PC    ; Restore registers and return  
*  
* WRITE DISK DRIVER
*  
WRITSC BSR   SEEKTS   ; SEEK TRACK & SECTOR
       PSHS  U,X
       BSR   MAPIN    ; MAP RAM DISK INTO I/O SPACE
*
       CLRB
WRIT3  LDA   ,X+      ; COPY FCB BLOCK TO RAM DISK 
       STA   ,U+
       INCB
       BNE    WRIT3
*
       BSR    MAPOUT  ; MAP OUT RAM DISK
       CLRB           ; SET Z, CLEAR C
       PULS   U,X,PC  ; Restore registers and return
*  
* RESTORE DISK DRIVER (SEEK TRACK 00)
*  
RESTR1 PSHS A
       CLRA           ; Track 0
       LDAB  #$01     ; Sector 1
       LBSR  SEEKTS
       PULS  A,PC
*  
* CHECK FOR BUSY  
* Doubles as VERIFY
*  
BUSY   CLRB            Never busy
       RTS
*  
* DRIVE SELECT DISK DRIVER
*  
DRVSEL PSHS  X
       LDA   3,X       GET DRIVE # FROM FCB
       CMPA  #3  
       BLS   DRVS2     IF > 3, SET IT TO 0  
       CLRA  
DRVS2  BSR   MXWT      MOVE X TO WORKING TRK  
       LDB   TRKCNT
       STB   0,X       SAVE TRACK
       STA   WRKDR     SAVE DESIRED DRV AS WORKING DRV
       BSR   MXWT      MOVE X TO WORKING TRK  
       LDA   0,X       GET WORKING TRK ON DESIRED DRV
       STA   TRKCNT    UPDATE 1771 TRACK REG
       CLRB            ; SET Z, CLEAR C
       PULS   X,PC
*  
* MOVE INDEX REG TO POINT TO  
* WORKING TRACK STORAGE  
*  
MXWT   LDX   #WRKTR    POINT TO START OF STG  
       LDB   WRKDR     GET WORKING DRIVE
       ABX
MXWT2  RTS  
*  
* CHECK DRIVE READY DISK DRIVER
*  
CHKDRV LDA   3,X
       CLRB             ; CLEAR C, SET Z
       RTS  
 END
