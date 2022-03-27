;       EV96 module main
;       ================
;
; This file contains a code to operate the 87C196KC mx board.
; Reset location is (2080H).
;
; The serial link is provided by an internal UART. 
;
;
;   Memory partitions
;                         (0000H-0017H) I/O control registersor SFRs (internal RAM)
;                         (0018H-0019H) STACK POINTER (internal RAM)
;                         (001AH-00FFH) INTERNAL DATA MEMORY 
;                         (0100H-1FFDH) EXTERNAL MEMORY OR I/O                         
;                         (1FFEH-1FFFH) PORT 3 AND PORT 4
;                         (2000H-2013H) INTERRUPT VECTORS
;                               (2014H) PPW
;                         (2015H-2017H) RESERVED
;                               (2018H) CHIP CONFIGURATION BYTE
;                         (2019H-201FH) RESERVED
;                         (2020H-202FH) SECURITY KEY
;                         (2030H-203FH) INTERRUPT VECTORS
;                         (2040H-206FH) RESERVED
;                         (2070H-2071H) SIGNATURE WORD
;                         (2072H-2073H) VOLTAGE LEVELS
;                         (2074H-207FH) RESERVED
;                         (2080H-7FFFH) ROM 
;						  (8000H-FFFFH) RAM		
;
;               ( Note that all of these partitions, (except 1D00H-1EFFH and 
;                 2018H), are reserved by the MCS-96 architecture. )
; 
	cpu	80196
	
;	QWER: equ  78H

	org 00h
	
	db 0ffh
;

	org 28h

chr:  		db 0FFh
sptemp: 	db 0FFh
temp0:  	db 0FFh
temp1:  	db 0FFh
rcv_flag: 	db 0FFh

temp3		db 0FFh
temp4		db 0FFh
tempio		db 0FFh
COUNT       dw 0FFFFh
;
	org 200ch
	
	dw ser_port_int
;
	org 2080H
	
;
; Define symbols for the register mapped I/O locations
; ----------------------------------------------------
;
zero            equ   00H      ; R/W   Zero Register
ad_command      equ   02H      ;   W   A to D command register
ad_result_lo    equ   02H      ; R     Low byte of result and channel 
ad_result_hi    equ   03H      ; R     High byte of result
hsi_mode        equ   03H      ;   W   Controls HSI transition detector
hsi_time        equ   04H      ; R     HSI time tag
hso_time        equ   04H      ;   W   HSO time tag  
hsi_status      equ   06H      ; R     HSI status register (reads fifo)
hso_command     equ   06H      ;   W   HSO command tag
sbuf            equ   07H      ; R/W   Serial port buffer
int_mask        equ   08H      ; R/W   Interrupt mask register
int_pending     equ   09H      ; R/W   Interrupt pending register
spcon           equ   11H      ;   W   Serial port control register 
spstat          equ   11H      ; R     Serial port status register
watchdog        equ   0AH      ;   W   Watchdog timer
timer1          equ   0AH      ; R     Timer1 register
timer2          equ   0CH      ; R     Timer2 register
port0           equ   0EH      ; R     I/O port 0
baud_reg        equ   0EH      ;   W   Baud rate register
ioport1         equ   0FH      ; R/W   I/O port 1
ioport2         equ   10H      ; R/W   I/O port 2
ioc0            equ   15H      ;   W   I/O control register 0 (HSI/O)
ios0            equ   15H      ; R     I/O status register 0
ioc1            equ   16H      ;   W   I/O control register 1 (Port2)
ios1            equ   16H      ; R     I/O status register 1
pwm_control     equ   17H      ;   W   PWM control register 
sp              equ   18H      ; R/W   System stack pointer
;
; This section defines utility macros non-specific to this program
; ----------------------------------------------------------------
;
DEFINE_BIT      macro   name,bitnum
                name    equ bitnum
                endm

SET_BIT         macro   regnum,bitnum
                orb     regnum,#( 1 SHL (bitnum mod 8)  )
                endm

CLR_BIT         macro   regnum,bitnum
                andb    regnum,#not( 1 SHL (bitnum mod 8) )
                endm

; BL             macro   label
;                bnc     label
;                endm
;$eject
;
; This section contains EQUates which may change with different versions
; ----------------------------------------------------------------------
;
;;;;;;;;;offset  equ     8000H           ; Code offset before REMAP           
;
; Tell the commands what to use for psw while monitor is running
;
;;;;;;;;;;;;;;;rism_psw	equ	0000H		; No Interrupts enabled
;
; This section contains several macros generate specifically for this program
; ---------------------------------------------------------------------------
;
; ENTER_RISM
;       A macro which generates the prologue for the RISM ISR
;
; EXIT_RISM
;       A macro which generates the epilogue for the RISM ISR
;
; SEND_DATA_BYTE
;       A macro which passes the lower eight bits of RISM_DATA to
;       the serial port, it assumes the port is ready for data
;
; BYTE_PROTECT
;       A macro which terminates the RISM ISR if the RISM is about
;       to write into a byte it should not modify.
;
; WORD_PROTECT
;       A macro which terminates the RISM ISR if the RISM is about
;       to write into a word it should not modify.
;
; DWORD_PROTECT
;       A macro which terminates the RISM ISR if the RISM is about
;       to write into a double-word it should not modify.
;
;$eject
ENTER_RISM      macro
                pushf
                endm
                
EXIT_RISM       macro
                popf
                ret
                endm

SEND_DATA_BYTE  macro
                stb     RISM_DATA, txd_rxd[0]
                endm

BYTE_PROTECT    macro                   ; No special protection
                endm

WORD_PROTECT    macro                   ; No special protection               
                endm

DWORD_PROTECT   macro                   ; No special protection
                endm
;;$eject
;
; These registers are used only by the diagnostic routines.
; ---------------------------------------------------------
; They are not required for normal execution.
;
    org 2080h
;	-----------

	ld	sp, #0E100h
	ldb	ioc1, #00100000b

baud_high	equ 80h
baud_low	equ	4dh
 	
	ldb	baud_reg, #baud_low
	ldb	baud_reg, #baud_high
	
	ldb spcon,	#01001001b
	
	stb	sbuf, chr
	stb	chr, 8100h
	ldb chr, 8100h
	ldb	temp0, #00100000b
	
	ldb	int_mask, #01000000b
	ei
	
	LDB  tempio, IOPORT2
	XORB tempio, #01000000B
;	ORB  tempio, #10000000B
	STB  tempio, IOPORT2
	
loop:
;	
	LD COUNT, #0FFh
;
	LDB  tempio, IOPORT2
	XORB tempio, #01000000B
;	ORB  tempio, #10000000B
	STB  tempio, IOPORT2
;
LOOP2:
;
	DJNZ COUNT, LOOP2 ;loop 0FFh times
;	
	LDB  tempio, IOPORT2
	XORB tempio, #01000000B
;	ORB  tempio, #10000000B
	STB  tempio, IOPORT2	
;	
	br loop
	
ser_port_int:
	pushf
	
rd_again:
	ldb		sptemp, spstat
	orb		temp0, sptemp
	andb	sptemp, #01100000b
	jne		rd_again
	
get_byte:
	jbc		temp0, 6, put_byte
	stb		sbuf, chr
	stb	chr, 8200h
	ldb chr, 8200h
	andb	temp0, #10111111b
	ldb		rcv_flag, #0FFh
	
put_byte:
	jbc		rcv_flag, 0, continue
	jbc		temp0, 5, continue
	ldb		sbuf, chr
	andb	temp0, #11011111b
	
;	andb	chr, #01111111b
	andb	chr, #01111111b
	cmpb	chr, #0Dh
	jne		clr_rcv
	ldb		chr, #0Ah
	br		continue
	
clr_rcv:
;	
;                            I/O Test is OK		
;
	cmpb	chr, #31h
	jne		n_c1
	ldb		temp3, 0C000H    ;-4000H Read from D71055C (2)
	br      clr_rcv2
n_c1:	
	cmpb	chr, #32h
	jne		n_c2
	ldb		temp3, 0C800H    ;-3800H Read from D71055C (1)	
	br      clr_rcv2
n_c2:	
	cmpb	chr, #33h
	jne		n_c3
	ldb		temp3, 0D000H    ;-3000HRead from ADC TC7109
	br      clr_rcv2
n_c3:	
	cmpb	chr, #34h
	jne		n_c4
	ldb		temp3, 0D800H    ;-2800HRead from READY
	br      clr_rcv2
n_c4:	
	cmpb	chr, #35h
	jne		clr_rcv2
	ldb		temp3, 0E000H    ;-2000H Read from RAM
;	
;	
clr_rcv2:
	clrb	rcv_flag
	
continue:
	popf
	ret
	

	

























	
	
