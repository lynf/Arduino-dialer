/*
 *eeprom_module.asm
 *
 *  Created: 3/18/2014 1:26:05 PM
 *   Author: lynf
 */ 
;
;
;######################################################################################
; This software is Copyright by Francis Lyn and is issued under the following license:
;
; Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License
;
;######################################################################################
;
;
; INCLUDE this module in main code
; 
; ATmega8L cpu, 16.0000 MHz external crystal.
; HFUSE: EF, LFUSE: D9
;
; Polled service routines for EEPROM random read and write. Read routine
; enters with EEPROM address specified in (rgb:rga). Routine return with
; EEPROM data in (rmp).
;
; Write routine enters with EEPROM address specified in (rgb:rga), and data
; to write in (rmp).
;
; Sequential read routine does not specify an EPROM start address but instead
; uses current (EEARH:EEARL) pointing to the last accessed EEPROM data.
; (EEARH:EEARL) is incremented by 1, then routine returns with EEPROM data in (rmp).
;
; Sequential read routine does not specify an EPROM start address but instead
; uses current (EEARH:EEARL) pointing to the last accessed EEPROM data, and with
; data to write in (rmp). (EEARH:EEARL) is incremented by 1, then routine writes
; (rmp) to EEPROM.
;
; Registers: rmp, Y
;
;***************************************************************************
;* 
;*		(OK)
;*
;* Wait until the EEPROM is ready to be programmed, then programs the EEPROM
;* with register variable "rmp" at address "YH:YL"
;*
;*
;***************************************************************************
;
;	Entry:
;	rmp			; data byte to write to EEPROM
;	YL			; address low byte to write to
;	YH			; address high byte to write to
;
EEWrite:
	sbic	EECR,EEPE		; If EEPE not clear
	rjmp	EEWrite			;	wait more

	out 	EEARH,YH		; Load EEPROM address high byte
	out		EEARL,YL		; Load EEPROM address low byte
		

	out		EEDR,rmp		; output data to EEPROM
	cli						; Disable global interrupts	
	sbi 	EECR,EEMPE		; set master write enable	
	sbi		EECR,EEPE		; set EEPROM Write strobe
							; This instruction takes 4 clock cycles since
							; it halts the CPU for two clock cycles
	sei						; Enable global interrupts
	ret

;***************************************************************************
;* 
;* EERead		(OK)
;*
;* Wait until the EEPROM is ready to be programmed, then
;* read the register variable "rmp" from address "YH:YL"
;*
;*
;***************************************************************************
;
;	Entry:
;	YL			; EEPROM address low to read from
;	YH			; EEPROM address high to read from
;
;	Exit:
;	rmp			; result data byte
;
EERead:
	sbic	EECR,EEPE		; If EEPE not clear
	rjmp	EERead			;	wait more

	out 	EEARH,YH		; output EEPROM address high byte
	out		EEARL,YL		; output EEPROM address low byte


	sbi		EECR,EERE		; set EEPROM Read strobe
							; This instruction takes 4 clock cycles since
							; it halts the CPU for two clock cycles
	in		rmp,EEDR		; get EEPROM data
	ret

		
;***************************************************************************
;* 
;* EEWrSeq		(OK)
;*
;* Increment the address stored in EEAR and write the register variable
;*  "rmp" data into the EEPROM.
;*
;*
;***************************************************************************
;
;	YL			; Temporary storage of address low byte
;	YH			; Temporary storage of address high byte
;
;	Entry:
;	rmp			; Data to write to EEPROM 
;

EEWrSeq:
	sbic	EECR,EEPE		; If EEPE not clear
	rjmp	EEWrSeq			;	wait more
;
	in		YL,EEARL		; Get address low byte from EEPROM address register
	in		YH,EEARH		; Get address high byte from EEPROM address register
 	adiw	Y,1				; Increment address
	out		EEARL,YL		; Output address low byte
	out		EEARH,YH		; Output address high byte
;	
	out		EEDR,rmp		; Write data to EEPROM
	cli						; Disable global interrupts
	sbi 	EECR,EEMPE		; Set master write enable
	sbi		EECR,EEPE		; Set EEPROM Write strobe
							; This instruction takes 4 clock cycles since
							; It halts the CPU for two clock cycles
	sei						; Enable global interrupts
	ret
;
;***************************************************************************
;* 
;* EERdSeq		(OK)
;*
;* Increment the address stored in EEAR and read the EEPROM data into the
;* register variable "rmp"
;* 
;*
;***************************************************************************
;
;	YL			; Temporary storage of address low byte
;	YH			; Temporary storage of address high byte
;
;	Exit:
;	rmp			; EEPROM data read
;

EERdSeq:
	sbic		EECR,EEPE	; If EEPE not clear
	rjmp		EERdSeq		;	wait more

; Read sequence
	in		YL,EEARL		; Get address low byte from EEPROM address register
	in		YH,EEARH		; Get address high byte from EEPROM address register
	adiw	Y,1				; Increment address
	out		EEARL,YL		; Output address low byte
	out		EEARH,YH		; Output address high byte

	sbi		EECR,EERE		; Set EEPROM Read strobe
							; This instruction takes 4 clock cycles since
							; It halts the CPU for two clock cycles
	in		rmp,EEDR		; Get data
	ret
;
;
; End of source code
;
;####################################################################################
;
.EXIT
;
;####################################################################################
;
;
