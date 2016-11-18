/*
 * holtek-autodialler.asm
 *
 *  Created: 8/23/2016 5:39:51 PM
 *   Author: lynf
 */
;
;	2016-Sept-01 revised for ATmega328P
;
;
;######################################################################################
; This software is Copyright by Francis Lyn and is issued under the following license:
;
; Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License
;
;######################################################################################
;
; Change Log:
; ===========
; 1. Revise <initz:> to enable pull-ups on all unused ports as precaution
;	to reduce unnecessary device current consupmtion.
; 
;
;
; ATmega328P cpu, 16.0000 MHz external crystal.
; HFUSE: DF, LFUSE: F7
;
;
; ATmega328P Nano controller and interface board with telephone style keypad, hook relay, red
; and blue LED indicators. Store up to fortyone 20 digit telephone numbers (telno) in internal EEPROM,
; dials any stored number at the specified 2 digit telno index (address).
;
; Autodialer has 2 operating modes, the RUN and PROGRAM modes. Autodialer starts up in the RUN mode. Special
; key sequences are used to toggle between operating modes, enter telno numbers and index numbers,
; and to cancel the current entry. These are:
;
;		99# toggle between RUN and PROGRAM modes
;		nn# enter the 1 or 2 digit index number nn
;		*   cancel the current operation
;
; Index 99 is used to toggle the dialer between the run and the program modes. The redled
; comes on in the program mode. The bluled comes on in the run mode.
;
; Programming the dialer. Press 99# to enter the program mode. Enter a 2 digit index, followed
; by the telno associated with the index, followed by the # key. For example, to program telno
; 416 745 0172 at index 00, press the following keys:
;
;	007450172#
;
; Load the remaining indeces and telnos in this manner. When finished, return to the run mode
; by entering 99# again. Up to 41 indeces (00 through 40) are accepted. Each telno can be up
; to 20 digits long. When entering index number in programming mode, the complete 2 digit index
; number must be entered. This is different to operation in run mode where a single digit index
; number may be entered and recognized.
;
; Dialing a number is done only from the run mode by entering the desired index number and
; pressing the # key. The dialer will accept a 1 digit index number only for indeces 0 through 9.
; To call indeces 10 through 40, you must enter the 2 digits. For example, to call the telno stored
; at 09 index, press 9#. Note you can still enter 09# as a valid index entry.
;
; After dialing our a number in the run mode, pressing the '#' without entering an index number will
; redial the previously entered index number. For example if 9# is pressed the telno stored at index
; 09 is dialed out. Pressing '#' again redials the telno stored at index 09 again.
;
; The '*' key acts as a reset key during many of the key entry operations. Press the '*' key if
; you make an error in the entry of any number and redo the number entry again.
;
; Changes:
; ========
; 1. Increase size of telno storage from 21 to 41 since EEPROM is 1 kB size.
;	 Revise buffer size testing in <prgm4>, <inztsb>, <runm4>
; 2. Revise <prgm3a:> to remove test for 1 digit index.
;
;
; Port pin assignments:
; PD7...4 keypad row scan return lines, Row 4...1
; PC3...0 keypad column scan output lines, Col 4...1 (Col 1 is for 4x4 keypad, not used on 4x3 keypad)
;
; PB0 is HOOK relay output driver
; PB1 is blue LED indicator driver
; PB2 is red LED indicator driver
;
; PB3...5 are control outputs to tone generator:
;
; PB3 data output to HT9200A
; PB4 clock outputto HT9200A
; PB5 nce output to HT9200A
;
;
;

.list						; Listing on


;
; General equates
;
.equ	FALSE = 0x0			; Logical 0
.equ	TRUE = !FALSE		; Logical 1
;.equ	Vcc_low = true		; For 3.3 V controller board, UNO
.equ	Vcc_low = false		; For 5 V controller board, UNO
;.equ	baud_low = true		; 9600 baud for later boards
.equ	baud_low = false	; 19200 baud for 1st board
;
; Processor operating voltage
;

.if	Vcc_low
.equ F_CPU = 8000000
.else
.equ F_CPU = 16000000
.endif

;
;
; USART definitions
;
.if		baud_low
.equ	BAUD = 9600			; Baud rate
.if		Vcc_low
.equ	BAUD_PRE = 51		; Baud rate prescaler - 8.00 MHz clock
.else
.equ	BAUD_PRE = 103		; Baud rate prescaler - 16.00 MHz clock
.endif
;
.else
;
.equ	BAUD = 19200		; Baud rate
.if		Vcc_low
.equ	BAUD_PRE = 25		; Baud rate prescaler - 8.00 MHz clock
.else
.equ	BAUD_PRE = 51		; Baud rate prescaler - 16.00 MHz clock
.endif
;
.endif
;
.equ	NULL = 0x0			; Null terminator
.equ	BELL = 0x07			; Bell
.equ	BS = 0x08			; Backspace
.equ	HT = 0x09			; Tab
.equ	LF = 0x0a			; Linefeed
.equ	CR = 0x0d			; Carriage return
.equ	ctlW = 0x17			; Control W
.equ	ctlX = 0x18			; Control X
.equ	ctlZ = 0x1a			; Control Z
.equ	SP = 0x20			; Space
.equ	ESC = 0x1b			; Escape
.equ	DEL = 0x7f			; Delete
.equ	CMA	= 0x2c			; Comma
;
.equ	sblen = 0x1f		; Serial input buffer length
;
;
; Timer0 parameters
;

.if		Vcc_low
.equ	OCR0Aload = 32		; OCR0A 8 bit register, 32 x 128 us = 4.096 ms
.else
.equ	OCR0Aload = 64		; OCR0A 8 bit register, 64 x 64 us = 4.096 ms
.endif

;
; Timer2 parameters
;
.equ	OCR2Aload = 64		; OCR2A 8 bit register, 64 x 0.5 us = 32 us
;
;
; Keypad switch scanner
;
.equ	NCOLS = 4			; Number of keypad columns
.equ	NROWS = 4			; Number of keypad rows
.equ	DBNCE = 4			; Number of debounce cycles
;
;
; Register flaga
;
.EQU	SWAPF = 0			; Swap control flag
.equ	t500f = 1			; 500 ms tick flag
.equ	t70f = 2			; 70 ms tick flag
.equ	t100f = 3			; 100 ms tick flag
.equ	KYRDY = 4			; Debounced key ready flag
.equ	KYDWN = 5			; Key down sensed flag
.equ	KYUP = 6			; All keys up flag
.equ	FULL = 7			; Telephone number buffer full flag
;
; Register flagb
;
.equ	spirdyb = 0			; SPI ready flag
.equ	startfb = 1			; SPI start flag
.equ	t4fb = 2			; 4 ms flag, flagb register


;
; HC2900A dtmf generator chip equates
;
; Buffer size definitions for storing 21 telnos, 18 digits per telno
;
;.equ	tisize = 10			; Size of tibuff, 18 bcd numbers plus end byte
;.equ	tssize = 210		; Size of tsbuff, 21 x 18 numbers = 21 x 10 bytes = 210
;.equ	maxdgpair = 9		; Maximum number of digit-pairs in telno
;
; Buffer size definitions for storing 41 telnos, 20 digits per telno
;
.equ	tisize = 11			; Size of tibuff, 20 bcd numbers plus end byte
.equ	tssize = 451		; Size of tsbuff, 21 x 18 numbers = 41 x 11 bytes = 451
.equ	maxdgpair = 10		; Maximum number of digit-pairs in telno
;
;
.equ	t70v = 17 			; Timer 0 reload value, 70 ms
.equ	t100v = 24 			; Timer 1 reload value, 100 ms
.equ	t500v = 122			; Timer 2 reload value, 500 ms
.equ	HOOK =	PB0			; HOOK relay driver
.equ	bluled = PB1		; Blue LED driver
.equ	redled = PB2		; Red LED driver

;
; SPI interface signals to HC9200A
;
.equ	data = PB3			; Data output
.equ	clk = PB4			; Clock output
.equ	nce = PB5			; Chip enable output

.equ	nbits = 6			; 5 data bits + 1
;
; Register definitions
;
.def	LASTKY = R2			; Keycode of last key pressed
.def	INKY = R3			; Save key for next test
.def	KYNUM = R4			; Key matrix number buffer
.def	KYCODE = R5			; Debounced key number buffer
.def	ROW = R6			; Row counter
.def	COL = R7			; Column counter
.def	REPTS = R8			; Debounce counter
.def	SRsav = R9			; SREG save
.def	tm0v = R10			; Timer 0 count buffer
.def	tm1v = R11			; Timer 1 count buffer
.def	tm2v = R12			; Timer 2 count buffer
.def	inbuf = R13			; Keypad entry holding buffer
.def	indexb = R14		; Index buffer
;
.def	rmp = R16			; Multipurpose register
.def	rga = R17			; GP register RGA
.def	rgb = R18			; GP register RGB
.def	rgc = R19			; GP register RGC
.def	rgd = R20			; GP register RGD
.def	rge	= R21			; GP register RGE
.def	rgu	= R22			; Variable register
.def	rgv	= R23			; Variable register
.def	flaga = R24			; Flag A register, 8 flags
.def	flagb = R25			; Flag B register, 8 flags
;
; --- Macro definitions ---
;
.macro	ldzptr				; Load ZH:ZL pointer with address*2
		ldi		ZH,high(@0*2)
		ldi		ZL,low(@0*2)
.endm
;
.macro	ldxptr				; Load XH:XL pointer with address to access data memory
		ldi		XH,high(@0)
		ldi		XL,low(@0)
.endm
;
.macro	ldyptr				; Load YH:YL pointer with address to access data memory
		ldi		YH,high(@0)
		ldi		YL,low(@0)
.endm
;
; Exchange contents of registers
;
.macro	xchreg				; Exchange registers
		push	@0
		push	@1
		pop		@0
		pop		@1
.endm
;
;
; ============================================
;       E E P R O M   D E F I N I T I O N S
; ============================================
;
; m328P device has 1KB EEPROM, 3ff bytes
;
.ESEG
.ORG	0x0
;
;tsbuff:
;.byte	tssize					; Space for storing 21 each, 
								; 18 digit phone numbers (in packed bcd format)
;
tsbuff:
.byte	tssize					; Space for storing 41 each, 
								; 20 digit phone numbers (in packed bcd format)
;
;
; ============================================
;       S R A M   D E F I N I T I O N S
; ============================================
;
.DSEG
.ORG		0x0100
;
;  Telephone number input storage buffer
;
tibuff:
.byte	tisize					; Space for saving 20 bcd numbers, plus end marker byte
;
bitcount:						; SPI bit transmitter counter
.byte	1
;
spidata:						; SPI data buffer
.byte	1
;
;
; USART serial input buffer
;
uart_st:
sinb:						; Serial input buffer
.byte		sblen
;
getcnt:
.byte		1				; Input buffer getbyte counter
;
putcnt:
.byte		1				; Input buffer putbyte counter
;
;
uart_end:
buffend:
;
;
; ============================================
;   R E S E T   A N D   I N T   V E C T O R S
; ============================================
;
;
.CSEG
.ORG	$0000
		rjmp		Main		; Int vector 1 - Reset vector
;
.ORG	OC0Aaddr
		jmp			Timer0_COMPA	; Timer 0 Output Compare A handler
;
.ORG	OC2Aaddr
		jmp			Timer2_COMPA	; Timer 2 Output Compare A handler

;
.ORG	URXCaddr
		jmp			URXCint		;  USART Rx Complete
;
;
; End of interrupt vectors, start of program code space
;
.ORG	0x0034
;
;
;
; ============================================
;     I N T E R R U P T   S E R V I C E S
; ============================================
;
;
; TCNT0 run in Output Compare mode, using OCR0A register to
; generate output compare interrupt every 4.096 ms. TCNT0 operates
; in Clear Timer on Compare Match (WGM02:0 = 2). OCR0A defines
; the counter's TOP value.
;
; On Compare Match, TCNT0 counter is cleared.
; Clk_t0 = 16 MHz/1024 = 7.8125 kHz, 128 us period.
;
Timer0_COMPA:
		push	rmp					; Save registers
		push	rga
		in		SRsav,SREG
;
		sbr		flagb,(1<<t4fb)		; Set t4fb, 4 ms flag
;
; Service 70 ms, 100 ms, 500 ms delay timers. Count down to 0, reload
; count buffer and set timer flag
;
tbase0:
		dec		tm0v
		brne	tbase0x
		ldi		rmp,t70v
		mov		tm0v,rmp
		sbr		flaga,(1<<t70f)			; 70 ms flag
tbase0x:
;
tbase1:
		dec		tm1v
		brne	tbase1x
		ldi		rmp,t100v
		mov		tm1v,rmp
		sbr		flaga,(1<<t100f)		; 100 ms flag
tbase1x:
;
tbase2:
		dec		tm2v
		brne	tbase2x
		ldi		rmp,t500v
		mov		tm2v,rmp
		sbr		flaga,(1<<t500f)		; Set every 500 ms
tbase2x:
;
;
; --- Keypad scanner service routine ---
;
; Port lines PC3..0 used to scan four column lines (active low on scanned line) sequentially
; every 4 ms, or one column per interrput cycle. Scan direction from left to right. Port lines PB5..2
; read the row return lines, low if key in scanned column is pressed (closed), high otherwise.
; If no key found, LASTKY returns with 0xff.
;
; All four rows are scanned on each interrupt cycle, or per column scan cycle. Scanning
; starts at R1,C4 (key position 0) and proceeds in numerical order to R4,C1 at end of
; scan cycle.
;
;		Col:	4	3	2	1
;   ----------------------------
;	Row:	1 |	3	7	B	F <--- (KYNUM) has keypad matrix number
;			  |
;			2 |	2	6	A	E
;			  |
;			3 |	1	5	9	D
;			  |
;			4 |	0	4	8	C
;			  |
;
;			  |
; The pin assignments for scanning keypad are as follows:
;
; Port		UNO		Keypad
;--------------------------
; PC3		A3		Col 4
; PC2		A2		Col 3
; PC1		A1		Col 2
; PC0		A0		Col 1	(not used)
;
; PD7		D7		Row 1
; PD6		D6		Row 2
; PD5		D5		Row 3
; PD4		D4		Row 4
;
; Back view of keypad used:
;
;	x	0	0	0 |  0	 0	 0	 0	
;  C1  C2  C3  C4 | R4  R3  R2	R1		<-- C1 not used, for 4 column keypad
;  A0  A1  A2  A3 | D7  D6  D5  D4
;
colum:
		mov		rmp,COL
		clc							; Clear carry flag, first bit is 0
		ldi		rga,0xff
colum1:
		rol		rga					; Shift 0 left into register
		dec		rmp
		brne	colum1				; Until rga has scan pattern
;
		out		PORTC,rga			; Write scan code outputs to keypad columns
;
; Scan the row return lines PD7...4 for a key press. Each input line is scanned for
; a low condition, indicating the key on the current column/row address is pressed,
; pulling the input line low.
;
keyscan:
		ldi		rmp,NROWS			; Initialize switch counter
		mov		ROW,rmp
		in		rmp,PIND			; Read the switch inputs
		ori		rmp,0B00001111		; Non-row port lines are masked high
		swap	rmp					; Put return data in low nibble
;
; At this point rmp has return lines image in low nibble. Each line is tested sequentially
; for a low level. When a low is sensed, the current KYNUM value is saved in KYCODE after
; the same key is present for DBNCE scans.
;
;  Entry:	rmp has switch return lines input image
;
nxrow:
		ror		rmp					; Test the switch return line result
		mov		INKY,rmp			; Save the pattern for rest of test
		brcs	nxrow1				; C set if the switch is up
		sbr		flaga,(1<<KYDWN)	; Flag key down
		cbr		flaga,(1<<KYUP)		; Clear all keys up
		mov		rmp,LASTKY			; Save old LASTKY for test
		mov		LASTKY,KYNUM		; Update LASTKY to current key
		eor		rmp,LASTKY			; Old key = this key?
		breq	nxrow2				;  Same key found, do debounce
		ldi		rmp,DBNCE			;  Else reload debounce counter
		mov		REPTS,rmp
		rjmp	nxrow1
;
; Same key found in succession, perform debounce so that key is accepted only after it remains
; the same for DBNCE number of scans.
;
nxrow2:
		mov		rmp,REPTS			; Check number of good scans
		tst		rmp					; Test for 0
		breq	nxrow1				; Continue if 0
		dec		REPTS				; Count down
		brne	nxrow1
		mov		KYCODE,LASTKY		; Load keycode result
		sbr		flaga,(1<<KYRDY)	; Foreground, key is ready
;
; Arrive here at the end of a switch scan, prepare for scanning next switch
;
nxrow1:
		inc		KYNUM				; Prepare to scan next switch
		mov		rmp,INKY			; Get saved return line pattern
		dec		ROW					; Scan all the switches
		brne	nxrow				; Continue with remaining switches if ROW <> 0
;
; Arrive here at end of a complete switch scan cycle
;
nxcol:
		dec		COL					; Count COL
		brne	nxcolex				;  Not 0, continue with remaining columns
;
; Arrive here at end of column scans, restart new scan cycle
;
		eor		KYNUM,KYNUM			; Clear key counter matrix
		ldi		rmp,NCOLS
		mov		COL,rmp				; Reload COL counter
		sbrs	flaga,KYDWN			; Was a key pressed on this scan cycle?
		rjmp	nxcol1				; No, no-key return code
		cbr		flaga,(1<<KYDWN)	; Yes, clear key down flag
		rjmp	nxcolex
nxcol1:
		ldi		rmp,0xff			;  No key pressed, return 0xff
		mov		LASTKY,rmp
		sbr		flaga,(1<<KYUP)		; All keys up
nxcolex:
;
		out		SREG,SRsav			; Restore SREG
		pop		rga
		pop		rmp
		reti
;
;
; TCNT2 run in Output Compare mode, used for software SPI driver
;
; ClkT2 input = 16 MHz/8, prescaler = 8, 0.5 us interrupt rate.
; 64 x 0.5 us = 32 us interrupt rate, 31.25 kHz
;
Timer2_COMPA:
		push	rmp
		push	rga
		in		SRsav,SREG
;
; --- HT9200A communications ---
;
; SPI protocol controls dtmf generaotr chip. Five data bits are
; written serially to the device, LSB first, at 31.25 kHz clock rate
;
; Entry: (spidata) <-- byte to send, only five bits sent
;		 spirdyb flag cleared by user, set at end of transmission
;
		sbrc	flagb,spirdyb		; SPI ready flag set?
		rjmp	spiex				;	Yes, exit
		in		rmp,PINB			; Read Port B
		sbrc	rmp,clk				; Clock lo?
		rjmp	clocklo				;	No, clock hi, drop clock and exit
;
; Clock was lo, raise clock, send data bit
;
		sbi		PORTB,clk			; Raise clock line
spitxbit:
		lds		rmp,spidata			;	Yes, send data
		ror		rmp					; Data bit to C
		sts		spidata,rmp			; Save byte
		brcs	datahi
		cbi		PORTB,data			; Clear data line
		rjmp	spiex
datahi:
		sbi		PORTB,data			; Set data line
		rjmp	spiex
;
; Clock was hi, count bits sent and drop clock if bitcount not 0.
; If bitcount = 0, reload bitcount, set spirdyb and exit with clock hi
; Start flag set causes loading of data bit while clock = 1.
;
spistart:
		cbr		flagb,(1<<startfb)	; Clear the start flag
		rjmp	spitxbit
clocklo:
		sbrc	flagb,startfb
		rjmp	spistart
		lds		rmp,bitcount
		dec		rmp
		sts		bitcount,rmp
		breq	spiend
		cbi		PORTB,clk			; Drop clock line
		rjmp	spiex
spiend:
		ldi		rmp,nbits			; Reload bit counter
		sts		bitcount,rmp
		sbr		flagb,(1<<spirdyb)	; Signal SPI ready
spiex:
		out		SREG,SRsav
		pop		rga
		pop		rmp
		reti
;
;
; USART Receive complete interrupt handler. Interrupt invoked by RXC0 set
; when UDRE buffer has received character ready. Routine puts the received
; character into the SINB serial input buffer and increments the PUTCNT counter.
; When SBLEN+1 counts reached, PUTCNT is rolled back to 0 counts. SINB acts
; as a circular buffer.
;
URXCint:
		in		SRsav,SREG		; Save SREG
		push	rga
		push	rmp
		push	XH
		push	XL
;
		ldxptr	sinb			; Sinb base address
		lds		rmp,putcnt		; Get current putcnt offset
		clr		rga
		add		XL,rmp			; Add putcnt offset to sinb pointer
		adc		XH,rga			; Update pointer (16 bit addition)
		lds		rga,UDR0		; rga <-- UDR0
		st		X,rga			; Store to SINB buffer
		inc		rmp				; Increment putcnt
		cpi		rmp,sblen		; Past end of buffer?
		brne	URXCint1
		clr		rmp				;	Yes, reset putcnt
URXCint1:
		sts		putcnt,rmp		; Update putcnt to next free location
;
		pop		XL
		pop		XH
		pop		rmp
		pop		rga
		out		SREG,SRsav			; Restore SREG
		reti
;
;
;###########################################################################
;
;
; ============================================
;         Initialization Routines
; ============================================
;
; Initialize the controller hardware
;
initz:	
;
; Activate pull-up resistors on all input pins, used and unused
;
		ldi		rmp,0b00111000		; PB5,4,3 SPI lines, PB2,1,0 control outputs
		out		PORTB,rmp
;
		ldi		rmp,0b00110000		; PC3...0 keypad column scan outputs
		out		PORTC,rmp
;
		ldi		rmp,0b11111100		; PD7...4 keypad row scan return inputs
		out		PORTD,rmp
;
;
; Initialize PB5...0 as tone generator control ouptuts
;
		ldi		rmp,(1<<bluled)|(1<<redled)|(1<<HOOK)|(1<<nce)|(1<<clk)|(1<<data)
		out		DDRB,rmp					; Set output direction
;
; Initialize PC3...0 as outputs for keypad column scanner outputs
;
		ldi		rmp,(1<<PC3)|(1<<PC2)|(1<<PC1)|(1<<PC0)	; Set up as outputs - PC0 not used.
		out		DDRC,rmp
;
;
;===============================
; --- Timer Initialization ----
;===============================
;
; === TCNT0 Initialization === (OK)
;
; Setup 8 bit Timer/Counter0 Compare A in CTC mode
; Setup TCNT0 prescaler = 1024, (64 us @ 16 MHz) period
;
InitTimer0:
		ldi		rmp,(1<<CS02)|(1<<CS00)	; Divide by 1024 prescaler, (Fclk = 15.625 kHz @ 16 MHz)
		out		TCCR0B,rmp				; Timer/Counter0 control register B
;
; Setup TCNT0 for CTC mode
;
		ldi		rmp,(1<<WGM01)			; CTC mode
		out		TCCR0A,rmp				; Timer/Counter0 control register A
;
; Initialize OCR0A output compare register
;
		ldi		rmp,OCR0Aload			; Set OCR0A = 64 for 4.096 ms period
		out		OCR0A,rmp
;
; Enable Timer/Counter0 Compare A Match Interrput in TIMSK0
;
		lds		rmp,TIMSK0
		sbr		rmp,(1<<OCIE0A)			; Enable Timer/Counter0 Output Compare A Match Interrupt
		sts		TIMSK0,rmp
;
; Set up software timers 
;
		rcall	sync70			; 70 ms
		rcall	sync100			; 100 ms
		rcall	sync500			; 500 ms
;
;
; === TCNT2 Initialization === (OK)
;
; Setup 8 bit Timer/Counter2 Compare A in CTC mode
; Setup TCNT2 prescaler = 8, (5 us @ 16 MHz) period
;
InitTimer2:
		ldi		rmp,(1<<CS21)		; Divide by 8 prescaler
		sts		TCCR2B,rmp			; TCNT2 control register B
;
		ldi		rmp,(1<<WGM21)		; CTC mode
		sts		TCCR2A,rmp			; TCNT2 control register A
;
; Initialize OCR2A output compare register
;
		ldi		rmp,OCR2Aload		; Set OCR0A = 64 for 32 us period
		sts		OCR2A,rmp
;
; Enable TCNT2 Compare A Match Interrput in TIMSK2
;
		lds		rmp,TIMSK2
		sbr		rmp,(1<<OCIE2A)		; Enable TCNT2 Output Compare A Match Interrupt
		sts		TIMSK2,rmp
;
; SPI interrupt handler runs as part of TCNT2 routine
;
		sbr		flagb,(1<<spirdyb)	; SPI ready flag
		ldi		rmp,nbits			; SPI bit counter
		sts		bitcount,rmp		; Load bitcount
;
; Enable global interrupt and exit
;
		sei						; Enable global interrupt 
		ret
;
;
; Initilize keypad scanner variables
;
inzscn:
		cbr		flaga,(1<<KYRDY)	; Clear key ready flag
		cbr		flaga,(1<<KYDWN)	; Clear key down flag
		ldi		rmp,NCOLS			; Load columns scan counter
		mov		COL,rmp
		ldi		rmp,DBNCE			; Load debounce scans counter
		mov		REPTS,RMP
		ldi		rmp,0xff			; Load keycode with No-key-down keycode
		mov		KYCODE,rmp
		mov		LASTKY,rmp
		ldi		rmp,0x0				; Load keypad matrix counter
		mov		KYNUM,rmp
		ret
;
;
; Initialize the USART for 19200 baud asynchronous operation
;
inzuart:
		cli							; Clear global interrupts
		ldi		rmp,high(BAUD_PRE)
		sts		UBRR0H,rmp			; Load baud rate register high
		ldi		rmp,low(BAUD_PRE)
		sts		UBRR0L,rmp			; Load baud rate register low
;
; Setup frame for 1 start, 8 data, 1 stop and no parity
;
		ldi		rmp,(1<<UCSZ00)|(1<<UCSZ01)
		sts		UCSR0C,rmp
;
; Enable the USART RX, TX, and RXC interrupt
;
		ldi		rmp,(1<<RXEN0)|(1<<TXEN0)|(1<<RXCIE0) ; |(1<<TXCIE0)|(1<<UDRIE0)
		sts		UCSR0B,rmp			; Enable RX, TX and RXC interrupt
;
		sei							; Set global interrupts
		ret
;
;
;
; ============================================
;     M A I N    P R O G R A M    I N I T
; ============================================
;
Main:
;
; Initialize the stack pointer to end of SRAM
;
		ldi		rmp, HIGH(RAMEND) ; Init MSB stack
		out		SPH,rmp
		ldi		rmp, LOW(RAMEND) ; Init LSB stack
		out		SPL,rmp
;
; Initialize the engine
;
		call	wdt_off			; Disable watchdog. Must be done soon after a reset
		rcall	initz			; Initialize the controller
		rcall	inzscn			; Initialize keypad scanner variables
		rcall	inztib			; Initialize telno entry buffer
;
; Initialize the USART
;
		rcall	inzuart			; Initialize the USART
;
; Display sign-on banner
;
		rcall	clrscn			; Clear screen
		ldzptr	scrnm			; Load zptr macro
		rcall	pptr			; Print screen
;
;
; --- Run Mode ---
;
;  The automatic dialer program starts up in the RUN mode. A 1 or 2 digit
;  index to telno is expected. The legal sequences are:
;
;		*		Restart. Used to cancel any operation at any time
;		nn#		Dial number at index 'nn' - 'nn' may be 1 or 2 digits
;		99#		Toggle between PROGRAM/RUN modes
;
;
runm0:
		eor		indexb,indexb		; Clear index buffer
;
; This entry point is to prepare for a re-dialing command
;
runm01:
		cbi		PORTB,HOOK			; HOOK relay off
		cbi		PORTB,redled		; Red led off
		sbi		PORTB,bluled		; Blue led on
;
		cbr		flaga,(1<<FULL)		; Clear buffer full flag
		cbr		flaga,(1<<swapf)	; Clear swap control flag
		eor		inbuf,inbuf			; Clear number entry buffer
		eor		rgb,rgb				; Clear digits entered counter
		rcall	inztib				; Setup  input loading buffer
;
; Process any keypad entries
;
runm1:
		rcall	wgetky				; Wait for a keypad entry
;
; Arrive here with rga = keypad number token
;
runm2:
		ldi		rmp,0x0a			; '*' key
		cp		rga,rmp				; Match?
		brne	runm3				;  No
		rcall	flashblu			; Blink bluled
		rjmp	runm0				; Restart
;
; If '#' key entered prepare to dial telno.			(OK)
;
runm3:
		ldi		rmp,0x0b			; '#' key
		cp		rga,rmp				; Match?
		breq	runm3a				;	Yes, prepare to dial indexed telno
		rjmp	gidx				;	No, normal index number entry
;
runm3a:
		mov		rga,indexb			; Get the stored index
;
; Examine the index number in rga and check if 1 or 2 digit index
;
		andi	rga,0x0f			; Examine 2nd nibble number only
		ldi		rgv,0x0f
		cp		rgv,rga				; Is lo nibble 0x0f?
		brne	runm30				;  No, index was 2 digits
		mov		rga,indexb			; Get the stored index
		andi	rga,0xf0			; Clear lo nibble, single digit index
		swap	rga					; Adjust single index number to 2 bcd digits with leading 0
		rjmp	runm4				; Process the index number
;
;  Check the 2 digit index for a '99' command code
;
runm30:
		mov		rga,indexb			; Get the index number again
		ldi		rgv,0x99			; '99' is command code to toggle mode
		cp		rga,rgv				; Index = '99' toggle mode number?
		brne	runm4
		rjmp	prgm0				;  Yes, go to the PROGRAM mode
;
; Test for a legal index number between 00 and 40.
;
runm4:
		ldi		rgv,0x40			; Test for a legal index, 0 ... 40
		cp		rgv,rga				; index > 40
		brcc	runm5				;	C = 1 if index > 40
		rjmp	runm0				; Index > 40 (BCD), restart
;
; The index number is legal. Convert BCD number to a binary number. This binary
; index number represents the offset to the location to store the telno in tsbuff.	(OK)
;
; Entry:	rga = legal index number in bcd format
; Exit:		tsbuff <-- X pointer base address
;
runm5:
		rcall	flashblu			; Blink bluled
		clr		YL					; tsbuff base <-- (YH:YL)
		clr		YH					; EEPROM base address is 0x00
		rcall	a2bn				; Convert packed bcd in (rmp) to binary
		tst		rga					; rga = 0?
		breq	runm6				;  Yes, 0 offset
runm51:
		adiw	YL,maxdgpair+1			; Adjust pointer to indexed telno
		dec		rga
		brne	runm51
;
;  Dial out the indexed telno until 0xf character. Test for good first digit before dialling.
;
runm6:
		rcall	EERead				; Get 1st packed digit from buffer
									; Y incremented by <dial>
		mov		rga,rmp				; Move to rga
		andi	rga,0xf0			; Mask off lo bcd nibble
		ldi		rgv,0xf0			; Test for end marker
		cp		rga,rgv				; End marker?
		brne	runm7				;	No, valid digit
		rjmp	runm01				;   Yes, immediate exit to mode start
;
; Close phone line connection
;
runm7:
		sbi		PORTB,HOOK			; HOOK relay on, close connection to phone line
		rcall	sync500				; HOOK relay 500 ms settle time
runm71:
		sbrs	flaga,t500f			; Timeout delay
		rjmp	runm71
;
; tsbuff <-- Y pointer at 2nd indexed telno to dial out
;
runm8:
		cbi		PORTB,nce			; Enable dtmf generator
		rcall	dial				; Number-to-dtmf output
		sbi		PORTB,nce			; Disable dtmf generator
		ldi		rgb,2				; Delay counter
		rcall	sync500
;
; Arrive here at end of dialing		(OK)
;
runm9:
		sbrs	flaga,t500f
		rjmp	runm9				; 1 s timeout
		cbr		flaga,(1<<t500f)
		dec		rgb
		brne	runm9
		rjmp	runm01				; Return to main dialer
;
; Arrive here after a normal keypad number entry to pick up index number
; (from runm3:)
;
; Entry:	(rga) = index, tibuff <-- X pointer
;
;
gidx:
		sbrc	flaga,FULL			; Load if FULL = 0
		rjmp	runm1				; Else skip
;
; Pack incoming BCD digits into inbuf, copy inbuf to the tibuff.
; In RUN mode, up to 2 index digits only can be loaded, i.e. the first
; byte of tibuff only.
;
gidx1:
		sbrc	flaga,SWAPF			; 2nd digit load low
		rjmp	gidx2				
		sbr		flaga,(1<<SWAPF)	; Swap next nibble
		ori		rga,0xf0			; Set hi nibble to 0xf
		swap	rga					; 1st digit, load hi
		mov		rmp,inbuf			; Get inbuf data
		or		rmp,rga				; Update inbuf contents
		mov		inbuf,rmp
		rjmp	gidx3
;
gidx2:
		cbr		flaga,(1<<SWAPF)	; Clear swap flag
		mov		rmp,inbuf			; Get inbuf data
		andi	rmp,0xf0			; Prepare field for lo bcd merge
		or		rmp,rga				; Merge rga to inbuf data
		mov		inbuf,rmp
		sbr		flaga,(1<<FULL)		; Byte assembled
gidx3:
;		st		X,rmp				; Store index to tibuff
		sts		tibuff,rmp
		mov		indexb,rmp			; Store index to indexb
		rcall	flashblu			; Blink bluled - destroys rmp!
		rjmp	runm1
;
;
; Get packed BCD numbers from tsbuff, convert each bcd digit into dtmf tone
; generation code. Each byte from tsbuff contains 2 bcd digits. Hi digit sent
; first followed by lo digit. Send out all digits of telno.
;
; Entry:	tsbuff <-- XH:XL pointer, at indexed telno 
;			rgb digit pairs counter
;
; Called from runm8:. Modified to transmit all telno digits. This routine calls
; dout: to send single bcd digit in (rga) to dtmf generator chip
;
;
dial:
		ldi		rgb,maxdgpair			; Telno size
		cbr		flagb,(1<<t4fb)		; Clear 4 ms flag
		ldi		rgd,3				; Delay = 5*2 = 12 ms, for osc startup
diala:
		sbrs	flagb,t4fb
		rjmp	diala
		cbr		flagb,(1<<t4fb)		; Clear 4 ms flag
		dec		rgd
		brne	diala				; Wait for dtmf clock to stabilize
;
dial1:
		rcall	EERead			; Get a buffer byte
		mov		rga,rmp			; Move to rga
		swap	rga				; Send hi digit first
		andi	rga,0x0f		; Mask off hi nibble - (digit pair's lo nibble)
		ldi		rmp,0x0f		; End-byte marker
		cp		rga,rmp			; End-byte?
		brne	dial2			;	No, dial digit
		ret						;	Yes, exit
;
dial2:
		rcall	dout			; Send out first digit of pair
		rcall	EERead			; Get same buffer byte and advance pointer
		mov		rga,rmp			; Move to rga
		adiw	YL,1			; Advance to next digit
		andi	rga,0x0f		; Mask off hi nibble - (digit pair's hi nibble)
		ldi		rmp,0x0f		; End-byte marker
		cp		rga,rmp			; End-byte?
		brne	dial3			;	No, dial digit
		ret						;	Yes, exit
;
dial3:
		rcall	dout			; Send out second digit of pair
		dec		rgb				; Count bytes dialed
		brne	dial1			; Loop for more digits
		ret						; Final done
;
; The bcd digit in (rga) (lo nibble) is the same as the dtmf code required by
; the dtmf generator chip. Sends only one dtmf number per call.
;
dout:
		cpi		rga,0				; Is digit 0?
		brne	dout0				;	No, continue
		ldi		rga,0xa				;	Yes, change to 0xa code
dout0:
		rcall	spiwr				; Send data to dtmf chip
		rcall	sync70				; Start 70 ms timer
dout1:
		sbrs	flaga,t70f
		rjmp	dout1				; Wait for timeout
		rcall	notone				; Inter-digit pause
		rcall	sync70				; Start 70 ms timer
dout2:
		sbrs	flaga,t70f
		rjmp	dout2				; Wait for timeout
		ret
;
; Disable dtmf tone generator by sending tone off code
;
notone:
		ldi		rga,0xff			; No-tone code
		rcall	spiwr				; Send data to dtmf chip
		ret
;
; Wait for spirdyb flag to be set and load rga into spidata buffer
; 
spiwr:
	sbrs	flagb,spirdyb			; Flag set?
	rjmp	spiwr					;	No, wait
	sts		spidata,rga				; Load data to transmit buffer
	sbr		flagb,(1<<startfb)		; Set start flag
	cbr		flagb,(1<<spirdyb)		; Clear the flag
	ret
;
;
; --- Program Mode ---
;
; The PROGRAM mode expects a 2 digit index, plus up to a 20 digit
; telno. The legal sequences are:
;
;	 *			Restart. Used to cancel any operation at any time
;	 nndd..d#	Index 'nn',followed by 'dd..d', up to 18 teleno digits
;	 99#		Toggle between PROGRAM/RUN modes
;
;
prgm0:
		cbi		PORTB,HOOK			; HOOK relay off, open connection to phone line
		sbi		PORTB,redled		; Red led on
;
		cbi		PORTB,bluled		; Blue led off
;
		cbr		flaga,(1<<FULL)		; Clear buffer full flag
		cbr		flaga,(1<<swapf)	; Clear swap control flag
		eor		inbuf,inbuf			; Clear number entry buffer
		eor		rgb,rgb				; Clear digits entered counter
		rcall	inztib				; Setup input loading buffer
;
; Process any keypad entries
;
prgm1:
		rcall	wgetky				; Wait for a keypad entry
		rcall	flashred			; Blink redled
;
; Arrive here with (rga) = keypad number token
;
prgm2:
		ldi		rmp,0x0a			; '*' key
		cp		rga,rmp				; Match?
		brne	prgm3
		rcall	flashred			; Blink redled
		rjmp	prgm0				; Restart
;
; If '#' key entered prepare to load tibuff to tsbuff.		(OK)
;
prgm3:
		ldi		rmp,0x0b			; '#' key
		cp		rga,rmp				; Match?
		breq	prgm3a				;	Yes, load tsbuff with tibuff
		rjmp	gnum				;	No, bcd digit entry, load inbuf input buffer
prgm3a:
		ldxptr	tibuff				; X pointer to base of telephone number input buffer
		ld		rga,X				; Get the index number
;
;  Check the 2 digit index for a '99' command code	(OK)
;
prgm30:
		ld		rga,X				; Get the index number again
		ldi		rgv,0x99			; '99' is command code to toggle mode
		cp		rga,rgv				; Index = '99' toggle mode number?
		brne	prgm4				;	No, continue
		rjmp	runm0				;	Yes, go to the RUN mode
;
; Test for a legal index number between 00 and 40.
;
prgm4:
		rcall	flashred			; Blink redled
		ldi		rgv,0x40			; Test for a legal index, 0 ... 40
		cp		rgv,rga				; index > 40
		brcc	prgm5				;	C = 1 if index > 40
		rjmp	prgm0				; Index >= 40 (BCD), restart
;
; The index number is legal. Convert BCD number to a binary number. This binary
; index number represents the offset to the location to store the telno in tsbuff.		(OK)
;
; Entry:	(rga) = legal index number in bcd format
; Exit:		tsbuff <-- Y, pointer to base address
;
prgm5:
		clr		YL					; tsbuff base <-- (YH:YL)
		clr		YH
		rcall	a2bn				; Convert packed bcd in (rmp) to binary
		tst		rga					; rga = 0?
		breq	prgm6				;  Yes, done
prgm51:
		adiw	YL,maxdgpair+1		; Adjust pointer to indexed telno position in tsbuff
		dec		rga
		brne	prgm51
;
; Exit:		tsbuff <-- X pointer as offset by index number, for SRAM buffer
; Exit:		tsbuff <-- Y pointer as offset by index number, for EEPROM buffer
;
;  Store the telno in the tibuff input buffer to the tsbuff buffer storage location		(OK)
;
prgm6:
		rcall	store				; Store telno to tsbuff
		rjmp	prgm0				; loop to start
;
;  Arrive here after a normal keypad number entry to pick up index number
; (from prgm3:)
;
; Entry:	(rga) = index, tibuff <-- X pointer
; 
;
gnum:
		sbrc	flaga,FULL			; Load if FULL=0
		rjmp	prgm1
;
;  Pack incoming BCD digits into inbuf, copy inbuf to the tibuff.			(OK)
;  In PROGRAM mode, the 2 index digits must be entered, followed by up
;  to 20 digits of the telno. 
;
;	rgb = 0, digits counter
;
gnum1:
		sbrc	flaga,SWAPF			; 2nd digit load low
		rjmp	gnum2				
		sbr		flaga,(1<<SWAPF)	; Swap next nibble
		ori		rga,0xf0			; Set hi nibble to 0xf
		swap	rga					; 1st digit, load hi
		mov		rmp,inbuf			; Get inbuf data
		or		rmp,rga				; Update inbuf contents
		mov		inbuf,rmp
		rjmp	gnum3
;
gnum2:
		cbr		flaga,(1<<SWAPF)	; Clear swap flag
		mov		rmp,inbuf			; Get inbuf data
		andi	rmp,0xf0			; Prepare field for lo bcd merge
		or		rmp,rga				; Merge rga to inbuf data
		mov		inbuf,rmp
;
; inbuf has the assembled data for storing into tibuff
;
gnum3:
		st		X,rmp				; Store inbuf to tibuff
		inc		rgb					; Count the nibbles loaded
		sbrc	rgb,0				; After every 2nd digit, bit 0 = 0
		rjmp	gnum4
		eor		inbuf,inbuf			; Clear input build buffer
		adiw	XH:XL,1				; Increment pointer to next word
gnum4:
		ldi		rgv,(maxdgpair*2+2)		; Buffer full with nibbles?
		cp		rgb,rgv
		brne	gnumx				;	No, not full
		sbr		flaga,(1<<FULL)		;	Yes, mark as full
gnumx:
		rcall	flashred			; Blink redled
		rjmp	prgm1
;
;
;  Store the tibuff input buffer to the indexed storage location in the		(OK)
;  EEPROM tsbuff storage buffer.
;
; Entry:	tsbuff destination address <--- Y pointer, set from prgm5
;
store:
		ldxptr	(tibuff+1)			; Source data pointer
		ldi		rgb,maxdgpair			; Bytes to move
store1:
		ld		rmp,X+				; Fetch a byte from tibuff, increment pointer
		rcall	EEWrite				; Store a byte to EEPROM @YHL
		adiw	Y,1					; Increment to next destination address
		dec		rgb					; Count bytes moved
		brne	store1
		ret
;
; Fill telephone number entry buffer with 0xff'S		(OK)
;
inztib:
		ldxptr	tibuff			; Table in SRAM
		ldi		rgb,tisize		; Counter
		ldi		rmp,0xff		; Fill byte
inztib1:
		st		X+, rmp			; Fill a byte
		dec		rgb				; All locations filled?
		brne	inztib1			;   No, continue
		ldxptr	tibuff			; Rewind pointer
		ret
;
; Fill telephone number storage buffer with 0xff's - Not for EEPROM as		(OK)
; writing does automatic filling with ff. Buffer > 0xff, use 16 bit counter
;
inztsb:
		ldxptr	tsbuff			; Table in SRAM <-- XHL
		ldi		YH,high(tssize)
		ldi		YL,low(tssize)	; Counter
		ldi		rmp,0xff		; Fill byte
inztsb1:
		st		X+,rmp			; Fill a byte @XHL
;
		ldi		rga,1			; 16 bit decrement
		sub		YL,rga
		clr		rga
		sbc		YH,rga
;
		tst		YH				; High counts 0?
		brne	inztsb1			;	No, loop
		tst		YL				; Low counts 0?
		brne	inztsb1			;   No, continue
;
		ret
;
; ==============================
;	Timer service routines 
; ==============================
;
;
sync70:
	ldi		rmp,t70v			; Load count buffer for 70 ms
	mov		tm0v,rmp
	cbr		flaga,(1<<t70f)
	ret
;
sync100:
	ldi		rmp,t100v			; Load count buffer for 100 ms
	mov		tm1v,rmp
	cbr		flaga,(1<<t100f)
	ret
;
sync500:
		ldi		rmp,t500v		; Load count buffer for 500 ms
		mov		tm2v,rmp
		cbr		flaga,(1<<t500f)
		ret

;
; ===========================
;	Keypad service routines
; ===========================
;
; Wait for a key entry and return with value in rga after key is released		(OK)
;
wgetky:
	sbrs	flaga,KYRDY
	rjmp	wgetky
	cbr		flaga,(1<<KYRDY)
	rcall	kyin
wgetky1:
	sbrs	flaga,KYUP
	rjmp	wgetky1
	ret
;
; Scan for a keypad input and return with token in rga, C = 1 if key found		(OK)
; else C = 0 if no key found.
;
scnky:
	sbrc	flaga,KYRDY			; Test for key press
	rjmp	scn1				;  Key ready
	ldi		rga,0x00			; Retrun 0x00 is no key found
	clc							;  Key not ready, Clear C
	ret
;
scn1:
	cbr		flaga,(1<<KYRDY)	; Clear KYRDY flag
	rcall	kyin				; Fetch key token
	sec							; Set C
	ret
;
;
; =====================================================
; Translate key matrix number to reported keypad number
; =====================================================
;
; The recieved data in KYCODE is compared against a translation table		(OK)
; and a matching data entry will return a token byte in rga. Keytable
; translation table consists of pairs of bytes, first byte is key scan
; code, followed by the corresponding token byte. Table ends with a ^Z.
;
;  Entry:	KYCODE has scanned key matrix number of detected key
;  Exit: 	rga has corresponding token if valid matrix number found, 
;			else rga has 0xff if no key found.
;
; The byte in the KYCODE buffer is compared to each of the keytable
; scan code bytes until a match is found. If a match is found, the
; corresponding token is returned in rga, else 0xff is returned.
;
;
kyin:
		ldzptr	keytable		; Point to translation table
		lpm		rmp,Z+			; Get table entry, then increment table pointer to token byte
;
; Compare table entry with KYCODE key, the value of sensed keypad matrix number
;
kyin2:
		cp		rmp, KYCODE		; Compare scanned KYCODE to table codes
		breq	kyin1			;   Match found
;
; No match this time, try next table code
;
		adiw	ZL,1			; Next table code
		lpm		rmp,Z+			; Get next table entry
		cpi		rmp,ctlZ		; End of table reached?
		breq	kyinx			;   Yes, error exit
		rjmp	kyin2			;	No, keep searching table
;
; Arrive here if KYCODE matches a table code
;
kyin1:
		lpm		rga, Z			; Fetch the token
		ret
;
; Arrive here if end of table reached and no match found
;
kyinx:
		ldi		rga, 0xff		; Return oxff
		ret
;
;
; Key translation table - for returning key number
;
keytable:
.db		0x00,0x0a		; *, (matrix number, token byte)
.db		0x01,0x07		; 7
.db		0x02,0x04		; 4
.db		0x03,0x01		; 1
.db		0x04,0x00		; 0
.db		0x05,0x08		; 8
.db		0x06,0x05		; 5
.db		0x07,0x02		; 2
.db		0x08,0x0b		; #
.db		0x09,0x09		; 9
.db		0x0a,0x06		; 6
.db		0x0b,0x03		; 3
;
.db		0x0c,0x0c		; C
.db		0x0d,0x0d		; D
.db		0x0e,0x0e		; E
.db		0x0f,0x0f		; F
.db		ctlZ,0			; End of table
;
;
;  Convert the packed BCD digits in rga to binary number, return results
;  in rga. Since the maximum BCD number is 99, no range checking is needed.
;
a2bn:
	push	rga				; Save input
	andi	rga,0x0f		; Lo byte nibble only
	mov		rgv,rga			; Save lo byte nibble
	ldi		rgb,10			; decimal 10
	pop		rga				; Get original binary number
	swap	rga				; Swap bcd digits, hi nibble in low position
	andi	rga,0x0f		; Hi nibble only
	mul		rga,rgb			; 10's digit x 10, low order result in R0
	mov		rga,R0
	add		rga,rgv			; Add in 1's digit
	ret
;
; Blue led normally on during number entry modes. The flasher routine blinks
; the grnled off to acknowledge acceptance of a key entry.
;
; Flash blue led off for 100 ms
;
flashblu:
	cbi		PORTB,bluled		; Blue led off
	rcall	sync100
flashblu1:
	sbrs	flaga,t100f			; 100 ms delay
	rjmp	flashblu1
	sbi		PORTB,bluled		; Blue led on
	ret
;
; Red led normally on during program mode. The flasher routine blinks
; the redled off to acknowledge acceptance of a key entry.
;
; Flash red led off for 100 ms
;
flashred:
	cbi		PORTB,redled		; Red led off
	rcall	sync100
flashred1:
	sbrs	flaga,t100f			; 100 ms delay
	rjmp	flashred1
	sbi		PORTB,redled		; Red led on
	ret
;
;
;###########################################################################
;
;
; Turn off watchdog
;
wdt_off:
		cli							; Clear global interrupts
;
; Reset WD timer
;
		wdr
;
		in		rmp,MCUSR				; Clear WDRF bit
		andi	rmp,(0xff & (0<<WDRF))	; WDRF bit = 0
		out		MCUSR,rmp
;
; Set WDCE and WDE bits, keep old prescaler setting
;
		lds		rmp,WDTCSR
		ori		rmp,(1<<WDCE)|(1<<WDE)
		sts		WDTCSR,rmp
;
; Turn off WDT
;
		ldi		rmp,(0<<WDE)			; Clear WD system reset enable
		sts		WDTCSR,rmp
;
		sei								; Set global interrupts
		ret
;
;
; Zero RAM data space
;
zbuf:
		ldi		rgb,(buffend-uart_st)
		ldxptr	uart_st			; Start sinb
		clr		rmp
zbuf1:
		st		X+,rmp
		dec		rgb
		brne	zbuf1
		ret
;
; Zero lower registers R0...R15
;
zregs:
		ldi		rga,16
		clr		rmp
		ldxptr	0x0			; Register file base address
zregs1:
		st		X+,rmp
		dec		rga
		brne	zregs1
		ret
;
;
;
;###########################################################################
;
;
; --- Low level video drivers ---
;
; Register rga used to pass data to console output routine
;
; Print rga data as two hexadecimal digits.			(OK)
;
pahex:
	push	rga
	swap	rga				; Show MSD nibble first
	rcall	pahex1
	pop		rga
pahex1:
	andi	rga, 0x0f		; Mask off higher nibble
	ldi		rgv, 0x30 		; Add ascii '0' to convert
	add		rga, rgv		; Convert to ascii
	cpi		rga, 0x3a		; Check if > 9
	brcs	pahex2			;  No, it is 0 ... 9
	ldi		rgv, 0x07		;  Yes, convert to A ... F
	add		rga, rgv
pahex2:
	call	co
	ret
;
; Print rga contents as decimal (0...255). Leading			(OK)
; zero suppression is provided only on the 100's
; digit, so at least two digits are always printed.
;
; Registers rga, rgb not saved
;
pdec:
	;	ldi		rgb,100			; Get 100's digit
	;	call	div8u
	;	tst		rga				; Do leading zero suppression
	;	breq	pdec1
	;	call	pnum
pdec1:
	;	ldi		rga,10			; Get 10's digit
	;	xchreg	rga,rgb
	;	call	div8u			; rgb has units
	;	call	pnum
	;	xchreg	rga,rgb
pnum:
	;	ori		rga,0x30		; Ascii "0"
	;	call	co				; Show ascii decimal
		ret
;
; "div8u" - 8/8 Bit Unsided Division				(OK)
;
; This subroutine divides the two register variables "rga" (dividend) and 
; "rgb" (divisor). The result is placed in "rga" and the remainder in "rgb".
;  
; High registers used:	4 (rga,rgb,rgc,rgv)
;
;                                  
; Register Variables:
;	rgc	remainder
;	rga	dividend & result
;	rgb divisor
;	rgv	loop counter
;
; Entry:	(rga) = dividend
;			(rgb) = divisor
; Exit:		(rga) = integer part of quotient
;			(rgb) = integer remainder 
;                                    
div8u:	
		push	rgc
		push	rgv
		sub		rgc,rgc			; clear remainder and carry
        ldi		rgv,9			; init loop counter
d8u_1:	rol		rga				; shift left dividend
        dec		rgv				; decrement counter
        brne	d8u_2			; if done
		mov		rgb,rgc			; move remainder to rgb
		pop		rgv
		pop		rgc
        ret						;    return
;
d8u_2:	rol		rgc				; shift dividend into remainder
        sub		rgc,rgb			; remainder = remainder - divisor
        brcc	d8u_3			; if result negative
        add		rgc,rgb			;    restore remainder
        clc						;    clear carry to be shifted into result
        rjmp	d8u_1			; else
d8u_3:	sec						;    set carry to be shifted into result
        rjmp	d8u_1
;
;
;
;###########################################################################
;
;
; Scan for console character and return with character if any,
; else return with rga = 0. Data is available in sinb when putcnt
; is greater than getcnt.
;
; Registers:
;	rmp, rga, rgb, XHL (preserved across routine)
;
; Exit:	rga <-- character, if any, else 0
;
getc:
		lds		rmp,getcnt
		lds		rga,putcnt
		cp		rga,rmp			; Compare getcnt to putcnt
		breq	getc2			; Same, no new data
getc0:
		push	XH
		push	XL				; Save X registers
		ldxptr	sinb			; sinb base address
		lds		rmp,getcnt		; Get current getcnt offset
		clr		rga
		add		XL,rmp			; Add getcnt offset to sinb pointer
		adc		XH,rga			; Update pointer (16 bit addition)
;		
		ld		rga,X			; rga <-- @XHL
		pop		XL
		pop		XH				; Restore X registers
		inc		rmp				; Increment getcnt
		cpi		rmp,sblen		; Past end of buffer?
		brne	getc1
		clr		rmp				;	Yes, reset getcnt
getc1:
		sts		getcnt,rmp		; Update getcnt to next buffer location
		ret
getc2:
		clr		rga
		ret
;
; Wait for a received data byte, return received data in rga.
;
ci:	
		lds		rmp,getcnt
		lds		rga,putcnt
		cp		rga,rmp			; Compare getcnt to putcnt
		breq	ci				; No incoming data
		rjmp	getc0			; Get new data
;
; Load UDR0 from rga. Wait until transmitter is empty before loading.		(OK)
;
co:	
	lds		rmp,UCSR0A		; Get USART control status register
	sbrs	rmp,UDRE0		; Test if UDR0 is empty
	rjmp	co
;
; Send data
;
	sts		UDR0,rga		; UDR0 <-- rga
	ret
;
;
;###########################################################################
;
;
; Print CR and LFs	(OK)
;
crllf:
	rcall	crlf			; Two CRLF
crlf:
	push	rga
	ldi		rga,CR			; Carriage return
	call	co
	ldi		rga,LF			; Linefeed
	call	co
	rjmp	cco
;
; Print spaces	(OK)
;
dblsp:
	call	space
space:
	push	rga
	ldi		rga,SP			; Space
cco:
	call	co
	pop		rga
	ret
;
; Print comma	(OK)
;
prcma:
	push	rga
	ldi		rga,cma
	rjmp	cco
;
; Print delete character at cursor	(OK)
;
bksp:
	push	rga
	call	cbak			; Delete character at cursor
	call	ceol			; Clear cursor to end of line
	pop		rga
	ret
;
; Print message string, ^Z terminated. Routine is called with		(OK)
; code address of string loaded in ZH:ZL.
;
pptr:
	push	rga
pptr1:
	lpm		rga,Z+			; String byte to rga, Z+
	cpi		rga,ctlZ		; byte ^Z?
	brne	pptr2			; Print if not ^Z
	pop		rga
	ret
pptr2:
	cpi		rga,NULL		; Skip any nulls in string
	breq	pptr1
	rcall	co
	rjmp	pptr1
;
;
; --- Video and Cursor control routines ---
;
; Clear screen	(OK)
;
clrscn:
	push	zh
	push	zl
	ldzptr	scrn		; Home cursor
	call	pptr
	ldzptr	clrs		; Clear entire screen
	rjmp	video
;
; --- Move cursor down ---
;
cdown:
	push	zh
	push	zl
	ldzptr	cudn		; Cursor down one row
	rjmp	video
;
; --- Clear to end of line ---
;
ceol:
	push	zh
	push	zl
	ldzptr	eol			; Clear to end of line
	rjmp	video
;
; --- Cursor back one column ---
;
cbak:
	push	zh
	push	zl
	ldzptr	cubk			; Cursor back 1 column
	rjmp	video
;
; --- Highlight on ---
;
vhi:
	push	zh
	push	zl
	ldzptr	hi			; Highlight on
	rjmp	video
;
; --- Normal ---
;
vlo:
	push	zh
	push	zl
	ldzptr	lo			; Normal - attributes off
	rjmp	video
;
; --- Reverse ---	(OK)
;
vrev:
	push	zh
	push	zl
	ldzptr	rev			; Reverse on
video:
	rcall	pptr
	pop		zl
	pop		zh
	ret
;
; --- Video position cursor sequences ---
; Lead-in sequence
;
vpxy1:
	push	zh
	push	zl
	ldzptr	pxy1			; Lead-in sequence
	rjmp	video
;
; Middle sequence
;
vpxy2:
	push	zh
	push	zl
	ldzptr	pxy2			; Middle sequence
	rjmp	video
;
; End sequence
;
vpxy3:
	push	zh
	push	zl
	ldzptr	pxy3			; Trailing sequence
	rjmp	video
;
; --- Save cursor position ---
;
vscp:
	push	zh
	push	zl
	ldzptr	scp			; Save cursor position
	rjmp	video
;
; --- Restore cursor position ---
;
vrcp:
	push	zh
	push	zl
	ldzptr	rcp					; Restore cursor position
	rjmp	video
;
; --- Position cursor at row, column immediately following rcall to pxy ---
;
; Row & column values must be given as ascii decimal.			(OK)
;
pxy:
	call	vpxy1			; Lead-in sequence
	pop		ZH				; Point to string start address
	pop		ZL
	clc
	rol		ZL				; 16 bit multiply by 2 for word address
	rol		ZH
;
	lpm		rga,Z+			; Pick up row value
	call	pdec			; Print it and ..
	call	vpxy2			; Middle sequence		+++++ Uses Z pointer, must save Z +++
	lpm		rga,Z+			; Pick up column value
	call	pdec			; Print it and ..
	call	vpxy3			; End sequence
;
	clc
	ror		ZH
	ror		ZL
	push	ZL				; Return to caller
	push	ZH
	ret
;
; Position cursor at (YH)-->row, (YL)-->col		(OK)
;
gotoxy:
	call	vpxy1			; Send lead-in string
	mov		rga,YH			; Get row value
	call	pdec			; Send row
	call	vpxy2			; Send middle string
	mov		rga,YL			; Get col value
	call	pdec			; Send col
	call	vpxy3			; Send trailing string
	ret
;
;
; --- Message strings data area ---
;
; Terminal control sequences
;
cudn:	.db	ESC,"[B",ctlZ		; Move cursor down
cubk:	.db	ESC,"[D",ctlZ		; Cursor back one column
scrn:	.db	ESC,"[H",ctlZ		; Home cursor
eos:	.db	ESC,"[0J",ctlZ		; Clear from cursor to end of screen
clrs:	.db	ESC,"[J",ctlZ		; Clear entire screen
eol:	.db	ESC,"[K",ctlZ		; Erase to end of line
hi:		.db	ESC,"[1m",ctlZ		; Highlight on
lo:		.db	ESC,"[m",ctlZ		; Normal - attributes off
rev:	.db	ESC,"[7m",ctlZ		; Reverse on
pxy1:	.db	ESC,"[",ctlZ		; Lead-in sequence
pxy2:	.db	";",ctlZ			; Middle sequence
pxy3:	.db	"H",ctlZ			; Trailing sequence
dlc:	.db	ESC,"[1M",ctlZ		; Delete line at cursor
scp:	.db	ESC,"7",ctlZ		; Save cursor position
rcp:	.db	ESC,"8",ctlZ		; Restore cursor position
;
;
scrnm:
		.db	"===<< DTMF Autodialer >>===",cr,lf,lf,ctlZ
;
;
;
.include	"eeprom_module.asm"
;
;
; End of source code
;
.exit
;
;##########################################################################
;
