.include "m8515def.inc"

; Global variables
.def player = r19
.def timer_counter = r20
.def hand = r21
.def iswinning = r24
; We'll be using X as cursor position.

.def temp0 = r16; Mostly used for printing.
.def temp1 = r17; Mostly used for DDRAM address argument.
.def temp2 = r18

.def selOrHasten = r22 ;boolean for roll button

.def timer_dec = r23
.def led_temp = r25

.equ slot_data = 0x0060
.set count = 215 ; initial counter
.set dec_step = 10 ; timer decrementer
.set dec_max = 107 ; timer decrementer max

.org $00
	rjmp MAIN
.org $01
	rjmp button_select
.org $02
	rjmp button_roll
.org $06
	rjmp ISR_TOV1


WELCOME_MSG:
    .db "      Congklak Congkak!", 0

INITIAL_SLOTS:
    .db 7, 7, 7, 7, 7, 7, 7, 0  ; Player 1
    .db 7, 7, 7, 7, 7, 7, 7, 0  ; Player 2

P1_WIN_MSG:
    .db "PLAYER 1 WINS!", 0 ; Win message for Player 1

P2_WIN_MSG:
    .db "PLAYER 2 WINS!", 0 ; Win message for Player 2


; MACRO =================================================================================
; =======================================================================================

.MACRO WRITE_PORTB
	out PORTB, @0 ; out data from register @0 to PORTB
	sbi PORTA,0 ; SETB EN
	cbi PORTA,0 ; CLR EN
.ENDMACRO

; MAIN PROGRAM ==========================================================================
; =======================================================================================


MAIN:
	; Initialization
	; Set stack pointer to the RAM end
	ldi	temp0, low(RAMEND)
	out	SPL, temp0
	ldi	temp0, high(RAMEND)
	out	SPH, temp0
	
	rcall INIT_INTERRUPT
	rcall INIT_LCD
	rcall INIT_TIMER
	rcall INIT_LED

	rcall INIT_MEMORY
	rcall INIT_HUD

    rcall button_roll


WAIT_FOR_WINNING:         ; wait until a winner can be determined
	cpi isWinning, 1      ; there's a winner
    breq WIN
    rjmp WAIT_FOR_WINNING

WIN:
	; Win state
    cli

PRINT_WIN_MESSAGE:
    ldi XL, 15
    rcall GetSlot
    mov temp1, temp0 ; save Player 2's home slot into temp1
    ldi XL, 7
    rcall GetSlot    ; save Player 1's home slot into temp0
    sub temp0, temp1 ; Player 1 - Player 2
    brlt WIN_P2      ; if Player 1 < Player 2 then P2 is the winner

    WIN_P1:
    ldi ZL, low(2*P1_WIN_MSG)   ; init Z pointer to P1's win message
    ldi ZH, high(2*P1_WIN_MSG)
    rjmp WIN_CELEBRATION

    WIN_P2:
    ldi ZL, low(2*P2_WIN_MSG)   ; init Z pointer to P2's win message
    ldi ZH, high(2*P2_WIN_MSG)

    WIN_CELEBRATION:
    rcall ClearLcd
    ldi led_temp, 0xFF    ; turn on all LEDs
    out PORTC, led_temp

    LOADBYTE:
	lpm r0, Z+            ; load byte from program memory into r0, increment Z pointer
	tst r0                ; check if we've reached the end of the message
	breq FINISH           ; if so, quit
	rcall WRITE_LCD_LED   ; else, write the character to LCD and change the LED state
	rjmp LOADBYTE         ; loop

WRITE_LCD_LED:
	sbi PORTA, 1          ; SETB RS
	WRITE_PORTB r0        ; write to PORTB for LCD
    in led_temp, PORTC    ; read current LED config
    lsr led_temp          ; shift turn off one LED from the highest order
    out PORTC, led_temp   ; send to LED
	ret

FINISH:
    rcall TurnPhaseDelay  ; give delay so players can see the message clearly
    rcall TurnPhaseDelay
	rjmp PRINT_WIN_MESSAGE ; loop to give celebration effect


; INITIAL FUNCTIONS =====================================================================
; =======================================================================================


INIT_LCD:
	cbi PORTA, 1	; RS = 0
	ldi temp0, 0x38	; 8bit, 2line, 5x7
	WRITE_PORTB temp0

	ldi temp0, 0b1100	; disp ON, cursor OFF, blink OFF
	WRITE_PORTB temp0

	ldi temp0, $06	; increase cursor, display sroll OFF
	WRITE_PORTB temp0

	ser temp0
	out	DDRA, temp0	; Set port A as output
	out	DDRB, temp0	; Set port B as output

	rcall ClearLcd

    sbi PORTA, 1
    ldi ZL, low(2*WELCOME_MSG)   ; init Z pointer to P2's win message
    ldi ZH, high(2*WELCOME_MSG)

    LOADCHAR:
    lpm r0, Z+
    tst r0
    breq FIN_INIT_LCD
    WRITE_PORTB r0
    rjmp LOADCHAR

    FIN_INIT_LCD:
    cbi PORTA, 1
    rcall TurnPhaseDelay
    rcall TurnPhaseDelay
    rcall TurnPhaseDelay
    rcall ClearLcd
	ret

INIT_TIMER:
	ldi temp1, (1<<CS01) ; use clk / 8 prescaling
	out TCCR1A, temp1    ; out to TCCR1A
	out TCCR1B, temp1    ; out to TCCR1B
	ldi temp1,(1<<TOV1)  ; using Timer 1 overflow
	out TIFR, temp1
	ldi temp1,(1<<TOIE1) ; enable Timer 1
	out TIMSK, temp1
    ldi timer_counter, count ; load initial counter
	ret

INIT_MEMORY: ; Initialize game data
	; Fill all the slots
	ldi ZL, low(INITIAL_SLOTS*2)
	ldi ZH, high(INITIAL_SLOTS*2)
	ldi XL, low(slot_data)
	ldi XH, high(slot_data)
	ldi temp0, 16
	lpm r0, Z+
	INIT_MEMORY_loop:
		st X+, r0
		lpm r0, Z+
		dec temp0
		brne INIT_MEMORY_loop
	; Set the registers
	ldi hand, 0
	ldi player, 0
	ret


INIT_HUD:
	clr XH
	clr XL

	; Fill the LCD with initial slot values
	INIT_HUD_loop:
		rcall GetSlot
		rcall GetDdramLocation
		rcall PrintSlot
		adiw XL, 1
		cpi XL, 16
		brne INIT_HUD_loop

	; Add cursor to origin
	clr XH
	clr XL
	ldi temp0, 1
	rcall GetDdramLocation
	rcall SetGameCursor

	; Set LEDs
	ldi temp0, 0b1
	out PORTC, temp0
	ret

INIT_LED:
	ser temp1
	out DDRC, temp1
	ret

INIT_INTERRUPT:
	ldi selOrHasten, 0
	ldi temp0,0b00001010
	out MCUCR,temp0
	ldi temp0,0b11000000
	out GICR,temp0
	sei
	ret

; INTERRUPTS ============================================================================
; =======================================================================================

ISR_TOV1:
	cpi isWinning, 1   ; if there's a winner (the game ends), don't do anything
    brne cont_isr_tov1
    reti
    cont_isr_tov1:
    dec timer_counter ; decrement timer counter
    breq SHIFT_LED ; shift LED if counter reaches zero
    reti

SHIFT_LED:
	in led_temp, PORTC           ; load current LED config
    andi led_temp, 0b11111100    ; clear Player LED indicator configs
    breq TIMEOUT                 ; if timer indicator in LED is zero, player has run out of time
	lsr led_temp                 ; else, shift right (turn off one LED from the highest order)
    andi led_temp, 0b11111100    ; clear Player LED indicator configs that was carried after shifting
    or led_temp, player          ; get current player (Player 1 = 0, Player 2 = 1)
    inc led_temp                 ; increment so player 1 -> 1, player 2 -> 2
	out PORTC, led_temp          ; output to LED
    ldi timer_counter, count     ; reload counter
    sub timer_counter, timer_dec
        
	reti                         ; return from interrupt

TIMEOUT: ; Penalty Manager
    ; save all current state
    push temp0
    push temp1
    push temp2
    push XL
    push XH

    ldi XL, 15
    rcall GetSlot
    mov temp1, temp0   ; temp1 is player 2's home slot contents

    ldi XL, 7
    rcall GetSlot      ; temp0 is player 1's home slot contents

    tst player         ; if player = 0 then it's Player 1
    brne Penalty_P2    ; else it's Player 2

    Penalty_P1:
    mov temp2, temp0   ; copy Player 1's home to temp2
    lsr temp2          ; divide by 4, save to temp2
    lsr temp2
    sub temp0, temp2   ; subtract temp2 from Player 1's home
    push temp1         ; save current temp1 because GetDdramLocation will overwrite it
    rcall SetSlot      ; save Player 1's home to RAM
    rcall GetDdramLocation
    rcall PrintSlot    ; update slot
    pop temp1          ; retrieve temp1

    add temp1, temp2   ; add temp2 to Player 2's home
    ldi XL, 15
    mov temp0, temp1
    rcall SetSlot      ; save Player 2's home to RAM
    rcall GetDdramLocation
    rcall PrintSlot    ; update slot
    rjmp reset_led

    Penalty_P2:
    mov temp2, temp1   ; copy Player 2's home to temp2
    lsr temp2          ; divide by 4, save to temp2
    lsr temp2
    sub temp1, temp2   ; subtract temp2 from Player 2's home
    ldi XL, 15
    push temp0         ; save temp0 because we need to use the register to store to RAM
    mov temp0, temp1   ; move Player 2's home to temp0
    rcall SetSlot      ; store Player 2's home to RAM
    rcall GetDdramLocation
    rcall PrintSlot    ; update slot
    pop temp0          ; retrieve temp0

    add temp0, temp2   ; add temp2 to Player 1's home
    ldi XL, 7
    rcall SetSlot      ; store to RAM
    rcall GetDdramLocation
    rcall PrintSlot    ; update slot

    reset_led:
    ldi led_temp, 0xFC  ; reset timer LED
    or led_temp, player ; get current player (P1 = 0, P2 = 1)
    inc led_temp        ; increment so P1 -> 1, P2 -> 2
    out PORTC, led_temp ; write to LED

    ; back to previous state
    pop XH
    pop XL
    pop temp2
    pop temp1
    pop temp0
    reti


button_select:
	sei
	ldi selOrHasten,1
	ldi temp2, 0b10000000 ; disable select button
	out GICR, temp2
	rcall TurnPhase
	ldi temp2, 0b11000000 ; re-enable select button
	out GICR, temp2
	ldi selOrHasten,0

    add_timer_dec:
    cpi timer_dec, dec_max
    brge ret_int_btn_sel
    ldi temp2, dec_step
    add timer_dec, temp2     ; increment timer steps to make the timer go faster
    ret_int_btn_sel:
	reti

button_roll:
	cpi selOrHasten,0		;selOrHasten = 0 then the program is on selecting slot phase
	breq selectingPhase		;	else then it's on turning phase (then roll button is for hastening turn)
		ldi selOrHasten, 2	;a flag for not using delay (will be used in turnPhase)
		rjmp returnfromInt

	selectingPhase:
	rcall RollSelectPhase

	returnfromInt:
	reti


; RUNTIME FUNCTIONS =====================================================================
; =======================================================================================


GetSlot: ; Obtain slot value in cursor_position from database, stored in temp0.
	push XL
	push XH

	adiw XL, slot_data>>1
	adiw XL, slot_data>>1
	ld temp0, X

	pop XH
	pop XL
	ret

SetSlot: ; Set slot value in cursor_position from temp0 to database.
	push XL
	push XH
	push temp0

	adiw XL, slot_data>>1
	adiw XL, slot_data>>1
	st X, temp0

	pop temp0
	pop XH
	pop XL
	ret


ClearLcd: ; Clear the LCD.
	push temp0

	cbi PORTA, 1
	ldi temp0, 0b1
    WRITE_PORTB temp0

	pop temp0
	ret


MoveLcdCursor: ; Move LCD cursor to DDRAM address temp1
	push temp1

	cbi PORTA, 1
	ori temp1, 0b10000000
    WRITE_PORTB temp1

	pop temp1
	ret

Write: ; Write char(temp0) on cursor.
	sbi PORTA, 1
    WRITE_PORTB temp0
	ret


PrintSlot: ; Update view in DDRAM pointed by temp1 in LCD with value in temp0.
	push temp0
	push temp1
	push temp2
	
	inc temp1

	; Move the cursor to the position specified in temp1
	rcall MoveLcdCursor

	; Print in ASCII decimal
	rcall BIN2DEC
	; Tens first
	subi temp0, -'0'
	; If the tens is zero, replace it with space
	cpi temp0, '0'
	brne PrintSlot_skip1
		ldi temp0, ' '
	PrintSlot_skip1:
	rcall Write
	; Then the ones
	subi temp1, -'0'
	mov temp0, temp1
	rcall Write

	pop temp2
	pop temp1
	pop temp0
	ret


SetGameCursor: ; Temp0 = 0: Remove game cursor in DDRAM address in temp1.
;                Temp0 = 1: Place a game cursor from DDRAM address in temp1.
	push temp0
	push temp1
	push temp2
	; Jump to location
	rcall MoveLcdCursor

	; Figure what to draw by temp0.
	ldi temp2, ' '
	add temp0, temp2
	cpse temp0, temp2
	ldi temp0, '['

	; Draw it
	rcall WRITE
	
	; Shift cursor 3 times to the right
	subi temp1, -3
	rcall MoveLcdCursor

	; Draw the cursor closing
	cpse temp0, temp2
	ldi temp0, ']'
	rcall WRITE
	
	pop temp2
	pop temp1
	pop temp0
	ret


GetDdramLocation: ; Find the location of slot by X in DDRAM address, store result in temp1.
	push temp0
	push r0
	push r1

	; [THIS CODE IS OBTAINED BY EMPIRICAL EXPERIMENT. DON'T ASK ME WHY/HOW IT WORKED.]
	mov temp1, XL
	inc temp1
	ldi temp0, 3
	mul temp1, temp0
	mov temp1, r0
	cpi temp1, 0x8*3 + 1
	brlt GetDdramLocation_skip1
		neg temp1
		ldi temp0, 0x58 + 8*3
		add temp1, temp0
	GetDdramLocation_skip1:
	; [END OF EMPIRICAL CODE]

	pop r1
	pop r0
	pop temp0
	ret


Bin2Dec: ; Converts a binary number in temp0 to 2-digit BCD, stored in temp1 (ones) and temp0 (tens)
	mov temp1, temp0
	ldi temp0, 0
	BIN2DEC_loop1:
		cpi temp1, 10
		brlt BIN2DEC_endloop1
		inc temp0
		subi temp1, 10
		rjmp BIN2DEC_loop1
	BIN2DEC_endloop1:
	ret


UpdateHandLed: ; Update led to match the player and hand data.
	push hand
	push player
	
	andi hand, 0x2F
	lsl hand
	lsl hand

	inc player
	or hand, player
	out PORTC, hand

	pop player
	pop hand
	ret


MOVE_CURSOR_RIGHT:
	; Remove the cursor from the last location.
	; (DDRAM location is still stored in temp1 up to this point.)
	clr temp0
	rcall SetGameCursor

	; Move on to the next location.
	dec XL

	; If the player is in the enemy home, move on to the next field again.
	mov temp0, player
	swap temp0
	lsr temp0 ; Shift left 3 times
	eor temp0, XL ; XOR with the cursor position
	cpi temp0, 0b1111
	brne not_enemy_home
		dec XL
	not_enemy_home:
	andi XL, 0xF
	rcall GetDdramLocation

	; Move cursor to the current position.
	ldi temp0, 1
	rcall SetGameCursor
	ret
	
RollSelectPhase:
	; Remove the cursor from the last location.
	clr temp0
	rcall SetGameCursor

	; Move on to the next location.
	dec XL

	andi XL, 0b111
	cpi XL, 0b111
	brne RollSelectPhase_notHome
		dec XL
	RollSelectPhase_notHome:
	mov temp0, player
	swap temp0
	lsr temp0
	or XL, temp0
	
	; Move one more step if the slot contains 0 value
	zeroValue:
		rcall GetSlot
		tst temp0
		brne notZeroValue
		rjmp RollSelectPhase
	notZeroValue:

	; Move cursor to the current position.
	rcall GetDdramLocation
	ldi temp0, 1
	rcall SetGameCursor
	ret

MOVE_CURSOR_LEFT:
	; Remove the cursor from the last location.
	; (DDRAM location is still stored in temp1 up to this point.)
	clr temp0
	rcall SetGameCursor

	; Move on to the next location.
	inc XL

	; If the player is in the enemy home, move on to the next field again.
	mov temp0, player
	swap temp0
	lsr temp0 ; Shift left 3 times
	eor temp0, XL ; XOR with the cursor position
	cpi temp0, 0b1111
	brne not_enemy_home2
		inc XL
	not_enemy_home2:
	andi XL, 0xF
	rcall GetDdramLocation
	
	; Move cursor to the current position.
	ldi temp0, 1
	rcall SetGameCursor
	ret

TurnPhaseDelay:
	cpi selOrHasten, 2	;if it's 2, skip delay
		breq skipDelay
	
	push r18
	push r19
	push r20

    ldi  r18, 1
    ldi  r19, 100
    ldi  r20, 255
	TurnPhaseDelay_L1:
	dec  r20
    brne TurnPhaseDelay_L1
    dec  r19
    brne TurnPhaseDelay_L1
    dec  r18
    brne TurnPhaseDelay_L1
	
	pop r20
	pop r19
	pop r18

	skipDelay:
	ret


TurnPhase: ; The turn phase of the game.
	push temp0
	; push temp1
	push temp2
	
	; While the slot on pointer is not zero...
	TurnPhase_notLandingOnZero:
		; Load the slot in pointer to temp0
		rcall GetSlot

		; Move it to hand. Also update the LED information.
		mov hand, temp0
		rcall UpdateHandLed

		; Update the data of the emptied slot in the data memory and the LCD.
		clr temp0
		rcall SetSlot ; In the data memory
		rcall GetDdramLocation
		rcall PrintSlot ; In the LCD

		; Long delay so the user can contemplate their mistakes.
		rcall TurnPhaseDelay
		rcall TurnPhaseDelay

		; While the hand is not zero...
		TurnPhase_filledHand:
			rcall MOVE_CURSOR_LEFT
			
			; Get the current slot data to temp0. Increment it by one.
			rcall GetSlot
			mov temp2, temp0 ; This represents the amount of the last slot filled. We'll need this later.
			inc temp0

			; Decrement the value in hand by one. Update it in LED.
			dec hand
			rcall UpdateHandLed

			; Update the data of the emptied slot in the data memory and the LCD.
			rcall SetSlot ; In the data memory
			;rcall GetDdramLocation
			rcall PrintSlot ; In the LCD

			; Delay.
			rcall TurnPhaseDelay

			; If the hand is not zero yet, go back to the top.
			tst hand
			brne TurnPhase_filledHand
		; Else...
		
		; Check if player's in home. If so, end the phase without swapping player.
		mov temp0, XL
		andi temp0, 0b111
		cpi temp0, 0b111
		breq TurnPhase_noSwap

		; Check if the last slot filled is zero. If so, go back to the top
		tst temp2
		brne TurnPhase_notLandingOnZero
	
	; Check if player is in own slot.
	mov temp0, XL
	andi temp0, 0b1000
	lsl temp0
	swap temp0
	cp temp0, player
	brne TurnPhase_swap
	; If it is, perform nembak.		
		; Remove the cursor.
		clr temp0
		rcall SetGameCursor	

		; Pick the opposite slot.
		subi XL, 14
		neg XL

		; Get its data, move it into temp2.
		rcall GetSlot
		mov temp2, temp0

		; Empty the slot. Write it.
		clr temp0
		rcall SetSlot
		rcall GetDdramLocation
		rcall PrintSlot

		; Go to home.
		subi XL, 14
		neg XL
		ori XL, 0b111
	
		; Update datas.
		rcall GetSlot
		add temp0, temp2
		rcall SetSlot
		rcall GetDdramLocation
		rcall PrintSlot
	
		; Delay
		rcall TurnPhaseDelay
		rcall TurnPhaseDelay
	
	TurnPhase_swap:
	; Swap players
	ldi temp0, 1
	eor player, temp0

	TurnPhase_noSwap:
	; Clear the cursor
	clr temp0
	rcall SetGameCursor	
	
	clr temp1 ; Will not be using this for DDRAM for a moment
	TurnPhase_findNonZero:
	; Go to the slot 0 of the player
	mov XL, player
	swap XL
	lsr XL
	
	; Find the first nonzero value of the player.
	TurnPhase_findNonZero_loop:
		rcall GetSlot
		tst temp0
		brne TurnPhase_nonZeroFound
		inc XL
		; Check if they're on their home slot
		mov temp2, XL
		andi temp2, 0b111
		cpi temp2, 0b111
		; If not, back to loop.
		brne TurnPhase_findNonZero_loop
	; Else, swap players. Increment zero.
	ldi temp0, 1
	eor player, temp0
	inc temp1
	cpi temp1, 1
	breq TurnPhase_findNonZero

	; Up to this point, then nothing is found. We have a winning condition
	inc isWinning
	pop temp2
	; pop temp1
	pop temp0
	ret

	TurnPhase_nonZeroFound:
	rcall GetDdramLocation
	ldi temp0, 1
	rcall SetGameCursor

    mov led_temp, player
    inc led_temp
    ori led_temp, 0b11111100
    out PORTC, led_temp

	pop temp2
	; pop temp1
	pop temp0
	ret
