PWMPIN EQU P2.0	; PWM output pin
PWM_FLAG EQU 0	; Flag to indicate high/low signal
PWM_WIDTH EQU 10.0





PWM_SETUP:
	MOV TMOD,#00H ; Timer0 in Mode 0
	MOV R7, #160	; Set pulse width control
	; The value loaded in R7 is value X as
	; discussed above.
	;SETB EA ; Enable Interrupts
	SETB ET0 ; Enable Timer 0 Interrupt
	SETB TR0 ; Start Timer
	RET
 
ljmp Main

TIMER_0_INTERRUPT:
    
    JB PWM_FLAG, HIGH_DONE	; If PWM_FLAG flag is set then we just finished
				; the high section of the
LOW_DONE:			; cycle so Jump to HIGH_DONE
	SETB PWM_FLAG		; Make PWM_FLAG=1 to indicate start of high section
	SETB PWMPIN		; Make PWM output pin High
	MOV TH0, R7		; Load high byte of timer with R7
				; (pulse width control value)
	CLR TF0			; Clear the Timer 0 interrupt flag
	RETI			; Return from Interrupt to where
				; the program came from
HIGH_DONE:
	CLR PWM_FLAG		; Make PWM_FLAG=0 to indicate start of low section
	CLR PWMPIN		; Make PWM output pin low
	MOV A, #0FFH		; Move FFH (255) to A
	CLR C			; Clear C (the carry bit) so it does
				; not affect the subtraction
	SUBB A, R7		; Subtract R7 from A. A = 255 - R7.
	MOV TH0, A		; so the value loaded into TH0 + R7 = 255
	CLR TF0			; Clear the Timer 0 interrupt flag
	RETI			; Return from Interrupt to where
				; the program came from


    PWM_STOP:
	CLR TR0			; Stop timer to stop PWM
	RET


Main:
    mov SP, #0x7F
    lcall PWM_SETUP

   
   