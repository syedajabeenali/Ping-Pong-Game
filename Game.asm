[org 0x100]
jmp start

oldtimer: dd 0
oldkb: dd 0
PlayerAturn : dw 1
PlayerBturn :dw 0
BallRow: db 23
BallColumn: db 40
Right_Down_BounceFlag : dw 0
Left_Down_BounceFlag : dw 0
Right_Up_BounceFlag : dw 1
Left_Up_BounceFlag : dw 0
paddleA_loc: dw 0
paddleB_loc: dw 0
Game_Start: dw 0
A_Scores: dw 0
B_Scores: dw 0
GameEnd_string : db '*********Game Ended**********'
playerA_wins_string : db 'Player A wins'
playerB_wins_string : db 'Player B wins'

;-------------------------------------------------
; Subroutine to clear the screen 
;-------------------------------------------------
clearScreen:
    mov ax, 0xb800
    mov es, ax
    mov di, 0
    mov ax, 0x0720    ; Space with white foreground
    mov cx, 2000      ; 2000 words for full screen
    rep stosw
    ret

;--------------------------------------------------------------
; Subroutine to calculate screen offset based on row and column
;--------------------------------------------------------------
findLocation:
   
    push bp
    mov bp, sp
    mov bx, 0
    mov al, 80          ; 80 characters per row
    mul byte [bp+6]     ; Multiply row number by 80 to get row offset
    mov bl, [bp+4]      ; Get column number
    add ax, bx
    shl ax, 1           ; Each character takes 2 bytes
    mov di, ax          ; Store calculated offset in DI
    pop bp
    ret 4


;--------------------------------------------------------------
; Subroutine to calculate ball location
;--------------------------------------------------------------
findBallLocation:
   
    push bp
    mov bp, sp
    mov bx, 0
    mov al, 80          ; 80 characters per row
    mul byte [cs:BallRow]     ; Multiply row number by 80 to get row offset
    mov bl, [cs:BallColumn]      ; Get column number
    add ax, bx
    shl ax, 1           ; Each character takes 2 bytes
    mov si, ax          ; Store calculated offset in DI
    pop bp
    ret

;----------------------------------------
; Display Player A's Scores
;----------------------------------------
A_Scores_display:
    call clearScreen       ; Clear the screen
    pusha                  ; Save all registers

    ; Display setup for Player A's score
    mov ah, 0x13           ; BIOS Teletype function
    mov al, 1              ; Write string
    mov bh, 0              ; Page number
    mov bl, 7              ; Text color
    mov dx, 0x0816         ; Cursor position (row and column)
    mov cx, 29             ; Length of string
    push cs
    pop es                 ; Set ES to current segment
    mov bp, GameEnd_string ; Pointer to string to display
    INT 0x10               ; Call BIOS interrupt to display string
	
    ;Printing Win string
	mov ah, 0x13           ; BIOS Teletype function
    mov al, 1              ; Write string
    mov bh, 0              ; Page number
    mov bl, 7              ; Text color
    mov dx, 0x0921        ; Cursor position (row and column)
    mov cx, 13             ; Length of string
    push cs
    pop es                 ; Set ES to current segment
    mov bp, playerA_wins_string ; Pointer to string to display
    INT 0x10               ; Call BIOS interrupt to display string

    popa                   ; Restore all registers
    ret                    ; Return to caller

;----------------------------------------
; Display Player B's Scores
;----------------------------------------
B_Scores_display:
    call clearScreen       ; Clear the screen
    pusha                  ; Save all registers

    ; Display setup for Player B's score
    mov ah, 0x13           ; BIOS Teletype function
    mov al, 1              ; Write string
    mov bh, 0              ; Page number
    mov bl, 7              ; Text color
    mov dx, 0x0816         ; Cursor position (row and column)
    mov cx, 29             ; Length of string
    push cs
    pop es                 ; Set ES to current segment
    mov bp, GameEnd_string ; Pointer to string to display
    INT 0x10               ; Call BIOS interrupt to display string
     
	;Printing Win string
	mov ah, 0x13           ; BIOS Teletype function
    mov al, 1              ; Write string
    mov bh, 0              ; Page number
    mov bl, 7              ; Text color
    mov dx, 0x0921         ; Cursor position (row and column)
    mov cx, 13             ; Length of string
    push cs
    pop es                 ; Set ES to current segment
    mov bp, playerB_wins_string ; Pointer to string to display
    INT 0x10               ; Call BIOS interrupt to display string
    	
    popa                   ; Restore all registers
    ret                    ; Return to caller

;----------------------------------------
; Move Paddle A to the Left
;----------------------------------------
movePaddleA_left:
    ; Clear the paddle's last position
    mov cx, 20                  ; Paddle width
    mov di, [cs:paddleA_loc]    ; Current paddle position
    cmp di, 1                   ; Prevent paddle from moving out of the screen
    jbe skipmove

ClearPaddleA:
    mov word [es:di], 0x0720    ; Clear the screen content
    add di, 2
    loop ClearPaddleA

    ; Update the paddle location
    mov di, [cs:paddleA_loc]
    sub di, 2                   ; Move left
    mov [cs:paddleA_loc], di

    ; Redraw the paddle at the new location
    mov cx, 20                  ; Paddle width
Player:
    mov word [es:di], 0xF120    ; Redraw paddle
    add di, 2
    loop Player

skipmove:
    ret

;----------------------------------------
; Move Paddle A to the Right
;----------------------------------------
movePaddleA_Right:
    mov ax, 0xb800              ; Video memory segment
    mov es, ax

    ; Clear the paddle's last position
    mov cx, 20                  ; Paddle width
    mov di, [cs:paddleA_loc]    ; Current paddle position
    cmp di, 120                 ; Prevent paddle from moving out of the screen
    je skipmove1

ClearPaddleright:
    mov word [es:di], 0x0720    ; Clear the screen content
    add di, 2
    loop ClearPaddleright

    ; Update the paddle location
    mov di, [cs:paddleA_loc]
    add di, 2                   ; Move right
    mov [cs:paddleA_loc], di

    ; Redraw the paddle at the new location
    mov cx, 20                  ; Paddle width
rightside:
    mov word [es:di], 0xF120    ; Redraw paddle
    add di, 2
    loop rightside

skipmove1:
    ret

;----------------------------------------
; Move Paddle B to the Left
;----------------------------------------
movePaddleB_left:
    ; Clear the paddle's last position
    mov cx, 20                  ; Paddle width
    mov di, [cs:paddleB_loc]    ; Current paddle position
    cmp di, 3840                ; Prevent paddle from moving out of the screen
    jbe skipmove2

ClearPaddleB:
    mov word [es:di], 0x0720    ; Clear the screen content
    add di, 2
    loop ClearPaddleB

    ; Update the paddle location
    mov di, [cs:paddleB_loc]
    sub di, 2                   ; Move left
    mov [cs:paddleB_loc], di

    ; Redraw the paddle at the new location
    mov cx, 20                  ; Paddle width
Player2:
    mov word [es:di], 0xF120    ; Redraw paddle
    add di, 2
    loop Player2

skipmove2:
    ret

;----------------------------------------
; Move Paddle B to the Right
;----------------------------------------
movePaddleB_Right:
    mov ax, 0xb800              ; Video memory segment
    mov es, ax

    ; Clear the paddle's last position
    mov cx, 20                  ; Paddle width
    mov di, [cs:paddleB_loc]    ; Current paddle position
    cmp di, 3960                ; Prevent paddle from moving out of the screen
    je skipmove3

ClearPaddleright2:
    mov word [es:di], 0x0720    ; Clear the screen content
    add di, 2
    loop ClearPaddleright2

    ; Update the paddle location
    mov di, [cs:paddleB_loc]
    add di, 2                   ; Move right
    mov [cs:paddleB_loc], di

    ; Redraw the paddle at the new location
    mov cx, 20                  ; Paddle width
rightside2:
    mov word [es:di], 0xF120    ; Redraw paddle
    add di, 2
    loop rightside2

skipmove3:
    ret
	

;-----------------------------------------------------------------------
; ### Subroutine to initialize game's screen according to given requirements
;-----------------------------------------------------------------------
InitialScreen:
    call clearScreen

    ; Paddle for Player A
    push byte 0       ; Row number
    push byte 30   ; Column number
    call findLocation   ; Updates DI
	mov word [paddleA_loc], di
    mov cx, 20          ; 20 cells wide
    mov ax, 0xb800
    mov es, ax
PlayerA:
    mov word [es:di], 0xF120
    add di, 2
    loop PlayerA

    ; Paddle for Player B
    push byte 24        ; Row number
    push byte 30        ; Column number
    call findLocation   ; Updates DI
	mov word [paddleB_loc], di
    mov cx, 20          ; 20 cells wide
PlayerB:
    mov word [es:di], 0xF120
    add di, 2
    loop PlayerB

    ; Ball on Player B's Side
    push byte 23        ; Row number
    push byte 40        ; Column number
    call findLocation   ; Updates DI
    mov word [es:di], 0x0F2A

    ret

;------------------------------------------------------
; ### ISR for Keyboard Interrupt
;------------------------------------------------------
keyboradISR:
    push ax
    push es

    ;----------------------------------------
    ; Check if the game should start
    ;----------------------------------------
    in al, 0x60             ; Get scan code from keyboard port 0x60
    cmp al, 0x39            ; Check for key 'Space key' press
    jne Next                ; If not 'Space key', skip
    mov word [cs:Game_Start], 1

Next:
    ;----------------------------------------
    ; Determine player's turn and handle paddle movement
    ;----------------------------------------

    cmp word [cs:PlayerAturn], 1
    je MovePlayerA_paddle   ; If it's Player A's turn, handle their paddle

    cmp word [cs:PlayerBturn], 1
    je MovePlayerB_paddle   ; If it's Player B's turn, handle their paddle

    ;----------------------------------------
    ; Player A's paddle movement handling
    ;----------------------------------------
MovePlayerA_paddle:
    in al, 0x60             ; Get scan code from keyboard port 0x60
    cmp al, 0x4B            ; Check for 'Left Arrow key' press
    je Move_paddleA_onLeft
    cmp al, 0x4D            ; Check for 'Right Arrow key' press
    je Move_paddleA_onRight
    jmp skipAction          ; Skip if no relevant key is pressed

Move_paddleA_onLeft:
    call movePaddleA_left   ; Move Player A's paddle left
    jmp skipAction

Move_paddleA_onRight:
    call movePaddleA_Right  ; Move Player A's paddle right
    jmp skipAction

    ;----------------------------------------
    ; Player B's paddle movement handling
    ;----------------------------------------
MovePlayerB_paddle:
    in al, 0x60             ; Get scan code from keyboard port 0x60
    cmp al, 0x4B            ; Check for 'Left Arrow key' press
    je Move_paddleB_onLeft
    cmp al, 0x4D            ; Check for 'Right Arrow key' press
    je Move_paddleB_onRight
    jmp skipAction          ; Skip if no relevant key is pressed

Move_paddleB_onLeft:
    call movePaddleB_left   ; Move Player B's paddle left
    jmp skipAction

Move_paddleB_onRight:
    call movePaddleB_Right  ; Move Player B's paddle right
    jmp skipAction

    ;----------------------------------------
    ; End of ISR: Acknowledge interrupt and return
    ;----------------------------------------
skipAction:
    mov al, 0x20            ; Send End of Interrupt (EOI) signal
    out 0x20, al

    pop es
    pop ax
    iret                    ; Return from interrupt

	
	
;---------------------------------------------------
; #### ISR for Timer Interrupt (Ball Movement)
;---------------------------------------------------
BallMovement:
    push ax
    mov ax, 0xb800
    mov es, ax

    ; Check if the game has started
    cmp word [cs:Game_Start], 1
    jne endSubroutine

    ; Check if any player has won
    cmp word [cs:A_Scores], 5
    je Display_A_Scores

    cmp word [cs:B_Scores], 5
    je Display_B_Scores
    jmp Comparisons

Display_A_Scores:
    call A_Scores_display
    jmp endSubroutine

Display_B_Scores:
    call B_Scores_display
    jmp endSubroutine

Comparisons:
    ; Handle different ball bounce states
    cmp word [cs:Right_Up_BounceFlag], 1
    je BounceBack_rightwardUp

    cmp word [cs:Right_Down_BounceFlag], 1
    je BounceBack_rightwardDown

    cmp word [cs:Left_Down_BounceFlag], 1
    je BounceBack_LeftwardDown

    cmp word [cs:Left_Up_BounceFlag], 1
    je BounceBack_LeftwardUp

    jmp endSubroutine

;----------------------------------------
; Bounce ball in the upper right direction
;----------------------------------------
BounceBack_rightwardUp:
    call findBallLocation
    mov word [es:si], 0x0720

    add byte [cs:BallColumn], 1
    sub byte [cs:BallRow], 1

    call findBallLocation
    mov word [es:si], 0x0F2A

    cmp byte [cs:BallRow], 1
    jne nextComparison3

    ; Change bounce direction
    mov word [cs:Right_Up_BounceFlag], 0
    mov word [cs:Right_Down_BounceFlag], 1

    ; Switch turn
    mov word [cs:PlayerAturn], 0
    mov word [cs:PlayerBturn], 1

    ; Check for goal
    cmp word [es:si-158], 0xF120
    jne Increment_ScoreB
    jmp endSubroutine

Increment_ScoreB:
    inc word [cs:B_Scores]

    ; Reset ball position
    mov word [cs:Game_Start], 0
    mov word [es:si], 0x0720
    mov byte [cs:BallRow], 1
    mov byte [cs:BallColumn], 40
    call findBallLocation
    mov word [es:si], 0x0F2A
    jmp endSubroutine

nextComparison3:
    cmp byte [cs:BallColumn], 79
    jne endSubroutine

    mov word [cs:Right_Up_BounceFlag], 0
    mov word [cs:Left_Up_BounceFlag], 1
    jmp endSubroutine

;----------------------------------------
; Bounce ball in the lower right direction
;----------------------------------------
BounceBack_rightwardDown:
    call findBallLocation
    cmp word [es:si], 0xF120
    je endSubroutine
    mov word [es:si], 0x0720

    add byte [cs:BallColumn], 1
    add byte [cs:BallRow], 1

    call findBallLocation
    mov word [es:si], 0x0F2A

    cmp byte [cs:BallColumn], 79
    jne nextComparison

    cmp byte [cs:BallRow], 23
    je setLeftUp

    mov word [cs:Right_Down_BounceFlag], 0
    mov word [cs:Left_Down_BounceFlag], 1
    jmp endSubroutine

nextComparison:
    cmp byte [cs:BallRow], 23
    jne endSubroutine

    ; Switch turn
    mov word [cs:PlayerBturn], 0
    mov word [cs:PlayerAturn], 1

    ; Check for goal
    cmp word [es:si+158], 0xF120
    jne Increment_ScoreA

    mov word [cs:Right_Down_BounceFlag], 0
    mov word [cs:Right_Up_BounceFlag], 1
    jmp endSubroutine

Increment_ScoreA:
    inc word [cs:A_Scores]

    ; Reset ball position
    mov word [cs:Game_Start], 0
    mov word [es:si], 0x0720
    mov byte [cs:BallRow], 23
    mov byte [cs:BallColumn], 40
    call findBallLocation
    mov word [es:si], 0x0F2A

    mov word [cs:Right_Down_BounceFlag], 0
    mov word [cs:Right_Up_BounceFlag], 1
    jmp endSubroutine

setLeftUp:
    mov word [cs:Right_Down_BounceFlag], 0
    mov word [cs:Left_Up_BounceFlag], 1
    jmp endSubroutine

;----------------------------------------
; Bounce ball in the lower left direction
;----------------------------------------
BounceBack_LeftwardDown:
    call findBallLocation
    cmp word [es:si], 0xF120
    je endSubroutine
    mov word [es:si], 0x0720

    sub byte [cs:BallColumn], 1
    add byte [cs:BallRow], 1

    call findBallLocation
    mov word [es:si], 0x0F2A

    cmp byte [cs:BallRow], 23
    jne nextComparison4

    ; Switch turn
    mov word [cs:PlayerBturn], 0
    mov word [cs:PlayerAturn], 1

    ; Check for goal
    cmp word [es:si+160], 0xF120
    jne Increment_AScore

    mov word [cs:Left_Down_BounceFlag], 0
    mov word [cs:Left_Up_BounceFlag], 1
    jmp endSubroutine

Increment_AScore:
    inc word [cs:A_Scores]

    mov word [cs:Left_Down_BounceFlag], 0
    mov word [cs:Right_Up_BounceFlag], 1

    ; Reset ball position
    mov word [cs:Game_Start], 0
    mov word [es:si], 0x0720
    mov byte [cs:BallRow], 23
    mov byte [cs:BallColumn], 40
    call findBallLocation
    mov word [es:si], 0x0F2A
    jmp endSubroutine

nextComparison4:
    cmp byte [cs:BallColumn], 0
    jne endSubroutine

    mov word [cs:Left_Down_BounceFlag], 0
    mov word [cs:Right_Down_BounceFlag], 1
    jmp endSubroutine

;----------------------------------------
; Bounce ball in the upper left direction
;----------------------------------------
BounceBack_LeftwardUp:
    call findBallLocation
    cmp word [es:si], 0xF120
    je endSubroutine
    mov word [es:si], 0x0720

    sub byte [cs:BallColumn], 1
    sub byte [cs:BallRow], 1

    call findBallLocation
    mov word [es:si], 0x0F2A

    cmp byte [cs:BallRow], 1
    jne nextComparison2

    ; Switch turn
    mov word [cs:PlayerAturn], 0
    mov word [cs:PlayerBturn], 1

    ; Check for goal
    cmp word [es:si-158], 0xF120
    jne Increment_Score_B

    mov word [cs:Left_Up_BounceFlag], 0
    mov word [cs:Left_Down_BounceFlag], 1
    jmp endSubroutine

Increment_Score_B:
    inc word [cs:B_Scores]

    ; Reset ball position
    mov word [cs:Game_Start], 0
    mov word [es:si], 0x0720
    mov byte [cs:BallRow], 1
    mov byte [cs:BallColumn], 40
    call findBallLocation
    mov word [es:si], 0x0F2A

    mov word [cs:Left_Up_BounceFlag], 0
    mov word [cs:Left_Down_BounceFlag], 1
    jmp endSubroutine

nextComparison2:
    cmp byte [cs:BallColumn], 0
    jne endSubroutine

    mov word [cs:Left_Up_BounceFlag], 0
    mov word [cs:Right_Up_BounceFlag], 1
    jmp endSubroutine

endSubroutine:
    mov al, 0x20
    out 0x20, al

    pop ax
    iret


;----------------------------------------------
; Program starts from here
;----------------------------------------------
start:
    call InitialScreen
	
	mov ax,0
	mov es,ax
	
    ; Hook the timer interrupt (08h) and keyborad Interrupt (09h)
    cli
    mov word [es:8*4], BallMovement
    mov word [es:8*4+2], cs
	mov word [es:9*4], keyboradISR
    mov word [es:9*4+2], cs
    sti
	
   ; Keep TSR in memory
    mov dx, start
    add dx, 15
    mov cl, 4
    shr dx, cl
    mov ax, 0x3100
    int 0x21                ; DOS TSR function to keep in memory
	
;----------------------------------------------
;                  THE END
;----------------------------------------------