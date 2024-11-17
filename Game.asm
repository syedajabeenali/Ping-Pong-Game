[org 0x100]
jmp start

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

;-----------------------------------------------------------------------
; Subroutine to initialize game's screen according to given requirements
;-----------------------------------------------------------------------
InitialScreen:
    call clearScreen

    ; Paddle for Player A
    push byte 0         ; Row number
    push byte 30        ; Column number
    call findLocation   ; Updates DI
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

;------------------------------
; Program starts from here
;------------------------------
start:
    call InitialScreen
    mov ax, 0x4C00
    int 21h
