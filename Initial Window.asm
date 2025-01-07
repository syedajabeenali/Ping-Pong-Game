[org 0x0100]
jmp start

pos: dw 0
backpos: dw 2000
divisor : dw 6000
pingpong :   db "    #   PPPPP  III  N   N  GGGGG   PPPPP OOOOO  N   N  GGGGG     #", 0
             db "   #   P   P   I   NN  N  G       P   P O   O  NN  N  G         #", 0
             db "   #   PPPPP   I   N N N  G  GG   PPPPP O   O  N N N  G  GG     #", 0
             db "   #   P       I   N  NN  G   G   P     O   O  N  NN  G   G     #", 0
             db "   #   P      III  N   N  GGGGG   P     OOOOO  N   N  GGGGG     #", 0
			 
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
display_PingPong:

    mov ah, 0x13            ; BIOS Teletype function
    mov al, 1               ; Write string
    mov bh, 0               ; Page number
    mov bl, 0x8D          ; Green color

    mov cx, 66             ; Length of "PLAYER B WINS!"
    push cs
    pop es                  ; Set ES to current segment
  
    int 0x10                ; Call BIOS
    
    ret	2

delay :
pusha

mov cx,0xffff

delay1:
    loop delay1
    popa
    ret 

star:

     cmp word [pos],1000
    je clearscreen
    inc cx
mov ax, 0xb800
mov es, ax
mov word di,[pos]
shl di,1
mov ah,0x41
mov al,'*'
mov word [es:di], ax
add di,160
mov word [es:di],ax
add di,160
mov word [es:di],ax
mov word di,[backpos]
shl di,1
mov word [es:di-2],ax
mov word [es:di-162],ax
mov word [es:di-322],ax


cmp cx,80
je addtworows

inc word [pos]
dec word [backpos]



mov ax, 0x20
out 0x20, al
iret
start:

call clearScreen

mov ax,[divisor]
out 0x40,al
mov al,ah
out 0x40, al
    mov cx,0
    xor ax,ax
    mov es,ax
    cli
    mov word[es:8*4],star
    mov word[es:8*4+2],cs
   
    sti
    jmp $
	
    terminate:
    mov ax,0x4c00
    int 21h


addtworows:
add word [pos],161
sub word [backpos],161
mov cx,0

mov ax, 0x20
out 0x20, al
iret
jmp start


 cmp word [pos],1000
    je terminate

    clearscreen:
       

mov di,1678
mov ax, 0xb800
mov es, ax


loop23:
mov cx,6
mov ax,0x0020
loop1:

   
mov  [es:di],ax
shl bx,1
mov [es:di+bx+2],ax
shr bx,1
add di,160
loop loop1
call delay
add bx,2
mov di,1678
sub di,bx
cmp di,1598
jne loop23

mov dx, 10*256+4
  mov bp, pingpong        
call display_PingPong
mov dx, 11*256+4
  mov bp, pingpong+66   
call display_PingPong
mov dx, 12*256+4
  mov bp, pingpong+132  
call display_PingPong
mov dx, 13*256+4
  mov bp, pingpong+198
call display_PingPong
mov dx, 14*256+4
  mov bp, pingpong+264 
call display_PingPong