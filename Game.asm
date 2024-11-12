[org 0x0100]
jmp start


;-----------------------------------------------------------------
start:

mov ax, 0x4c00 ; terminate program
int 0x21