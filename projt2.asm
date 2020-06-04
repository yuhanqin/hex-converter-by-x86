DATA SEGMENT
    intmsg db 0dh,0ah,'please input the integral part '
           db '(no more than 20 numbers):',0dh,0ah,'$'
    decmsg db 0dh,0ah,'please input the decimal part '
           db '(no more than 4 numbers):',0dh,0ah,'$'
    orimsg db 0dh,0ah,'please input '
           db 'the origianl type of the number system '
           db '(between 2 and 16):', 0dh,0ah,'$'
    desmsg db 0dh,0ah,'please input '
           db 'the desired type of the number system '
           db '(between 2 and 16):', 0dh,0ah,'$'
    dn2msg db 0dh, 0ah, 'please input '
           db 'the disired length of the decimal part '
           db '(between 1 and 20):', 0dh, 0ah,'$'
    errormsg db 0dh,0ah,'invalid input', 0dh, 0ah, '$'
    msg db 0dh,0ah,'press esc to end, '
        db 'or other keys to continue',0dh,0ah,'$'

    ; 存放输入的原进制
    orismax db 3
    orisact db ?
    oristri db 3 dup(?)
    ; 存放输入的目的进制
    dessmax db 3
    dessact db ?
    desstri db 3 dup(?)

    ; 存放原来进制数
    oritype dw ?
    ; 存放目的进制数
    destype dw ?
    
    ; 存放输入的整数部分
    intsmax db 21
    intsact db ?
    intstri db 21 dup(?)
    ; 存放输入的小数部分
    decsmax db 5
    decsact db ?
    decstri db 5 dup(?)
    ; 存放输入的小数长度
    dn2smax db 3
    dn2sact db ?
    dn2stri db 3 dup(?)

    ; 存放预处理后的数据
    intlen db ? ;整数部分的长度
    intnum dw 24 dup(?) ;整数部分的数字
    decnum dw 4 dup(0) ;小数部分的数字
    dn2    db ? ;小数部分的长度

    ; 存放运算过程中的中间变量
    w2pf db ?
    TEM DW ?
    NUM1 DW 20 DUP(?)
    LEN2 DW ?
    ANSWER DW ?
    SHUZI DW 0



DATA ENDS

STACKS SEGMENT STACK
    db 100 dup(?)
STACKS ENDS

CODE SEGMENT
    ASSUME CS:CODE,DS:DATA,SS:STACKS
    START:MOV AX,DATA
    MOV DS,AX
    MOV ES,AX
back:
    mov ah, 09h
    lea dx, msg
    int 21h
    mov ah, 01h
    int 21h
    cmp al, 1BH
    je finish    ;end the program
    mov ah, 09h
    lea dx, orimsg
    int 21h
    mov ah, 0ah
    lea dx, orismax
    int 21h    ;input the origianl type
    mov ah, 09h
    lea dx, desmsg
    int 21h
    mov ah, 0ah
    lea dx, dessmax
    int 21h    ;input the desired type
    mov ah, 09h
    lea dx, intmsg
    int 21h
    mov ah, 0ah
    lea dx, intsmax
    int 21H    ;input the integral part
    mov ah, 09h
    lea dx, decmsg
    int 21h
    mov ah, 0ah
    lea dx, decsmax
    int 21H    ;input the decimal part
    mov ah, 09h
    lea dx, dn2msg
    int 21h
    mov ah, 0ah
    lea dx, dn2smax
    int 21h    ;input the decimal part numbers

    mov bx, 0
    call precheck
    cmp bx, 1
    je back    ;start over if error

    mov ah, 06h
    mov dl, 0dh
    int 21h
    mov dl, 0ah
    int 21h

    call inttrans

    call dectrans

    jmp back    ;next mission
     
finish:
    MOV AH,4CH
    INT 21H

    ; -----------------------------------
    precheck proc
    ; function: 对数据做预处理
    ; 1. 检查输入的进制是否符合要求（2-16）
    ;       不符合则输出errormsg，bx赋值1，结束proc
    ;       符合则将进制转化成16进制数字（2-10），分别储存在oritype destype中
    ; 2. 检查是否有超出进制的数，如果有则输出errormsg，bx赋值1，并结束proc
    ; 3. 检查是否是负数（第一位是否是‘-’号），如果是输出‘-’
    ; 4. 将数据整数和小数部分的位数补充为4的倍数，整数在前面添零，小数在尾部添零
    ; 5. 添零后整数部分长度为4*n，则将n保存在内存intlen中，小数同理
    ; 6. 将数据从ASCII码转换成数字，并保存在内存中
    ; Receivers：
    ;   内存：oristri desstri intsact intstri decsact decstri
    ; Returns:
    ;   内存：oritype destype intlen intnum declen decnum
    ;   寄存器：bx 输入错误时为1，正常为0
    ; Requires: nothing
    ; -----------------------------------

    ; original type
    lea si,orisact
    cmp byte ptr [si],1
    je less10_o
    cmp byte ptr [si],2
    je more10_o
    jmp error   ; oritype less or more than 10  
less10_o:
    lea si,oristri
    cmp byte ptr [si],32h
    jb error
    cmp byte ptr [si],39h
    ja error
    mov al,[si]
    sub al,30h
    mov ah, 0
    mov oritype,ax
    jmp orityf   
more10_o:
    lea si,oristri
    cmp byte ptr [si],31h
    jne error
    cmp byte ptr [si+1],30h
    jb error
    cmp byte ptr [si+1],36h
    ja error
    mov al,[si+1]
    sub al, 26h
    mov ah, 0
    mov oritype,ax

orityf:
    ;desired type
    lea si,dessact
    cmp byte ptr [si],1
    je less10_d
    cmp byte ptr [si],2
    je more10_d
    jmp error   ; destype less or more than 10   
less10_d:
    lea si,desstri
    cmp byte ptr [si],32h
    jb error
    cmp byte ptr [si],39h
    ja error
    mov al,[si]
    sub al,30h
    mov ah, 0
    mov destype,ax
    jmp destyf  
more10_d:
    lea si,desstri
    cmp byte ptr [si],31h
    jne error
    cmp byte ptr [si+1],30h
    jb error
    cmp byte ptr [si+1],36h
    ja error
    mov al,[si+1]
    sub al, 26h
    mov ah, 0
    mov destype,ax
destyf:
    jmp errorf

error:
    mov ah,09h
    lea dx,errormsg
    int 21h
    mov bx,1
    jmp finishpre

errorf:

    ;if negative
    lea si,intstri
    cmp byte ptr [si],'-'
    jne ispos
    mov ah,09h
    mov al,'-'
    int 21h
    mov byte ptr [si], '0'
ispos:
    ;move to 4n times
    mov ax,0
    mov al, intsact
    mov bx, 0
    mov bl, 4
    div bl
    cmp ah,0
    je fourtimes
    add al, 1
    mov intlen, al
    sub ah, 4
    neg ah
    mov cx, 0
    mov cl, ah;
    mov ax, 0;
    lea di, intnum
    rep stosw
    jmp transf
fourtimes:
    mov intlen, al
    lea di, intnum

; transfer the ASCII to numbers in intergal
transf:
    lea si, intstri
    mov cx, 0
    mov cl, intsact
looptransf:
    lodsb
    cmp al, '0'
    jb error
    cmp al, '9'
    jna is09
    cmp al, 'A'
    jb error
    cmp al, 'F'
    jna iscap
    cmp al, 'a'
    jb error
    cmp al, 'f'
    jna islow
    jmp error
is09:
    sub al, 30h
    jmp transf2
iscap:
    sub al, 55
    jmp transf2
islow:
    sub al, 87
    mov bx, 0
transf2:
    mov ah, 0
    cmp ax, word ptr oritype
    jnb error2
    stosw
    loop looptransf
    jmp errorf2

error2:
    jmp error

; trnasfer ASCII to numbers in decimal
errorf2:
    lea di, decnum
    lea si, decstri
    mov cx, 0
    mov cl, decsact
looptransfdec:
    lodsb
    cmp al, '0'
    jb error2
    cmp al, '9'
    jna is09dec
    cmp al, 'A'
    jb error2
    cmp al, 'F'
    jna iscapdec
    cmp al, 'a'
    jb error2
    cmp al, 'f'
    jna islowdec
    jmp error2
is09dec:
    sub al, 30h
    jmp transf2dec
iscapdec:
    sub al, 55
    jmp transf2dec
islowdec:
    sub al, 87
    mov bx, 0
transf2dec:
    mov ah, 0
    cmp ax, word ptr oritype
    jnb error2
    mov ah, 0
    stosw
    loop looptransfdec
    
;move to 4n times
    mov ax,0
    mov al, decsact
    cmp al, 4
    je trans2f
    sub al, 4
    neg al
    mov cx, 0
    mov cl, al
    mov ax, 0
    rep stosw
trans2f:

;desired decimal part length
    lea si,dn2sact
    cmp byte ptr [si],1
    je less10_dn2
    cmp byte ptr [si],2
    je more10_dn2
    jmp error   ; dn2 less or more than 10
error3:
    jmp error  
less10_dn2:
    lea si,dn2stri
    cmp byte ptr [si],32h
    jb error3
    cmp byte ptr [si],39h
    ja error3
    mov al,[si]
    sub al,30h
    mov dn2,al
    mov bx, 0
    jmp finishpre
    
more10_dn2:
    lea si,dn2stri
    cmp byte ptr [si],31h
    jne error3
    cmp byte ptr [si+1],30h
    jb error3
    cmp byte ptr [si+1],36h
    ja error3
    mov al,[si+1]
    sub al, 20h
    mov dn2,al
    mov bx, 0
finishpre:
    ret
    precheck endp


    ; -----------------------------------
    inttrans proc
    ; function: 整数部分的进制转化，并输出
    ; Receivers：
    ;   内存：oritype intlen intnum
    ; Returns: nothing
    ; Requires: nothing
    ; -----------------------------------
    MOV CX,0
    MOV CL,intlen
    PUSH CX
    LEA DI,intnum
    MOV SI,0

;½«Êý¾Ý·Ö¸îÎª4¸öÒ»¶Î
SIXT:
    MOV BX,0
    MOV BX,WORD PTR oritype
    MOV AX,0
    MOV AX,WORD PTR [DI]
    MUL BX
    MUL BX
    MUL BX
    ADD AX,WORD PTR TEM
    MOV TEM,AX
    
    ADD DI,2
    MOV AX,0
    MOV AX,WORD PTR [DI]
    MUL BX
    MUL BX
    ADD AX,WORD PTR TEM
    MOV TEM,AX
    
    ADD DI,2
    MOV AX,0
    MOV AX,WORD PTR [DI]
    MUL BX
    ADD AX,WORD PTR TEM
    MOV TEM,AX

    ADD DI,2
    MOV AX,0
    MOV AX,WORD PTR [DI]
    ADD AX,WORD PTR TEM
    
    ADD DI,2
    MOV NUM1[SI],AX
    ADD SI,2
    DEC CX
    MOV TEM,0
    CMP CX,0
    JNE SIXT

;Ã¿¶Î·Ö±ð³ý·¨
    
    POP CX
    MOV WORD PTR LEN2,CX
CHUFA:
    MOV CX,WORD PTR LEN2
    MOV AX,0
    ADD CX,1
    LEA DI,NUM1
    REPE SCASW
    CMP CX,1
    JB NEXT_STEP

CHUFA1:    
    MOV CX,WORD PTR LEN2
    LEA DI,NUM1
    MOV BX,WORD PTR destype
    MOV AX,WORD PTR [DI]
    DIV BX
    MOV [DI],AX  ;ÉÌ·Å½øDIÀï£¬ÓàÊýÔÚDXÀï
    CMP CX,1
    JE CHUFAP
    
    
    
CHUFAPLUS:    
    MOV AX,DX
    MOV BX,WORD PTR oritype
    MUL BX
    MUL BX
    MUL BX
    MUL BX
    ADD AX,WORD PTR [DI+2] ;AX=364
    ADC DX,0
    
    ADD DI,2
    MOV BX,WORD PTR destype
    DIV BX
    MOV [DI],AX
    DEC CX
    CMP CX,1
    JNE CHUFAPLUS
    
CHUFAP:
    PUSH DX
    
    MOV AX,0
    MOV AX,WORD PTR SHUZI
    ADD AX,1
    MOV SHUZI,AX
    
    MOV DX,0
    JMP CHUFA
NEXT_STEP:
    
    LEA DI,WORD PTR ANSWER
    MOV CX,SHUZI
    
    
POPS:
    POP DX
    CMP DX,0AH
    JL L111
    JMP L112
    
L111:
    ADD DX,30H
    JMP L113
    
L112:
    ADD DX,37H
    JMP L113
       
L113:    
    MOV [DI],DX
    ADD DI,1
    LOOP POPS
    
    MOV BYTE PTR[DI],'.'
    MOV BYTE PTR[DI+1],'$'
    
    MOV AH,9
    LEA DX,ANSWER
    INT 21H
    ret
    inttrans endp


    ; -----------------------------------
    dectrans proc
    ; function: 小数部分的进制转化，并输出
    ; Receivers：
    ;   内存：declen decnum
    ; Returns: nothing
    ; Requires: nothing
    ; -----------------------------------
    mov cx, 3 ; 使用秦九韶算法获取小数部分真实值（16进制）
    mov bx, oritype ; 原来进制
    mov si, 0
    mov ax, decnum[si]
    push ax
decreal:
    add si, 2
    pop ax
    mul bx
    mov dx, word ptr decnum[si]
    add ax, dx
    push ax ; 保存获得的真实值
    loop decreal
    mov cl, dn2 ; 设置循环次数，即保留的小数位数
    mov ax, bx
    mul bx
    mul bx
    mul bx
    mov bx, ax ; 获取除数
    pop ax ; 导入真实值
pfdec:
    mov dx, 0
    push bx ; 保存除数
    mov bx, destype
    mul bx
    pop bx ; 导入除数
    div bx
    mov byte ptr w2pf, al ; 保存并准备输出商
    push dx ; 保存余数
    call pf ; 输出商
    pop ax ; 导入余数到ax
    loop pfdec
    ret
    dectrans endp

pf proc
	; 输出内存w2pf中保存的字符
    mov dl, w2pf
    cmp dl, 0AH
    jb isnumpf
    add dl, 37H
    jmp ready2pf
isnumpf:
    add dl, 30H
ready2pf:
    mov ah, 6
    int 21h
    ret
pf endp


CODE ENDS
    END START