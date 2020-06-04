DATA SEGMENT
	intmsg db 'please input the integral part:', 0dh 0ah, '$'
	decmsg db 'please input the decimal part:', 0dh, 0ah, '$'
	orimsg db 'please input the origianl type of the number system '
           db  '(between 2 and 16):', 0dh,0ah,'$'
	desmsg db 'please input the desired type of the number system '
		   db  '(between 2 and 16):', 0dh,0ah,'$'
	errormsg db 'invalid input', 0dh, 0ah, '$'
	msg db 'press esc to end, or other keys to continue',0dh,0ah,'$'

	; 存放输入的原进制
	orismax db 3
	orisact db ?
	oristri db 3 dup(?)
	; 存放输入的目的进制
	dessmax db 3
	dessact db ?
	desstri db 3 dup(?)

	; 存放原来进制数
	oritype db ?
	; 存放目的进制数
	destype db ?
	
	; 存放输入的整数部分
	intsmax db 20
	intsact db ?
	intstri db 20 dup(?)
	; 存放输入的小数部分
	decsmax db 20
	decsact db ?
	decstri db 20 dup(?)

	; 存放预处理后的数据
	intlen db ? ;整数部分的长度
	intnum db 23 dup(?) ;整数部分的数字
	declen db ? ;小数部分的长度
	decnum db 23 dup(?) ;小数部分的数字

	; 存放运算过程中的中间变量
	intbuf 20 dup(?)    ;整数的余数
	decbuf 20 dup(?)    ;小数的进位



DATA ENDS

STACKS SEGMENT STACK
	db 100 dup(?)
STACKS ENDS

CODE SEGMENT
    ASSUME CS:CODE,DS:DATA,SS:STACKS
    START:MOV AX,DATA
    MOV DS,AX
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
    lea dx, orisamx
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

    mov bx, 0
    call precheck
    cmp bx, 1
    je back    ;start over if error

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
    ；       不符合则输出errormsg，bx赋值1，结束proc
    ；       符合则将进制转化成16进制数字（2-10），分别储存在oritype destype中
    ; 2. 检查是否有超出进制的数，如果有则输出errormsg，bx赋值1，并结束proc
    ; 3. 检查是否是负数（第一位是否是‘-’号），如果是输出‘-’
    ; 4. 将数据整数和小数部分的位数补充为4的倍数，整数在前面添零，小数在尾部添零
    ; 5. 添零后整数部分长度为4*n，则将n保存在内存intlen中，小数同理
    ; 6. 将数据从ASCII码转换成数字，并保存在内存中
    ; Receivers：
    ;	内存：oristri desstri intsact intstri decsact decstri
    ; Returns:
    ;	内存：oritype destype intlen intnum declen decnum
    ;	寄存器：bx 输入错误时为1，正常为0
    ; Requires: nothing
    ; -----------------------------------

    ret
    precheck endp


	; -----------------------------------
    inttrans proc
    ; function: 整数部分的进制转化，并输出
    ; Receivers：
    ;	内存：oritype intlen intnum
    ; Returns: nothing
    ; Requires: nothing
    ; -----------------------------------

    ret
    inttrans endp


    ; -----------------------------------
    dectrans proc
    ; function: 小数部分的进制转化，并输出
    ; Receivers：
    ;	内存：declen decnum
    ; Returns: nothing
    ; Requires: nothing
    ; -----------------------------------

    ret
    dectrans endp


CODE ENDS
    END START
