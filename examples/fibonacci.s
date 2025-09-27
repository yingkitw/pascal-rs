    .intel_syntax noprefix
    .section .text
    main:
        mov eax, 10
        mov [rbp - 8], eax
        mov eax, 0
        mov [rbp - 24], eax
        mov eax, 1
        mov [rbp - 32], eax
        mov eax, [rbp - 8]
        push rax
        mov eax, 1
        pop rdx
        cmp edx, eax
        setge al
        movzx eax, al
        test eax, eax
        je _else_1
        mov eax, [rbp - 8]
        push rax
        mov eax, 2
        pop rdx
        cmp edx, eax
        setge al
        movzx eax, al
        test eax, eax
        je _else_3
        mov eax, 3
        mov [rbp - 16], eax
    _while_start_5:
        mov eax, [rbp - 16]
        push rax
        mov eax, [rbp - 8]
        pop rdx
        cmp edx, eax
        setle al
        movzx eax, al
        test eax, eax
        jz _while_end_6
        mov eax, [rbp - 24]
        push rax
        mov eax, [rbp - 32]
        pop rdx
        add eax, edx
        mov [rbp - 40], eax
        mov eax, [rbp - 32]
        mov [rbp - 24], eax
        mov eax, [rbp - 40]
        mov [rbp - 32], eax
        mov eax, [rbp - 16]
        push rax
        mov eax, 1
        pop rdx
        add eax, edx
        mov [rbp - 16], eax
        jmp _while_start_5
    _while_end_6:
    _else_3:
    _endif_4:
    _else_1:
    _endif_2:
