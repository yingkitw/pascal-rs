    .intel_syntax noprefix
    .section .text
    main:
        mov eax, 42
        mov [rbp - 8], eax
        mov eax, [rbp - 8]
        push rax
        mov eax, 1
        pop rdx
        add eax, edx
        mov [rbp - 16], eax
        mov eax, [rbp - 16]
        push rax
        mov eax, 40
        pop rdx
        cmp edx, eax
        setg al
        movzx eax, al
        test eax, eax
        je _else_1
        mov eax, 100
        mov [rbp - 8], eax
        jmp _endif_2
    _else_1:
        mov eax, 200
        mov [rbp - 8], eax
    _while_start_3:
        mov eax, [rbp - 8]
        push rax
        mov eax, 0
        pop rdx
        cmp edx, eax
        setg al
        movzx eax, al
        test eax, eax
        jz _while_end_4
        mov eax, [rbp - 8]
        push rax
        mov eax, 1
        pop rdx
        sub edx, eax
        mov eax, edx
        mov [rbp - 8], eax
        jmp _while_start_3
    _while_end_4:
    _endif_2:
