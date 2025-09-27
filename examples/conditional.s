    .intel_syntax noprefix
    .section .text
    main:
        mov eax, 15
        mov [rbp - 8], eax
        mov eax, 10
        mov [rbp - 16], eax
        mov eax, [rbp - 8]
        push rax
        mov eax, [rbp - 16]
        pop rdx
        cmp edx, eax
        setg al
        movzx eax, al
        test eax, eax
        je _else_1
        mov eax, [rbp - 8]
        push rax
        mov eax, [rbp - 16]
        pop rdx
        sub edx, eax
        mov eax, edx
        mov [rbp - 8], eax
        jmp _endif_2
    _else_1:
        mov eax, [rbp - 16]
        push rax
        mov eax, [rbp - 8]
        pop rdx
        sub edx, eax
        mov eax, edx
        mov [rbp - 16], eax
        mov eax, [rbp - 8]
        push rax
        mov eax, [rbp - 16]
        pop rdx
        cmp eax, edx
        sete al
        movzx eax, al
        test eax, eax
        je _else_3
        mov eax, 0
        mov [rbp - 8], eax
    _else_3:
    _endif_4:
    _endif_2:
