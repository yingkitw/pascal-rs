    .intel_syntax noprefix
    .section .text
    main:
        mov eax, 10
        mov [rbp - 8], eax
        mov eax, 5
        mov [rbp - 16], eax
        mov eax, 3
        mov [rbp - 24], eax
        mov eax, [rbp - 8]
        push rax
        mov eax, [rbp - 16]
        pop rdx
        cmp edx, eax
        setg al
        movzx eax, al
        mov [rbp - 32], eax
        mov eax, [rbp - 16]
        push rax
        mov eax, [rbp - 24]
        pop rdx
        cmp edx, eax
        setl al
        movzx eax, al
        mov [rbp - 40], eax
        mov eax, [rbp - 32]
        push rax
        mov eax, [rbp - 40]
        pop rdx
        and eax, edx
        test eax, eax
        je _else_1
        mov eax, [rbp - 8]
        push rax
        mov eax, [rbp - 16]
        pop rdx
        add eax, edx
        mov [rbp - 24], eax
        mov eax, [rbp - 32]
        push rax
        mov eax, [rbp - 40]
        pop rdx
        or eax, edx
        test eax, eax
        je _else_3
        mov eax, [rbp - 24]
        push rax
        mov eax, 2
        pop rdx
        imul eax, edx
        mov [rbp - 24], eax
    _else_3:
    _endif_4:
    _else_1:
    _endif_2:
