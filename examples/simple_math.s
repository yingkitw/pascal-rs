    .intel_syntax noprefix
    .section .text
    main:
        mov eax, 10
        mov [rbp - 8], eax
        mov eax, 5
        mov [rbp - 16], eax
        mov eax, [rbp - 8]
        push rax
        mov eax, [rbp - 16]
        pop rdx
        add eax, edx
        mov [rbp - 24], eax
        mov eax, [rbp - 24]
        push rax
        mov eax, 2
        pop rdx
        imul eax, edx
        mov [rbp - 24], eax
        mov eax, [rbp - 24]
        push rax
        mov eax, 3
        pop rdx
        sub edx, eax
        mov eax, edx
        mov [rbp - 24], eax
