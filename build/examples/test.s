    .intel_syntax noprefix
    .section .text
    main:
        mov eax, [rbp - 16]
        push rax
        mov eax, 1
        pop rdx
        add eax, edx
        mov [rbp - 8], eax
