    .intel_syntax noprefix
    .section .text
    main:
        mov eax, 1
        mov [rbp - 8], eax
        mov eax, 0
        mov [rbp - 16], eax
    _while_start_1:
        mov eax, [rbp - 8]
        push rax
        mov eax, 5
        pop rdx
        cmp edx, eax
        setle al
        movzx eax, al
        test eax, eax
        jz _while_end_2
        mov eax, [rbp - 16]
        push rax
        mov eax, [rbp - 8]
        pop rdx
        add eax, edx
        mov [rbp - 16], eax
        mov eax, [rbp - 8]
        push rax
        mov eax, 1
        pop rdx
        add eax, edx
        mov [rbp - 8], eax
        jmp _while_start_1
    _while_end_2:
