    .intel_syntax noprefix
    .section .text
    main:
        mov eax, 0
        mov [rbp - 24], eax
        mov eax, 1
        mov [rbp - 32], eax
        mov eax, 0
        mov [rbp - 40], eax
        mov eax, 1
        mov [rbp - 8], eax
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
        mov eax, [rbp - 24]
        push rax
        mov eax, [rbp - 8]
        pop rdx
        add eax, edx
        mov [rbp - 24], eax
        mov eax, [rbp - 8]
        push rax
        mov eax, 1
        pop rdx
        add eax, edx
        mov [rbp - 8], eax
        mov eax, 1
        mov [rbp - 8], eax
    _while_start_3:
        mov eax, [rbp - 8]
        push rax
        mov eax, 3
        pop rdx
        cmp edx, eax
        setle al
        movzx eax, al
        test eax, eax
        jz _while_end_4
        mov eax, 1
        mov [rbp - 16], eax
    _while_start_5:
        mov eax, [rbp - 16]
        push rax
        mov eax, 3
        pop rdx
        cmp edx, eax
        setle al
        movzx eax, al
        test eax, eax
        jz _while_end_6
        mov eax, [rbp - 32]
        push rax
        mov eax, [rbp - 8]
        pop rdx
        imul eax, edx
        push rax
        mov eax, [rbp - 16]
        pop rdx
        imul eax, edx
        mov [rbp - 32], eax
        mov eax, [rbp - 16]
        push rax
        mov eax, 1
        pop rdx
        add eax, edx
        mov [rbp - 16], eax
        mov eax, [rbp - 8]
        push rax
        mov eax, 1
        pop rdx
        add eax, edx
        mov [rbp - 8], eax
        jmp _while_start_5
    _while_end_6:
        mov eax, 0
        mov [rbp - 40], eax
    _while_start_7:
        mov eax, [rbp - 40]
        push rax
        mov eax, 10
        pop rdx
        cmp edx, eax
        setl al
        movzx eax, al
        push rax
        mov eax, [rbp - 24]
        push rax
        mov eax, 50
        pop rdx
        cmp edx, eax
        setl al
        movzx eax, al
        pop rdx
        and eax, edx
        test eax, eax
        jz _while_end_8
        mov eax, [rbp - 40]
        push rax
        mov eax, 2
        pop rdx
        xchg eax, edx
        cdq
        idiv edx
        mov eax, edx
        push rax
        mov eax, 0
        pop rdx
        cmp eax, edx
        sete al
        movzx eax, al
        test eax, eax
        je _else_9
        mov eax, [rbp - 24]
        push rax
        mov eax, [rbp - 40]
        pop rdx
        add eax, edx
        mov [rbp - 24], eax
        jmp _endif_10
    _else_9:
        mov eax, [rbp - 24]
        push rax
        mov eax, 1
        pop rdx
        sub edx, eax
        mov eax, edx
        mov [rbp - 24], eax
        mov eax, [rbp - 40]
        push rax
        mov eax, 1
        pop rdx
        add eax, edx
        mov [rbp - 40], eax
    _endif_10:
        mov eax, 0
        mov [rbp - 8], eax
    _loop_start_11:
        mov eax, [rbp - 8]
        push rax
        mov eax, 1
        pop rdx
        add eax, edx
        mov [rbp - 8], eax
        mov eax, [rbp - 24]
        push rax
        mov eax, [rbp - 8]
        pop rdx
        add eax, edx
        mov [rbp - 24], eax
        mov eax, [rbp - 8]
        push rax
        mov eax, 5
        pop rdx
        cmp edx, eax
        setge al
        movzx eax, al
        cmp eax, 0
        je _loop_end_12
        jmp _loop_start_11
    _loop_end_12:
        jmp _while_start_7
    _while_end_8:
        jmp _while_start_3
    _while_end_4:
        jmp _while_start_1
    _while_end_2:
