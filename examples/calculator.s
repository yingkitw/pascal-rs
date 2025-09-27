    .intel_syntax noprefix
    .section .text
    main:
        mov eax, 15
        mov [rbp - 8], eax
        mov eax, 3
        mov [rbp - 16], eax
        mov eax, 1
        mov [rbp - 32], eax
        mov eax, [rbp - 32]
        push rax
        mov eax, 1
        pop rdx
        cmp eax, edx
        sete al
        movzx eax, al
        test eax, eax
        je _else_1
        mov eax, [rbp - 8]
        push rax
        mov eax, [rbp - 16]
        pop rdx
        add eax, edx
        mov [rbp - 24], eax
        jmp _endif_2
    _else_1:
        mov eax, [rbp - 32]
        push rax
        mov eax, 2
        pop rdx
        cmp eax, edx
        sete al
        movzx eax, al
        test eax, eax
        je _else_3
        mov eax, [rbp - 8]
        push rax
        mov eax, [rbp - 16]
        pop rdx
        sub edx, eax
        mov eax, edx
        mov [rbp - 24], eax
        jmp _endif_4
    _else_3:
        mov eax, [rbp - 32]
        push rax
        mov eax, 3
        pop rdx
        cmp eax, edx
        sete al
        movzx eax, al
        test eax, eax
        je _else_5
        mov eax, [rbp - 8]
        push rax
        mov eax, [rbp - 16]
        pop rdx
        imul eax, edx
        mov [rbp - 24], eax
        jmp _endif_6
    _else_5:
        mov eax, [rbp - 32]
        push rax
        mov eax, 4
        pop rdx
        cmp eax, edx
        sete al
        movzx eax, al
        test eax, eax
        je _else_7
        mov eax, [rbp - 16]
        push rax
        mov eax, 0
        pop rdx
        cmp eax, edx
        setne al
        movzx eax, al
        test eax, eax
        je _else_9
        mov eax, [rbp - 8]
        push rax
        mov eax, [rbp - 16]
        pop rdx
        xchg eax, edx
        cdq
        idiv edx
        mov [rbp - 24], eax
        jmp _endif_10
    _else_9:
        mov eax, 0
        mov [rbp - 24], eax
    _endif_10:
        jmp _endif_8
    _else_7:
        mov eax, 0
        mov [rbp - 24], eax
        mov eax, [rbp - 24]
        push rax
        mov eax, 100
        pop rdx
        cmp edx, eax
        setg al
        movzx eax, al
        test eax, eax
        je _else_11
        mov eax, [rbp - 24]
        push rax
        mov eax, 1000
        pop rdx
        cmp edx, eax
        setg al
        movzx eax, al
        test eax, eax
        je _else_13
        mov eax, [rbp - 24]
        push rax
        mov eax, 1000
        pop rdx
        sub edx, eax
        mov eax, edx
        mov [rbp - 24], eax
        jmp _endif_14
    _else_13:
        mov eax, [rbp - 24]
        push rax
        mov eax, 100
        pop rdx
        sub edx, eax
        mov eax, edx
        mov [rbp - 24], eax
    _endif_14:
    _else_11:
    _endif_12:
    _endif_8:
    _endif_6:
    _endif_4:
    _endif_2:
