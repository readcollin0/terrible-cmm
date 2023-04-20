.data
glob_1_last_buttons:
    .word 0x0
glob_2_timer_last_tick:
    .word 0x0
glob_3_heights:
    .word 11,11,11,11,11,12,12,12,12,11,11,11,11,11,11,11,10,10,10,10,10,10,10,10,10,10,10,10,10,11,11,11,12,12,12,13,13,13,14,14,15,15,16,16,17,17,18,18,19,19,20,20,21,21,22,23,24,25,24,23,23,22,21,21,20,19,18,15,15,14,13,13,12,11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,11,11,11,11,12,12,12,12,11,11,11,11,11,11,11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,11,11,11,11,12,12,12,12,11,11,11,11,11,11,11,10,10,10,10,10,10,10,10,10,10,10,10,10,11,11,11,12,12,12,13,13,13,14,14,15,15,16,16,17,17,18,18,19,19,20,20,21,21,22,23,24,25,24,23,23,22,21,21,20,19,18,15,15,14,13,13,12,11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10
glob_4_heights_bak:
    .word 11,11,11,11,11,12,12,12,12,11,11,11,11,11,11,11,10,10,10,10,10,10,10,10,10,10,10,10,10,11,11,11,12,12,12,13,13,13,14,14,15,15,16,16,17,17,18,18,19,19,20,20,21,21,22,23,24,25,24,23,23,22,21,21,20,19,18,15,15,14,13,13,12,11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,11,11,11,11,12,12,12,12,11,11,11,11,11,11,11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,11,11,11,11,12,12,12,12,11,11,11,11,11,11,11,10,10,10,10,10,10,10,10,10,10,10,10,10,11,11,11,12,12,12,13,13,13,14,14,15,15,16,16,17,17,18,18,19,19,20,20,21,21,22,23,24,25,24,23,23,22,21,21,20,19,18,15,15,14,13,13,12,11,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10
glob_5_integrity_error_str:
    .word 73,78,84,69,71,82,73,84,89,32,69,82,82,79,82
glob_6_n_str:
    .word 110
glob_7_h1_str:
    .word 104,49
glob_8_h2_str:
    .word 104,50
glob_9_h1v_str:
    .word 104,49,118
glob_10_h2v_str:
    .word 104,50,118
glob_11_heights_progress:
    .word 0x0
glob_12_progress_str:
    .word 112,114,111,103,114,101,115,115
glob_13_heights_addr_str:
    .word 104,101,105,103,104,116,115,95,97,100,100,114
glob_14_n1_str:
    .word 110,49
glob_15_height_str:
    .word 104,101,105,103,104,116
glob_16_addr_str:
    .word 97,100,100,114
.text
    li sp, 0x4000
main:
    addi sp, sp, -0x4
    sw ra, 0(sp)
    jal timer_init
    jal heights_integrity_check
L_17_while:
    jal draw_sky
    jal draw_heights
    jal heights_integrity_check
    b L_17_while
L_18_while_end:
ret_main:
    lw ra, 0(sp)
    addi sp, sp, 0x4
    ret
timer_init:
    li t0, 0x7F30
    sw zero, 0(t0)
ret_timer_init:
    ret
heights_integrity_check:
    addi sp, sp, -0x18
    sw ra, 0(sp)
    sw s4, 4(sp)
    sw s3, 8(sp)
    sw s2, 12(sp)
    sw s1, 16(sp)
    sw s0, 20(sp)
    la s0, glob_3_heights
    la s1, glob_4_heights_bak
    li s4, 0x0
    li tp, 0x71
    bgt s4, tp, L_23_while_end
L_19_while:
    lw s2, 0(s0)
    lw s3, 0(s1)
    beq s2, s3, L_22_else
    li a0, 0xF00
    li a1, 0x0
    jal draw_set_color
    li a0, 0x0
    li a1, 0x0
    la a2, glob_5_integrity_error_str
    li a3, 0xF
    jal draw_str_at
    la a0, glob_5_integrity_error_str
    li a1, 0xF
    mv a2, s0
    jal io_uart_write_str
    jal io_uart_write_newline
    la a0, glob_6_n_str
    li a1, 0x1
    mv a2, s4
    jal io_uart_display_num
    la a0, glob_7_h1_str
    li a1, 0x2
    mv a2, s0
    jal io_uart_display_num
    la a0, glob_8_h2_str
    li a1, 0x2
    mv a2, s1
    jal io_uart_display_num
    la a0, glob_9_h1v_str
    li a1, 0x3
    mv a2, s2
    jal io_uart_display_num
    la a0, glob_10_h2v_str
    li a1, 0x3
    mv a2, s3
    jal io_uart_display_num
L_20_while:
    b L_20_while
L_21_while_end:
L_22_else:
    addi s4, s4, 0x1
    addi s0, s0, 0x4
    addi s1, s1, 0x4
    li tp, 0x71
    ble s4, tp, L_19_while
L_23_while_end:
ret_heights_integrity_check:
    lw ra, 0(sp)
    lw s4, 4(sp)
    lw s3, 8(sp)
    lw s2, 12(sp)
    lw s1, 16(sp)
    lw s0, 20(sp)
    addi sp, sp, 0x18
    ret
draw_set_color:
    li t0, 0x7F34
    slli a1, a1, 0xC
    or t1, a0, a1
    sw t1, 0(t0)
ret_draw_set_color:
    ret
draw_str_at:
    li t0, 0x8000
    slli a0, a0, 0x2
    or t0, t0, a0
    slli a1, a1, 0x9
    or t0, t0, a1
    ble a3, zero, L_25_while_end
L_24_while:
    lw t1, 0(a2)
    andi t1, t1, 0xFF
    sw t1, 0(t0)
    addi a3, a3, -0x1
    addi t0, t0, 0x4
    addi a2, a2, 0x4
    bgt a3, zero, L_24_while
L_25_while_end:
ret_draw_str_at:
    ret
io_uart_write_str:
    addi sp, sp, -0xC
    sw ra, 0(sp)
    sw s1, 4(sp)
    sw s0, 8(sp)
    mv s0, a0
    mv s1, a1
    ble s1, zero, L_27_while_end
L_26_while:
    lw t0, 0(s0)
    mv a0, t0
    jal io_uart_write
    addi s0, s0, 0x4
    addi s1, s1, -0x1
    bgt s1, zero, L_26_while
L_27_while_end:
ret_io_uart_write_str:
    lw ra, 0(sp)
    lw s1, 4(sp)
    lw s0, 8(sp)
    addi sp, sp, 0xC
    ret
io_uart_write:
    li t0, 0x7F08
L_28_do_while:
    lw t1, 0(t0)
    bne t1, zero, L_28_do_while
L_29_do_while_break:
    sw a0, 0(t0)
ret_io_uart_write:
    ret
io_uart_write_newline:
    addi sp, sp, -0x4
    sw ra, 0(sp)
    li a0, 0xA
    jal io_uart_write
    li a0, 0xD
    jal io_uart_write
ret_io_uart_write_newline:
    lw ra, 0(sp)
    addi sp, sp, 0x4
    ret
io_uart_display_num:
    addi sp, sp, -0x8
    sw ra, 0(sp)
    sw s0, 4(sp)
    mv s0, a2
    jal io_uart_write_str
    li a0, 0x3A
    jal io_uart_write
    li a0, 0x20
    jal io_uart_write
    mv a0, s0
    jal io_uart_write_num
    jal io_uart_write_newline
ret_io_uart_display_num:
    lw ra, 0(sp)
    lw s0, 4(sp)
    addi sp, sp, 0x8
    ret
io_uart_write_num:
    addi sp, sp, -0x10
    sw ra, 0(sp)
    sw s2, 4(sp)
    sw s1, 8(sp)
    sw s0, 12(sp)
    mv s0, a0
    li s1, 0xF0000000
    li s2, 0x8
    ble s2, zero, L_32_while_end
L_30_while:
    and t0, s0, s1
    srli t0, t0, 0x1C
    addi t0, t0, 0x30
    li tp, 0x39
    ble t0, tp, L_31_else
    addi t0, t0, 0x27
L_31_else:
    mv a0, t0
    jal io_uart_write
    addi s2, s2, -0x1
    slli s0, s0, 0x4
    bgt s2, zero, L_30_while
L_32_while_end:
ret_io_uart_write_num:
    lw ra, 0(sp)
    lw s2, 4(sp)
    lw s1, 8(sp)
    lw s0, 12(sp)
    addi sp, sp, 0x10
    ret
draw_sky:
    addi sp, sp, -0x4
    sw ra, 0(sp)
    li a0, 0x7EF
    li a1, 0x7EF
    jal draw_set_color
    li a0, 0x20
    jal draw_fill_background
ret_draw_sky:
    lw ra, 0(sp)
    addi sp, sp, 0x4
    ret
draw_fill_background:
    li t0, 0x8000
    li t1, 0x0
    li t2, 0x0
    li tp, 0x1E
    bge t2, tp, L_36_while_end
L_33_while:
    li tp, 0x50
    bge t1, tp, L_35_while_end
L_34_while:
    sw a0, 0(t0)
    addi t0, t0, 0x4
    addi t1, t1, 0x1
    li tp, 0x50
    blt t1, tp, L_34_while
L_35_while_end:
    li t1, 0x0
    li tp, 0xFFFFFE03
    and t0, t0, tp
    addi t0, t0, 0x200
    addi t2, t2, 0x1
    li tp, 0x1E
    blt t2, tp, L_33_while
L_36_while_end:
ret_draw_fill_background:
    ret
draw_heights:
    addi sp, sp, -0x18
    sw ra, 0(sp)
    sw s4, 4(sp)
    sw s3, 8(sp)
    sw s2, 12(sp)
    sw s1, 16(sp)
    sw s0, 20(sp)
    la tp, glob_11_heights_progress
    lw t0, 0(tp)
    li tp, 0x21
    bge t0, tp, L_37_else
    li s0, 0x50
    li s1, 0x0
    b L_38_if_end
L_37_else:
    li s0, 0x71
    sub s0, s0, t0
    addi s1, t0, -0x21
L_38_if_end:
    la s2, glob_3_heights
    slli t0, t0, 0x2
    add s2, s2, t0
    srli t0, t0, 0x2
    addi t0, t0, 0x1
    li tp, 0x71
    blt t0, tp, L_39_else
    li t0, 0x0
L_39_else:
    la tp, glob_11_heights_progress
    sw t0, 0(tp)
    li s4, 0x0
draw_heights_loop:
    beq s0, zero, L_41_while_end
L_40_while:
    lw s3, 0(s2)
    mv a0, s4
    mv a1, s3
    jal draw_height
    addi s4, s4, 0x1
    addi s2, s2, 0x4
    addi s0, s0, -0x1
    bne s0, zero, L_40_while
L_41_while_end:
    beq s1, zero, L_42_else
    mv s0, s1
    li s1, 0x0
    la s2, glob_3_heights
    b draw_heights_loop
L_42_else:
ret_draw_heights:
    lw ra, 0(sp)
    lw s4, 4(sp)
    lw s3, 8(sp)
    lw s2, 12(sp)
    lw s1, 16(sp)
    lw s0, 20(sp)
    addi sp, sp, 0x18
    ret
draw_height:
    addi sp, sp, -0x14
    sw ra, 0(sp)
    sw s3, 4(sp)
    sw s2, 8(sp)
    sw s1, 12(sp)
    sw s0, 16(sp)
    mv s0, a1
    li s1, 0xBA00
    li s2, 0x0
    li s3, 0x20
    slli a0, a0, 0x2
    or s1, s1, a0
    li a0, 0x0
    li a1, 0x292
    jal draw_set_color
    li tp, 0xD
    bge s2, tp, L_44_while_end
L_43_while:
    sw s3, 0(s1)
    addi s2, s2, 0x1
    bge s2, s0, dh_done
    addi s1, s1, -0x200
    li tp, 0xD
    blt s2, tp, L_43_while
L_44_while_end:
    li a0, 0x0
    li a1, 0x999
    jal draw_set_color
    li tp, 0x14
    bge s2, tp, L_46_while_end
L_45_while:
    sw s3, 0(s1)
    addi s2, s2, 0x1
    bge s2, s0, dh_done
    addi s1, s1, -0x200
    li tp, 0x14
    blt s2, tp, L_45_while
L_46_while_end:
    li a0, 0x0
    li a1, 0xFFF
    jal draw_set_color
    bge s2, s0, L_48_while_end
L_47_while:
    sw s3, 0(s1)
    addi s2, s2, 0x1
    addi s1, s1, -0x200
    blt s2, s0, L_47_while
L_48_while_end:
dh_done:
ret_draw_height:
    lw ra, 0(sp)
    lw s3, 4(sp)
    lw s2, 8(sp)
    lw s1, 12(sp)
    lw s0, 16(sp)
    addi sp, sp, 0x14
    ret
