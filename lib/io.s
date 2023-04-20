.data
glob_1_last_buttons:
    .word 0x0
.text
test_io:
    addi sp, sp, -0x4
    sw ra, 0(sp)
L_2_while:
    jal io_wait_for_button_press
    b L_2_while
L_3_while_end:
ret_test_io:
    lw ra, 0(sp)
    addi sp, sp, 0x4
    ret
io_set_leds:
    li t0, 0x1F00
    sw a0, 0(t0)
ret_io_set_leds:
    ret
io_set_7seg:
    li t0, 0x1F18
    sw a0, 0(t0)
ret_io_set_7seg:
    ret
io_get_buttons:
    li t0, 0x1F24
    lw a0, 0(t0)
ret_io_get_buttons:
    ret
io_get_pressed_buttons:
    li t0, 0x1F24
    lw t1, 0(t0)
    la tp, glob_1_last_buttons
    lw t2, 0(tp)
    xori t2, t2, -0x1
    and t2, t2, t1
    la tp, glob_1_last_buttons
    sw t1, 0(tp)
    mv a0, t2
    b ret_io_get_pressed_buttons
ret_io_get_pressed_buttons:
    ret
io_wait_for_button_press:
    addi sp, sp, -0x4
    sw ra, 0(sp)
    li a0, 0x0
L_4_while:
    bne a0, zero, L_5_while_end
    jal io_get_pressed_buttons
    b L_4_while
L_5_while_end:
ret_io_wait_for_button_press:
    lw ra, 0(sp)
    addi sp, sp, 0x4
    ret
