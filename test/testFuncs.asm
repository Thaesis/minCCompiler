		.data
st177:		.asciiz "Hello from main1\n"
st370:		.asciiz "arg1="
st683:		.asciiz "Hello from main3\n"
st907:		.asciiz "\n"
st930:		.asciiz "Hello from main2\n"
a:        .space 12 # 12 char array

		.text
		.globl main
main:
		sw $fp, 0($sp)		# prev fp
		sw $ra, -4($sp)		# prev ra
		addu $fp, $zero, $sp	# curr fp
		addiu $sp, $sp, -32	# function stack (main)

        li $t0, 99  # 'c'
		la $t1, a		# get address of var
		sb $t0, 0($t1)	# assign a[0]
        li $t0, 104  # 'h'
		sb $t0, 1($t1)	# assign a[1]
        li $t0, 97  # 'a'
		sb $t0, 2($t1)	# assign a[2]
        li $t0, 114  # 'r'
		sb $t0, 3($t1)	# assign a[3]
		jal start_main1
					# return value -> $t0
		lw $t0, 4($sp)		# restore i from stack	# 1st arg
		jal start_main2
					# return value -> $t0
		lw $t0, 16($sp)		# restore v from stack	# 1st arg
### array variable not indexed ###

		la $a1, a		# get address of var	# 2nd arg
		jal start_main3

end_main:
		addiu $sp, $sp, 32	# restore sp, exiting function
		lw $fp, 0($sp)
		lw $ra, -4($sp)
		addiu $v0, $zero, 10
		syscall
###
###
start_main1:
		sw $fp, 0($sp)		# prev fp
		sw $ra, -4($sp)		# prev ra
		addu $fp, $zero, $sp	# curr fp
		addiu $sp, $sp, -16	# function stack (main1)

		la $a0, st177
		addiu $v0, $zero, 4	# print string
		syscall			# do the print
		addiu $t0, $zero, 0	# (return val in $t0)
		j end_main1		# exit function

end_main1:
		addiu $sp, $sp, 16	# restore sp, exiting function
		lw $fp, 0($sp)
		lw $ra, -4($sp)
		jr $ra
###
###
start_main2:
		sw $fp, 0($sp)		# prev fp
		sw $ra, -4($sp)		# prev ra
		addu $fp, $zero, $sp	# curr fp
		addiu $sp, $sp, -16	# function stack (main2)

		la $a0, st930
		addiu $v0, $zero, 4	# print string
		syscall			# do the print
		la $a0, st370
		syscall			# do the print
		lw $a0, 4($sp)		# restore arg1 from stack
		addiu $v0, $zero, 1	# print integer
		syscall			# do the print
		la $a0, st907
		addiu $v0, $zero, 4	# print string
		syscall			# do the print
		addiu $t0, $zero, 99	# (return val in $t0)
		j end_main2		# exit function

end_main2:
		addiu $sp, $sp, 16	# restore sp, exiting function
		lw $fp, 0($sp)
		lw $ra, -4($sp)
		jr $ra
###
###
start_main3:
		sw $fp, 0($sp)		# prev fp
		sw $ra, -4($sp)		# prev ra
		addu $fp, $zero, $sp	# curr fp
		addiu $sp, $sp, -24	# function stack (main3)

		la $a0, st683
		addiu $v0, $zero, 4	# print string
		syscall			# do the print
end_main3:
		addiu $sp, $sp, 24	# restore sp, exiting function
		lw $fp, 0($sp)
		lw $ra, -4($sp)
		jr $ra
###
