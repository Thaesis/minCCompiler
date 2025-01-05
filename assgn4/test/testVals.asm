		.data
a:    .space 20 # 5 int array
w:    .space 60 # 15 int array
z:    .space 12 # 12 char array
		.text
###
main:
		sw $fp, 0($sp)		# prev fp
		sw $ra, -4($sp)		# prev ra
		addu $fp, $zero, $sp	# curr fp
		addiu $sp, $sp, -32	# function stack (main)

		addiu $t0, $zero, 1	# assign b
		addu $a0, $zero, $t0
		addiu $v0, $zero, 1	# print integer
		syscall			# do the print
		addiu $a0, $zero, 10
		addiu $v0, $zero, 11	# print char
		syscall			# do the print
		li $t2, 2		# int 2
		la $t3, w		# get address of var
		sw $t2, 44($t3)		# assign w[11]
		addu $a0, $zero, $t2
		addiu $v0, $zero, 1	# print integer
		syscall			# do the print
		addiu $a0, $zero, 10
		addiu $v0, $zero, 11	# print char
		syscall			# do the print
		la $t5, a		# get address of var
		addiu $t2, $t2, 1	# w[11] + 1
		sw $t2, 4($t5)		# ret = lhs <op> rhs (assign a[1])
		addu $a0, $zero, $t2
		addiu $v0, $zero, 1	# print integer
		syscall			# do the print
		addiu $a0, $zero, 10
		addiu $v0, $zero, 11	# print char
		syscall			# do the print
		addiu $a0, $zero, 4
		addiu $v0, $zero, 1	# print integer
		syscall			# do the print
		addiu $a0, $zero, 10
		addiu $v0, $zero, 11	# print char
		syscall			# do the print
end_main:
		addiu $sp, $sp, 32	# restore sp, exiting function
		lw $fp, 0($sp)
		lw $ra, -4($sp)
		addiu $v0, $zero, 10
		syscall
###
