.text
.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	movl $7, %eax
	leave
	ret
/* end function main */

