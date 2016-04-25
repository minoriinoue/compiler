	.section	__TEXT,__text,regular,pure_instructions # To declare this is code. Directive. Order to assembler.
	.macosx_version_min 10, 11
	.globl	_main ## Declares starting point
	.align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## BB#0:
	pushq	%rbp ## Remember the register the base pointer points at.
Ltmp0:
	.cfi_def_cfa_offset 16
Ltmp1:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp ## set base and stack pointer at the same spot.
Ltmp2:
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp  ## substract 48 from stack pointer. substract [value] [from]
	movl	$0, -4(%rbp) ## the end of argv array.
	movl	%edi, -8(%rbp) ## argv[1]
	movq	%rsi, -16(%rbp) ## argv[0]
	cmpl	$2, -8(%rbp) ## compare 2 and the value of the argument stored in -8(%rbp)
	je	LBB0_2 ## jump if equal
## BB#1:
	leaq	L_.str(%rip), %rdi # rdi = register for arguments to pass
	movb	$0, %al ## %al = The lowest 8 bit
	callq	_printf
	movl	$0, -4(%rbp) ## Q. why again?
	movl	%eax, -32(%rbp) ## 4-byte Spill. %eax is the place to put return value.
	jmp	LBB0_7 ## jump with no condition. 'go to' in C
LBB0_2:
	movq	-16(%rbp), %rax
	movq	8(%rax), %rdi # %rax = -16(%rbp), 8(%rax) = 8(-16(%rbp)) => %rdi(the argument to pass to the next function) is now same as the value -8(%rbp), argv[1]. Q. why double standard??
	callq	_atoi
	movl	%eax, -20(%rbp) # set the return value of _atoi to -20(%rbp)
	movl	$0, -24(%rbp) # set 0 to -24(%rbp) which corresponds to sum.
	movl	$0, -28(%rbp) # set 0 to -28(%rbp) which corresponds to i in the loop
LBB0_3:                                 ## =>This Inner Loop Header: Depth=1
	movl	-28(%rbp), %eax # set the return value %eax to 0
	cmpl	-20(%rbp), %eax ## compare the return value with the return value of atoi.
	jge	LBB0_6 ## jump if greater or equal to num_loop.
## BB#4:                                ##   in Loop: Header=BB0_3 Depth=1
	movl	-28(%rbp), %eax # move value of i(-28(%rbp)) to %eax
	addl	-24(%rbp), %eax  ## add the value of sum to %eax
	movl	%eax, -24(%rbp) ## set the summated value (sum and i) in %eax back to variable sum.
## BB#5:                                ##   in Loop: Header=BB0_3 Depth=1
	movl	-28(%rbp), %eax # move value of i to %eax
	addl	$1, %eax         ## add value 1 to i (%eax)
	movl	%eax, -28(%rbp) # set back the summated i (%eax) to variable i.
	jmp	LBB0_3 # jump with no condition back to the beginning of the loop.
LBB0_6:
	leaq	L_.str.1(%rip), %rdi # set the string to the register for aguments for the coming function.
	movl	-24(%rbp), %esi # set the result of summation 'sum' to %esi
	movb	$0, %al
	callq	_printf # call the function printf
	movl	%eax, -36(%rbp) ## set the return value %eax to -36(%rbp). 4-byte Spill
LBB0_7:
	movl	-4(%rbp), %eax ## move 0 (-4(%rbp)) to %eax
	addq	$48, %rsp # add 48 back to the stack (stating) pointer.
	popq	%rbp # set back the stack base pointer to the starting point.
	retq
	.cfi_endproc

	.section	__TEXT,__cstring,cstring_literals
L_.str:                                 ## @.str
	.asciz	"Wrong number of arguments.\n" ## .asciz: Declares to put strings.

L_.str.1:                               ## @.str.1
	.asciz	"sum = %d\n"


.subsections_via_symbols
